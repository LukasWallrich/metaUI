// Real standalone Shiny process; independent numerical reference fits live in tests/.
const { chromium } = require(process.env.METAUI_PLAYWRIGHT || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { spawn, spawnSync } = require('node:child_process');
const root = process.env.METAUI_SMOKE_DIR;
assert(root, 'Set METAUI_SMOKE_DIR to the fresh browser-fixture.R output directory');
const evidence = path.join(root, 'evidence');
fs.mkdirSync(evidence, {recursive:true});
const port = Number(process.env.METAUI_SMOKE_PORT || 8765);
const url = `http://127.0.0.1:${port}/`;
const log = fs.createWriteStream(path.join(evidence, 'shiny.log'));
const server = spawn('Rscript', ['--vanilla','-e',
  `shiny::runApp('.',host='127.0.0.1',port=${port},launch.browser=FALSE)`],
  {cwd:path.join(root,'app'),env:process.env});
server.stdout.pipe(log); server.stderr.pipe(log);
let serverError; server.on('error', e => {serverError=e;});
let browser, page;
const errors = [];
async function waitServer() {
  for(let i=0;i<120;i++) {
    if(serverError) throw serverError;
    if(server.exitCode !== null) throw Error(`Shiny exited with ${server.exitCode}; see shiny.log`);
    try {
      const response=await fetch(url);
      if(response.ok) {
        if(!(await response.text()).includes("Synthetic browser smoke fixture"))
          throw Error("Selected port serves another application; choose METAUI_SMOKE_PORT");
        return;
      }
    } catch(error) { if(error.message.includes("Selected port")) throw error; }
    await new Promise(resolve=>setTimeout(resolve,250));
  }
  throw Error('Shiny did not become reachable in 30 seconds');
}
async function settledSample(effects) {
  await page.waitForFunction(k=> {
    const overview=document.querySelector('#selection_overview')?.textContent.trim() ?? '';
    return overview.startsWith(k+' of ') && !document.querySelector('.shiny-busy');
  }, effects, {timeout:60000});
}
async function saveDownload(selector, name) {
  const pending = page.waitForEvent('download');
  await page.locator(selector).click();
  const target = path.join(evidence, name);
  await (await pending).saveAs(target);
  return target;
}
// Exported code runs in a fresh R process against the workbook of the same analysis.
function reproduce(script, workbook, selected) {
  const run = spawnSync('Rscript', ['--vanilla', path.basename(script), path.basename(workbook)],
    {cwd:evidence, env:process.env, encoding:'utf8', timeout:180000});
  const output = (run.stdout || '') + (run.stderr || '');
  fs.writeFileSync(script + '.log', output);
  assert.equal(run.status, 0, 'exported script failed: ' + output);
  assert(output.includes('Agrees with the app'), 'exported script disagrees with the app: ' + output);
  assert(output.includes(`Selected ${selected} of`), 'exported script selected other rows: ' + output);
  assert(!/^Warning/m.test(output), 'exported script warned: ' + output);
}
(async()=>{
  await waitServer();
  browser=await chromium.launch({headless:true,
    ...(process.env.METAUI_CHROMIUM ? {executablePath:process.env.METAUI_CHROMIUM}: {})});
  page=await browser.newPage({viewport:{width:1440,height:1000}});
  page.on('pageerror',e=>errors.push(e.message));
  await page.goto(url);
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
  await page.getByText('Select effects', {exact:true}).waitFor();
  await page.waitForFunction(()=>Array.isArray(window.Shiny.shinyapp.$inputValues.outliers_z_scores));
  const helpButton = page.getByRole('button',{name:'Help for Year'});
  const helpPanel = page.locator('#' + await helpButton.getAttribute('aria-controls'));
  await helpButton.focus();
  await page.keyboard.press('Enter');
  await helpPanel.waitFor();
  assert.equal(await helpPanel.innerHTML(), '<b>Year</b> help');
  await page.keyboard.press('Escape');
  await helpPanel.waitFor({state:'hidden'});
  await page.locator('#go').click(); await settledSample(16);
  await page.getByText('Selection details', {exact:true}).click();
  assert((await page.locator('#effectestimate').innerText()).includes('unsupported'));
  await page.locator('#go').click();
  await page.waitForFunction(()=>/Reused|Precomputed/.test(document.querySelector('#calculation_status')?.textContent));
  await page.getByRole('tab',{name:'Sample',exact:true}).click();
  await page.locator('#sample_overview table').waitFor();
  assert((await page.locator('#sample_overview').innerText()).includes('16'));
  await page.getByRole('tab',{name:'Forest plot',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#foreststudies table'));
  for (const [id, extension] of [['forest_pdf','pdf'], ['forest_png','png'], ['forest_csv','csv']]) {
    const pending = page.waitForEvent('download');
    await page.locator('#' + id).click();
    const exported = await pending;
    const target = path.join(evidence, 'forest.' + extension);
    await exported.saveAs(target);
    const bytes = fs.readFileSync(target);
    assert(bytes.length > 0, id + ' produced an empty file');
    if (extension === 'pdf') assert.equal(bytes.subarray(0,5).toString(), '%PDF-', id + ' is not a PDF');
    if (extension === 'png') assert.equal(bytes.subarray(0,8).toString('hex'), '89504e470d0a1a0a', id + ' is not a PNG');
    if (extension === 'csv') assert(bytes.toString().includes('Selected effect'), id + ' is missing effect rows');
  }
  await page.getByRole('tab',{name:'Summary',exact:true}).click();
  await page.getByText('Assess practical equivalence', {exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('Smallest bound'));
  await page.locator('#sesoi').fill('1');
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('Interval within bounds'));
  const equivalenceHeader = await page.locator('#equivalence thead').innerText();
  for (const column of ['Assessment','Smallest bound']) assert(equivalenceHeader.includes(column), 'equivalence table lost column ' + column);
  assert(await page.locator('#equivalence tbody tr').count() >= 2, 'expected one assessment row per model');
  assert.equal((await page.locator('#apply_state').innerText()).trim(), '');
  await page.getByText('Download data or code, upload data', {exact:true}).click();
  const downloadEvent=page.waitForEvent('download');
  await page.locator('#downloadData').click();
  const download=await downloadEvent;
  await download.saveAs(path.join(evidence,'round-trip.xlsx'));
  await page.locator('.metaui-data-tools').screenshot({path:path.join(evidence,'data-and-code-downloads.png')});
  const script = await saveDownload('#downloadCode', 'selection.R');
  const quarto = await saveDownload('#downloadQuarto', 'selection.qmd');
  assert(fs.readFileSync(quarto, 'utf8').startsWith('---\ntitle: "Reproduce a metaUI selection'), 'Quarto download lacks its header');
  assert(fs.readFileSync(script, 'utf8').includes('bound <- 1'), 'R script lost the equivalence bound');
  reproduce(script, path.join(evidence,'round-trip.xlsx'), 16);
  // Clear the bound so the upload has to restore it.
  await page.locator('#sesoi').fill('');
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('Smallest bound'));
  await page.locator('#uploadData').setInputFiles(path.join(evidence,'round-trip.xlsx'));
  await page.waitForFunction(()=>document.querySelector('#uploadData_progress')?.textContent.includes('Upload complete'));
  await page.locator('#executeUpload').click();
  await page.waitForFunction(()=>document.querySelector('#selection_status')?.textContent.includes('Uploaded data:'));
  await settledSample(16);
  assert.equal(await page.locator('#sesoi').inputValue(), '1', 'upload did not restore the equivalence bound');
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('Interval within bounds'));
  await page.locator('#uploadData').setInputFiles(path.join(root,'upload.xlsx'));
  await page.waitForFunction(()=>document.querySelector('#uploadData_progress')?.textContent.includes('Upload complete'));
  await page.locator('#executeUpload').click(); await settledSample(4);
  const selected=await page.locator('#selection_status').innerText();
  assert(selected.includes('Selected 4 of 16')); assert(selected.includes('Excluded 12'));
  const choices=await page.locator('#metaUI__filter_Group').evaluate(el=>[...el.selectedOptions].map(x=>x.value));
  assert.deepEqual(choices,['G1','G2']);
  await page.screenshot({path:path.join(evidence,'uploaded-summary.png')});
  const uploadedWorkbook = await saveDownload('#downloadData', 'uploaded.xlsx');
  const uploadedScript = await saveDownload('#downloadCode', 'uploaded-selection.R');
  assert(fs.readFileSync(uploadedScript, 'utf8').includes('uploaded data (upload.xlsx'), 'R script does not disclose uploaded data');
  reproduce(uploadedScript, uploadedWorkbook, 4);
  // A single excluded year makes the next result three effects.
  await page.locator('#metaUI__filter_Year_include_NA').uncheck();
  // This output starts hidden; it must resume when a filter changes.
  await page.locator('#apply_state_box').waitFor({state:'visible'});
  await page.waitForFunction(()=>document.querySelector('#apply_state')?.textContent.includes('Filters changed'));
  await page.locator('#go').click(); await settledSample(3);
  await page.locator('#apply_state_box').waitFor({state:'hidden'});
  await page.locator('#resetFilters').click();
  await page.waitForFunction(()=>document.querySelector('#metaUI__filter_Group').selectedOptions.length===8);
  await page.locator('#go').click(); await settledSample(15);
  // Reset restores the built year range, deliberately excluding the new 1900 row.
  assert((await page.locator('#selection_status').innerText()).includes('Excluded 1'));
  assert.deepEqual(errors,[]);
  assert.equal(await page.locator('.shiny-output-error').count(),0);
  await page.setViewportSize({width:390,height:844});
  await page.screenshot({path:path.join(evidence,'mobile.png')});
  // Crowded moderator levels: the data here are too few to need it, so a table is added to the page.
  const moderatorTable = (dots) => page.evaluate((dots) => {
    document.querySelector('#smoke-moderators')?.remove();
    const row = (n) => '<tr><td>Level</td><td class="metaui-track-cell"><div class="metaui-track">' +
      Array.from({length:n},(_,i)=>`<i class="metaui-track-dot" style="left:${10+(i%40)*2}%;width:6px;height:6px"></i>`).join('') + '</div></td></tr>';
    document.body.insertAdjacentHTML('beforeend',`<table id="smoke-moderators" class="table metaui-estimates metaui-moderators" style="width:360px"><tbody>${dots.map(row).join('')}</tbody></table>`);
  }, dots);
  const moderatorState = () => page.evaluate(() => {
    const table = document.querySelector('#smoke-moderators');
    return [...table.tBodies[0].rows].map(row => ({
      bars:[...row.querySelectorAll('.metaui-track-bar')].reduce((sum,bar)=>sum+Number(bar.dataset.count),0),
      dots:[...row.querySelectorAll('.metaui-track-dot')].filter(dot=>dot.getBoundingClientRect().width>0).length,
      height:row.getBoundingClientRect().height}));
  });
  const redraw = async () => { await page.evaluate(()=>$(document).trigger('shown.bs.tab')); await page.waitForTimeout(250); };
  await page.setViewportSize({width:390,height:844});
  await moderatorTable([30,3]); await redraw();
  let levels = await moderatorState();
  assert.equal(levels[0].bars,30,'a crowded level should count every effect in its bars');
  assert.equal(levels[0].dots,0,'a crowded level should hide its dots');
  assert.equal(levels[1].dots,3,'a small level should keep its dots');
  assert(levels[0].height < 90,'a crowded phone row should stay short: '+levels[0].height);
  await page.setViewportSize({width:1200,height:844}); await page.waitForTimeout(400);
  levels = await moderatorState();
  assert.equal(levels[0].bars,0,'30 effects should not make a histogram on a larger screen');
  assert.equal(levels[0].dots,30,'dots should return on a larger screen');
  await moderatorTable([120]); await redraw();
  levels = await moderatorState();
  assert.equal(levels[0].bars,120,'a very crowded level should make a histogram on any screen');
  // Drawn while hidden, it has no width until shown.
  await page.setViewportSize({width:390,height:844});
  await moderatorTable([30]);
  await page.evaluate(()=>{document.querySelector('#smoke-moderators').style.display='none';}); await redraw();
  assert.equal((await moderatorState())[0].bars,0,'a hidden table should wait for its width');
  await page.evaluate(()=>{document.querySelector('#smoke-moderators').style.display='';}); await redraw();
  assert.equal((await moderatorState())[0].bars,30,'a table should draw its histogram once shown');
  await page.evaluate(()=>document.querySelector('#smoke-moderators').remove());
  await page.setViewportSize({width:1440,height:1000});
  fs.writeFileSync(path.join(evidence,'result.json'),JSON.stringify({
    ok:true,checks:['standalone startup','analysis','cache disclosure','forest render and PDF/PNG/CSV exports','practical interval assessment',
      'keyboard inline filter help','sample summary on its tab','download/upload round trip with equivalence bound','R/Quarto code export reproduced in fresh R (built and uploaded data)','picker and missing-value restoration','numeric filtering','reset'],
    uploaded_selection:selected,js_errors:errors},null,2));
  console.log('Generated-app browser smoke checks passed.');
})().catch(async error=>{
  console.error(error);
  if(page) {await page.screenshot({path:path.join(evidence,'failure.png')}).catch(()=>{});
    fs.writeFileSync(path.join(evidence,'failure.txt'),await page.locator('body').innerText().catch(()=>''));
    fs.writeFileSync(path.join(evidence,'failure-details.json'),JSON.stringify({errors,dom:await page.evaluate(()=>({download:document.querySelector('#executeDownload')?.outerHTML,go:window.Shiny?.shinyapp?.$inputValues?.go})).catch(()=>null)},null,2));}
  process.exitCode=1;
}).finally(async()=>{if(browser) await browser.close(); server.kill('SIGTERM');log.end();});
