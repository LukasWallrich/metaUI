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
    return (overview.startsWith(k+' of ') || overview.startsWith('Results use '+k+' of ')) && !document.querySelector('.shiny-busy');
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
  // Saved opening results must stay until the server's own values replace them.
  await page.addInitScript(()=>{
    window.__metauiBlanked=[];
    document.addEventListener('DOMContentLoaded',()=>{
      const saved=[...document.querySelectorAll('[data-metaui-opening]')];
      window.__metauiOpening=saved.length;
      new MutationObserver(()=>saved.forEach(el=>{
        if(el.hasAttribute('data-metaui-opening')&&!el.textContent.trim()) window.__metauiBlanked.push(el.id);
      })).observe(document.body,{childList:true,subtree:true,characterData:true});
    });
  });
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
  assert(await page.evaluate(()=>window.__metauiOpening) > 0, 'the fixture should open on saved results');
  assert.deepEqual(await page.evaluate(()=>window.__metauiBlanked), [], 'saved opening results were blanked');
  await page.getByText('Selection details', {exact:true}).click();
  assert((await page.locator('#effectestimate').innerText()).includes('Not estimated:'));
  await page.locator('#go').click();
  await page.waitForFunction(()=>/Reused|Precomputed/.test(document.querySelector('#calculation_status')?.textContent));
  // Tabs are drawn in the background, so a reader who waits opens them finished.
  await page.waitForFunction(()=>document.querySelector('#sample_overview table'),null,{timeout:120000});
  await page.waitForFunction(()=>!document.querySelector('.metaui-preloading')&&!document.documentElement.classList.contains('metaui-background'),null,{timeout:120000});
  await page.getByRole('tab',{name:'Sample',exact:true}).click();
  assert.equal(await page.locator('.metaui-pane-loading').count(),0);
  await page.locator('#sample_overview table').waitFor();
  assert((await page.locator('#sample_overview').innerText()).includes('16'));
  // One row per effect or per study.
  await page.waitForFunction(()=>/of 16 effects/.test(document.querySelector('#sample_table .dataTables_info')?.textContent||''),null,{timeout:60000});
  await page.locator('.metaui-sample-view[data-value="studies"]').click();
  await page.waitForFunction(()=>/of 8 studies/.test(document.querySelector('#sample_table .dataTables_info')?.textContent||''),null,{timeout:60000});
  assert(/ to /.test(await page.locator('#sample_table tbody').innerText()), 'study rows should give the range of their effect sizes');
  await page.locator('.metaui-sample-view[data-value="effects"]').click();
  await page.waitForFunction(()=>/of 16 effects/.test(document.querySelector('#sample_table .dataTables_info')?.textContent||''),null,{timeout:60000});
  // Copy link puts a link to this selection on the clipboard, or shows it for copying by hand.
  await page.locator('.metaui-copy-link').click();
  await page.waitForFunction(()=>{const s=document.querySelector('.metaui-copy-status');return s&&(s.textContent==='Copied'||/metaui=/.test(s.querySelector('input')?.value||''));});
  await page.getByRole('tab',{name:'Forest plot',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#foreststudies table'));
  // View switches: every effect opens in the browser; the order comes back from the server, still showing every effect.
  await page.locator('.metaui-forest-all[data-value="effects"]').click();
  await page.waitForFunction(()=>!document.querySelector('#foreststudies .metaui-forest-effect.metaui-forest-closed'));
  await page.locator('.metaui-forest-sort[data-value="estimate"]').click();
  await page.waitForFunction(()=>document.querySelector('.metaui-forest-sort[data-value="estimate"]')?.getAttribute('aria-pressed')==='true'&&document.querySelector('.metaui-forest-all[data-value="effects"]')?.getAttribute('aria-pressed')==='true'&&!document.querySelector('#foreststudies .metaui-forest-effect.metaui-forest-closed'),null,{timeout:60000});
  await page.locator('.metaui-forest-sort[data-value="name"]').click();
  await page.locator('.metaui-forest-all[data-value="studies"]').click();
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
  // Moderator tables open on the level estimates, with individual effects on a toggle.
  await page.getByRole('tab',{name:'Moderation',exact:true}).click();
  await page.evaluate(()=>{ const select=$('#moderator'); if(select[0].selectize) select[0].selectize.setValue('metaUI__filter_Group'); else select.val('metaUI__filter_Group').trigger('change'); });
  await page.locator('table.metaui-moderators').first().waitFor({timeout:60000});
  assert(await page.locator('table.metaui-moderators-plain').count() > 0, 'moderator tables should open without individual effects');
  assert.equal(await page.locator('.metaui-moderator-effects[data-value="hide"]').first().getAttribute('aria-pressed'), 'true');
  // Showing them redraws the table with its dots, and the switch keeps the new choice.
  await page.locator('.metaui-moderator-effects[data-value="show"]').first().click();
  await page.waitForFunction(()=>document.querySelector('table.metaui-moderators:not(.metaui-moderators-plain)')&&document.querySelector('.metaui-moderator-effects[data-value="show"]')?.getAttribute('aria-pressed')==='true',null,{timeout:60000});
  await page.locator('.metaui-moderator-effects[data-value="hide"]').first().click();
  await page.waitForFunction(()=>document.querySelector('table.metaui-moderators-plain'),null,{timeout:60000});
  await page.getByRole('tab',{name:'Summary',exact:true}).click();
  await page.getByText('Assess practical equivalence', {exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('90% interval lies within'));
  await page.locator('#sesoi').fill('1');
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('Interval within bounds'));
  const equivalenceHeader = await page.locator('#equivalence thead').innerText();
  for (const column of ['Assessment','90% interval lies within']) assert(equivalenceHeader.includes(column), 'equivalence table lost column ' + column);
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
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('90% interval lies within'));
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
  await page.waitForFunction(()=>document.querySelector('#metaUI__filter_Year').closest('.shiny-input-container').classList.contains('metaui-narrowed'),
    null, {timeout:5000});
  // This output starts hidden; it must resume when a filter changes.
  await page.locator('#apply_state_box').waitFor({state:'visible'});
  await page.waitForFunction(()=>document.querySelector('#apply_state')?.textContent.includes('Update results to apply'));
  // A tab opened straight after Update shows its message, never the previous selection.
  await page.getByRole('tab',{name:'Summary',exact:true}).click();
  await page.evaluate(()=>{
    window.__stale=false; window.__fresh=false;
    $(document).on('shiny:value.smoke', e=>{ if(e.name==='sample_overview') window.__fresh=true; });
    const pane=document.querySelector('#sample_overview').closest('.tab-pane');
    const watch=()=>{
      if(pane.classList.contains('active')&&!pane.classList.contains('metaui-pane-loading')&&!window.__fresh) window.__stale=true;
      if(!window.__fresh||pane.classList.contains('metaui-pane-loading')) requestAnimationFrame(watch);
    };
    requestAnimationFrame(watch);
  });
  await page.locator('#go').click();
  await page.getByRole('tab',{name:'Sample',exact:true}).click();
  assert.equal(await page.locator('.metaui-pane-loading .metaui-pane-status[role=status]').count(),1,'an early tab should show its status message');
  await settledSample(3);
  await page.waitForFunction(()=>window.__fresh&&!document.querySelector('.metaui-pane-loading'),null,{timeout:60000});
  assert.equal(await page.evaluate(()=>window.__stale),false,'Sample showed before its new values arrived');
  await page.evaluate(()=>$(document).off('shiny:value.smoke'));
  await page.getByRole('tab',{name:'Summary',exact:true}).click();
  await page.locator('#apply_state_box').waitFor({state:'hidden'});
  await page.locator('#resetFilters').click();
  await page.waitForFunction(()=>document.querySelector('#metaUI__filter_Group').selectedOptions.length===8);
  await page.locator('#go').click(); await settledSample(15);
  // Reset restores the built year range, deliberately excluding the new 1900 row.
  assert((await page.locator('#selection_status').innerText()).includes('Excluded 1'));
  assert.deepEqual(errors,[]);
  // Tabs drawn in the background may report that this small selection is too small for some methods.
  const failed = await page.locator('.shiny-output-error:not(.shiny-output-error-validation)').evaluateAll(els=>els.map(el=>el.id+': '+el.textContent.trim().slice(0,200)));
  assert.deepEqual(failed,[],'outputs failed');
  const validation = await page.locator('.shiny-output-error-validation').allInnerTexts();
  const expected = [/^Egger test requires at least three studies\.$/, /^Z-curve could not be estimated for this selection; reason below\.$/, /^P-curve requires explicit effect direction; it is not inferred\.$/, /^Moderator model could not be estimated for this selection: /];;
  for (const text of validation) assert(expected.some(rx=>rx.test(text.trim())), 'unexpected validation message: ' + text);
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
