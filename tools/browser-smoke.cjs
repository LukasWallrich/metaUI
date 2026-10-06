// Real standalone Shiny process; independent numerical reference fits live in tests/.
const { chromium } = require(process.env.METAUI_PLAYWRIGHT || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { spawn } = require('node:child_process');
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
    const table=document.querySelector('#sample table');
    if (!table) return false;
    const column=[...table.querySelectorAll('thead th')].findIndex(th=>th.innerText.trim()==='Effects');
    const cell=table.querySelector('tbody tr')?.children[column];
    return column >= 0 && cell?.innerText.trim()===String(k) && !document.querySelector('.shiny-busy');
  }, effects, {timeout:60000});
}
(async()=>{
  await waitServer();
  browser=await chromium.launch({headless:true,
    ...(process.env.METAUI_CHROMIUM ? {executablePath:process.env.METAUI_CHROMIUM}: {})});
  page=await browser.newPage({viewport:{width:1440,height:1000}});
  page.on('pageerror',e=>errors.push(e.message));
  await page.goto(url);
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
  await page.getByRole('button',{name:'Dismiss',exact:true}).click();
  await page.waitForFunction(()=>Array.isArray(window.Shiny.shinyapp.$inputValues.outliers_z_scores));
  await page.getByRole('button',{name:'Help for Year'}).focus();
  await page.keyboard.press('Enter');
  await page.locator('#metaui-help-1:visible').waitFor();
  assert.equal(await page.locator('#metaui-help-1').innerHTML(), '<b>Year</b> help');
  await page.keyboard.press('Escape');
  await page.locator('#metaui-help-1').waitFor({state:'hidden'});
  await page.locator('#go').click(); await settledSample(16);
  assert((await page.locator('#effectestimate').innerText()).includes('unsupported'));
  await page.locator('#go').click();
  await page.waitForFunction(()=>document.querySelector('#calculation_status')?.textContent.includes('Reused'));
  const overview = await page.locator('#sample').innerText();
  await page.getByRole('tab',{name:'Sample',exact:true}).click();
  await page.locator('#sample_overview table').waitFor();
  assert.equal(await page.locator('#sample_overview').innerText(), overview);
  await page.getByRole('tab',{name:'Forest Plot',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#foreststudies img')?.naturalWidth>0);
  for (const [id, extension] of [['forest_pdf','pdf'], ['forest_png','png'], ['forest_csv','csv']]) {
    const pending = page.waitForEvent('download');
    await page.locator('#' + id).click();
    const exported = await pending;
    await exported.saveAs(path.join(evidence, 'forest.' + extension));
  }
  await page.getByRole('tab',{name:'Summary',exact:true}).click();
  await page.locator('#sesoi').fill('1');
  await page.waitForFunction(()=>document.querySelector('#equivalence')?.innerText.includes('Interval within bounds'));
  assert((await page.locator('#apply_state').innerText()).includes('Results match'));
  const downloadEvent=page.waitForEvent('download');
  await page.locator('#downloadData').click();
  const download=await downloadEvent;
  await download.saveAs(path.join(evidence,'round-trip.xlsx'));
  await page.locator('#uploadData').setInputFiles(path.join(evidence,'round-trip.xlsx'));
  await page.waitForFunction(()=>document.querySelector('#uploadData_progress')?.textContent.includes('Upload complete'));
  await page.locator('#executeUpload').click();
  await page.waitForFunction(()=>document.querySelector('#selection_status')?.textContent.includes('Uploaded data:'));
  await settledSample(16);
  await page.locator('#uploadData').setInputFiles(path.join(root,'upload.xlsx'));
  await page.waitForFunction(()=>document.querySelector('#uploadData_progress')?.textContent.includes('Upload complete'));
  await page.locator('#executeUpload').click(); await settledSample(4);
  const selected=await page.locator('#selection_status').innerText();
  assert(selected.includes('Selected 4 of 16')); assert(selected.includes('Excluded 12'));
  const choices=await page.locator('#metaUI__filter_Group').evaluate(el=>[...el.selectedOptions].map(x=>x.value));
  assert.deepEqual(choices,['G1','G2']);
  await page.screenshot({path:path.join(evidence,'uploaded-summary.png')});
  // A single excluded year makes the next result three effects.
  await page.locator('#metaUI__filter_Year_include_NA').uncheck();
  await page.locator('#go').click(); await settledSample(3);
  await page.locator('#resetFilters').click();
  await page.waitForFunction(()=>document.querySelector('#metaUI__filter_Group').selectedOptions.length===8);
  await page.locator('#go').click(); await settledSample(15);
  // Reset restores the built year range, deliberately excluding the new 1900 row.
  assert((await page.locator('#selection_status').innerText()).includes('Excluded 1'));
  assert.deepEqual(errors,[]);
  assert.equal(await page.locator('.shiny-output-error').count(),0);
  await page.setViewportSize({width:390,height:844});
  await page.screenshot({path:path.join(evidence,'mobile.png')});
  fs.writeFileSync(path.join(evidence,'result.json'),JSON.stringify({
    ok:true,checks:['standalone startup','analysis','cache disclosure','forest render and PDF/PNG/CSV exports','practical interval assessment',
      'keyboard inline filter help','sample summary on both tabs','download/upload round trip','picker and missing-value restoration','numeric filtering','reset'],
    uploaded_selection:selected,js_errors:errors},null,2));
  console.log('Generated-app browser smoke checks passed.');
})().catch(async error=>{
  console.error(error);
  if(page) {await page.screenshot({path:path.join(evidence,'failure.png')}).catch(()=>{});
    fs.writeFileSync(path.join(evidence,'failure.txt'),await page.locator('body').innerText().catch(()=>''));
    fs.writeFileSync(path.join(evidence,'failure-details.json'),JSON.stringify({errors,dom:await page.evaluate(()=>({download:document.querySelector('#executeDownload')?.outerHTML,go:window.Shiny?.shinyapp?.$inputValues?.go})).catch(()=>null)},null,2));}
  process.exitCode=1;
}).finally(async()=>{if(browser) await browser.close(); server.kill('SIGTERM');log.end();});
