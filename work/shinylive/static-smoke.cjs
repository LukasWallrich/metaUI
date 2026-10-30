// Exploration: drive a generated metaUI app either as a shinylive static export
// (MODE=static, app inside the shinylive iframe) or as a normal Shiny server
// (MODE=server). Records timings and saves downloads for numerical comparison.
// Usage: METAUI_PLAYWRIGHT=... URL=... MODE=static|server OUT=dir [BAYES=1] node static-smoke.cjs
const { chromium } = require(process.env.METAUI_PLAYWRIGHT || 'playwright');
const fs = require('node:fs');
const path = require('node:path');
const url = process.env.URL, mode = process.env.MODE || 'static';
const out = process.env.OUT; fs.mkdirSync(out, {recursive: true});
const bayes = process.env.BAYES === '1';
// Match the UI wait below: a contended machine can take >150 s under CDP.
const startupPolls = Math.ceil(Number(process.env.METAUI_STARTUP_TIMEOUT || 600000) / 250);
const timings = {}, notes = [], errors = [], consoleMsgs = [];
const t0 = Date.now();
const mark = (k) => { timings[k] = (Date.now() - t0) / 1000; console.log(k, timings[k]); };

async function appFrame(page) {
  if (mode === 'server') return page.mainFrame();
  for (let i = 0; i < startupPolls; i++) {
    const f = page.frames().find(fr => fr !== page.mainFrame() && /app_/.test(fr.url()));
    if (f) return f;
    await page.waitForTimeout(250);
  }
  throw Error('shinylive app iframe not found');
}
// Idle = Shiny not busy and no visible output recalculating, stable for 500 ms.
const idle = (app, timeout = 600000) => app.waitForFunction(() => {
  const busy = !window.Shiny?.shinyapp || document.documentElement.classList.contains('shiny-busy') ||
    [...document.querySelectorAll('.recalculating')].some(el => el.offsetParent !== null);
  if (busy) { window.__idleSince = null; return false; }
  window.__idleSince = window.__idleSince || Date.now();
  return Date.now() - window.__idleSince > 500;
}, null, {timeout, polling: 100});
// Fetch a download handler's URL from inside the app frame (through the service
// worker in shinylive). Used when the browser download path is not captured.
async function fetchDownload(app, id) {
  const b64 = await app.evaluate(async (id) => {
    const href = document.getElementById(id).href;
    const r = await fetch(href);
    if (!r.ok) throw Error('HTTP ' + r.status + ' for ' + href);
    const buf = new Uint8Array(await r.arrayBuffer());
    let bin = ''; for (let i = 0; i < buf.length; i += 0x8000) bin += String.fromCharCode(...buf.subarray(i, i + 0x8000));
    return btoa(bin);
  }, id);
  return Buffer.from(b64, 'base64');
}
async function timed(name, fn) { const s = Date.now(); await fn(); timings[name] = (Date.now() - s) / 1000; console.log(name, timings[name]); }

(async () => {
  const browser = await chromium.launch({headless: true,
    ...(process.env.METAUI_CHROMIUM ? {executablePath: process.env.METAUI_CHROMIUM} : {})});
  const context = await browser.newContext({viewport: {width: 1440, height: 1000}, acceptDownloads: true});
  const page = await context.newPage();
  page.setDefaultTimeout(120000);
  page.on('pageerror', e => errors.push(e.message));
  page.on('console', m => { if (m.type() === 'error') consoleMsgs.push(m.text()); });
  await page.goto(url);
  mark('index_loaded');
  const precomputed = await page.locator('#metaui-preview').count();
  if (precomputed) {
    const preview = page.frameLocator('#metaui-preview');
    await preview.locator('.metaui-primary').waitFor({timeout: 30000});
    if (await preview.locator('input:not(:disabled), button:not(:disabled), select:not(:disabled)').count())
      throw Error('Preview contains enabled controls');
    await page.screenshot({path: path.join(out, 'preview-desktop.png')});
    await page.setViewportSize({width: 390, height: 844});
    await page.screenshot({path: path.join(out, 'preview-mobile.png')});
    await page.setViewportSize({width: 1440, height: 1000});
  }
  const app = await appFrame(page);
  mark('iframe_created');
  await app.locator('#go').waitFor({timeout: 600000});
  mark('ui_visible');
  await app.waitForFunction(() => Array.isArray(window.Shiny?.shinyapp?.$inputValues?.outliers_z_scores) &&
    !document.querySelector('#go').disabled, null, {timeout: 600000});
  mark('ready_to_analyse');
  if (precomputed) {
    await page.waitForFunction(() => document.documentElement.dataset.metauiLive === 'true', null, {timeout: 600000});
    await page.screenshot({path: path.join(out, 'handover.png')});
    notes.push('Precomputed controls disabled; live hand-over completed');
  }
  const startQuery = await app.evaluate(() => location.search);

  // 1. Full analysis
  await timed('first_analysis_s', async () => {
    await app.locator('#go').click();
    await app.waitForFunction(() => document.querySelector('#effectestimate')?.innerText.includes('Source-reference'), null, {timeout: 600000});
    await idle(app);
  });
  if (await app.locator('[data-metaui-precomputed-plot]').count()) throw Error('Precomputed plot survived a new analysis');
  fs.writeFileSync(path.join(out, 'workbook-first-analysis.xlsx'), await fetchDownload(app, 'executeDownload'));
  const estimates = await app.locator('#effectestimate').innerText();
  fs.writeFileSync(path.join(out, 'estimates.txt'), estimates);
  await page.screenshot({path: path.join(out, 'summary.png')});

  // 2. Re-analysis (cache) and z-score filter change
  await timed('reanalysis_cached_s', async () => { await app.locator('#go').click(); await page.waitForTimeout(300); await idle(app); });
  await timed('filter_change_reanalysis_s', async () => {
    await app.evaluate(() => {
      const el = $('#outliers_z_scores').data('ionRangeSlider');
      el.update({from: -1.5, to: el.result.to});
    });
    await app.waitForFunction(() => document.querySelector('#apply_state')?.textContent.includes('Filters changed'), null, {timeout: 30000});
    await app.locator('#go').click(); await page.waitForTimeout(300); await idle(app);
  });
  notes.push('filtered selection: ' + (await app.locator('#selection_status').textContent()).replace(/\s+/g, ' '));
  const filteredQuery = await app.evaluate(() => location.search);
  notes.push('iframe query after filtering: ' + filteredQuery.slice(0, 80) + (filteredQuery.length > 80 ? '…' : ''));
  notes.push('top-level page URL after filtering: ' + page.url());
  const share = await app.locator('#share_selection a').getAttribute('href').catch(() => null);
  fs.writeFileSync(path.join(out, 'share-href.txt'), share || '');

  // 3. Forest plot and exports
  await app.getByRole('tab', {name: 'Forest Plot', exact: true}).click();
  await timed('forest_render_s', async () => {
    await app.waitForFunction(() => document.querySelector('#foreststudies img')?.naturalWidth > 0, null, {timeout: 120000});
  });
  for (const [id, ext] of [['forest_pdf', 'pdf'], ['forest_png', 'png'], ['forest_csv', 'csv']]) {
    try {
      const pending = page.waitForEvent('download', {timeout: 120000});
      const s = Date.now();
      await app.locator('#' + id).click();
      const d = await pending; const target = path.join(out, 'forest.' + ext); await d.saveAs(target);
      timings['forest_' + ext + '_s'] = (Date.now() - s) / 1000;
      const bytes = fs.readFileSync(target);
      const ok = ext === 'pdf' ? bytes.subarray(0, 5).toString() === '%PDF-' :
        ext === 'png' ? bytes.subarray(0, 8).toString('hex') === '89504e470d0a1a0a' : bytes.toString().includes('Selected effect');
      notes.push(`forest ${ext}: ${bytes.length} bytes, valid=${ok}, name=${d.suggestedFilename()}`);
    } catch (e) {
      notes.push(`forest ${ext}: browser download FAILED ${e.message.split('\n')[0]}`);
      try {
        const s = Date.now(); const bytes = await fetchDownload(app, id);
        timings['forest_' + ext + '_fetch_s'] = (Date.now() - s) / 1000;
        fs.writeFileSync(path.join(out, 'forest-fetched.' + ext), bytes);
        const ok = ext === 'pdf' ? bytes.subarray(0, 5).toString() === '%PDF-' :
          ext === 'png' ? bytes.subarray(0, 8).toString('hex') === '89504e470d0a1a0a' : bytes.toString().includes('Selected effect');
        notes.push(`forest ${ext} via in-frame fetch: ${bytes.length} bytes, valid=${ok}`);
      } catch (e2) { notes.push(`forest ${ext} via in-frame fetch: FAILED ${e2.message.split('\n')[0]}`); }
    }
  }
  await page.screenshot({path: path.join(out, 'forest.png')});

  // 4. Other tabs render without errors
  for (const tab of ['Publication Bias', 'Outlier Diagnostics', 'Sample']) {
    const t = app.getByRole('tab', {name: tab, exact: true});
    if (await t.count()) { await timed('tab_' + tab.replace(/ /g, '_') + '_s', async () => { await t.click(); await page.waitForTimeout(300); await idle(app); }); }
  }
  await page.screenshot({path: path.join(out, 'last-tab.png')});

  // 5. Reset to full selection, workbook download, upload round trip
  await app.locator('#resetFilters').click();
  // The dynamic z slider has no shinyjs reset snapshot; restore its full bounds.
  await app.evaluate(() => {
    const slider = $('#outliers_z_scores').data('ionRangeSlider');
    slider.update({from: slider.options.min, to: slider.options.max});
  });
  await page.waitForTimeout(300);
  await idle(app);
  await app.waitForFunction(() => !document.querySelector('#go').disabled, null, {timeout:120000});
  await app.locator('#go').click({force:true}); await page.waitForTimeout(300); await idle(app);
  await app.getByRole('tab', {name: 'Summary', exact: true}).click();
  await app.getByText('Download or upload data', {exact: true}).click();
  const wb = path.join(out, 'workbook.xlsx');
  try {
    await timed('workbook_download_s', async () => {
      const ev = page.waitForEvent('download', {timeout: 300000});
      await app.locator('#downloadData').click();
      const d = await ev; await d.saveAs(wb);
    });
  } catch (e) {
    notes.push('workbook: browser download FAILED ' + e.message.split('\n')[0]);
    await timed('workbook_fetch_s', async () => fs.writeFileSync(wb, await fetchDownload(app, 'executeDownload')));
    notes.push('workbook via in-frame fetch: ' + fs.statSync(wb).size + ' bytes, zip=' + (fs.readFileSync(wb).subarray(0, 2).toString() === 'PK'));
  }
  await timed('workbook_upload_s', async () => {
    await app.locator('#uploadData').setInputFiles(wb);
    await app.waitForFunction(() => document.querySelector('#uploadData_progress')?.textContent.includes('Upload complete'), null, {timeout: 60000});
    await app.locator('#executeUpload').click();
    await app.waitForFunction(() => document.querySelector('#selection_status')?.textContent.includes('Uploaded data:'), null, {timeout: 300000});
    await idle(app);
  });
  notes.push('after upload: ' + (await app.locator('#selection_status').textContent()).replace(/\s+/g, ' '));

  // Mobile viewport; release this webR worker before restoring a fresh link.
  await page.setViewportSize({width: 390, height: 844});
  await page.waitForTimeout(1000);
  await page.screenshot({path: path.join(out, 'mobile.png')});
  const memory = await app.evaluate(() => performance.memory ? {usedJSHeapMB: performance.memory.usedJSHeapSize / 1e6} : null).catch(() => null);

  const outputErrors = await app.locator('.shiny-output-error').allInnerTexts();
  await page.close();

  // 6. Selection link restore in a fresh page
  if (share) {
    const linkPage = await context.newPage();
    linkPage.on('pageerror', e => errors.push('link page: ' + e.message));
    const linkUrl = new URL(share, mode === 'server' ? url : url).toString();
    notes.push('selection link opened as: ' + linkUrl.slice(0, 90) + '…');
    const s = Date.now();
    await linkPage.goto(linkUrl);
    if (await linkPage.locator('#metaui-preview').count()) throw Error('Full-data preview shown for a selection link');
    let la;
    if (mode === 'server') la = linkPage.mainFrame();
    else { for (let i = 0; i < startupPolls && !la; i++) { la = linkPage.frames().find(fr => fr !== linkPage.mainFrame() && /app_/.test(fr.url())); if (!la) await linkPage.waitForTimeout(250); } }
    notes.push('link page iframe URL: ' + la.url().slice(0, 120));
    try {
      await la.waitForFunction(() => /restored|not applied/i.test(document.querySelector('#link_notice')?.textContent || '') ||
        /Selected/.test(document.querySelector('#selection_status')?.textContent || ''), null, {timeout: 300000});
      await la.waitForFunction(() => document.querySelector('#effectestimate')?.innerText.includes('Source-reference'), null, {timeout: 300000});
      await idle(la);
      notes.push('link notice: ' + (await la.locator('#link_notice').innerText().catch(() => '(none)')));
      notes.push('link selection: ' + (await la.locator('#selection_status').textContent().catch(() => '')).replace(/\s+/g, ' '));
    } catch (e) { notes.push('selection link: FAILED ' + e.message.split('\n')[0]); }
    timings.link_restore_s = (Date.now() - s) / 1000;
    await linkPage.screenshot({path: path.join(out, 'link-restore.png')});
    await linkPage.close();
  }


  fs.writeFileSync(path.join(out, 'result.json'), JSON.stringify({mode, url, timings, notes, memory,
    startQuery, js_errors: errors, console_errors: consoleMsgs.slice(0, 40),
    output_errors: outputErrors}, null, 2));
  console.log(JSON.stringify({timings, notes}, null, 2));
  await browser.close();
})().catch(e => { console.error(e); fs.writeFileSync(path.join(out, 'failure.json'), JSON.stringify({error: e.message, timings, notes, errors, consoleMsgs}, null, 2)); process.exitCode = 1; process.exit(1); });
