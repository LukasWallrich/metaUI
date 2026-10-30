// Exploration: time shinylive startup, cold then warm (same browser context), and
// mark each phase so server-side request logs can be aligned.
// Usage: METAUI_PLAYWRIGHT=... URL=... RUNS=2 OUT=file.json node startup.cjs
const pw = require(process.env.METAUI_PLAYWRIGHT || 'playwright');
const engine = process.env.BROWSER || 'chromium';
const fs = require('node:fs');
const url = process.env.URL, runs = Number(process.env.RUNS || 2);
(async () => {
  const browser = await pw[engine].launch({headless: true, args: process.env.CHROMIUM_ARGS ? [process.env.CHROMIUM_ARGS] : [],
    ...(engine === 'chromium' && process.env.METAUI_CHROMIUM ? {executablePath: process.env.METAUI_CHROMIUM} : {})});
  const context = await browser.newContext({viewport: {width: 1440, height: 1000}});
  const results = [];
  for (let r = 0; r < runs; r++) {
    const page = await context.newPage();
    const t0 = Date.now(), res = {run: r + 1, start_epoch: t0 / 1000};
    await page.goto(url);
    let app;
    while (!app) { app = page.frames().find(f => f !== page.mainFrame() && /app_/.test(f.url())); if (!app) await page.waitForTimeout(100); }
    res.iframe_s = (Date.now() - t0) / 1000;
    await app.waitForFunction(() => window.Shiny?.shinyapp?.isConnected?.() &&
      Array.isArray(window.Shiny.shinyapp.$inputValues?.outliers_z_scores) && !document.querySelector('#go').disabled,
      null, {timeout: 600000, polling: 100});
    res.ready_s = (Date.now() - t0) / 1000;
    const s = Date.now();
    await app.locator('#go').click();
    await app.waitForFunction(() => document.querySelector('#effectestimate')?.innerText.includes('Source-reference'), null, {timeout: 600000, polling: 100});
    res.first_analysis_s = (Date.now() - s) / 1000;
    res.heap_MB = await page.evaluate(() => performance.memory ? performance.memory.usedJSHeapSize / 1e6 : null);
    res.engine = engine;
    results.push(res); console.log(JSON.stringify(res));
    await page.close();
  }
  fs.writeFileSync(process.env.OUT, JSON.stringify(results, null, 2));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
