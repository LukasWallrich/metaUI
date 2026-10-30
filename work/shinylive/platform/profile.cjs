// Profile an instrumented scratch export; serialize with the same timing lock.
// flock /tmp/shinylive-speed-timing.lock env URL=... OUT=/tmp/.../profile.json node work/shinylive/platform/profile.cjs
const {chromium} = require('/home/lukas/flora-validation/node_modules/playwright');
const fs = require('node:fs');
(async () => {
  const browser = await chromium.launch({executablePath: '/home/lukas/.cache/ms-playwright/chromium-1248/chrome-linux64/chrome', headless: true});
  try {
    const page = await browser.newPage();
    const events = [];
    page.on('console', message => {
      events.push({epoch: Date.now(), text: message.text()});
      fs.writeFileSync(process.env.OUT, JSON.stringify(events, null, 2));
    });
    await page.goto(process.env.URL);
    let app;
    while (!app) {
      app = page.frames().find(frame => /app_/.test(frame.url()));
      if (!app) await page.waitForTimeout(200);
    }
    await app.locator('#go').waitFor({timeout: 600000});
    await page.waitForTimeout(1000);
  } finally {
    await browser.close();
  }
})().catch(error => {console.error(error); process.exitCode = 1;});
