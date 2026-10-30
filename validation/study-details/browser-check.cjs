// Checks study-detail rows in a running demo app (APP_URL) by keyboard and pointer,
// at desktop and narrow widths, and saves screenshots. Built by demo-build.R.
const { chromium } = require(process.env.METAUI_PLAYWRIGHT || 'playwright');
const assert = require('node:assert/strict');
const url = process.env.APP_URL;
async function run(name, viewport) {
  const browser = await chromium.launch({ headless: true, ...(process.env.METAUI_CHROMIUM ? { executablePath: process.env.METAUI_CHROMIUM } : {}) });
  const page = await browser.newPage({ viewport });
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  await page.goto(url);
  await page.waitForFunction(() => window.Shiny?.shinyapp?.$socket?.readyState === 1);
  await page.waitForFunction(() => !document.querySelector('#go').disabled, null, { timeout: 30000 });
  await page.click('#go');
  await page.getByRole('tab', { name: 'Sample' }).click();
  const toggle = page.locator('#sample_table .metaui-study-toggle').first();
  await toggle.waitFor({ timeout: 60000 });
  const n = await page.locator('#sample_table .metaui-study-toggle').count();
  const label = (await toggle.innerText()).trim();
  const title = await toggle.getAttribute('title');
  // Keyboard: focus and press Enter
  await toggle.focus();
  await page.keyboard.press('Enter');
  await page.locator('#sample_table td.metaui-study-child').first().waitFor();
  assert.equal(await toggle.getAttribute('aria-expanded'), 'true');
  const region = page.locator('#' + await toggle.getAttribute('aria-controls'));
  const text = await region.innerText();
  // Copy the reference by keyboard; record what the copy event put on the clipboard.
  await page.evaluate(() => window.addEventListener('copy', e => {
    window.metauiCopied = { text: e.clipboardData.getData('text/plain'), html: e.clipboardData.getData('text/html') };
  }));
  await region.locator('.metaui-copy-ref').focus();
  await page.keyboard.press('Enter');
  await region.locator('.metaui-copy-status', { hasText: 'Reference copied' }).waitFor();
  const copied = await page.evaluate(() => window.metauiCopied);
  assert.match(copied.text, /\(\d{4}\)\. .+ https:\/\/doi\.org\//);
  assert.match(copied.html, /<i>/);
  // Expand a study with withheld abstract too
  const withheld = page.locator('#sample_table .metaui-study-toggle', { hasText: 'Ioannidis' });
  await withheld.click();
  await page.locator('#sample_table td.metaui-study-child').nth(1).waitFor();
  const plain = await page.locator('#sample_table tbody td', { hasText: 'No DOI given' }).first().innerHTML();
  await page.locator('#sample_table').scrollIntoViewIfNeeded();
  await page.evaluate(() => [...document.querySelectorAll('h3')].find(h => h.innerText.trim() === 'Effect sizes').scrollIntoView({ block: 'start' }));
  await page.mouse.move(0, 0);
  const selected = await page.locator('#sample_table tr.selected').count(); assert.equal(selected, 0, 'toggle selected a row');
  await page.screenshot({ path: `${name}-expanded.png`, fullPage: false });
  // Hover tooltip is a native title attribute (not captured in screenshots)
  // Collapse via keyboard (Space) and check state
  await toggle.focus(); await page.keyboard.press('Space');
  await page.waitForFunction(el => el.getAttribute('aria-expanded') === 'false', await toggle.elementHandle());
  console.log(JSON.stringify({ name, toggles: n, label, title, plainCell: plain, firstDetails: text.slice(0, 400), copied: copied.text, errors }, null, 1));
  await browser.close();
}
(async () => {
  await run('desktop', { width: 1440, height: 1000 });
  await run('narrow', { width: 390, height: 844 });
})().catch(e => { console.error(e); process.exit(1); });
