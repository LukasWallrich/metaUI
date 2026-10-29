// Run against design_app() and a generated app using the settings below.
// Uses the same Playwright setup as tools/browser-smoke.cjs.
const { chromium } = require(process.env.METAUI_PLAYWRIGHT || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { execFileSync } = require('node:child_process');
const evidence = process.env.METAUI_DESIGN_EVIDENCE;
assert(evidence, 'Set METAUI_DESIGN_EVIDENCE to an evidence directory');
fs.mkdirSync(evidence, { recursive: true });
let browser;
(async () => {
  browser = await chromium.launch({headless: true,
    ...(process.env.METAUI_CHROMIUM ? {executablePath: process.env.METAUI_CHROMIUM} : {})});
  const page = await browser.newPage({viewport:{width:1440,height:1000}});
  const errors = []; page.on('pageerror', error => errors.push(error.message));
  await page.goto(process.env.METAUI_DESIGN_URL || 'http://127.0.0.1:18801');
  await page.locator('iframe').waitFor();
  await page.selectOption('#palette', 'plum');
  await page.waitForFunction(()=>document.querySelector('#r_code')?.textContent.includes('#69429A'));
  await page.selectOption('#font', 'georgia');
  await page.getByRole('radio', {name:'Compact', exact:true}).check();
  await page.evaluate(()=>{
    $('#text_size').data('ionRangeSlider').update({from:18}); $('#text_size').trigger('change');
    $('#radius').data('ionRangeSlider').update({from:0}); $('#radius').trigger('change');
  });
  await page.waitForFunction(()=>{
    const d=document.querySelector('iframe')?.contentDocument;
    return d && d.body && d.querySelector('.well') && getComputedStyle(d.body).fontFamily.includes('Georgia') && getComputedStyle(d.querySelector('.well')).borderRadius==='0px';
  });
  const preview = await page.evaluate(()=>{
    const d=document.querySelector('iframe').contentDocument;
    return {accent:getComputedStyle(d.documentElement).getPropertyValue('--metaui-accent').trim(),
      font:getComputedStyle(d.body).fontFamily,headingFont:getComputedStyle(d.querySelector('h2')).fontFamily,
      textSize:getComputedStyle(d.body).fontSize,padding:getComputedStyle(d.querySelector('.well')).padding,
      radius:getComputedStyle(d.querySelector('.well')).borderRadius,buttonBackground:getComputedStyle(d.querySelector('#go')).backgroundColor};
  });
  assert.equal(preview.accent, '#69429A'); assert.equal(preview.textSize,'18px');
  assert(preview.headingFont.includes('Georgia')); assert.equal(preview.padding,'16px');
  await page.screenshot({path:path.join(evidence,'helper-desktop.png')});
  for (const [id, extension] of [['download_r','R'],['download_json','json']]) {
    const downloading=page.waitForEvent('download'); await page.locator('#'+id).click();
    const download=await downloading; await download.saveAs(path.join(evidence,'design.'+extension));
  }
  const settings=JSON.parse(fs.readFileSync(path.join(evidence,'design.json'),'utf8'));
  assert.equal(settings.accent,'#69429A'); assert.equal(settings.font,'georgia');
  assert.equal(settings.text_size,18); assert.equal(settings.radius,0); assert.equal(settings.spacing,'compact');
  execFileSync('Rscript',['-e','e <- new.env(); sys.source(commandArgs(TRUE)[1],e); stopifnot(e$design$font == "georgia", e$design$radius == 0)',path.join(evidence,'design.R')]);
  // Manual edits leave the preset, so selecting it again can restore its colours.
  await page.getByRole('button',{name:'Accent colour picker',exact:true}).click();
  await page.getByRole('textbox',{name:'color input field',exact:true}).fill('#7B3E68');
  await page.getByRole('heading',{name:'App design',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#palette').value==='custom');
  await page.selectOption('#palette','plum');
  await page.waitForFunction(()=>document.querySelector('#r_code')?.textContent.includes('#69429A'));
  assert.equal(await page.locator('#font').inputValue(),'georgia');
  // A low-contrast edit must withhold exports, while preserving the last valid preview.
  await page.getByRole('button',{name:'Accent colour picker',exact:true}).click();
  await page.getByRole('textbox',{name:'color input field',exact:true}).fill('#FFFFFF');
  await page.getByRole('heading',{name:'App design',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#contrast').textContent.includes('export is unavailable'));
  assert.equal(await page.locator('#export_downloads a').count(),0);
  assert.equal(await page.evaluate(()=>getComputedStyle(document.querySelector('iframe').contentDocument.documentElement).getPropertyValue('--metaui-accent').trim()),'#69429A');
  await page.getByRole('button',{name:'Reset choices',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#r_code')?.textContent.includes('#096B74') && document.querySelector('#export_downloads a'));
  await page.waitForFunction(()=>document.querySelector('#palette').value==='teal');
  await page.getByRole('radio',{name:'Phone',exact:true}).check();
  await page.waitForFunction(()=>document.querySelector('iframe').style.width==='390px');
  await page.setViewportSize({width:390,height:844});
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
  await page.screenshot({path:path.join(evidence,'helper-mobile.png'),fullPage:true});
  // Compare a real generated app with the exported preview settings.
  await page.setViewportSize({width:1440,height:1000});
  await page.goto(process.env.METAUI_DESIGN_APP_URL || 'http://127.0.0.1:18802');
  await page.locator('#go').waitFor();
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1 && Array.isArray(window.Shiny.shinyapp.$inputValues.outliers_z_scores));
  const actual = await page.evaluate(()=>({accent:getComputedStyle(document.documentElement).getPropertyValue('--metaui-accent').trim(),
    font:getComputedStyle(document.body).fontFamily,headingFont:getComputedStyle(document.querySelector('h2')).fontFamily,
    textSize:getComputedStyle(document.body).fontSize,padding:getComputedStyle(document.querySelector('.well')).padding,
    radius:getComputedStyle(document.querySelector('.well')).borderRadius,buttonBackground:getComputedStyle(document.querySelector('#go')).backgroundColor}));
  assert.deepEqual(actual,preview);
  await page.locator('#go').click(); await page.locator('#sample table').waitFor();
  assert((await page.locator('#primary_estimate').innerText()).includes('0.24'));
  await page.screenshot({path:path.join(evidence,'generated-design.png')});
  assert.deepEqual(errors,[]);
  fs.writeFileSync(path.join(evidence,'result.json'),JSON.stringify({ok:true,settings,preview,actual},null,2));
  console.log('Author design browser checks passed.');
})().catch(error=>{console.error(error);process.exitCode=1;}).finally(async()=>{if(browser)await browser.close();});
