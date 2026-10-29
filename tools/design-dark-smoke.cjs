// Assert readable interactive states in a real generated dark app.
const { chromium } = require(process.env.METAUI_PLAYWRIGHT || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const evidence = process.env.METAUI_DESIGN_EVIDENCE;
assert(evidence, 'Set METAUI_DESIGN_EVIDENCE'); fs.mkdirSync(evidence,{recursive:true});
let browser, page;
function ratio(a,b) {
  const lum = value => value.match(/[\d.]+/g).slice(0,3).map(Number).map(x=>x/255)
    .map(x=>x<=.04045?x/12.92:((x+.055)/1.055)**2.4).reduce((s,x,i)=>s+x*[.2126,.7152,.0722][i],0);
  const [lo,hi]=[lum(a),lum(b)].sort((x,y)=>x-y); return (hi+.05)/(lo+.05);
}
async function readable(locator,label,pseudo=null) {
  const styles=await locator.evaluateAll((elements,pseudo)=>elements.map(el=>{
    const foreground=getComputedStyle(el,pseudo).color; let node=el,background;
    while(node){background=getComputedStyle(node).backgroundColor;if(background!=='rgba(0, 0, 0, 0)'&&background!=='transparent')break;node=node.parentElement;}
    return {text:el.textContent.trim(),foreground,background,opacity:getComputedStyle(el,pseudo).opacity};
  }),pseudo);
  assert(styles.length,label+' did not render');
  for(const style of styles){assert(ratio(style.foreground,style.background)>=4.5,label+' contrast: '+JSON.stringify(style));if(pseudo)assert.equal(style.opacity,"1",label+" must not fade the sort indicator");}
  return styles;
}
(async()=>{
  browser=await chromium.launch({headless:true,...(process.env.METAUI_CHROMIUM?{executablePath:process.env.METAUI_CHROMIUM}:{})});
  page=await browser.newPage({viewport:{width:1440,height:1000}});
  await page.goto(process.env.METAUI_DESIGN_DARK_URL || 'http://127.0.0.1:18803');
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&Array.isArray(window.Shiny.shinyapp.$inputValues.outliers_z_scores));
  await page.getByText('Download or upload data',{exact:true}).click();
  await page.locator('#executeUpload').click();
  await page.getByRole('heading',{name:'No file selected',exact:true}).waitFor();
  await page.waitForFunction(()=>{const m=document.querySelector('.modal'),d=document.querySelector('.modal-dialog');return m&&d&&getComputedStyle(m).opacity==='1'&&['none','matrix(1, 0, 0, 1, 0, 0)'].includes(getComputedStyle(d).transform);});
  const modal=await readable(page.locator('.modal-content'),'Upload-error dialog');
  await page.screenshot({path:path.join(evidence,'dark-dialog.png')});
  await page.getByRole('button',{name:'Dismiss',exact:true}).click();
  await page.locator('#go').click(); await page.locator('#sample table').waitFor();
  await readable(page.locator('#go'),'Primary action');
  await page.getByRole('tab',{name:'Sample',exact:true}).click();
  await page.locator('#sample_table .dataTables_paginate .paginate_button').first().waitFor();
  const sampleHeaders=await readable(page.locator('#sample_table thead th'),'Sample headers');
  await readable(page.locator('#sample_table thead th.sorting,#sample_table thead th.sorting_asc,#sample_table thead th.sorting_desc'),'Sorting indicators','::after');
  const paging=await readable(page.locator('#sample_table .dataTables_paginate .paginate_button'),'Pagination');
  await page.locator('#sample_table').scrollIntoViewIfNeeded();
  await page.screenshot({path:path.join(evidence,'dark-sample.png')});
  await page.getByRole('tab',{name:'Publication Bias',exact:true}).click();
  await page.locator('#eggers table thead th').first().waitFor();
  const biasHeaders=await readable(page.locator('#eggers table thead th'),'Egger headers');
  await page.getByRole('tab',{name:'Moderation',exact:true}).click();
  await readable(page.locator('#moderator + .selectize-control .selectize-input'),'Moderator input');
  await page.locator('#moderator + .selectize-control .selectize-input').click();
  await page.locator('.selectize-dropdown:visible').waitFor();
  await readable(page.locator('.selectize-dropdown:visible'),'Moderator choices');
  await page.keyboard.press('Escape');
  await page.getByRole('tab',{name:'Summary',exact:true}).click();
  await page.getByText('Assess practical equivalence',{exact:true}).click();
  await readable(page.locator('#sesoi'),'Equivalence input');
  const beforeBound=await page.locator('#model_comparison img').getAttribute('src');
  await page.locator('#sesoi').fill('0.3');
  await page.waitForFunction(before=>{const img=document.querySelector('#model_comparison img');return img?.naturalWidth>0&&img.src!==before;},beforeBound);
  await page.screenshot({path:path.join(evidence,'dark-equivalence.png')});
  await page.getByRole('tab',{name:'Forest Plot',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#foreststudies img')?.naturalWidth>0);
  await page.screenshot({path:path.join(evidence,'dark-forest.png')});
  await page.getByRole('tab',{name:'Summary',exact:true}).click();
  await page.evaluate(()=>{
    const slider=$('#metaUI__filter_Year');
    slider.data('ionRangeSlider').update({from:2002}); slider.trigger('change');
  });
  await page.waitForFunction(()=>document.querySelector('#apply_state')?.textContent.includes('Filters changed'));
  const staleNotice=await readable(page.locator('#apply_state_box'),'Stale notice');
  await readable(page.locator('#metaui_basis'),'Stale results banner');
  await readable(page.locator('#go'),'Update action');
  await page.waitForFunction(()=>getComputedStyle(document.querySelector('#go')).outlineStyle==='dashed');
  await page.screenshot({path:path.join(evidence,'dark-stale.png')});
  fs.writeFileSync(path.join(evidence,'dark-result.json'),JSON.stringify({ok:true,modal,sampleHeaders,paging,biasHeaders,staleNotice},null,2));
  console.log('Dark design browser checks passed.');
})().catch(async error=>{console.error(error);process.exitCode=1;if(page)await page.screenshot({path:path.join(evidence,'dark-failure.png')}).catch(()=>{});})
.finally(async()=>{if(browser)await browser.close();});
