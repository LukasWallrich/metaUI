// Owns the R server lifecycle for both author-design browser suites.
const fs=require('node:fs'); const path=require('node:path');
const {spawn}=require('node:child_process');
const assert=require('node:assert/strict');
const root=process.env.METAUI_DESIGN_ROOT; assert(root,'Set METAUI_DESIGN_ROOT to design-fixture.R output');
const base=Number(process.env.METAUI_DESIGN_PORT || 18801);
const evidence=path.join(root,'evidence');fs.mkdirSync(evidence,{recursive:true});
const servers=[];
function server(name,expression,port) {
  const log=fs.createWriteStream(path.join(evidence,name+'.log'));
  const proc=spawn('Rscript',['--vanilla','-e',expression],{env:process.env});
  proc.stdout.pipe(log,{end:false});proc.stderr.pipe(log,{end:false});proc.on('close',()=>log.end());
  const state={name,proc,log,port,error:null};proc.on('error',e=>state.error=e);servers.push(state);return state;
}
async function ready(s) {
  for(let i=0;i<240;i++) {
    if(s.error)throw s.error;
    if(s.proc.exitCode!==null)throw Error(s.name+' server exited; see '+s.name+'.log');
    try {const r=await fetch('http://127.0.0.1:'+s.port);if(r.ok){const html=await r.text();assert(html.includes(s.name==='helper'?'App design':'Mental health interventions'),'Port serves another app');return;}}catch(e){if(e.code==='ERR_ASSERTION')throw e;}
    await new Promise(r=>setTimeout(r,250));
  }
  throw Error(s.name+' server did not start within 60s');
}
async function run(script,env) {
  await new Promise((resolve,reject)=>{
    const p=spawn(process.execPath,[path.join(__dirname,script)],{env,stdio:'inherit'});
    p.on('error',reject);p.on('exit',code=>code===0?resolve():reject(Error(script+' exited '+code)));
  });
}
(async()=>{
  const helper=server('helper',`shiny::runApp(metaUI::design_app(),host='127.0.0.1',port=${base},launch.browser=FALSE)`,base);
  const light=server('light',`shiny::runApp(${JSON.stringify(path.join(root,'light'))},host='127.0.0.1',port=${base+1},launch.browser=FALSE)`,base+1);
  const dark=server('dark',`shiny::runApp(${JSON.stringify(path.join(root,'dark'))},host='127.0.0.1',port=${base+2},launch.browser=FALSE)`,base+2);
  await Promise.all(servers.map(ready));
  const env={...process.env,METAUI_DESIGN_EVIDENCE:evidence,METAUI_DESIGN_URL:'http://127.0.0.1:'+helper.port,
    METAUI_DESIGN_APP_URL:'http://127.0.0.1:'+light.port,METAUI_DESIGN_DARK_URL:'http://127.0.0.1:'+dark.port};
  await run('design-smoke.cjs',env); await run('design-dark-smoke.cjs',env);
})().catch(e=>{console.error(e);process.exitCode=1;}).finally(()=>{for(const s of servers){s.proc.kill('SIGTERM');}});
