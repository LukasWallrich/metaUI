// Owns the R server lifecycle for both author-design browser suites.
const fs=require('node:fs'); const path=require('node:path');
const {spawn}=require('node:child_process');
const assert=require('node:assert/strict');
const root=process.env.METAUI_DESIGN_ROOT; assert(root,'Set METAUI_DESIGN_ROOT to design-fixture.R output');
const base=Number(process.env.METAUI_DESIGN_PORT || 18801);
const evidence=path.join(root,'evidence');fs.mkdirSync(evidence,{recursive:true});
const servers=[],children=new Set();
let stopping=false,cleanupPromise;
function track(proc) {children.add(proc);proc.once('close',()=>children.delete(proc));return proc;}
function cleanup() {
  if(cleanupPromise)return cleanupPromise;
  cleanupPromise=Promise.all([...children].map(proc=>new Promise(resolve=>{
    if(proc.exitCode!==null||proc.signalCode!==null){resolve();return;}
    const timer=setTimeout(()=>proc.kill('SIGKILL'),3000);
    proc.once('close',()=>{clearTimeout(timer);resolve();});
    proc.kill('SIGTERM');
  })));
  return cleanupPromise;
}
for(const [signal,code] of [['SIGINT',130],['SIGTERM',143]])process.once(signal,()=>{
  stopping=true;
  cleanup().then(()=>process.exit(code));
});
function server(name,expression,port) {
  const log=fs.createWriteStream(path.join(evidence,name+'.log'));
  const proc=track(spawn('Rscript',['--vanilla','-e',expression],{env:process.env}));
  proc.stdout.pipe(log,{end:false});proc.stderr.pipe(log,{end:false});proc.on('close',()=>log.end());
  const state={name,proc,log,port,error:null};proc.on('error',e=>state.error=e);servers.push(state);return state;
}
async function ready(s) {
  for(let i=0;i<240;i++) {
    if(stopping)throw Error('Browser run interrupted');
    if(s.error)throw s.error;
    if(s.proc.exitCode!==null)throw Error(s.name+' server exited; see '+s.name+'.log');
    try {const r=await fetch('http://127.0.0.1:'+s.port);if(r.ok){const html=await r.text();assert(html.includes(s.name==='helper'?'App design':'Mental health interventions'),'Port serves another app');return;}}catch(e){if(e.code==='ERR_ASSERTION')throw e;}
    await new Promise(r=>setTimeout(r,250));
  }
  throw Error(s.name+' server did not start within 60s');
}
async function run(script,env) {
  if(stopping)throw Error('Browser run interrupted');
  await new Promise((resolve,reject)=>{
    const p=track(spawn(process.execPath,[path.join(__dirname,script)],{env,stdio:'inherit'}));
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
})().catch(e=>{if(!stopping){console.error(e);process.exitCode=1;}}).finally(cleanup);
