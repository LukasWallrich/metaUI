// Times the grid posterior in webR (Node). Run from the package root with WEBR_DIR set to a
// webR distribution (e.g. the shinylive/webr folder of an export_static() site):
// WEBR_DIR=<dir> node validation/bayesian-grid/webr-bench.cjs
const fs=require('fs'), path=require('path');
const root=process.cwd(), base=path.resolve(process.env.WEBR_DIR);
const {WebR}=require(path.join(base,'webr.cjs'));
(async()=>{
  const webR=new WebR({baseUrl:base+'/',interactive:false}); await webR.init();
  await webR.FS.writeFile('/grid.R',new Uint8Array(fs.readFileSync(path.join(root,'R/bayesian.R'))));
  console.log(await webR.evalRString('R.version.string'));
  const code=`source("/grid.R"); res <- NULL
    for (k in c(20, 100, 646)) { set.seed(k); y <- rnorm(k, .3, .3); s <- runif(k, .1, .4)
      t <- numeric(3); for (r in 1:3) t[r] <- system.time(g <- metaUI_bayesian_grid(y, s, .5))[["elapsed"]]
      res <- rbind(res, c(k = k, median_s = median(t), max_s = max(t), mu_median = g$qposterior(mu.p = .5))) }
    paste(capture.output(print(res, digits = 6)), collapse = "\\n")`;
  const txt=await webR.evalRString(code); console.log(txt);
  fs.writeFileSync(path.join(root,'validation/bayesian-grid/webr-timing.txt'),txt+'\n\nNode heap/rss (MB): '+JSON.stringify(Object.fromEntries(Object.entries(process.memoryUsage()).map(([k,v])=>[k,Math.round(v/1e6)])))+'\n');
  webR.close(); process.exit(0);
})().catch(e=>{console.error(e);process.exit(1)});
