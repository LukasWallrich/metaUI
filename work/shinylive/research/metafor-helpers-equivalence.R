# Reproduce from repository root. All generated evidence stays in the M scratch area.
# R_LIBS_USER=/tmp/shinylive-speed/M/lib:/tmp/shinylive-explore/lib \
# R_LIBS_SITE=/tmp/shinylive-explore/emptysite Rscript --vanilla work/shinylive/research/meta-equivalence.R
out <- '/tmp/shinylive-speed/M/equivalence'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
stopifnot(requireNamespace('meta'), requireNamespace('metafor'))
stopifnot(as.character(utils::packageVersion('meta')) == '8.5.0')
# Snapshot the installed implementations consulted in the audit.
source_out <- file.path(out, 'source')
dir.create(source_out, showWarnings=FALSE)
for (pkg in c('meta','metafor')) {
 funs <- if(pkg=='meta') c('trimfill.meta','metagen','metabias.meta','hetcalc','estimate.missing','linregcore') else c('trimfill.rma.uni','rma.uni','regtest')
 for (fun in funs) writeLines(deparse(getFromNamespace(fun,pkg)),file.path(source_out,paste0(pkg,'-',fun,'.R')))
}
set.seed(20261030)
cases <- list()
add <- function(id,y,v,group='fixture') cases[[id]] <<- list(y=y,v=v,group=group)
x <- data.frame(yi=c(.1,.1,.5,.2,-.2,.3,.4,.4,.6,.2,-.1,.1,.2,.3,.1,.8),vi=seq(.01,.04,length.out=16))
add('helper-fixture-effects', x$yi,x$vi)
# Exercise the same aggregation implementation used in generated apps.
source('R/analysis.R')
# Minimal prepared columns needed by aggregate(); see the package fixture.
if (requireNamespace('metaUI', quietly=TRUE)) {
 d <- metaUI::prepare_data(transform(x, study=rep(LETTERS[1:8],each=2),id=rep(1:2,8)), 'study','yi',variance='vi',es_id='id')
 a <- metaUI_aggregate(d); add('helper-fixture-study',a$metaUI__effect_size,a$metaUI__variance)
}
x <- read.csv('inst/examples/tiny.csv'); add('tiny-effects',x$yi,x$vi)
if (requireNamespace('metaUI',quietly=TRUE)) {
 d <- metaUI::prepare_data(x,'study','yi',variance='vi',es_id='effect'); a <- metaUI_aggregate(d)
 add('tiny-study',a$metaUI__effect_size,a$metaUI__variance)
}
x <- read.csv('inst/examples/dannheim/mental-health.csv'); add('dannheim',x$g,x$se_g^2)
r <- c(.12,.28,-.08,.35,.18,.42,.05,.24)
add('correlations-build',atanh(r),c(.010,.008,.014,.006,.009,.005,.012,.007)/(1-r^2)^2)
n <- rep(c(40,60,80,100),2)
add('correlations-alternative-raw',atanh(r),1/(n-1))
add('correlations-alternative-fisher',atanh(r),1/(n-3))
for (i in 1:300) {
 k <- if(i<=59) i+1 else sample(2:60,1)
 extreme <- i%%5==0
 v <- exp(runif(k, if(extreme) -18 else -6, if(extreme) 6 else 0))
 tau <- c(0,.01,.2,1,3)[1+i%%5]; mu <- c(-1,-.2,0,.2,1)[1+i%%5]
 y <- rnorm(k,mu,sqrt(v+tau^2))
 if(i%%3!=0) y <- mu + (if(i%%3==1) 1 else -1)*abs(y-mu) + (if(i%%3==1) 1 else -1)*sqrt(v)
 add(sprintf('sim-%03d',i),y,v,'simulation')
}
for(k in c(1,2,3,5,20,60)) {
 add(paste0('identical-',k),rep(.2,k),seq(.01,.1,length.out=k),'edge')
 add(paste0('equal-precision-',k),seq(-.5,.7,length.out=k),rep(.04,k),'edge')
}
add('ties-symmetric',c(-1,-1,0,0,1,1),rep(c(.02,.04),3),'edge')
add('zero-variance',c(.1,.3,.7),c(0,.01,.1),'edge')
add('missing',c(.1,NA,.3,.7),c(.01,.02,.03,.1),'edge')
# Explicitly force tau iteration failure, separately from ordinary default fits.
add('forced-nonconvergence',c(-3,-1,0,.2,1,8),c(.01,.02,.1,.3,.5,1),'forced')
saveRDS(cases,file.path(out,'datasets.rds'))
cap <- function(f) {
 w <- character(); err <- ''
 z <- tryCatch(withCallingHandlers(f(),warning=function(e) {w <<- c(w,conditionMessage(e));invokeRestart('muffleWarning')}),error=function(e) {err <<- conditionMessage(e);NULL})
 list(value=z,warnings=paste(unique(w),collapse='; '),error=err,status=if(nzchar(err)) 'error' else if(is.null(z)) 'NULL' else 'ok')
}
mfit <- function(d,method) meta::metagen(d$y,sqrt(d$v),sm='SMD',common=FALSE,random=TRUE,method.tau=method,method.random.ci='HK',adhoc.hakn.ci='',prediction=(method=='ML'),control=if(d$group=='forced') list(maxiter=1) else list())
rfit <- function(d,method) metafor::rma.uni(yi=d$y,sei=sqrt(d$v),method=method,test='knha',control=if(d$group=='forced') list(maxiter=1) else list())
num <- function(z,kind) {
 if(is.null(z)) return(numeric())
 if(kind=='meta') c(estimate=z$TE.random,lower=z$lower.random,upper=z$upper.random,k=z$k,tau2=z$tau2)
 else c(estimate=as.numeric(z$b),lower=z$ci.lb,upper=z$ci.ub,k=z$k,tau2=z$tau2)
}

# Frozen meta 8.5.0 oracle from the original audit, including every warning/error.
# Regenerated datasets above use its exact seed and recipe.
oracle <- read.csv('work/shinylive/research/evidence/meta-8.5.0-baseline.csv', na.strings='NA')
stopifnot(length(cases) == 324L, setequal(names(cases), unique(oracle$dataset)))
rows <- list()
for (id in names(cases)) {
 cat(id, '\n'); flush.console(); d <- cases[[id]]
 for (kind in c('REML-HK','ML-HK','trimfill-FE-then-ML-HK','Egger-LM')) {
  b <- cap(function() {
   if(kind=='trimfill-FE-then-ML-HK') metaUI_trimfill(data.frame(metaUI__effect_size=d$y,metaUI__se=sqrt(d$v)),control=if(d$group=='forced')list(maxiter=1) else list())
   else if(kind=='Egger-LM') { initial <- metaUI_univariate(d$y,sqrt(d$v),control=if(d$group=='forced')list(maxiter=1) else list()); metaUI_egger(d$y,sqrt(d$v)) }
   else metaUI_univariate(d$y,sqrt(d$v), sub('-HK','',kind), control=if(d$group=='forced') list(maxiter=1) else list())
  })
  z <- b$value
  q <- if(kind=='Egger-LM') {
   if(is.null(z)) c(intercept=NA,variance=NA,t=NA,p=NA) else c(intercept=unname(z$estimate[1]),variance=z$tau^2,t=unname(z$statistic),p=z$p.value)
  } else num(z,'rma')
  if(kind=='trimfill-FE-then-ML-HK') q <- c(q,k0=if(!is.null(z))z$k0 else NA,left=if(!is.null(z))as.numeric(z$left) else NA)
  a <- oracle[oracle$dataset==id & oracle$comparison==kind,]
  a$candidate <- as.numeric(q[a$quantity]); a$difference <- abs(a$meta-a$candidate)
  a$candidate_status <- b$status; a$candidate_warnings <- b$warnings; a$candidate_error <- b$error
  a$comparison[a$comparison=='trimfill-FE-then-ML-HK'] <- 'trimfill-helper'
  rows[[length(rows)+1]] <- a
 }
 write.csv(do.call(rbind,rows),file.path(out,'all-comparisons.csv'),row.names=FALSE)
}
z <- do.call(rbind,rows)
writeLines(capture.output(sessionInfo()),file.path(out,'session.txt'))
