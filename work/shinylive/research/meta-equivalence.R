# Reproduce from repository root. All generated evidence stays in the B scratch area.
# R_LIBS_USER=/tmp/shinylive-speed/B/lib:/tmp/shinylive-explore/lib \
# R_LIBS_SITE=/tmp/shinylive-explore/emptysite Rscript --vanilla work/shinylive/research/meta-equivalence.R
out <- '/tmp/shinylive-speed/B/equivalence'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
stopifnot(requireNamespace('meta'), requireNamespace('metafor'))
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
rows <- list(); details <- list()
compare <- function(id,label,a,b,qa,qb) {
 keys <- union(names(qa),names(qb)); if (!length(keys)) { keys <- 'unavailable'; qa <- qb <- c(unavailable=NA_real_) }; delta <- abs(qa[keys]-qb[keys]); names(delta) <- keys
 same_num <- length(delta)>0 && all(is.finite(delta) & delta<=1e-8)
 # Treat matching missing outputs as missing, without asserting numerical equality.
 rows[[length(rows)+1]] <<- data.frame(dataset=id,group=cases[[id]]$group,comparison=label,
  meta_status=a$status,candidate_status=b$status,meta_warnings=a$warnings,candidate_warnings=b$warnings,
  meta_error=a$error,candidate_error=b$error,
  numbers_match=same_num,warnings_match=identical(a$warnings,b$warnings),status_match=identical(a$status,b$status),
  quantity=keys,meta=as.numeric(qa[keys]),candidate=as.numeric(qb[keys]),difference=as.numeric(delta))
 # Avoid serialising formula environments (which can retain the whole audit).
 keep <- function(x) { if(!is.null(x$value)) x$value <- x$value[intersect(names(x$value),c('TE','seTE','yi','vi','trimfill','fill','left','side','k','k0','tau2','TE.random','lower.random','upper.random','b','ci.lb','ci.ub','estimate','statistic','p.value'))]; x }
 details[[paste(id,label,sep='/')]] <<- list(meta=keep(a),candidate=keep(b))
}
for(id in names(cases)) {
 cat(id, '\n'); flush.console()
 d <- cases[[id]]
 for(method in c('REML','ML')) {
  a <- cap(function() mfit(d,method)); b <- cap(function() rfit(d,method))
  compare(id,paste0(method,'-HK'),a,b,num(a$value,'meta'),num(b$value,'rma'))
 }
 a <- cap(function() meta::trimfill(mfit(d,'ML')))
 b <- cap(function() metafor::trimfill(rfit(d,'ML'),test='knha'))
 qa <- c(num(a$value,'meta'),k0=if(!is.null(a$value)) sum(a$value$trimfill) else NA_real_,left=if(!is.null(a$value)) as.numeric(a$value$left) else NA_real_)
 qb <- c(num(b$value,'rma'),k0=if(!is.null(b$value)) b$value$k0 else NA_real_,left=if(!is.null(b$value)) as.numeric(b$value$side=='left') else NA_real_)
 compare(id,'trimfill-direct',a,b,qa,qb)
 # Match common-effect trimming and Egger LM side selection; refit ML/HK on augmented data.
 b <- cap(function() {
  f <- metafor::rma.uni(d$y,sei=sqrt(d$v),method='FE')
  t <- metafor::trimfill(f,maxiter=50)
  z <- metafor::rma.uni(t$yi,vi=t$vi,method='ML',test='knha')
  z$k0 <- t$k0; z$side <- t$side; z
 })
 qb <- c(num(b$value,'rma'),k0=if(!is.null(b$value)) b$value$k0 else NA_real_,left=if(!is.null(b$value)) as.numeric(b$value$side=='left') else NA_real_)
 compare(id,'trimfill-FE-then-ML-HK',a,b,qa,qb)
 a <- cap(function() meta::metabias(mfit(d,'ML'),k.min=3,method.bias='Egger'))
 b <- cap(function() { r <- rfit(d,'ML'); metafor::regtest(r,model='lm',predictor='sei',ret.fit=TRUE) })
 qa <- if(is.null(a$value)) c(intercept=NA,variance=NA,t=NA,p=NA) else c(intercept=unname(a$value$estimate[1]),variance=a$value$tau^2,t=unname(a$value$statistic),p=a$value$p.value)
 qb <- if(is.null(b$value)) c(intercept=NA,variance=NA,t=NA,p=NA) else c(intercept=unname(coef(b$value$fit)[2]),variance=suppressWarnings(summary(b$value$fit))$sigma^2,t=b$value$zval,p=b$value$pval)
 compare(id,'Egger-LM',a,b,qa,qb)
}
z <- do.call(rbind,rows)
write.csv(z,file.path(out,'all-comparisons.csv'),row.names=FALSE)
write.csv(z[!is.na(z$difference)&z$difference>1e-8,],file.path(out,'numeric-mismatches.csv'),row.names=FALSE)
saveRDS(details,file.path(out,'fits.rds'))
s <- do.call(rbind,lapply(split(z,list(z$comparison,z$quantity),drop=TRUE),function(x) data.frame(comparison=x$comparison[1],quantity=x$quantity[1],finite_pairs=sum(is.finite(x$difference)),max_abs_difference=if(any(is.finite(x$difference))) max(x$difference[is.finite(x$difference)]) else NA,above_tolerance=sum(x$difference>1e-8,na.rm=TRUE))))
write.csv(s,file.path(out,'summary.csv'),row.names=FALSE)
writeLines(c(capture.output(sessionInfo()),capture.output(meta::settings.meta())),file.path(out,'session.txt'))
print(s,row.names=FALSE)
cat('Datasets:',length(cases),' simulated:',sum(vapply(cases,function(d)d$group=='simulation',logical(1))),'\n')
