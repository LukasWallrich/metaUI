# Run after meta-equivalence.R, from the repository root, with the same R libraries.
out <- '/tmp/shinylive-speed/B/equivalence'
z <- read.csv(file.path(out,'all-comparisons.csv'),check.names=FALSE)
# Full fitted objects are not needed to summarise the CSV evidence.
s <- do.call(rbind,lapply(split(z,list(z$comparison,z$quantity),drop=TRUE),function(x) data.frame(comparison=x$comparison[1],quantity=x$quantity[1],finite_pairs=sum(is.finite(x$difference)),max_abs_difference=if(any(is.finite(x$difference))) max(x$difference[is.finite(x$difference)]) else NA,above_tolerance=sum(x$difference>1e-8,na.rm=TRUE))))
write.csv(s,file.path(out,'summary.csv'),row.names=FALSE)
behavior <- unique(z[,c('dataset','group','comparison','meta_status','candidate_status','meta_warnings','candidate_warnings','meta_error','candidate_error','warnings_match','status_match')])
behavior <- behavior[!behavior$warnings_match | !behavior$status_match | behavior$meta_error!=behavior$candidate_error,]
write.csv(behavior,file.path(out,'behaviour-mismatches.csv'),row.names=FALSE)
print(read.csv(file.path(out,'summary.csv')),row.names=FALSE)
cat('\nPer-comparison numbers/status/warnings (datasets):\n')
for (lab in unique(z$comparison)) {
 x<-z[z$comparison==lab,]; b<-unique(x[,c('dataset','warnings_match','status_match')])
 cat(lab,': >1e-8 =',length(unique(x$dataset[!is.na(x$difference)&x$difference>1e-8])), ', warning mismatch =',sum(!b$warnings_match),', status mismatch =',sum(!b$status_match),'\n')
}
cat('\nDannheim:\n'); print(z[z$dataset=='dannheim',c('comparison','quantity','meta','candidate','difference')],row.names=FALSE,digits=15)
cat('\nREML/ML mismatches:\n'); print(z[z$comparison %in% c('REML-HK','ML-HK') & !is.na(z$difference)&z$difference>1e-8,],row.names=FALSE)
cat('\nBehaviour examples:\n'); print(behavior[behavior$group!='simulation',],row.names=FALSE)
cat('\nFilling sides on simulated cases, meta:\n')
x<-z[z$comparison=='trimfill-direct' & z$group=='simulation',]
k0<-x[x$quantity=='k0',c('dataset','meta')]; sides<-x[x$quantity=='left',c('dataset','meta')]
y<-merge(k0,sides,by='dataset',suffixes=c('_k0','_left'));print(with(y,table(meta_left,filled=meta_k0>0,useNA='ifany')))
cat('\nNonconvergence cases:\n');print(behavior[grepl('converg',paste(behavior$meta_error,behavior$candidate_error,behavior$meta_warnings,behavior$candidate_warnings)),],row.names=FALSE)
cat('\nMatched trimfill numeric discrepancies:\n');print(z[z$comparison=='trimfill-FE-then-ML-HK' & !is.na(z$difference)&z$difference>1e-8,],row.names=FALSE)
