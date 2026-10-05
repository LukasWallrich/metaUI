# Bounded local panel algorithms, separate from network/UI timings.
args<-commandArgs(TRUE); if(length(args)!=1) stop("Usage: measure-panels.R output.csv")
suppressPackageStartupMessages(library(metaUI))
set.seed(20261005)
rows<-list()
for(k in c(50,200,1000)) {
  x<-data.frame(study=rep(seq_len(k/2),each=2),id=rep(1:2,k/2),
                 yi=rep(rnorm(k/2,.3,.18),each=2)+rnorm(k,0,.12),vi=runif(k,.01,.05),year=seq_len(k))
  d<-prepare_data(x,"study","yi",variance="vi",es_id="id",direction="positive",filters="year")
  a<-metaUI:::metaUI_aggregate(d)
  meta_fit<-function() meta::metagen(a$metaUI__effect_size,a$metaUI__se,studlab=a$metaUI__study_id,
                                    common=FALSE,random=TRUE,method.tau="ML",method.random.ci="HK",prediction=TRUE,sm="SMD")
  for(phase in c("cold_panel","warm_panel")) for(op in c("heterogeneity","numeric_moderation","funnel","egger","pcurve","zcurve")) {
    cat(k,phase,op,"\n")
    started<-proc.time()[3]; status<-"ok";reason<-"";warnings<-character()
    png(tempfile(fileext=".png"),width=900,height=650)
    tryCatch(withCallingHandlers({
      if(op=="heterogeneity") metafor::rma.mv(metaUI__effect_size,V=metaUI__variance,
                      random=~1|metaUI__study_id/metaUI__effect_id,data=d,test="t",method="REML",sparse=TRUE)
      if(op=="numeric_moderation") metafor::rma.mv(metaUI__effect_size,V=metaUI__variance,
                      mods=~metaUI__filter_year,random=~1|metaUI__study_id/metaUI__effect_id,data=d,test="t",method="ML",sparse=TRUE)
      if(op=="funnel") metafor::funnel(meta_fit(),studlab=FALSE,contour=.95,col.contour="light grey")
      if(op=="egger") meta::metabias(meta_fit(),k.min=3,method.bias="Egger")
      if(op=="pcurve") {sel<-metaUI:::metaUI_pcurve_data(d);metaUI:::pcurve(sel,effect.estimation=FALSE)}
      if(op=="zcurve") {sel<-d[!duplicated(d$metaUI__study_id),];mod<-zcurve::zcurve(abs(sel$metaUI__effect_size/sel$metaUI__se),bootstrap=FALSE);zcurve::plot.zcurve(mod,annotation=TRUE,main="")}
    },warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e){status<<-"failed";reason<<-conditionMessage(e)})
    dev.off()
    rows[[length(rows)+1]]<-data.frame(k=k,phase=phase,operation=op,status=status,reason=reason,
                 warnings=paste(unique(warnings),collapse="; "),seconds=unname(proc.time()[3]-started))
    write.csv(do.call(rbind,rows),args[1],row.names=FALSE)
  }
}
