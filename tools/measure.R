# Run in a fresh process for each package version; see validation/reproduction.md.
args <- commandArgs(TRUE)
if (length(args) < 2) stop("Usage: measure.R baseline|candidate|candidate-directed output.csv [package-library]")
if (!args[1] %in% c("baseline", "candidate", "candidate-directed")) stop("Unknown mode: ", args[1])
if (length(args) == 3) .libPaths(c(args[3], .libPaths()))
suppressPackageStartupMessages(library(metaUI))
# Written first so interrupted runs keep their environment evidence.
writeLines(capture.output(sessionInfo()), paste0(args[2], ".session.txt"))
set.seed(20261005)
rows <- list()
for (k in c(50,200,1000)) {
  x <- data.frame(study = rep(seq_len(k/2), each=2), id=rep(1:2,k/2),
                  yi = rep(rnorm(k/2,.3,.18),each=2)+rnorm(k,0,.12), vi=runif(k,.01,.05))
  x$se <- sqrt(x$vi); x$p <- 2*pnorm(-abs(x$yi/x$se)); x$N <- 100
  d <- prepare_data(x,"study","yi",se="se",variance="vi",pvalue="p",sample_size="N",filters="id")
  if (args[1] == "candidate-directed") d$metaUI__direction <- "positive"
  models <- get_model_tibble()
  if (args[1] == "baseline") {
    agg <- function(yi,vi,r=.6) { inv<-solve(tcrossprod(sqrt(vi))*(r+diag(1-r,length(vi)))); c(yi=sum(inv%*%yi)/sum(inv),vi=1/sum(inv)) }
    a <- do.call(rbind,lapply(split(x,x$study),function(z) agg(z$yi,z$vi)))
    da <- data.frame(metaUI__study_id=seq_len(nrow(a)),metaUI__effect_size=a[,1],metaUI__variance=a[,2],metaUI__se=sqrt(a[,2]),metaUI__es_type="SMD")
    run <- function(i) {
      spec <- models[i,]; df <- if (spec$aggregated) da else d
      status<-"ok"; msg<-""; started<-proc.time()[3]
      tryCatch({ mod <- eval(parse(text=spec$code)); val<-eval(parse(text=spec$es)); if(any(!is.finite(val))) stop("nonfinite estimate") },error=function(e){status<<-"failed";msg<<-conditionMessage(e)})
      data.frame(operation=spec$name,status=status,reason=msg,seconds=unname(proc.time()[3]-started))
    }
  } else {
    # Unspecified direction is intentional; do not favour positive synthetic results.
    run <- function(i) {
      tab <- metaUI:::metaUI_fit_models(d,models[i,])$table
      data.frame(operation=tab$Model,status=tab$status,reason=tab$reason,seconds=tab$fit_seconds)
    }
  }
  for (phase in c("cold_fit","warm_fit")) for(i in seq_len(nrow(models))) {
    cat(args[1],k,phase,models$name[i],"\n")
    r<-run(i); r$k<-k; r$phase<-phase; r$version<-args[1]; rows[[length(rows)+1]]<-r
    write.csv(do.call(rbind,rows),args[2],row.names=FALSE)
  }
  for(phase in c("cold_render","warm_render")) {
    # The actual generated app caps the individual forest at 200 effects.
    for (op in c("model_comparison","forest")) {
      started<-proc.time()[3]; status<-"ok"; reason<-""
      png(tempfile(fileext=".png"),width=900,height=if(k<=200) 400+25*k else 200)
      tryCatch({
        if(op=="forest") {
          if(k>200) {status<-"limited";reason<-"App's documented 200-effect forest cap";plot.new();text(.5,.5,reason)} else {
            mod<-robumeta::robu(metaUI__effect_size~1,data=d,studynum=metaUI__study_id,var.eff.size=metaUI__variance,small=FALSE)
            robumeta::forest.robu(mod,es.lab="metaUI__es_label",study.lab="metaUI__study_id")
          }
        } else {
          plot_df<-if(args[1]=="baseline") data.frame(Model=models$name,es=rep(.3,nrow(models)),LCL=rep(.2,nrow(models)),UCL=rep(.4,nrow(models))) else metaUI:::metaUI_fit_models(d,models)$table
          # Fit costs are separately measured; render costs start after data are ready.
          started<-proc.time()[3]
          print(ggplot2::ggplot(plot_df,ggplot2::aes(x=es,y=Model))+ggplot2::geom_point()+ggplot2::geom_errorbarh(ggplot2::aes(xmin=LCL,xmax=UCL)))
        }
      },error=function(e){status<<-"failed";reason<<-conditionMessage(e)})
      dev.off(); rows[[length(rows)+1]]<-data.frame(operation=op,status=status,reason=reason,seconds=unname(proc.time()[3]-started),k=k,phase=phase,version=args[1])
      write.csv(do.call(rbind,rows),args[2],row.names=FALSE)
    }
  }
}
