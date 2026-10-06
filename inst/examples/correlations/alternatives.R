# Synthetic source data. Compare two declared correlation variance calculations.
# Run from a copy of this example directory; the output must be fresh.
library(metaUI)
x <- data.frame(study=LETTERS[1:8],effect=1,
  r=c(.12,.28,-.08,.35,.18,.42,.05,.24))
# Illustrative study sample sizes: both formulas target the same correlation.
x$n <- rep(c(40,60,80,100),length.out=nrow(x))
x$z <- atanh(x$r)
x$variance_r <- (1-x$r^2)^2/(x$n-1)
x$variance_z <- 1/(x$n-3)
d <- prepare_data(x, "study", "r", variance="variance_r", es_type="COR",
  variance_scale="r", es_id="effect", primary_label="Raw-r delta variance",
  alternatives=list(`Fisher-z variance`=list(es_field="z",variance="variance_z",
    es_type="ZCOR",variance_scale="z",
    justification="Fisher-z normal approximation var(z)=1/(n-3), versus propagation of the raw-r approximation var(r)=(1-r^2)^2/(n-1). Both target the same population correlation; these are approximate sampling-variance choices for independent bivariate-normal observations.")))
generate_shiny(d, "Correlation variance choices (synthetic)", save_to_folder="app-alternatives",
  launch_app=FALSE)
