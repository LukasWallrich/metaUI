test_that("optional fields preserve membership and invalid required rows are reported", {
  d <- prepared()
  expect_equal(nrow(d), 16)
  expect_true(all(is.na(d$metaUI__N)))
  expect_true(all(is.na(d$metaUI__pvalue)))
  x <- fixture(); x$vi[2] <- 0; x$yi[4] <- Inf
  expect_warning(d <- prepared(x), "2 rows excluded")
  expect_equal(attr(d, "metaUI_validation")$exclusions$row, c(2L, 4L))
  expect_error(prepared(x, na.rm = FALSE), "Invalid required")
  x <- fixture(); x$id[2] <- 1
  expect_error(prepared(x), "unique within study")
  expect_error(prepared(es_type = "OR"), "Supported metrics")
  expect_error(prepare_data(fixture(), "study", "yi"), "Supply variance")
  x <- fixture(); x$se <- rep(2, 16)
  expect_error(prepare_data(x, "study", "yi", se = "se", variance = "vi"), "disagree")
  x <- fixture(); x$p <- NA_real_; x$N <- 100; x$N[1] <- NA; x$p[2] <- 2
  d <- prepare_data(x, "study", "yi", variance = "vi", pvalue = "p", sample_size = "N")
  expect_equal(nrow(d), 16)
  expect_equal(attr(d,"metaUI_validation")$optional_inputs$invalid_p, 1)
})

test_that("true per-effect nesting and RVE agree with independent R fits", {
  x <- fixture(); d <- prepared(x); r <- fit(d)$table
  ref <- metafor::rma.mv(yi, V = vi, random = ~ 1 | study/id, data = x, method = "REML", test = "t", sparse = TRUE)
  rve <- robumeta::robu(yi ~ 1, data = x, studynum = study, var.eff.size = vi, small = FALSE)
  expect_equal(r$fit_es, c(as.numeric(ref$b), as.numeric(rve$reg_table$b.r)), tolerance = 1e-8)
  expect_equal(r$fit_LCL, c(ref$ci.lb, rve$reg_table$CI.L), tolerance = 1e-8)
  expect_equal(r$status, c("ok", "ok"))
  wrong <- metafor::rma.mv(yi, V = vi, random = ~ 1 | study/yi, data = x, method = "REML", test = "t", sparse = TRUE)
  expect_gt(abs(as.numeric(ref$b) - as.numeric(wrong$b)), 1e-6)
  d2 <- prepare_data(x, "study", "yi", variance = "vi", es_label = "label")
  expect_equal(length(unique(d2$metaUI__effect_id)), 16)
  expect_equal(fit(d2)$table$fit_es, r$fit_es, tolerance = 1e-8)
})

test_that("GLS aggregation matches explicit covariance algebra", {
  d <- prepared(); a <- metaUI:::metaUI_aggregate(d, .6)
  idx <- which(d$metaUI__study_id == "B")
  vi <- d$metaUI__variance[idx]; y <- d$metaUI__effect_size[idx]
  V <- diag(sqrt(vi)) %*% matrix(c(1,.6,.6,1),2) %*% diag(sqrt(vi))
  inv <- solve(V)
  expect_equal(a$metaUI__variance[2], 1/sum(inv), tolerance = 1e-10)
  expect_equal(a$metaUI__effect_size[2], as.numeric(t(rep(1,2)) %*% inv %*% y / sum(inv)), tolerance = 1e-10)
  expect_true(all(is.na(a$metaUI__N)))
  expect_error(metaUI:::metaUI_aggregate(d, 1), "correlation")
})

test_that("correlation scales are explicit and backtransform exactly once", {
  x <- fixture(); x$yi <- x$yi/2
  expect_error(prepared(x, es_type = "COR"), "variance_scale")
  d <- prepared(x, es_type = "COR", variance_scale = "r")
  expect_equal(d$metaUI__effect_size, atanh(x$yi))
  expect_equal(d$metaUI__variance, x$vi/(1-x$yi^2)^2)
  z <- x; z$yi <- atanh(x$yi); z$vi <- x$vi/(1-x$yi^2)^2
  dz <- prepared(z, es_type = "ZCOR", variance_scale = "z")
  expect_equal(dz$metaUI__effect_size, z$yi)
  r <- fit(d)$table; rz <- fit(dz)$table
  expect_equal(r$es, tanh(r$fit_es), tolerance = 1e-10)
  expect_equal(r$es, rz$es, tolerance = 1e-8)
  expect_equal(r$LCL, tanh(r$fit_LCL), tolerance = 1e-10)
  x$yi[1] <- 1
  expect_warning(d <- prepared(x, es_type="COR", variance_scale="r"), "1 rows excluded")
})

test_that("directional models require direction; failure is explicit", {
  d <- prepared(); m <- get_model_tibble()
  r <- fit(d, m)$table
  expect_equal(r$status[4:5], c("unsupported", "unsupported"))
  expect_match(r$reason[4], "explicit")
  broken <- m[1, ]; broken$code <- 'stop("reference failure")'
  r <- fit(d, broken)$table
  expect_equal(r$status, "failed"); expect_equal(r$reason, "reference failure")
  x <- fixture(); x$vi <- .01
  r <- fit(prepared(x), m[6:7, ])$table
  expect_true(all(r$status == "unsupported"))
})

test_that("PET and PEESE reflect signs without choosing direction", {
  x <- fixture(); positive <- fit(prepared(x), get_model_tibble()[6:7, ])$table
  x$yi <- -x$yi; negative <- fit(prepared(x), get_model_tibble()[6:7, ])$table
  expect_equal(negative$es, -positive$es, tolerance = 1e-8)
  expect_equal(negative$LCL, -positive$UCL, tolerance = 1e-8)
})

test_that("directional bias fits match explicitly oriented reference fits", {
  x <- fixture(); x$yi <- x$yi + .45
  d <- prepared(x, direction="positive"); a <- metaUI:::metaUI_aggregate(d)
  m <- get_model_tibble()[4:5, ]
  plus <- fit(d, m)$table
  ref_p <- puniform::puni_star(yi=a$metaUI__effect_size,vi=a$metaUI__variance,alpha=.05,side="right",method="ML",boot=FALSE)
  ref_s <- suppressWarnings(weightr::weightfunct(a$metaUI__effect_size,a$metaUI__variance,steps=c(.025,1),fe=FALSE))
  expect_equal(plus$fit_es, c(ref_p$est,ref_s$output_adj$par[2]),tolerance=1e-8)
  x$yi <- -x$yi
  minus <- fit(prepared(x,direction="negative"),m)$table
  expect_equal(minus$status, c("ok","ok"))
  expect_equal(minus$es,-plus$es,tolerance=1e-8)
  expect_equal(minus$LCL,-plus$UCL,tolerance=1e-8)
  # Same right-sided assumption on reflected data is a different model scenario.
  expect_equal(prepared(x,direction="positive")$metaUI__direction,rep("positive",16))
  renamed <- m; renamed$name <- c("My p-uniform", "My selection model")
  expect_equal(fit(prepared(x,direction="negative"),renamed)$table$es, minus$es, tolerance=1e-8)
  expect_match(fit(prepared(x),renamed)$table$reason, "explicit")
})

test_that("synthetic Fisher-z data agree with direct reference fits", {
  x <- fixture()
  d <- prepare_data(x,"study","yi",variance="vi",es_id="id",es_type="ZCOR",variance_scale="z",direction="negative")
  r <- fit(d)$table
  ref <- metafor::rma.mv(yi,V=vi,random=~1|study/id,data=x,method="REML",test="t",sparse=TRUE)
  rve <- robumeta::robu(yi~1,data=x,studynum=study,var.eff.size=vi,small=FALSE)
  expect_equal(r$fit_es,c(as.numeric(ref$b),rve$reg_table$b.r),tolerance=1e-6)
  expect_equal(r$es,tanh(r$fit_es),tolerance=1e-10)
})

test_that("post-preparation edits and uploads cannot mix effects/scales or IDs", {
  d <- prepared(); d$metaUI__es_type[2] <- "ZCOR"
  expect_error(fit(d), "Mixed or missing")
  d <- prepared(); d$metaUI__effect_id[2] <- d$metaUI__effect_id[1]
  expect_error(fit(d), "unique within study")
  d <- prepared(); d$metaUI__variance[1] <- -1
  expect_error(fit(d), "contract failed")
})

test_that("p-curve selection has an explicit direction and model-specific N", {
  x <- fixture(); x$yi <- rep(c(.35,.2,-.3,.2),4)
  d <- prepared(x)
  expect_error(metaUI:::metaUI_pcurve_data(d), "explicit effect direction")
  plus <- metaUI:::metaUI_pcurve_data(prepared(x,direction="positive"))
  expect_equal(attr(plus,"direction_exclusions"),4)
  expect_equal(attr(plus,"selection_exclusions"),8)
  expect_true(all(plus$TE > 0)); expect_true(all(is.na(plus$n)))
  expect_error(metaUI:::metaUI_pcurve_data(prepared(x,direction="positive"),require_N=TRUE), "valid N")
  x$yi <- -x$yi
  minus <- metaUI:::metaUI_pcurve_data(prepared(x,direction="negative"))
  expect_identical(plus,minus)
  # The underlying p-curve normal Wald statistics are identical under declared reflection.
  expect_equal(2*pnorm(-abs(plus$TE/plus$seTE)),2*pnorm(-abs(minus$TE/minus$seTE)))
  expect_equal(nrow(prepared(na.rm=TRUE)),16)
})

test_that("failed extraction cannot leave an apparently usable effect", {
  m <- get_model_tibble()[1, ]; m$UCL <- 'Inf'
  r <- fit(prepared(),m)$table
  expect_equal(r$status,"failed"); expect_true(is.na(r$es)); expect_true(is.na(r$fit_es))
})

test_that("p-curve statistics reflect declared direction without N imputation", {
  x<-fixture();x$yi<-rep(c(.35,.2,-.3,.2),4)
  plus<-metaUI:::metaUI_pcurve_data(prepared(x,direction="positive"))
  x$yi<- -x$yi;minus<-metaUI:::metaUI_pcurve_data(prepared(x,direction="negative"))
  plotfile<-tempfile(fileext=".png");grDevices::png(plotfile)
  on.exit({grDevices::dev.off();unlink(plotfile)})
  a<-metaUI:::pcurve(plus,effect.estimation=FALSE)
  b<-metaUI:::pcurve(minus,effect.estimation=FALSE)
  expect_equal(a$pcurveResults,b$pcurveResults,tolerance=1e-8)
  expect_equal(a$kAnalyzed,b$kAnalyzed)
  expect_error(fit(prepared()[0,]),"No eligible rows")
})

test_that("slider rounding encloses all effects and never silently filters extremes", {
  x<-c(-1.817429,1.689891,-.001817,1817.429,0)
  lo<-vapply(x,metaUI:::signif_floor,numeric(1),digits=2)
  hi<-vapply(x,metaUI:::signif_ceiling,numeric(1),digits=2)
  expect_true(all(lo <= x));expect_true(all(hi >= x))
  expect_equal(lo[1],-1.9)
  d<-prepared()
  bounds<-c(metaUI:::signif_floor(min(d$metaUI__es_z)),metaUI:::signif_ceiling(max(d$metaUI__es_z)))
  expect_true(all(d$metaUI__es_z >= bounds[1] & d$metaUI__es_z <= bounds[2]))
})

test_that("IDs can also be display labels and small variances require matching SE", {
  x<-fixture()
  d<-prepare_data(x,"study","yi",variance="vi",es_id="id",es_label="id")
  expect_equal(d$metaUI__effect_id,x$id)
  x$vi<-1e-10;x$se<-1e-4
  expect_error(prepare_data(x,"study","yi",variance="vi",se="se"),"disagree")
})

test_that("factor study IDs ignore unused levels in first and last selection", {
  x<-fixture();x$study<-factor(x$study);x$yi<-rep(.4,16)
  d<-prepared(x,direction="positive");d<-d[d$metaUI__study_id!="A",]
  for(selection in c("first","last")) {
    s<-metaUI:::metaUI_pcurve_data(d,selection)
    expect_equal(nrow(s),7);expect_equal(attr(s,"direction_exclusions"),0)
  }
  r<-fit(d)$table
  expect_equal(r$filtered_rows,c(2,2))
})


test_that("study labels can also be effect display labels", {
  x <- fixture()
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id", es_label = "study")
  expect_identical(as.character(d$metaUI__es_label), x$study)
  expect_identical(as.character(d$metaUI__study_id), x$study)
})


test_that("slider specs enclose data with an integer, legible grid", {
  set.seed(20261006)
  ranges <- c(list(c(-2.17, .966), 2001:2016, c(.03, .97), c(20, 5000), c(3, 3), c(-.004, .0031), c(120000, 980000)),
    replicate(200, sort(round(runif(2, -10^runif(1, -3, 5), 10^runif(1, -3, 5)), sample(0:4, 1))), simplify = FALSE))
  for (x in ranges) {
    s <- metaUI:::metaUI_slider_spec(x)
    expect_true(s$min <= min(x) && s$max >= max(x))
    expect_true(s$ticks >= 1 && s$ticks <= 10 && s$ticks == round(s$ticks))
    interval <- (s$max - s$min) / s$ticks
    expect_lt(abs(interval / s$step - round(interval / s$step)), 1e-6)
    if (all(x == round(x))) expect_identical(s$step, 1)
  }
  slider <- as.character(metaUI:::metaUI_slider_input("z", "z", metaUI:::metaUI_slider_spec(c(-2.17, .966))))
  expect_match(slider, 'data-min="-2.5"'); expect_match(slider, 'data-max="1"')
  expect_match(slider, 'data-grid-num="7"'); expect_equal(lengths(regmatches(slider, gregexpr("data-grid-num", slider))), 1)
})
