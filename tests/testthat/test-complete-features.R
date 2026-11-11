skip_without_app_packages()

test_that("alternatives preserve identities and enforce each scale contract", {
  x <- fixture(); x$corrected <- x$yi * .9; x$vc <- x$vi * .81
  args <- list(Corrected = list(es_field="corrected", variance="vc", es_type="SMD", justification="Illustrative correction"))
  d <- prepared(x, alternatives=args, primary_label="Original")
  a <- metaUI:::metaUI_select_computation(d,"alt1")
  expect_equal(a$metaUI__effect_size,x$corrected)
  expect_equal(a$metaUI__variance,x$vc)
  expect_identical(a$metaUI__effect_id,d$metaUI__effect_id)
  expect_identical(a$metaUI__es_z,d$metaUI__es_z)
  expect_equal(metaUI:::metaUI_select_computation(a)$metaUI__effect_size,x$yi)
  expect_equal(names(metaUI:::metaUI_computations(d)),c("Original","Corrected"))
  edited <- d; edited$metaUI__alt_1_fit_variance[1] <- 1
  expect_equal(metaUI:::metaUI_validate_upload(edited,d)$metaUI__alt_1_fit_variance, d$metaUI__alt_1_fit_variance)
  x$yi[1] <- NA; x$corrected[1] <- NA
  expect_warning(dropped <- prepared(x,alternatives=args),"excluded")
  expect_equal(nrow(dropped),15)
  x$corrected[2] <- NA
  expect_error(prepared(x,alternatives=args),"invalid for retained")
  x <- fixture(); x$z <- atanh(x$yi); x$vz <- x$vi/(1-x$yi^2)^2
  d <- prepared(x,es_type="COR",variance_scale="r", alternatives=list(Z=list(es_field="z",variance="vz",es_type="ZCOR",variance_scale="z",justification="Same information on Fisher z")))
  expect_equal(metaUI:::metaUI_select_computation(d,"alt1")$metaUI__variance,d$metaUI__variance)
})

test_that("selection links validate build identity and recover exact categories", {
  x <- fixture(); x$group <- factor(rep(c("a, b","Über & ?"),8)); x$year <- 2001:2016
  d <- prepared(x,filters=c("group","year"))
  filters <- list(list(id="metaUI__filter_group",col="metaUI__filter_group",type="categorical"),list(id="metaUI__filter_year",col="metaUI__filter_year",type="numeric"))
  r <- data.frame(id=c(rep("outliers_z_scores",2),"metaUI__filter_group",rep("metaUI__filter_year",2),"metaUI__filter_year_include_NA"),selection=c(-1.8,2.2,"Über & ?",2002,2010,"TRUE"))
  q <- metaUI:::metaUI_selection_query(r,"abc",.3,d,filters)
  parsed <- metaUI:::metaUI_parse_selection(q,"abc",d,filters)
  expect_equal(parsed$filters$metaUI__filter_group$selection,"Über & ?")
  expect_equal(parsed$sesoi,.3)
  expect_null(metaUI:::metaUI_parse_selection(metaUI:::metaUI_selection_query(r,"abc",NULL,d,filters),"abc",d,filters)$sesoi)
  # A link from before a data update still applies, marked as such.
  stale <- metaUI:::metaUI_parse_selection(q,"xyz",d,filters)
  expect_true(stale$stale)
  expect_equal(stale$filters$metaUI__filter_group$selection,"Über & ?")
  expect_false(parsed$stale)
  expect_match(q, "^\\?metaui=abc&group=%C3%9Cber\\+%26\\+%3F&year=2002,2010&bound=0.3$")
  expect_equal(metaUI:::metaUI_parse_selection(paste0(q,"&fbclid=x1"),"abc",d,filters), parsed)
  expect_error(metaUI:::metaUI_parse_selection(sub("group=[^&]*", "group=Uber", q),"abc",d,filters),"Unknown")
  expect_error(metaUI:::metaUI_parse_selection(paste0(q,"&group=a%2C+b"),"abc",d,filters),"incomplete or damaged")
  expect_error(metaUI:::metaUI_parse_selection(paste0(q,"&zz"),"abc",d,filters),"incomplete or damaged")
  expect_equal(metaUI:::metaUI_parse_selection("?metaui=abc&group!=%C3%9Cber+%26+%3F&year-missing=no","abc",d,filters)$filters$metaUI__filter_group$selection, "a, b")
  r$selection[r$id=="metaUI__filter_group"] <- "not built"
  expect_error(metaUI:::metaUI_selection_query(r,"abc",NULL,d,filters),"Unknown")
})

# Independent reference for the Bayesian model: the marginal posterior of tau (mu integrated out
# under its flat prior) by stats::integrate on geometric segments, with explicit residuals.
bayes_reference <- function(y, sigma, s) {
  logp <- function(t) vapply(t, function(tt) { w <- 1/(sigma^2+tt^2); m <- sum(w*y)/sum(w)
    -.5*(tt/s)^2 + .5*sum(log(w)) - .5*log(sum(w)) - .5*sum(w*(y-m)^2) }, numeric(1))
  brk <- c(0, 10^seq(-6, 2, by = .25)); c0 <- max(logp(brk))
  dens <- function(t) exp(logp(t) - c0)
  seg <- function(f, upper = Inf) sum(vapply(seq_len(length(brk)-1), function(i) if (brk[i] >= upper) 0 else
    integrate(f, brk[i], min(brk[i+1], upper), rel.tol = 1e-10, abs.tol = 0)$value, numeric(1)))
  z <- seg(dens)
  cdf <- function(q) seg(function(t) dens(t)*vapply(t, function(tt) { w <- 1/(sigma^2+tt^2)
    pnorm((q - sum(w*y)/sum(w))*sqrt(sum(w))) }, numeric(1)))/z
  r <- range(y) + c(-1, 1)*3*max(sigma)
  list(mu = vapply(c(.025,.5,.975), function(a) uniroot(function(q) cdf(q)-a, r, tol=1e-12)$root, numeric(1)),
    tau = vapply(c(.025,.5,.975), function(a) exp(uniroot(function(lt) seg(dens, exp(lt))/z - a, log(c(1e-6, 100)),
      tol = 1e-12)$root), numeric(1)))
}

test_that("Bayesian grid posterior agrees with an independent adaptive integration", {
  cases <- list(
    list(y = c(-.2, .1, .4, .7), sigma = c(.1, .15, .2, .25), s = .5),
    list(y = c(.1, .5), sigma = c(.2, .3), s = .5),
    list(y = c(.3, .1, .9, -.4, .2), sigma = c(.01, .1, .3, 1, .05), s = .5),   # SE ratio 1:100
    list(y = c(.2, .25, .22, 1.8, .19, .3), sigma = rep(.1, 6), s = .025),     # outlier, prior far below the data
    list(y = c(.21, .19, .2, .2), sigma = c(.05, .08, .1, .06), s = 5),        # no heterogeneity, very wide prior
    list(y = c(0, 10, 10), sigma = c(1e-9, 1, 1), s = .5))                     # one study dominates the weights
  for (case in cases) {
    grid <- metaUI:::metaUI_bayesian_grid(case$y, case$sigma, case$s); ref <- bayes_reference(case$y, case$sigma, case$s)
    expect_lt(max(abs(grid$qposterior(mu.p = c(.025, .5, .975)) - ref$mu)) / grid$summary["sd", "mu"], 1e-6)
    expect_lt(max(abs(grid$qposterior(tau.p = c(.025, .5, .975)) - ref$tau)) / grid$summary["sd", "tau"], 1e-3)
    expect_equal(grid$pposterior(mu = ref$mu), c(.025, .5, .975), tolerance = 1e-6)
  }
  # Shifting all effects shifts mu and leaves tau and the posterior SDs unchanged.
  a <- metaUI:::metaUI_bayesian_grid(c(.2, .2, .5), c(.1, .1, .2), .5); b <- metaUI:::metaUI_bayesian_grid(c(.2, .2, .5) + 1e6, c(.1, .1, .2), .5)
  expect_equal(b$qposterior(mu.p = c(.025, .5, .975)) - 1e6, a$qposterior(mu.p = c(.025, .5, .975)), tolerance = 1e-6)
  expect_equal(b$summary[c("sd", "median"), "tau"], a$summary[c("sd", "median"), "tau"], tolerance = 1e-6)
  expect_equal(b$summary["sd", "mu"], a$summary["sd", "mu"], tolerance = 1e-6)
  # A very narrow posterior (prior and data in sharp conflict) is still resolved over many grid points.
  narrow <- metaUI:::metaUI_bayesian_grid(c(-1, 1), c(1e-14, 1e-14), 1e-10)
  expect_gt(sum(narrow$grid$weight > 1e-6), 100)
  expect_lt(max(abs(narrow$qposterior(tau.p = c(.025, .975)) -
    metaUI:::metaUI_bayesian_grid(c(-1, 1), c(1e-14, 1e-14), 1e-10, n = 20000L)$qposterior(tau.p = c(.025, .975)))) /
    narrow$summary["sd", "tau"], 1e-3)
  x <- data.frame(metaUI__effect_size=cases[[1]]$y,metaUI__se=cases[[1]]$sigma,metaUI__study_id=letters[1:4])
  s <- cases[[1]]$s; posterior <- metaUI:::metaUI_bayesian_fit(x,s)
  expect_equal(posterior$labels, letters[1:4])
  expect_error(metaUI:::metaUI_bayesian_fit(x[1,,drop=FALSE],s),"two studies")
  expect_error(metaUI:::metaUI_bayesian_fit(x,s,2),"study limit")
  expect_error(metaUI:::metaUI_bayesian_options(list(tau_scale=-1),"SMD"),"positive")
  expect_error(metaUI:::metaUI_bayesian_options(list(max_studies=1),"SMD"),"at least 2")
  expect_error(metaUI:::metaUI_bayesian_options(list(max_studies=2.5),"SMD"),"at least 2")
  expect_null(metaUI:::metaUI_bayesian_options(list(enabled=TRUE),"SMD")$max_studies)
  expect_silent(metaUI:::metaUI_bayesian_options(list(max_studies=500),"SMD"))
  expect_match(metaUI:::metaUI_bayesian_spec(list(tau_scale=.5))$code, "^metaUI_bayesian_fit\\(df, tau_scale = 0.5\\)$")
  d <- prepared(); o <- metaUI:::metaUI_bayesian_options(list(enabled=TRUE),"SMD")
  result <- fit(d,metaUI:::metaUI_bayesian_spec(o))
  expect_equal(result$table$status,"ok")
  expect_equal(result$table$k,8)
  expect_match(metaUI:::metaUI_interval_assessment(result$table,.5)$interval,"credible")
  sensitivity <- metaUI:::metaUI_bayesian_sensitivity(result$df_agg,o,result$fits[[1]])
  expect_equal(sensitivity$tau_prior_scale,c(.25,.5,1))
  expect_true(all(sensitivity$status=="ok"))
})

# Optional comparison with bayesmeta, which earlier versions used. Not needed for accuracy (the test
# above is); it shows that results agree with the previous implementation within bayesmeta's own error.
test_that("Bayesian grid posterior agrees with bayesmeta (previous implementation)", {
  skip_if_not_installed("bayesmeta")
  cases <- list(
    list(y = c(.1, .5), sigma = c(.2, .3), s = .5),
    list(y = c(.3, .1, .9, -.4, .2), sigma = c(.01, .1, .3, 1, .05), s = .5),   # SE ratio 1:100
    list(y = c(.2, .25, .22, 1.8, .19, .3), sigma = rep(.1, 6), s = .025),     # outlier, prior far below the data
    list(y = c(.21, .19, .2, .2), sigma = c(.05, .08, .1, .06), s = 5))        # no heterogeneity, very wide prior
  for (case in cases) {
    df <- data.frame(metaUI__effect_size = case$y, metaUI__se = case$sigma, metaUI__study_id = seq_along(case$y))
    grid <- metaUI:::metaUI_bayesian_fit(df, case$s)
    ref <- bayesmeta::bayesmeta(y = case$y, sigma = case$sigma, interval.type = "central",
      tau.prior = function(t) bayesmeta::dhalfnormal(t, scale = case$s))
    p <- c(.025, .05, .5, .95, .975)
    sd_mu <- ref$summary["sd", "mu"]; sd_tau <- ref$summary["sd", "tau"]
    # bayesmeta approximates the mean's posterior by a coarser mixture (delta = .01), accurate to
    # about .002 posterior SDs; the grid is closer to adaptive integration (see the test above and
    # validation/bayesian-grid.md), so the tolerance is bayesmeta's.
    expect_lt(max(abs(grid$qposterior(mu.p = p) - ref$qposterior(mu.p = p))) / sd_mu, 5e-3)
    expect_lt(abs(grid$summary["mean", "mu"] - ref$summary["mean", "mu"]) / sd_mu, 5e-3)
    expect_lt(max(abs(grid$qposterior(tau.p = c(.025, .5, .975)) - ref$qposterior(tau.p = c(.025, .5, .975)))) / sd_tau, 1e-3)
  }
})

test_that("standalone alternative results and download provenance survive restoration timeout", {
  path <- tempfile(); on.exit(unlink(path,recursive=TRUE))
  x <- fixture();x$alt <- x$yi*.9;x$year <- rep(2001:2008,each=2)
  d <- prepared(x,filters="year",alternatives=list(Corrected=list(es_field="alt",variance="vi",es_type="SMD",justification="Illustrative correction")))
  generate_shiny(d,"Alternative state fixture",save_to_folder=path,launch_app=FALSE,models=get_model_tibble()[1:2,])
  wd <- getwd();on.exit(setwd(wd),add=TRUE);setwd(path)
  env <- new.env(parent=globalenv());sys.source("global.R",env)
  shiny::testServer(env$server,{
    session$setInputs(outliers_z_scores=c(-1.8,2.2),metaUI__filter_year=c(2001,2008),metaUI__filter_year_include_NA=TRUE,computation="alt1",go=1)
    expect_equal(df_filtered()$metaUI__effect_size,x$alt)
    expect_equal(data_list()$dataset$metaUI__effect_size,x$yi)
    expect_equal(computations_comparison()$computation,c("As supplied","As supplied","Corrected","Corrected"))
    previous <- estimatesfiltered()$fit_es
    uploaded <- d;uploaded$metaUI__effect_size[1] <- .93;uploaded$metaUI__input_effect[1] <- .93
    state_values$uploaded_data <- uploaded;state_values$upload_info <- list(file="new.xlsx",rows=16,z_rule="test standardisation",checkbox_note="Test missing-value choices")
    state_values$restore_source <- "upload"
    saved <- split(capture_filters(),capture_filters()$id);saved$metaUI__filter_year$selection <- c(2002,2007)
    state_values$restore_started <- as.numeric(Sys.time())-6
    state_values$pending_upload_filters <- saved
    session$flushReact()
    expect_null(state_values$pending_upload_filters)
    expect_true(filters_changed())
    expect_equal(data_list()$dataset$metaUI__effect_size,x$yi)
    expect_equal(data_list()$summary$fit_es,previous)
    expect_equal(data_list()$provenance$value[1],"FALSE")
    expect_match(output$link_notice,"Uploaded data not analyzed")
    session$setInputs(go=2)
    expect_equal(data_list()$dataset$metaUI__effect_size[1],.93)
    expect_equal(df_filtered()$metaUI__effect_size,x$alt)
    expect_match(output$selection_status,"Uploaded")
  })
})

test_that("schema two authors new features while schema one keeps its contract", {
  root <- tempfile();dir.create(root);on.exit(unlink(root,recursive=TRUE))
  x <- fixture();x$alt <- x$yi*.9
  csv <- file.path(root,"data.csv");write.csv(x,csv,row.names=FALSE)
  config <- list(schema_version=1,data=csv,output=file.path(root,"app"),dataset_name="Config alternatives",
    mapping=list(study_label="study",es_field="yi",variance="vi",es_type="SMD",es_id="id",alternatives=list(Corrected=list(es_field="alt",variance="vi",es_type="SMD",justification="Illustrative correction"))))
  expect_error(build_app(config),"schema_version 2")
  old <- config; old$mapping$alternatives <- NULL; old["bayesian"] <- list(NULL)
  expect_error(build_app(old),"schema_version 2")
  config$schema_version <- 2
  build_app(config)
  report <- jsonlite::read_json(file.path(config$output,"validation.json"))
  expect_equal(report$alternatives[[1]]$name,"Corrected")
  expect_equal(report$primary_label,"As supplied")
})

test_that("review guards reject missing labels, odd option lists and percent-like categories", {
  x <- fixture(); x$corrected <- x$yi * .9
  alt <- list(es_field = "corrected", variance = "vi", es_type = "SMD", justification = "Illustrative")
  expect_error(prepared(x, alternatives = list(A = alt), primary_label = NA_character_), "primary_label")
  expect_error(prepared(x, alternatives = setNames(list(alt), NA_character_)), "uniquely named")
  alt$justification <- NA_character_
  expect_error(prepared(x, alternatives = list(A = alt)), "justification")
  expect_error(metaUI:::metaUI_bayesian_options(list(TRUE), "SMD"), "uniquely named")
  expect_error(metaUI:::metaUI_bayesian_options(list(max_study = 3), "SMD"), "Unknown Bayesian option")
  old <- options(OutDec = ","); on.exit(options(old))
  expect_match(metaUI:::metaUI_bayesian_spec(list(tau_scale = .5, max_studies = 20))$code, "tau_scale = 0.5, max_studies = 20", fixed = TRUE)
  options(old)
  expect_error(build_app(list(schema_version = TRUE, data = "x", dataset_name = "x", output = "x", mapping = list())), "schema_version")
  x <- fixture(); x$group <- factor(rep(c("50%25", "b"), 8)); x$year <- 2001:2016
  d <- prepared(x, filters = c("group", "year"))
  filters <- list(list(id="metaUI__filter_group",col="metaUI__filter_group",type="categorical"),list(id="metaUI__filter_year",col="metaUI__filter_year",type="numeric"))
  spec <- metaUI:::metaUI_slider_spec(d$metaUI__filter_year)
  r <- data.frame(id=c(rep("outliers_z_scores",2),"metaUI__filter_group",rep("metaUI__filter_year",2),"metaUI__filter_year_include_NA"),
    selection=c(-1.8,2.2,"50%25",spec$min,spec$max,"TRUE"))
  parsed <- metaUI:::metaUI_parse_selection(metaUI:::metaUI_selection_query(r,"abc",1/3,d,filters),"abc",d,filters)
  expect_equal(parsed$filters$metaUI__filter_group$selection, "50%25")
  expect_identical(parsed$sesoi, 1/3)
})
