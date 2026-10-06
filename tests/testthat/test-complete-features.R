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
  bad <- d; bad$metaUI__alt_1_fit_variance[1] <- 1
  expect_error(metaUI:::metaUI_validate_upload(bad,d),"Alternative source/fitting")
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
  q <- metaUI:::metaUI_selection_query(r,"abc",.3)
  parsed <- metaUI:::metaUI_parse_selection(q,"abc",d,filters)
  expect_equal(parsed$filters$metaUI__filter_group$selection,"Über & ?")
  expect_equal(parsed$sesoi,.3)
  expect_null(metaUI:::metaUI_parse_selection(metaUI:::metaUI_selection_query(r,"abc"),"abc",d,filters)$sesoi)
  expect_error(metaUI:::metaUI_parse_selection(q,"xyz",d,filters),"different dataset")
  expect_error(metaUI:::metaUI_parse_selection(paste0(q,"&unknown=1"),"abc",d,filters),"parameters")
  r$selection[r$id=="metaUI__filter_group"] <- "not built"
  expect_error(metaUI:::metaUI_parse_selection(metaUI:::metaUI_selection_query(r,"abc"),"abc",d,filters),"Unknown")
})

test_that("Bayesian posterior agrees with an independent normal-mixture quadrature", {
  skip_if_not_installed("bayesmeta")
  y <- c(-.2,.1,.4,.7); sigma <- c(.1,.15,.2,.25); s <- .5
  x <- data.frame(metaUI__effect_size=y,metaUI__se=sigma,metaUI__study_id=letters[1:4])
  posterior <- metaUI:::metaUI_bayesian_fit(x,s)
  tau <- seq(0,4,length.out=20001)
  w <- 1/outer(tau^2,sigma^2,"+"); sw <- rowSums(w)
  mu <- as.vector(w %*% y)/sw
  logp <- -.5*(tau/s)^2 + .5*rowSums(log(w)) - .5*log(sw) - .5*rowSums(w*(matrix(y,length(tau),4,byrow=TRUE)-mu)^2)
  p <- exp(logp-max(logp)); p[c(1,length(p))] <- p[c(1,length(p))]/2; p <- p/sum(p)
  cdf <- function(z) sum(p*pnorm((z-mu)*sqrt(sw)))
  qs <- vapply(c(.025,.5,.975),function(prob) uniroot(function(z)cdf(z)-prob,c(-3,3),tol=1e-8)$root,numeric(1))
  expect_lt(max(abs(as.numeric(posterior$qposterior(mu.p=c(.025,.5,.975)))-qs)),1e-3)
  expect_error(metaUI:::metaUI_bayesian_fit(x[1,,drop=FALSE],s),"two studies")
  expect_error(metaUI:::metaUI_bayesian_fit(x,s,2),"study limit")
  expect_error(metaUI:::metaUI_bayesian_options(list(tau_scale=-1),"SMD"),"positive")
  expect_error(metaUI:::metaUI_bayesian_options(list(max_studies=51),"SMD"),"2 to 50")
  expect_warning(metaUI:::metaUI_bayesian_options(list(max_studies=30),"SMD"),"block")
  d <- prepared(); o <- metaUI:::metaUI_bayesian_options(list(enabled=TRUE),"SMD")
  result <- fit(d,metaUI:::metaUI_bayesian_spec(o))
  expect_equal(result$table$status,"ok")
  expect_equal(result$table$k,8)
  expect_match(metaUI:::metaUI_interval_assessment(result$table,.5)$interval,"credible")
  sensitivity <- metaUI:::metaUI_bayesian_sensitivity(result$df_agg,o,result$fits[[1]])
  expect_equal(sensitivity$tau_prior_scale,c(.25,.5,1))
  expect_true(all(sensitivity$status=="ok"))
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
    expect_match(output$link_notice,"not yet analysed")
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
  expect_match(metaUI:::metaUI_bayesian_spec(list(tau_scale = .5, max_studies = 20))$code, "tau_scale = 0.5", fixed = TRUE)
  options(old)
  expect_error(build_app(list(schema_version = TRUE, data = "x", dataset_name = "x", output = "x", mapping = list())), "schema_version")
  x <- fixture(); x$group <- factor(rep(c("50%25", "b"), 8)); x$year <- 2001:2016
  d <- prepared(x, filters = c("group", "year"))
  filters <- list(list(id="metaUI__filter_group",col="metaUI__filter_group",type="categorical"),list(id="metaUI__filter_year",col="metaUI__filter_year",type="numeric"))
  spec <- metaUI:::metaUI_slider_spec(d$metaUI__filter_year)
  r <- data.frame(id=c(rep("outliers_z_scores",2),"metaUI__filter_group",rep("metaUI__filter_year",2),"metaUI__filter_year_include_NA"),
    selection=c(-1.8,2.2,"50%25",spec$min,spec$max,"TRUE"))
  parsed <- metaUI:::metaUI_parse_selection(metaUI:::metaUI_selection_query(r,"abc",1/3),"abc",d,filters)
  expect_equal(parsed$filters$metaUI__filter_group$selection, "50%25")
  expect_identical(parsed$sesoi, 1/3)
})
