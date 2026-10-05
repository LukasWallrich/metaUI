test_that("headless app loads and runs the generated server", {
  path <- tempfile("metaui-app-")
  on.exit(unlink(path, recursive = TRUE))
  d <- prepared()
  generate_shiny(d, "Tiny synthetic fixture", save_to_folder = path, launch_app = FALSE, date = "2026-10-05")
  expect_true(all(file.exists(file.path(path, c("global.R","ui.R","server.R","models.R","analysis.R","validation.json","dependencies.csv")))))
  expect_false(any(grepl("install.packages", readLines(file.path(path,"global.R")), fixed=TRUE)))
  report <- jsonlite::read_json(file.path(path,"validation.json"))
  expect_equal(report$retained_rows, 16)
  expect_equal(report$aggregation$correlation, .6)
  writeLines("# hand edit", file.path(path,"author.R"))
  original <- readLines(file.path(path,"server.R"))
  expect_error(generate_shiny(d, "again", save_to_folder=path, launch_app=FALSE), "Destination is not empty")
  expect_identical(readLines(file.path(path,"server.R")), original)
  expect_identical(readLines(file.path(path,"author.R")), "# hand edit")
  wd <- getwd(); on.exit(setwd(wd), add=TRUE); setwd(path)
  env <- new.env(parent = globalenv())
  sys.source("global.R", env)
  expect_s3_class(env$ui, "shiny.tag.list")
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(metaUI:::signif_floor(min(d$metaUI__es_z)), metaUI:::signif_ceiling(max(d$metaUI__es_z))), go = 1)
    session$flushReact()
    table <- estimatesfiltered()
    expect_equal(table$status[1:2], c("ok","ok"))
    ref <- metafor::rma.mv(metaUI__effect_size, V=metaUI__variance, random=~1|metaUI__study_id/metaUI__effect_id,
                          data=d, method="REML", test="t", sparse=TRUE)
    expect_equal(table$fit_es[1], as.numeric(ref$b), tolerance=1e-8)
    expect_true(nzchar(output$effectestimate))
    expect_true(nzchar(output$heterogeneity))
  })
})

test_that("declarative builder is deterministic and refuses ambiguous config", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive=TRUE))
  file.copy(system.file("examples", "tiny.csv", package="metaUI"), root)
  config <- jsonlite::read_json(system.file("examples", "tiny.json", package="metaUI"))
  config$output <- "app"
  path <- file.path(root,"config.json"); jsonlite::write_json(config,path,auto_unbox=TRUE)
  build_app(path)
  expect_true(file.exists(file.path(root,"app","validation.json")))
  expect_error(build_app(path), "Destination is not empty")
  config$unknown <- TRUE
  expect_error(build_app(config), "Unknown configuration")
  config$unknown <- NULL; config$schema_version <- 99
  expect_error(build_app(config), "schema_version")
})

test_that("categorical CSV filters are explicitly mapped and deterministic", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root,recursive=TRUE))
  x <- fixture(); x$region <- rep(c("West","East"),8); x$year <- 2001:2016
  write.csv(x,file.path(root,"data.csv"),row.names=FALSE)
  config <- list(schema_version=1,data="data.csv",output="app",dataset_name="Categorical fixture",
                 mapping=list(study_label="study",es_field="yi",variance="vi",es_type="SMD",es_id="id",
                              filters=list(Region="region",Year="year"),categorical_filters=list("region")),about=list(date="2026-10-05"))
  path <- file.path(root,"config.json");jsonlite::write_json(config,path,auto_unbox=TRUE)
  build_app(path)
  d <- readRDS(file.path(root,"app","dataset.rds"))
  expect_identical(levels(d$metaUI__filter_Region),c("East","West"))
  expect_true(any(grepl("Explicit categorical",attr(d,"metaUI_validation")$derivations)))
  wd <- getwd(); on.exit(setwd(wd),add=TRUE);setwd(file.path(root,"app"))
  env<-new.env(parent=globalenv());sys.source("global.R",env)
  shiny::testServer(env$server, {
    session$setInputs(metaUI__filter_Region=c("East","West"),metaUI__filter_Year=c(2001,2016),outliers_z_scores=c(-10,10),go=1)
    session$flushReact()
    expect_equal(estimatesfiltered()$input_rows[1],16)
    expect_true(nzchar(output$summary_metaUI__filter_Region_table))
    expect_true(nzchar(output$summary_metaUI__filter_Year_table))
    expect_true(is.list(output$summary_metaUI__filter_Region_plot))
  })
})

test_that("stale reports and explicitly overwritten model files are handled", {
  d<-prepared();expect_error(generate_shiny(d[1:4,],"subset",launch_app=FALSE),"stale")
  path<-tempfile();on.exit(unlink(path,recursive=TRUE))
  generate_shiny(d,"fixture",save_to_folder=path,launch_app=FALSE)
  modelpath<-tempfile(fileext=".R");on.exit(unlink(modelpath),add=TRUE)
  code<-metaUI:::generate_models.R(get_model_tibble()[1:2,])
  writeLines(c("# explicit replacement",code),modelpath)
  generate_shiny(d,"fixture",models=modelpath,save_to_folder=path,launch_app=FALSE,overwrite=TRUE)
  expect_identical(readLines(file.path(path,"models.R"))[1],"# explicit replacement")
})

test_that("literal quotes and braces in metadata generate parseable editable files", {
  path<-tempfile();on.exit(unlink(path,recursive=TRUE))
  generate_shiny(prepared(), 'Study "A" {literal}', citation='Example "quoted" citation',
                 eff_size_type_label='A \'label\'',save_to_folder=path,launch_app=FALSE)
  for (file in c("global.R","ui.R","server.R","labels_and_options.R"))
    expect_silent(parse(file.path(path,file)))
})
