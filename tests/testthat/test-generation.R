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
  expect_true(file.exists(file.path(path, "www", "metaui.css")))
  shiny::testServer(env$server, {
    z_range <- c(metaUI:::signif_floor(min(d$metaUI__es_z)), metaUI:::signif_ceiling(max(d$metaUI__es_z)))
    session$setInputs(outliers_z_scores = z_range)
    expect_match(output$apply_state, "Not analysed yet")
    expect_error(output$selection_status, "No analysis yet")
    session$setInputs(go = 1)
    session$flushReact()
    expect_false(filters_changed())
    expect_match(output$apply_state, "Results match")
    table <- estimatesfiltered()
    expect_equal(table$status[1:2], c("ok","ok"))
    ref <- metafor::rma.mv(metaUI__effect_size, V=metaUI__variance, random=~1|metaUI__study_id/metaUI__effect_id,
                          data=d, method="REML", test="t", sparse=TRUE)
    expect_equal(table$fit_es[1], as.numeric(ref$b), tolerance=1e-8)
    expect_true(nzchar(output$effectestimate))
    expect_true(nzchar(output$heterogeneity))
    expect_true(is.list(output$foreststudies))
    expect_false(estimatesreactive()$cache_hit)
    # Unapplied edits are flagged; displayed results keep the applied selection.
    session$setInputs(outliers_z_scores = c(0, z_range[2]))
    expect_true(filters_changed())
    expect_match(output$apply_state, "Filters changed")
    expect_equal(nrow(df_filtered()), 16)
    session$setInputs(outliers_z_scores = z_range)
    expect_false(filters_changed())
    session$setInputs(go = 2)
    expect_true(estimatesreactive()$cache_hit)
    expect_match(output$calculation_status, "Reused")
    expect_identical(estimatesfiltered()$fit_es, table$fit_es)
    expect_match(output$effectestimate, "95% interval")
  })
  # A separate reader starts with a separate cache.
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    expect_false(estimatesreactive()$cache_hit)
  })
})

test_that("uploaded data persist and invalidate session results", {
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  d <- prepared()
  generate_shiny(d, "Upload fixture", save_to_folder = path, launch_app = FALSE)
  uploaded <- as.data.frame(d); attr(uploaded, "metaUI_validation") <- NULL
  uploaded$metaUI__effect_size[1] <- .93
  uploaded$metaUI__input_effect[1] <- .93
  upload_file <- tempfile(fileext = ".xlsx"); on.exit(unlink(upload_file), add = TRUE)
  writexl::write_xlsx(list(dataset = uploaded,
    filters = data.frame(id = "outliers_z_scores", selection = c(-10, 10))), upload_file)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    original <- estimatesfiltered()$fit_es
    session$setInputs(go = 2); expect_true(estimatesreactive()$cache_hit)
    session$setInputs(uploadData = list(datapath = upload_file, name = "upload.xlsx"), executeUpload = 1)
    # Until the restored selection is analysed, the strip still describes the built results.
    expect_null(state_values$pending_upload_filters)
    expect_match(output$selection_status, "Built dataset")
    session$setInputs(go = 3)
    expect_match(output$selection_status, "Uploaded data")
    expect_false(estimatesreactive()$cache_hit)
    expect_equal(df_filtered()$metaUI__effect_size[1], .93)
    expect_false(identical(estimatesfiltered()$fit_es, original))
    session$setInputs(go = 4)
    expect_true(estimatesreactive()$cache_hit)
    expect_equal(df_filtered()$metaUI__effect_size[1], .93)
  })
})

test_that("uploads retain extreme/missing numeric inputs and restored picker selections", {
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE))
  x <- fixture(); x$year <- 2001:2016; x$group <- factor(rep(paste0("G", 1:8), each = 2))
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id",
    filters = c(Year = "year", Group = "group"))
  generate_shiny(d, "Upload extremes", save_to_folder = root, launch_app = FALSE)
  u <- as.data.frame(d); attr(u, "metaUI_validation") <- NULL
  u$metaUI__filter_Year[1] <- 1900; u$metaUI__filter_Year[2] <- NA_real_
  u$metaUI__es_z[1] <- 20
  u$metaUI__effect_size[1] <- mean(d$metaUI__effect_size) + 20 * sd(d$metaUI__effect_size)
  u$metaUI__input_effect[1] <- u$metaUI__effect_size[1]
  file <- tempfile(fileext = ".xlsx"); on.exit(unlink(file), add = TRUE)
  selections <- data.frame(id = c(rep("outliers_z_scores", 2), rep("metaUI__filter_Year", 2), rep("metaUI__filter_Group", 2)),
    selection = c(-100, 100, 1800, 2100, "G1", "G2"))
  writexl::write_xlsx(list(dataset = u, filters = selections), file)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  ui_code <- paste(readLines("ui.R"), collapse = "\n")
  expect_match(ui_code, "metaUI__filter_Year_include_NA")
  # Integer filters get exact integer bounds and steps rather than rounded fractions.
  expect_match(ui_code, "min = 2001,\\s+max = 2016,\\s+value = c\\(2001,\\s+2016\\),\\s+sep = \"\", step = 1")
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), metaUI__filter_Year = c(2001, 2016),
      metaUI__filter_Group = paste0("G", 1:8), metaUI__filter_Year_include_NA = TRUE)
    session$setInputs(uploadData = list(datapath = file, name = "extremes.xlsx"), executeUpload = 1)
    expect_false(is.null(state_values$pending_upload_filters))
    expect_match(output$selection_status, "Restoring saved filters")
    expect_error(data_list(), class = "shiny.silent.error")
    # Mock client applies the messages; real browser round trip verifies this too.
    session$setInputs(outliers_z_scores = c(-100, 100), metaUI__filter_Year = c(1800, 2100),
      metaUI__filter_Group = c("G1", "G2"), go = 2)
    expect_null(state_values$pending_upload_filters)
    expect_equal(nrow(df_filtered()), 4)
    expect_equal(df_filtered()$metaUI__es_z[1], 20)
    expect_true(is.na(df_filtered()$metaUI__filter_Year[2]))
    expect_equal(estimatesfiltered()$filtered_rows[1], 12)
    expect_match(output$selection_status, "Uploaded data")
    session$setInputs(metaUI__filter_Year_include_NA = FALSE, go = 3)
    expect_equal(nrow(df_filtered()), 3)
    saved <- data_list()$filters
    expect_identical(saved$selection[saved$id == "metaUI__filter_Year_include_NA"], "FALSE")
  })
})

test_that("restored filters disclose uploaded rows outside the saved selection", {
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE))
  x <- fixture(); x$year <- 2001:2016
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id", filters = c(Year = "year"))
  generate_shiny(d, "Saved-range disclosure", save_to_folder = root, launch_app = FALSE)
  u <- as.data.frame(d); attr(u, "metaUI_validation") <- NULL
  u$metaUI__filter_Year[1] <- 2020
  file <- tempfile(fileext = ".xlsx"); on.exit(unlink(file), add = TRUE)
  writexl::write_xlsx(list(dataset = u, filters = data.frame(
    id = c(rep("outliers_z_scores", 2), rep("metaUI__filter_Year", 2)), selection = c(-10, 10, 2001, 2016))), file)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), metaUI__filter_Year = c(2001, 2016), metaUI__filter_Year_include_NA = TRUE, go = 1)
    session$setInputs(uploadData = list(datapath = file, name = "new-year.xlsx"), executeUpload = 1, go = 2)
    expect_equal(nrow(df_filtered()), 15)
    expect_match(output$selection_status, "Excluded 1")
    expect_match(output$selection_status, "Year: 1")
    expect_false(grepl("metaUI__", output$selection_status))
    session$setInputs(metaUI__filter_Year = c(2000, 2030))
    expect_true(filters_changed())
    saved <- data_list()$filters
    expect_equal(as.numeric(saved$selection[saved$id == "metaUI__filter_Year"]), c(2001, 2016))
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


test_that("downloaded empty category selections re-upload without losing their marker", {
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE))
  x <- fixture(); x$group <- factor(rep(c("A", "B"), each = 8))
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id", filters = c(Group = "group"))
  generate_shiny(d, "Empty filter round trip", save_to_folder = root, launch_app = FALSE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), metaUI__filter_Group = character(), go = 1)
    file <- tempfile(fileext = ".xlsx"); on.exit(unlink(file), add = TRUE)
    writexl::write_xlsx(data_list(), file)
    session$setInputs(uploadData = list(datapath = file, name = "empty.xlsx"), executeUpload = 1)
    expect_false(is.null(state_values$uploaded_data))
    session$setInputs(metaUI__filter_Group = character(), go = 2)
    expect_equal(nrow(df_filtered()), 0)
    expect_match(output$selection_status, "Selected 0 of 16")
    expect_match(output$selection_status, "Excluded 16")
  })
})
