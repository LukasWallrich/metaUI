test_that("generated-app modules match the package's own definitions", {
  # inst/template_code copies run in generated apps; R/ copies run in the package.
  for (file in c("analysis.R", "helpers.R", "bayesian.R", "computations.R", "selection_links.R")) {
    env <- new.env(parent = asNamespace("metaUI"))
    sys.source(system.file("template_code", file, package = "metaUI"), envir = env)
    for (name in ls(env, all.names = TRUE)) {
      template <- get(name, envir = env)
      package <- get(name, envir = asNamespace("metaUI"))
      if (is.function(template)) {
        expect_identical(deparse(body(template)), deparse(body(package)), info = paste(file, name))
        expect_identical(deparse(formals(template)), deparse(formals(package)), info = paste(file, name))
      } else expect_identical(template, package, info = paste(file, name))
    }
  }
})
