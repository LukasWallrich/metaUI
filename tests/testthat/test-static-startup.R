test_that("static startup has bounded failure and successful recovery paths", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is needed for browser-free JavaScript checks")
  result <- system2(node, c(shQuote(test_path("fixtures", "static-startup.cjs")),
    shQuote(system.file("template_code", package = "metaUI"))), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})


test_that("static downloads preserve plain names and tolerate malformed encodings", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is needed for browser-free JavaScript checks")
  result <- system2(node, c(shQuote(test_path("fixtures", "static-download.cjs")),
    shQuote(system.file("template_code", package = "metaUI"))), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})
