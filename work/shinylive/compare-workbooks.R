# Compare the summary sheets of two metaUI workbooks (server vs static export).
# Usage: Rscript work/shinylive/compare-workbooks.R SERVER.xlsx STATIC.xlsx
args <- commandArgs(TRUE)
a <- as.data.frame(readxl::read_xlsx(args[1], "summary"))
b <- as.data.frame(readxl::read_xlsx(args[2], "summary"))
stopifnot(identical(a$Model, b$Model), identical(a$status, b$status), identical(a$warnings, b$warnings))
d <- data.frame(Model = a$Model, status = a$status, es_server = sprintf("%.8f", a$es),
  es_static = sprintf("%.8f", b$es), max_abs_diff = signif(pmax(abs(a$es - b$es), abs(a$LCL - b$LCL),
    abs(a$UCL - b$UCL)), 2), fit_s_server = round(a$fit_seconds, 2), fit_s_static = round(b$fit_seconds, 2))
print(d, row.names = FALSE)
p <- grep("Source-reference", b$Model)
cat(sprintf("\nstatic primary: g = %.8f, CI [%.8f, %.8f]\n", b$es[p], b$LCL[p], b$UCL[p]))
