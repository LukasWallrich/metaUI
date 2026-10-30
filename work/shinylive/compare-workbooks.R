# Compare the summary sheets of two metaUI workbooks (server vs static export).
# Usage: Rscript work/shinylive/compare-workbooks.R SERVER.xlsx STATIC.xlsx [TOLERANCE=1e-8]
args <- commandArgs(TRUE)
if (length(args) < 2L) stop('Supply SERVER.xlsx STATIC.xlsx [TOLERANCE]')
tolerance <- as.numeric(if (length(args) >= 3L) args[3] else Sys.getenv('METAUI_WORKBOOK_TOLERANCE', '1e-8'))
stopifnot(length(tolerance) == 1L, is.finite(tolerance), tolerance >= 0)
a <- as.data.frame(readxl::read_xlsx(args[1], "summary"))
b <- as.data.frame(readxl::read_xlsx(args[2], "summary"))
stopifnot(identical(a$Model, b$Model), identical(a$status, b$status), identical(a$warnings, b$warnings))
for (column in c('es', 'LCL', 'UCL')) {
  x <- a[[column]]; y <- b[[column]]
  if (!identical(is.na(x), is.na(y))) stop('NA patterns differ in ', column)
  present <- !is.na(x)
  if (any(!is.finite(x[present])) || any(!is.finite(y[present])) ||
      any(abs(x[present] - y[present]) > tolerance))
    stop('Numeric mismatch in ', column, ' (tolerance ', tolerance, ')')
}
d <- data.frame(Model = a$Model, status = a$status, es_server = sprintf("%.8f", a$es),
  es_static = sprintf("%.8f", b$es), max_abs_diff = signif(pmax(abs(a$es - b$es), abs(a$LCL - b$LCL),
    abs(a$UCL - b$UCL), na.rm = TRUE), 2), fit_s_server = round(a$fit_seconds, 2), fit_s_static = round(b$fit_seconds, 2))
p <- grep("Source-reference", b$Model)
stopifnot(length(p) == 1L)
print(d, row.names = FALSE)
cat(sprintf("\nstatic primary: g = %.8f, CI [%.8f, %.8f]\n", b$es[p], b$LCL[p], b$UCL[p]))
