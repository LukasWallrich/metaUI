# Synthetic data only. Build and launch are separate automation actions.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !dir.exists(args[1]))
dir.create(args[1], recursive = TRUE)
suppressPackageStartupMessages(library(metaUI))
x <- data.frame(study = rep(LETTERS[1:8], each = 2), id = rep(1:2, 8),
  yi = c(.1,.1,.5,.2,-.2,.3,.4,.4,.6,.2,-.1,.1,.2,.3,.1,.8),
  vi = seq(.01,.04,length.out = 16), year = 2001:2016,
  group = factor(rep(paste0("G", 1:8), each = 2)))
d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id",
  filters = c(Year = "year", Group = "group"))
generate_shiny(d, "Synthetic browser smoke fixture", launch_app = FALSE,
  save_to_folder = file.path(args[1], "app"), date = "2026-10-06")
u <- as.data.frame(d); attr(u, "metaUI_validation") <- NULL
u$metaUI__filter_Year[1] <- 1900; u$metaUI__filter_Year[2] <- NA_real_
u$metaUI__effect_size[1] <- mean(d$metaUI__effect_size) + 20 * sd(d$metaUI__effect_size)
u$metaUI__input_effect[1] <- u$metaUI__effect_size[1]
u$metaUI__es_z[1] <- 20
writexl::write_xlsx(list(dataset = u, filters = data.frame(
  id = c(rep("outliers_z_scores",2), rep("metaUI__filter_Year",2), rep("metaUI__filter_Group",2)),
  selection = c(-100,100,1800,2100,"G1","G2"))), file.path(args[1], "upload.xlsx"))
