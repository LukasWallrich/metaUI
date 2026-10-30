# Synthetic effects attached to real DOIs, to demonstrate study details. Not a real meta-analysis.
suppressPackageStartupMessages(library(metaUI))
raw <- data.frame(
  study = c("Willpower mindset 2023", "Negative mentoring 2021", "After-work eating 2025",
            "Reproducibility project 2015", "False-positive psychology 2011", "Ioannidis 2005",
            "Power primer 1992", "Leader stress 2025", "Unregistered DOI", "No DOI given"),
  doi = c("10.1371/journal.pone.0287911", "https://doi.org/10.1002/hrm.22076", "10.1111/joop.70140",
          "10.1126/science.aac4716", "10.1177/0956797611417632", "10.1371/journal.pmed.0020124",
          "10.1037/0033-2909.112.1.155", "10.5271/sjweh.4219", "10.99999/metaui.not-registered", NA),
  d = c(.12, .35, .05, .41, .22, -.08, .30, .18, .27, .10),
  vi = c(.020, .031, .015, .044, .026, .019, .037, .022, .028, .033),
  design = factor(c("Lab", "Field", "Field", "Lab", "Lab", "Field", "Lab", "Field", "Lab", "Field")))
data <- prepare_data(raw, "study", "d", variance = "vi", url = "doi", filters = c(Design = "design"))
info <- fetch_study_metadata(data, cache = "study-metadata.csv")
print(info[, c("study", "status", "year", "abstract_licence")])
generate_shiny(data, "Study details demo (synthetic effects, real DOIs)", study_info = info,
  save_to_folder = "app", overwrite = TRUE, launch_app = FALSE, date = "2026-10-30")
