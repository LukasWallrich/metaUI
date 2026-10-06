# Run from this copied example directory, with metaUI and dependencies installed.
# GPL-3-or-later. Building never launches or deploys.
data <- metaUI::prepare_data("mental-health.csv", study_label = "study",
  es_id = "row_uid", es_label = "study", es_field = "g", se = "se_g",
  es_type = "SMD", variance_scale = "SMD", direction = "unspecified")
metaUI::generate_shiny(data,
  "Dannheim et al. (2025): mental-health pool, source fit and explorations",
  eff_size_type_label = "Hedges g (negative = improvement)",
  models = "models.R", primary_model = "Source-reference: REML / Hartung-Knapp (rounded inputs)",
  save_to_folder = "mental-health-app", launch_app = FALSE,
  citation = paste("Dannheim I et al. (2025). Scand J Work Environ Health 51:265–281.",
    "doi:10.5271/sjweh.4219. CC-BY-4.0. Reanalysis, not endorsed by the authors.",
    "12 author-composited mental-health effects; original signed g, normal 95% CI-derived SE.",
    "The source-reference row uses REML/Hartung–Knapp; all other rows and diagnostics are explorations.",
    "Aggregation has no effect here: one effect per study. Directional models unsupported because",
    "no additional selection-model hypothesis is declared. RVE small=FALSE and small-k bias",
    "diagnostics may be anti-conservative; estimator warnings/failures are retained.",
    "Rounded inputs reproduce the published result approximately. See accompanying provenance.json."),
  date = "2026-10-06")
file.copy(c("provenance.json", "README.md"), "mental-health-app", overwrite = FALSE)
