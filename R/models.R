#' Get model specifications
#'
#' This function contains the model specifications that will be compared in the Shiny app.
#' If you want to maintain the default, just call [generate_shiny()] without specifying the models argument.
#' If you want to change the default models, have a look at the vignette and/or the source code of this function
#' (by running \code{get_model_tibble} in the console) and modify it accordingly.
#'
#' @return A tibble with the models to be included in the app - including details on how to estimate them
#'   and how to extract the relevant information from the model output.
#' @export
#' @examples
#' # Run the function to get the models
#' mod <- get_model_tibble()
#' # Modify them as desired
#' mod$name[1] <- "RE 2-level model"
#' # Then use when generating the app
#' if (exists("app_data")) {
#'   generate_shiny(app_data,
#'     dataset_name = "Your meta-analysis",
#'     eff_size_type_label = "Declared effect scale")
#' }

get_model_tibble <- function() {

        models_to_run <- tibble::tribble(
        ~name, ~aggregated, ~es, ~LCL, ~UCL, ~k,
            "Random-Effects Multilevel Model", FALSE, "mod$b", "mod$ci.lb", "mod$ci.ub", "mod$k",
            "Robust Variance Estimation", FALSE, "as.numeric(mod$reg_table$b.r)", "mod$reg_table$CI.L", "mod$reg_table$CI.U", "length(mod$k)",
            "Trim-and-fill", TRUE, "mod$TE.random", "mod$lower.random", "mod$upper.random", "mod$k",
            "P-uniform star", TRUE, "mod$est", "mod$ci.lb", "mod$ci.ub", "mod$k",
            "Hedges-Vevea Selection Model", TRUE, "as.numeric(mod$output_adj$par[2])", "mod$ci.lb_adj[2]", "mod$ci.ub_adj[2]", "mod$k",
            #"P-Curve (first value)", FALSE, "mod$dEstimate", NA, NA, "mod$kAnalyzed",
            #"P-Curve (last value)", FALSE, "mod$dEstimate", NA, NA, "mod$kAnalyzed",
            "Precision Effect Test", TRUE, "as.numeric(mod$coefficients[1])", "confint(mod)[1, 1]", "confint(mod)[1, 2]", "mod$df+2",
            "Precision Effect Estimate using Standard Error", TRUE, "as.numeric(mod$coefficients[1])", "confint(mod)[1, 1]", "confint(mod)[1, 2]", "mod$df+2"
        )

        models_code <- tibble::tribble(
        ~name, ~code,
            "Random-Effects Multilevel Model", metaUI_code_multilevel,
            "Robust Variance Estimation", metaUI_code_rve,
            "Trim-and-fill", metaUI_default_code[["Trim-and-fill"]],
            "P-uniform star", metaUI_default_code[["P-uniform star"]],
            "Hedges-Vevea Selection Model", metaUI_default_code[["Hedges-Vevea Selection Model"]],
            "P-Curve (first value)", metaUI_default_code[["P-Curve (first value)"]],
            "P-Curve (last value)", metaUI_default_code[["P-Curve (last value)"]],
            "Precision Effect Test", metaUI_default_code[["Precision Effect Test"]],
            "Precision Effect Estimate using Standard Error", metaUI_default_code[["Precision Effect Estimate using Standard Error"]]
        )

        # Can set up any helper functions for use in models_code (or to extract data in models_to_run)
        # Put helpers in the author model file; generated apps source it locally.
        # The two default fit helpers are in the editable analysis.R file.

        # Keep this at the end of the file!
        models_to_run %>% dplyr::left_join(models_code, by = "name")

}
