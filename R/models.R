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
#'     dataset_name = "Barroso et al 2021 - Maths Anxiety",
#'     eff_size_type_label = "Fisher's Z scores")
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
            "Random-Effects Multilevel Model", ('metafor::rma.mv(
                                yi = metaUI__effect_size,
                                V = metaUI__variance,
                                random = ~ 1 | metaUI__study_id/metaUI__effect_size,
                                tdist = TRUE, # knapp-hartung adjustment
                                data = df,
                                method = "REML",
                                sparse = TRUE
                            )'),
            "Robust Variance Estimation", ('robumeta::robu(
                        metaUI__effect_size ~ 1, data = df,
                        studynum = metaUI__study_id, var.eff.size = metaUI__variance, small = FALSE)'),
            "Trim-and-fill", ('metafor::trimfill(meta::metagen(
                                            TE = metaUI__effect_size,
                                            seTE = metaUI__se,
                                            data = df,
                                            studlab = df$metaUI__study_id,
                                            comb.fixed = FALSE,
                                            comb.random = TRUE,
                                            method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
                                            hakn = TRUE,
                                            prediction = TRUE,
                                            sm = df$metaUI__es_type[1]
                                        ))'),
            "P-uniform star", ('puniform::puni_star(
                    yi = df$metaUI__effect_size, vi = df$metaUI__variance,
                    alpha = .05,
                    side = "right", method = "ML", boot = FALSE
                    )'),
            "Hedges-Vevea Selection Model", ('weightr::weightfunct(df$metaUI__effect_size,
                df$metaUI__variance, steps = c(0.025, 1), fe = FALSE)'),
            "P-Curve (first value)", ('p_curve(df, "first")'),
            "P-Curve (last value)", ('p_curve(df, "last")'),
            "Precision Effect Test", ('lm(metaUI__effect_size ~ sqrt(metaUI__variance), data = df, weights = 1 / metaUI__variance)'),
            "Precision Effect Estimate using Standard Error", ('lm(metaUI__effect_size ~ metaUI__variance, data = df, weights = 1 / metaUI__variance)')
        )

        # Can set up any helper functions for use in models_code (or to extract data in models_to_run)
        # However, they need to be assigned to the global environment, so users should use <<- instead of <-
        # (Inside the package, a different workaround is needed)

        p_curve <- function(data, p_selection = c("first", "last")) {
        if (p_selection == "first") {
            p_vals <- data %>%
            dplyr::group_by(.data$metaUI__study_id) %>%
            dplyr::slice_head(n = 1) %>%
            dplyr::ungroup()
        } else {
            p_vals <- data %>%
            dplyr::group_by(.data$metaUI__study_id) %>%
            dplyr::slice_tail(n = 1) %>%
            dplyr::ungroup()
        }
            p_vals <- p_vals  %>%
            dplyr::rename(
                "studlab" = .data$metaUI__study_id,
                "TE" = .data$metaUI__effect_size,
                "seTE" = .data$metaUI__se,
                "n" = .data$metaUI__N
            )  %>%
            dplyr::mutate(TE = abs(.data$TE))

            pcurve(p_vals, effect.estimation = TRUE,
                            N = p_vals$n, dmin = 0, dmax = max(abs(p_vals$TE)))
        }

        my_assign("p_curve", p_curve)

        # Keep this at the end of the file!
        models_to_run %>% dplyr::left_join(models_code, by = "name")

}
