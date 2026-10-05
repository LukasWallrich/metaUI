# metaUI 0.1.2.9000 (correctness candidate)

* Use genuine per-effect IDs for all multilevel nesting; display labels may repeat.
* Keep optional p/N rows under default preparation, validate required inputs, and
  report excluded rows and derivations. Summed N is no longer inferred.
* Require explicit correlation variance scales and directional bias assumptions.
  Fit COR via Fisher z with delta-method variance; preserve ZCOR without a second
  transform; back-transform model summaries once. Other panels retain fitting scale.
* Report model applicability, failures, warnings, aggregation, and fitting values.
  Correct REML heterogeneity output and remove meaningless multilevel tau2.
* Add JSON `build_app()` and versioned schema. Nonempty output is refused by
  default; preserve hand edits by building a fresh destination.
* Generated apps check dependencies without installing packages. Record build
  versions and bundled attribution; replace archived waffle dependency with a
  categorical count bar chart and use an original SVG favicon.
* Correct the tutorial's Wald p calculation and Fisher-z labels.
* Add independent statistical references, headless server tests, and read-only CI.
* Correct slider rounding so default bounds retain all eligible rows.
* Existing launch_app=FALSE remains supported. No estimator rewrite, benchmark
  apps, agent skill, public deployment, or paper draft is included.

# metaUI 0.1.2 (under development)

## Minor enhancements
* generate_shiny() has gained an `options` argument to allow for more customization of the app. Currently it allows setting a Shiny theme, a limit for the number of rows in a forest plot, and a threshold for switching from checkboxes to selection lists based on the number of moderator levels, with sensible defaults.
* prepare_data gained an argument `arrange_filters` to allow for the ordering of filters in the app. It now defaults to ordering them in the order they are passed into the function, but this can be changed to alphabetical or no ordering (in which case the order of columns determines their position)
* Added the ability to create an app without any filters/moderators
* Now shows "(Missing)" level only where there are any missing values in that filter/moderator - unless

## Bug fixes
* Corrected random intercept specification in rma.mv (and added sparse = TRUE to speed up model fitting)
* Fixed bug in describing moderators where "Other"-category already existed
* Fixed creation of code to install required packages and added check to ensure that filters are factors or numeric (#28)
* Waffle plots in sample description no longer run out of colors in the presence of 11 categories.

# metaUI 0.1.1

* Fixed issue where k was not displayed for moderators with spaces in names
* Simplified implementation of `generate_shiny()` - it is now always saved, either to provided path or to temporary files, and launched from file. This should increase robustness. Also, removed unnecessary `app.R` so that `globals.R` does not need to be called explicitly.
* Added a `NEWS.md` file to track changes to the package.
* Added explicit option to include or exclude NAs on filters and report NA share in Sample descriptives
* Added `Reset` button to reset all filters
* Added option to add popups to filter, by specifying `filter_popups` in `generate_shiny()`
