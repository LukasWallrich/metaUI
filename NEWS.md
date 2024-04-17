# metaUI 0.1.2 (under development)
* Added the ability to create an app without any filters/moderators

## Bug fixes
* Corrected random intercept specification in rma.mv (and added sparse = TRUE to speed up model fitting)
* Fixed bug in describing moderators where "Other"-category already existed
* Fixed creation of code to install required packages and added check to ensure that filters are factors or numeric (#28)

# metaUI 0.1.1

* Fixed issue where k was not displayed for moderators with spaces in names
* Simplified implementation of `generate_shiny()` - it is now always saved, either to provided path or to temporary files, and launched from file. This should increase robustness. Also, removed unnecessary `app.R` so that `globals.R` does not need to be called explicitly.
* Added a `NEWS.md` file to track changes to the package.
* Added explicit option to include or exclude NAs on filters and report NA share in Sample descriptives
* Added `Reset` button to reset all filters
* Added option to add popups to filter, by specifying `filter_popups` in `generate_shiny()`
