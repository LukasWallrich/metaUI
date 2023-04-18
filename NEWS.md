# metaUI 0.1.0.9003

* Added a `NEWS.md` file to track changes to the package.
* Added explicit option to include or exclude NAs on filters and report NA share in Sample descriptives
* Added `Reset` button to reset all filters
* Added option to add popups to filter, by specifying `filter_popups` in `generate_shiny()`
* Removed unnecessary `app.R` so that `globals.R` does not need to be called explicitly and does not need `<<-`
