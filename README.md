# metaUI <img src="man/figures/logo.png" align="right" style="height:200px; padding: 10px;" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/lukaswallrich/metaUI/workflows/R-CMD-check/badge.svg)](https://github.com/lukaswallrich/metaUI/actions)
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/LukasWallrich/metaUI)
<!-- badges: end -->

The new metaUI package allows users to quickly create powerful and customisable Shiny web apps that allow readers to explore a meta-analytic dataset in depth. They can explore the impact of different analytical choices, see results for subgroups of particular interest, and even modify and update the underlying dataset. 

The motivation for the package came from the fact that meta-analyses are based on rich datasets that can be analyzed in numerous ways, and it is unlikely that authors and readers will always agree on the “best ways” to analyze the data. Whether it comes to the choice of model (e.g., random versus fixed effects), the methods for assessing or adjusting for publication bias (e.g., z-curve, p-curve, PET/PEESE etc), or the moderators to be considered, disagreements are likely to arise. This can lead to the inclusion of lengthy robustness checks and alternative analyses that are time-consuming and difficult to digest. metaUI, as an R package that supports researchers in creating interactive Shiny web apps can help - these apps allow readers (and reviewers) to explore meta-analytic datasets in a variety of different ways. Apart from allowing readers (and reviewers) to assess the robustness and trustworthiness of results more comprehensively, metaUI apps allow users to assess the results that are most relevant to them, such as by filtering the dataset to focus on a specific group of participants, region, outcome variable, or research method. With the opportunity for users to download the dataset used and to upload alternatives, it will also facilitate the updating of meta-analyses. 

The idea came from researchers who have created similar web apps for their meta-analyses that have been well received, yet they require substantial time investment and advanced coding skills to create. With metaUI, researchers can get a working app very swiftly – while they still have the flexibility to tailor the display in line with their interests and requirements. The package is now in a stable beta-stage, yet feedback and contributions are very welcome! 


## Installation

You can install the development version of metaUI from [GitHub](https://github.com/) with:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("LukasWallrich/metaUI")
```

## Getting started

The best way to get started in RStudio is with the metaUI template - so in RStudio, go to `File -> New File -> R Markdown` then select `From Template` and `Create a metaUI Shiny app`. To see the template in the most readable format, select `Visual` just above the file that has opened to enter the Visual Editor. Then you can follow the step-by-step instructions.

Outside R Studio, you can run `file.edit(system.file("rmarkdown/templates/create-a-metaui-shiny-app/skeleton/skeleton.Rmd", package = "metaUI"))` to open the template file and then take it from there. 

The [`Getting started` vignette](https://lukaswallrich.github.io/metaUI/articles/getting_started.html) provides a step-by-step example.

# Sources 

Much of the package code was based on two previous interactive meta-analyses apps:

 - Röseler, L., Körner, R., & Schütz, A. (2021). Dynamic Meta-Analysis. Retrieved from https://osf.io/ns65r/, Shiny app accessible [here](https://metaanalyses.shinyapps.io/bodypositions/)
 - Röseler, L., Weber, L., Helgerth, K. A. C., Stich, E., Günther, M., Tegethoff, P., Wagner, F. S., Ambrus, E., Antunovic, M., Barrera, F., Halali, E., Ioannidis, K., McKay, R., Milstein, N., Molden, D. C., Papenmeier, F., Rinn, R., Schreiter, M. L., Zimdahl, M., Allen, E., Bahník, S., Baumeister, R. F., Bermeitinger, C., Bickenbach, S. L. C., Blank, P. A., Blower, F. B. N., Bögler, H. L., Boo, F. L., Boruchowicz, C., Bühler, R. L., Burgmer, P., Cheek, N., N., Dohle, S., Dorsch, L., Dück, M. S., Fels, S.-A., Fischer, A. L., Frech, M.-L., Freira, L., Friedinger, K., Genschow, O., Harris, A., Hartig, B., Häusser, J. A., Hedgebeth, M., Henkel, M., Horvath, D., Hügel, J. C., Igna, E. L. E., Imhoff, R., Intelmann, P., Karg, A. H., Klamar, A., Klein, C., Klusmann, B., Knappe, E., Köppel, L.-M., Koßmann, L., Kraft, P., Kroworsch, M. K., Krueger, S. M., Kühling, S., Lagator, S., Lammers, J., Loschelder, D. D., Navajas, J., Norem, J., K., Novak, J. Onuki, Y., Page, E., Panse, F., Pavlovic, Z., Pearton, J., Rebholz, T. R., Rodgers, S., Röseler, J. J., Rostekova, A., Roßmaier, K. V., Sartorio, M., Scheelje, L., Schindler, S., Schreiner, N. B., Seida, C., Shanks, D. R., Siems, M.-C., Stitz, M., Starkulla, M., Stäglich, M., Thies, K., Thum, E., Undorf, M., Unger, B. D., Urlichich, D., Vadillo, M. A., Wackershauser-Sablotny, V., Wessel, I., Wolf, H., Zhou, A., & Schütz, A. (2022). OpAQ: Open Anchoring Quest, Version 1.1.48.95. https://dx.doi.org/10.17605/OSF.IO/YGNVB, Shiny app accessible [here](https://metaanalyses.shinyapps.io/OpAQ/)

The sample data was taken from the [psymetadata](https://github.com/josue-rodriguez/psymetadata)-package, and initially extracted from the following article:

- Barroso C, Ganley CM, McGraw AL, Geer EA, Hart SA, Daucourt MC (2021). “A meta-analysis of the relation between math anxiety and math achievement.” *Psychological Bulletin*, 147(2), 134. Data and details also available on [https://osf.io/5admx/](https://osf.io/5admx/).
