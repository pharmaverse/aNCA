# automated Non-Compartmental Analysis (aNCA)  
<img src='inst/shiny/www/images/aNCA_logo_bbg.png#gh-dark-mode-only' align="right" alt="aNCA logo dark bg" height="200" style="float:right; height:200px;">
<img src="inst/shiny/www/images/aNCA_logo_wbg.png#gh-light-mode-only" align="right" alt="aNCA logo light bg" height="200" style="float:right; height:200px;">


<!-- badges: start -->
[<img src="https://pharmaverse.org/shields/aNCA.svg">](https://pharmaverse.org)
[![License](https://img.shields.io/badge/License-Apache_2.0-yellow.svg)](https://opensource.org/licenses/Apache-2.0)
[![CRAN status](https://www.r-pkg.org/badges/version/aNCA)](https://CRAN.R-project.org/package=aNCA)
[![R build status](https://github.com/pharmaverse/aNCA/actions/workflows/main.yml/badge.svg)](https://github.com/pharmaverse/aNCA/actions)
[![RStudio community](https://img.shields.io/badge/community-shiny-blue?style=social&logo=rstudio&logoColor=75AADB)](https://forum.posit.co/new-topic?category=shiny&tags=shiny)
<!-- badges: end -->

<br/> 

> Our aim is to develop and share an open-source R Shiny application for performing Non-Compartmental Analysis (NCA) on clinical and non-clinical datasets worldwide and across pharmaceutical companies. 

<br/> 

## Description

This application enables users to upload their datasets and perform Non-Compartment Analysis (NCA) on both pre-clinical and clinical datasets, with the results being easily visualizable. Designed with user-friendliness in mind, this app aims to make NCA accessible and straightforward for all scientists. Among the features it currently possess, the App can:

* **Customize half life calculation**: Either by rule settings definitions or performing manual in-plot adjustments
* **Define AUC intervals of interest**: Providing by default last and to-infinite calculations 
* **Visualize data and results** with interactive boxplots, summary statistic tables and scatter plots
* **Produce PP and ADPP** dataset formats of the resulting parameters
* **Save your analysis settings** and reupload them later to keep on analysing!

## Installation
To run the application, clone the repository and load it locally using the following commands:
``` sh
git clone https://github.com/pharmaverse/aNCA.git # Clone the repository
cd aNCA # Navigate to the cloned directory
```
You can then run the application from the R console:
``` r
# install devtools if not present
if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')
devtools::load_all() # load all dependencies
aNCA::run_app() # run the applicaiton
```

## Quick start
The testing data will be automatically loaded upon application startup. You can provide your own dataset in the **data** tab. Here you can also specify pre-processing filters.

In the **NCA** tab, start off by loading the pre-processed data using *Submit* button. You will also need to choose dose number in the *Settings*. Then, you will be able to run the NCA analysis. From there, you can also specify different analysis options, like applying flag rule sets and selecting slopes.

After the setup is done and analysis is performed, you are free to explore the results in the **Outputs** tab. Application supports various customizable plots, as well as report exporting.

## Contributing 
### As developer 
To ensure a clean and informative git version history, please adhere to the [guidelines](man/GUIDELINES.md) of our git workflow. You can find further information on possible ways to use gits full power on our homemade [cheatsheet](man/GIT-CHEATSHEET.md).

### As user 
Feel free to open identified issues, to reach out to us for questions or fill our [google sheet]() for feedback.

## Documentation and references

* Please go to [our Website](https://pharmaverse.github.io/aNCA/) for further information on the `aNCA app` (still in development).
* The main package used by the App is `PKNCA`. You can find more of it on its [GitHub](https://github.com/billdenney/pknca)


<!-- 

## Getting started

You may have realised this template... doesn't contain an app template. That is 
due to the different tools available, and knowledge that there is a lot of 
diversity in how people make shiny apps.

We have though applied a RocheMeta file (see `project_metadata.yaml`), which will 
be used to index your app against our database of apps and packages. Please do 
look at that file and fill in the fields.

### Shiny frameworks

The easiest way to get started is via the very simple shiny app built into Rstudio. 
To start that, click new in Rstudio, and select `Shiny Web App`.

If your app is likely to grow - it is strongly recommended to look at Shiny 
Modules. 

There are two common frameworks for structuring a more advanced app. The 
more familiar would be [`golem`]() which structures a shiny app around the ideas
that inform an R package. 
[`rhino`](https://appsilon.github.io/rhino/) introduces concepts that might be 
new to many R developers, but can be 
seen as the 'most robust, but also more intensive' way to construct an app.

### Shiny tools at Roche

The following R packages exist to help you develop your shiny apps.

* RocheLogin: This R package can help you add Roche google authentication to your app
* RocheData: Do not bundle patient data into your apps - RocheData makes it easy to query Roche databases.
* ShinyCohortBuilder: Powerful tools to build filter panels that work across relational tables
* RocheDeploy: This package is optimized to push apps to the Apollo Connect server

## Sharing your work

A `project_metadata.yaml` file has been added by default to your repo to index your 
project and find it through RocheMeta REST API (https://connect.apollo.roche.com/RocheMetaAPI/). 
You can visit [`RocheMeta documentation`](https://go.roche.com/RocheMeta) to learn how to fill correctly the file. 

Some basic tags (`R`, `Shiny`) have been added as default to your `project_metadata.yaml` and the 
lifecycle stages have been set to `experimental` and `active development`. 

Tags help to find easily your project through an API. Tag your project with more tags! If you are not sure which other tags to use () you can use `RocheMeta::suggest_tags()` function.

## License

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.

## R version support

This Shiny application is supported on the latest release version of R, as well as the previous four minor release versions of R. For example, if the latest release R version is 4.1, then that version is supported, as well as 4.0, 3.6, 3.5, and 3.4.

-->

