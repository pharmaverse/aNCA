# automated Non-Compartmental Analysis (aNCA)  
<img src='inst/shiny/www/images/aNCA_logo_bbg.png#gh-dark-mode-only' align="right" alt="aNCA logo dark bg" height="200" style="float:right; height:200px;">
<img src="inst/shiny/www/images/aNCA_logo_wbg.png#gh-light-mode-only" align="right" alt="aNCA logo light bg" height="200" style="float:right; height:200px;">


<!-- badges: start -->
[<img src="http://pharmaverse.org/shields/aNCA.svg">](https://pharmaverse.org)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![CRAN status](https://www.r-pkg.org/badges/version/aNCA)](https://CRAN.R-project.org/package=aNCA)
[![R build status](https://github.com/rstudio/shiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/shiny/actions)
[![RStudio community](https://img.shields.io/badge/community-shiny-blue?style=social&logo=rstudio&logoColor=75AADB)](https://forum.posit.co/new-topic?category=shiny&tags=shiny)
<!-- badges: end -->

<br/> 
Our aim is to develop and share an open-source R Shiny application for performing Non-Compartmental Analysis (NCA) on clinical and non-clinical datasets. By fostering community collaboration, our goal is to create a user-friendly tool that simplifies pharmacokinetic analysis and visualization for scientists worldwide
<br/> 
<br/> 

## Description

This application enables users to upload their datasets and perform Non-Compartment Analysis (NCA) on both pre-clinical and clinical datasets, with the results being easily visualizable. The NCA can be tailored to calculate pharmacokinetic parameters for various dosing regimens and time points, given certain restrictions. It also features manual slope selection, simplifying the process of conducting lambda-z-regression and PK-timepoint exclusions. Furthermore, the pharmacokinetic parameters can be dynamically visualized through customized graphics such as line and mean plots. The calculated pharmacokinetic parameters can be compiled in a dynamic table, visualized using boxplots, or exported as a comprehensive report. Designed with user-friendliness in mind, this app aims to make NCA accessible and straightforward for all scientists.



## Installation

To run you can first install the development version of the package from GitHub through your `R console`:

```r
# Check if devtools is installed, and install it if necessary
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install aNCA from GitHub
devtools::install_github("pharmaverse/aNCA")
```

Alternatively, you can also clone the repository and install locally through your `terminal`:

``` sh
# Clone the repository
git clone https://github.com/pharmaverse/aNCA.git

# Navigate to the cloned directory
cd aNCA

# Install the package
R -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools'); devtools::install()"
```



## Use

Once correctly installed, the aNCA app can be simply run with the following two lines of code:

``` r
library(aNCA)
devtools::load_all()
aNCA::run_app()
```



## Contributing (developers or user requests)

To ensure a clean and informative git version history, please adhere to the [guidelines](man/GUIDELINES.md) of our git workflow. You can find further information on possible ways to use gits full power on our homemade [cheatsheet](man/GIT-CHEATSHEET.md).


## Documentation and references

* Please go to [our Website](https://pharmaverse.github.io/aNCA/) for further information on the `aNCA app` (still in development).
* The main package used by the App is `PKNCA`, which has plenty of documentation available in its [GitHub](https://github.com/billdenney/pknca)


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

