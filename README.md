# automated Non-Compartmental Analysis (aNCA) <img src="man/figures/aNCA_logo_wbg.png" align="right" alt="" width="200" />

<!-- badges: start -->

[![Pharmaverse](https://pharmaverse.org/shields/aNCA.svg)](https://pharmaverse.org)
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

- **Customize half life calculation**: Either by rule settings definitions or performing manual in-plot adjustments
- **Define AUC intervals of interest**: Providing by default last and to-infinite calculations
- **Visualize data and results** with interactive boxplots, summary statistic tables and scatter plots
- **Produce PP and ADPP** dataset formats of the resulting parameters
- **Save your analysis settings** and reupload them later to keep on analysing!

## Installation

### Via pak (recommended)

We recommend using [pak](https://github.com/r-lib/pak) for package installation, along with all system dependencies. If you do not have `pak` available, you will need to set it up first:

```R
install.packages("pak")
```

then you can install [aNCA](.) by running:

```R
pak::pak("pharmaverse/aNCA")
```

in your R console.

### Via cloning the repository (for contributors)

Alternatively, you can set up the package by cloning the repository through your terminal/shell:

```bash
git clone https://github.com/pharmaverse/aNCA.git
```

and then loading it directly using [devtools](https://github.com/r-lib/devtools) in your IDE (e.g. RStudio) console:

```R
devtools::load_all()
```

## Quick start

To run the application, simply invoke:

```R
aNCA::run_app()
```

The testing data will be automatically loaded upon application startup. You can provide your own dataset in the **data** tab. Here you can also specify pre-processing filters.

In the **NCA** tab, start off by loading the pre-processed data using _Submit_ button. You will also need to choose dose number in the _Settings_. Then, you will be able to run the NCA analysis. From there, you can also specify different analysis options, like applying flag rule sets and selecting slopes.

After the setup is done and analysis is performed, you are free to explore the results in the **Outputs** tab. Application supports various customizable plots, as well as report exporting.

For more detailed instructions, check out the [Get Starged](https://pharmaverse.github.io/aNCA/articles/aNCA.html) page.

## Contributing

### As developer

To ensure a clean codebase and smooth cooperation, please adhere to the [contributing guidelines](CONTRIBUTING.md).

### As user

Feel free to open identified [issues](https://github.com/pharmaverse/aNCA/issues/new/choose), to reach out to us for questions or report in our [google sheet](https://forms.gle/c9ULTTv1s75yRaLj7) for feedback.

## Documentation and references

- Please go to [our Website](https://pharmaverse.github.io/aNCA/) for further information on the **aNCA app** (still in development).
- The main package used by the App is `PKNCA`. You can find more of it on its [GitHub](https://github.com/billdenney/pknca)
