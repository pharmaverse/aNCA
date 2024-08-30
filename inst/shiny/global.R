
# # PACKAGES ----
#
# # install packages which are not in Apollo CRAN
# #options(repos = c(CRAN = "https://cran.r-project.org"))
# options(repos = c(REPO_NAME = "https://packages.roche.com/CRAN/latest"))
# #options(repos = c(REPO_NAME = "https://packages.roche.com/Validated-4.3/latest"))
#
# # FOR IMPROVE R !!!
# # Install all 3 of them in this order: ImproveRcore -> ImproverRget -> ImproveR
# #  install.packages("~/aNCA_dev/R/packages/ImproveRcore.tar.gz", repos = NULL, type="source")
# #  install.packages("~/aNCA_dev/R/packages/improveRget.tar.gz", repos = NULL, type = "source")
# #  install.packages("~/aNCA_dev/R/packages/improveR.tar.gz", repos = NULL, type = "source")
#
#
# suppressPackageStartupMessages({
#   # library(improveRcore)
#   # library(improveRget)
#   # library(improveR)
#   # library(improveRmodify)
#   library(logging)
#   library(httr)
#   library(pak)
#   library(devtools)
#   library(pkgdown)
#   library(haven)
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   library(stringr)
#   library(stringi)
#   #library(EnableRF)
#   library(nestcolor)
#   #library(filters)
#   library(rtables)
#   library(ggrepel)
#   library(grid)
#   library(gridExtra)
#   library(ggpubr)
#   #library(labelled)
#   library(yaml)
#   library(formatters)
#   library(assertthat)
#   library(tern)
#   library(shiny)
#   library(tools)
#   library(tidyverse)
#   library(devtools)
#   library(shinyWidgets)
#   library(DT)
#   library(shinydashboard)
#   library(plotly)
#   library(shinythemes)
#   library(shinyalert)
#   library(shinyjs)
#   library(bslib)
#   library(fresh)
#   library(cowplot)
#   library(psych)
#   library(rlang)
#   library(htmlwidgets)
#   library(xgxr)
#   library(shinyFiles)
#   #library(rice)
#   library(zip)
#   library(checkmate)
#   library(shinyBS)
#   library(reactlog)
#  # library(improveRmodify)
#   library(shinyjqui)
#   library(PKNCA)
#   library(shinyjs)
#   library(rio)
#   # load the a app library
#   library(aNCA)
# })

# reactlog::reactlog_enable()
# # Import functions
# import = file.path(getwd(), "../../R")
# files = list.files(import, pattern = "\\.R$")
# lapply(file.path(import, files), source)

# #set plotting theme
# xgx_theme_set()

#Global variables

# defaultcols = c('USUBJID', 'STUDYID', 'COMPOUND', 'ANALYTE',
#                 'MATRIX', 'DOSNO', 'PROFTYPE', 'GROUP', 'GROUPN',
#                 'GROUPU', 'RRLTU', 'AVALU', 'LLOQ', 'ADM',
#                 'DOSEA', 'DOSEAUNIT', 'TAU', 'ADUR', 'NDUR', 'PERIOD',
#                 'SEQUENCE', 'COUNTRY', 'SITEID', 'AGE', 'SEX', 'RACE',
#                 'COMPTYPE', 'FLAGTIME', 'FATIMIMP', 'DUR', 'FLGSLOPE',
#                 'SLOPETOL', 'AUCMETHD')


# FUNCTIONS ----

## NCA Functions ----



