# GOAL:

Increase tests so that file R/PKNCA.R is 100% covered.


# General rules
Be terse.
Do no more than asked.
User may ask a followup question.
Always tell user what you will do.  NEVER proceed without user's permission.  Even in "plan" mode.

If user asks you to explain the code,  either a single line, several lines, an entire function, then do the following
- print line #
- print actual code line #
- WAIT
- if the user replies "n" or "next" go to next line
- if the user asks a question or questions, answer.  Do not proceed till user provides "n" or "next"


# To obtain a new coverage report

Proceed in order.  This step is done when you have "coverage report"

##  Method 0
Ask user to provide this report.

##  Method 1 
load_all()
devtools::test_coverage_active_file()


## Method 2
library(covr)
load_all()
covr::file_report(file = "R/PKNCA.R")
# NO covr::report()   # generates coverage report for entire package


# STOP




