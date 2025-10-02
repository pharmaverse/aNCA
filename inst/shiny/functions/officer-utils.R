# Using officer create a power point presentation with 2 tables and 1 plot

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

# Create a new PowerPoint presentation
ppt <- read_pptx("inst/shiny/template.pptx")
# Add a title slide

ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
ppt <- ph_with(ppt, value = "My Presentation", location = ph_location_type(type = "ctrTitle"))

ppt <- add_slide(ppt, layout = "Content with Caption")
ppt <- ph_with(ppt, value = flextable::flextable(res_dose_slides$Group_1$info), location = "Table Placeholder 1")
ppt <- ph_with(ppt, value = res_dose_slides$Group_1$statistics, location = "Table Placeholder 2")
ppt <- ph_with(ppt, value = res_dose_slides$Group_1$meanplot, location = "Content Placeholder 1")
print(ppt, target = "test_pres_officer.pptx")


install.packages("officer")
