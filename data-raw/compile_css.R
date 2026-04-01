# Compile SCSS to CSS (development-time only)
#
# Run this script after editing any .scss file under inst/shiny/www/styles/.
# The output (inst/shiny/www/main.css) is committed to the repository so the
# Shiny app can use it directly without requiring sass at runtime.
#
# Usage:
#   Rscript data-raw/compile_css.R

sass::sass(
  sass::sass_file("inst/shiny/www/styles/main.scss"),
  output = "inst/shiny/www/main.css"
)

message("CSS compiled: inst/shiny/www/main.css")
