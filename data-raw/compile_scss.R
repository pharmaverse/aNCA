# Compile SCSS to CSS for the Shiny app.
# Run this manually after modifying any .scss files under inst/shiny/www/styles/.
#

sass::sass(
  sass::sass_file("inst/shiny/www/styles/main.scss"),
  output = "inst/shiny/www/main.css"
)

