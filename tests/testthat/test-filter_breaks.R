describe("filter_breaks", {
  
  # Create a standard ggplot object for use in most tests.
  # A fixed coordinate system ensures a predictable plot range.
  base_plot <- ggplot(data.frame(x = c(0, 10), y = c(0, 10)), aes(x, y)) +
    geom_blank() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    theme_test()
  
  ## Input validation and error handling
  it("should stop when an invalid axis is specified", {
    expect_error(
      filter_breaks(breaks = 1:5, plot = base_plot, axis = "z"),
      "Error: Invalid axis specified. Use 'x' or 'y'."
    )
  })
  
  it("should stop when panel grob is truly missing", {
    testthat::with_mocked_bindings(
      code = {
        expect_error(
          filter_breaks(breaks = 1:5, plot = base_plot),
          "Error: Panel grob not found."
        )
      },
      "ggplot_gtable" = function(data) {
        g <- ggplot2::ggplot_gtable(data)
        # Use a flexible grep to find the panel, as its name can vary
        panel_index <- which(grepl("panel", sapply(g$grobs, `[[`, "name")))
        g$grobs[[panel_index]] <- NULL
        return(g)
      }
    )
  })
  
  it("should stop when the plot does not have a valid height or width", {
    plot_no_panel <- ggplot() + theme_void()
    expect_error(
      filter_breaks(breaks = 1:5, plot = plot_no_panel),
      "Error: Panel border lacks a 'width' or 'height' property."
    )
  })
  

  ## Handling of the 'breaks' argument

  it("should correctly handle NA, unsorted, and duplicate values", {
    breaks_messy <- c(8, 2, NA, 5, 2, 1)
    breaks_clean <- c(1, 2, 5, 8)
    
    # Mock grid::convertUnit for the duration of the test code
    testthat::with_mocked_bindings(
      code = {
        expect_equal(
          filter_breaks(breaks = breaks_messy, plot = base_plot, min_cm_distance = 0.1),
          breaks_clean
        )
      },
      # Define the replacement function
      "convertUnit" = function(...) 10
    )
  })
  
  it("should return an empty vector for empty input", {
    expect_equal(filter_breaks(breaks = numeric(0), plot = base_plot), numeric(0))
  })
  
  it("should return the same vector for a single break", {
    expect_equal(filter_breaks(breaks = 5, plot = base_plot), 5)
  })
  
  ## Core filtering logic
  it("should return all breaks when min_cm_distance is 0", {
    breaks <- c(0, 1, 2, 3, 4, 5)
    expect_equal(
      filter_breaks(breaks = breaks, plot = base_plot, min_cm_distance = 0),
      breaks
    )
  })
  
  it("should return only the first break when min_cm_distance is very large", {
    breaks <- c(0, 1, 2, 3, 4, 5)
    expect_equal(
      filter_breaks(breaks = breaks, plot = base_plot, min_cm_distance = 100),
      0
    )
  })
})