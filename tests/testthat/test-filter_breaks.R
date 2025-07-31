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
        g
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

    # For the next call to filter_breaks, replace 'grid::convertUnit' with the value 10
    mockery::stub(filter_breaks, "grid::convertUnit", 10)

    expect_equal(
      filter_breaks(breaks = breaks_messy, plot = base_plot, min_cm_distance = 0.1),
      breaks_clean
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

  it("should correctly filter x-axis breaks based on distance", {
    breaks_in <- c(0, 1, 2.5, 2.6, 5, 8, 8.2)
    min_dist  <- 1.5
    breaks_out <- c(0, 2.5, 5, 8)

    mockery::stub(filter_breaks, "grid::convertUnit", 10)

    expect_equal(
      filter_breaks(breaks = breaks_in, plot = base_plot,
                    min_cm_distance = min_dist, axis = "x"),
      breaks_out
    )
  })

  it("should correctly filter y-axis breaks based on distance", {
    breaks_in <- c(0, 1, 3, 3.2, 7, 9)
    min_dist_cm  <- 5
    breaks_out <- c(0, 3, 7)

    # For the next call to filter_breaks, replace 'grid::convertUnit' with the value 20
    mockery::stub(filter_breaks, "grid::convertUnit", 20)

    expect_equal(
      filter_breaks(breaks = breaks_in, plot = base_plot,
                    min_cm_distance = min_dist_cm, axis = "y"),
      breaks_out
    )
  })
})
