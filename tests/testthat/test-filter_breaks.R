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
    # Mock convertUnit to ensure a stable environment for a simple comparison
    mockery::stub(filter_breaks, 'grid::convertUnit', 10)
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
    # Mock `grid::convertUnit` to return a fixed panel width of 10 cm.
    # Since the plot's x-axis range is 10 units (0 to 10), this creates a
    # simple 1-to-1 mapping: 1 data unit = 1 cm.
    mockery::stub(filter_breaks, 'grid::convertUnit', 10)
    
    breaks_in <- c(0, 1, 2.5, 2.6, 5, 8, 8.2)
    min_dist  <- 1.5 # Filter out breaks that are less than 1.5 units apart.
    
    # Expected logic:
    # 1. Keep 0.
    # 2. dist(1, 0) = 1.0. -> Drop.
    # 3. dist(2.5, 0) = 2.5. -> Keep 2.5. (Filtered: [0, 2.5])
    # 4. dist(2.6, 2.5) = 0.1. -> Drop.
    # 5. dist(5, 2.5) = 2.5. -> Keep 5. (Filtered: [0, 2.5, 5])
    # 6. dist(8, 5) = 3.0. -> Keep 8. (Filtered: [0, 2.5, 5, 8])
    # 7. dist(8.2, 8) = 0.2. -> Drop.
    breaks_out <- c(0, 2.5, 5, 8)
    
    expect_equal(
      filter_breaks(breaks = breaks_in, plot = base_plot, min_cm_distance = min_dist, axis = "x"),
      breaks_out
    )
  })
  
  it("should correctly filter y-axis breaks based on distance", {
    # Mock panel height to be 20 cm.
    # The plot's y-range is 10 units (0 to 10), so 1 data unit = 2 cm.
    mockery::stub(filter_breaks, 'grid::convertUnit', 20)
    
    breaks_in <- c(0, 1, 3, 3.2, 7, 9)
    min_dist_cm  <- 5 # This requires a data distance of 2.5 (5 cm / 2 cm per unit).
    
    # Expected logic (min data distance = 2.5):
    # 1. Keep 0.
    # 2. dist(1, 0) = 1. -> Drop.
    # 3. dist(3, 0) = 3. -> Keep 3. (Filtered: [0, 3])
    # 4. dist(3.2, 3) = 0.2. -> Drop.
    # 5. dist(7, 3) = 4. -> Keep 7. (Filtered: [0, 3, 7])
    # 6. dist(9, 7) = 2. -> Drop.
    breaks_out <- c(0, 3, 7)
    
    expect_equal(
      filter_breaks(breaks = breaks_in, plot = base_plot, min_cm_distance = min_dist_cm, axis = "y"),
      breaks_out
    )
  })
})