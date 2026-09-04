# Create a standard ggplot object for use in most tests.
# A fixed coordinate system ensures a predictable plot range.
base_plot <- ggplot(data.frame(x = c(0, 10), y = c(0, 10)), aes(x, y)) +
  geom_blank() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_test()

describe("filter_breaks", {

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

    # For the next call to filter_breaks, pretend the panel is 10 cm wide
    mockery::stub(filter_breaks, ".panel_size_cm", 10)

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

    mockery::stub(filter_breaks, ".panel_size_cm", 10)

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

    # For the next call to filter_breaks, pretend the panel is 20 cm tall
    mockery::stub(filter_breaks, ".panel_size_cm", 20)

    expect_equal(
      filter_breaks(breaks = breaks_in, plot = base_plot,
                    min_cm_distance = min_dist_cm, axis = "y"),
      breaks_out
    )
  })

  ## Label width
  it("should thin further when the labels need more room than the ticks do", {
    breaks <- 0:10

    # A 10 cm panel over the expanded 0-10 range puts the breaks 0.91 cm apart, so labels
    # needing 3 cm leave room for only every fourth one
    mockery::stub(filter_breaks, ".panel_size_cm", 10)
    mockery::stub(filter_breaks, ".label_extents_cm", rep(3, length(breaks)))

    expect_equal(
      filter_breaks(breaks = breaks, plot = base_plot, min_cm_distance = 0.1),
      c(0, 4, 8)
    )
  })

  it("should keep min_cm_distance as a floor when the labels are narrow", {
    breaks <- c(0, 0.2, 1, 2)

    mockery::stub(filter_breaks, ".panel_size_cm", 10)
    mockery::stub(filter_breaks, ".label_extents_cm", rep(0.1, length(breaks)))

    # The labels only need 0.1 cm, so the 0.5 cm floor is what drops 0.2
    expect_equal(
      filter_breaks(breaks = breaks, plot = base_plot, min_cm_distance = 0.5),
      c(0, 1, 2)
    )
  })

  it("should size the gap from the two labels either side of it", {
    breaks <- c(0, 1, 2, 3)

    mockery::stub(filter_breaks, ".panel_size_cm", 10)
    mockery::stub(filter_breaks, ".label_extents_cm", c(0.2, 0.2, 3, 0.2))

    # Only the wide label at 2 is dropped; 3 still fits next to the narrow label at 1
    expect_equal(
      filter_breaks(breaks = breaks, plot = base_plot, min_cm_distance = 0.1),
      c(0, 1, 3)
    )
  })

  it("should measure the gap against the last break it kept, not the first", {
    breaks <- 0:4

    mockery::stub(filter_breaks, ".panel_size_cm", 10)
    mockery::stub(filter_breaks, ".label_extents_cm", c(3, 0.2, 0.2, 0.2, 0.2))

    # Once the wide label at 0 is behind us the narrow ones can sit next to each other, so
    # 3 and 4 are kept. Measuring everything against the label at 0 would drop 3.
    expect_equal(
      filter_breaks(breaks = breaks, plot = base_plot, min_cm_distance = 0.1),
      c(0, 2, 3, 4)
    )
  })

  it("should reject a 'labels' argument that cannot produce one label per break", {
    expect_error(
      filter_breaks(breaks = 0:5, plot = base_plot, labels = c("a", "b")),
      "Error: 'labels' must be a function applied to the breaks."
    )
    expect_error(
      filter_breaks(breaks = 0:5, plot = base_plot, labels = \(x) c("a", "b")),
      "Error: 'labels' must return one label per break."
    )
  })

  it("should drop breaks that fit by position but not by label", {
    breaks <- 0:10
    mockery::stub(filter_breaks, ".panel_size_cm", 10)

    short <- filter_breaks(breaks = breaks, plot = base_plot, min_cm_distance = 0.1)
    wide <- filter_breaks(
      breaks = breaks, plot = base_plot, min_cm_distance = 0.1,
      labels = \(x) sprintf("%08.3f", x)
    )

    expect_equal(short, breaks)
    expect_lt(length(wide), length(short))
  })
})

describe(".panel_size_cm", {

  faceted_plot <- ggplot(data.frame(x = c(0, 10), y = c(0, 10), g = c("a", "b")), aes(x, y)) +
    geom_blank() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    facet_wrap(~ g) +
    theme_test()

  single <- ggplot_gtable(ggplot_build(base_plot))
  faceted <- ggplot_gtable(ggplot_build(faceted_plot))

  it("should leave out the room the axes and titles take", {
    device_cm <- grid::convertWidth(grid::unit(1, "npc"), "cm", valueOnly = TRUE)

    expect_lt(aNCA:::.panel_size_cm(single, "x"), device_cm)
    expect_gt(aNCA:::.panel_size_cm(single, "x"), 0)
  })

  it("should split the room between the panels of a faceted plot", {
    # Two panels plus the spacing between them, so each gets under half the single-panel room
    expect_lt(aNCA:::.panel_size_cm(faceted, "x"), aNCA:::.panel_size_cm(single, "x") / 2)
  })
})

describe(".label_extents_cm", {

  it("should leave a space either side of the label", {
    size <- calc_element("axis.text.x", base_plot$theme)$size
    bare_cm <- grid::convertWidth(
      grid::grobWidth(grid::textGrob("119.917", gp = grid::gpar(fontsize = size))),
      "cm", valueOnly = TRUE
    )

    expect_gt(aNCA:::.label_extents_cm("119.917", base_plot, "x"), bare_cm)
  })

  it("should measure width on the x axis and height on the y axis", {
    labels <- c("1", "119.917")

    x_cm <- aNCA:::.label_extents_cm(labels, base_plot, "x")
    y_cm <- aNCA:::.label_extents_cm(labels, base_plot, "y")

    # Width grows with the number of characters, height does not
    expect_gt(x_cm[2], x_cm[1])
    expect_equal(y_cm[1], y_cm[2])
    expect_lt(y_cm[2], x_cm[2])
  })

  it("should measure rotated x-axis labels by their height", {
    rotated <- base_plot + theme(axis.text.x = element_text(angle = 90))

    expect_equal(
      aNCA:::.label_extents_cm("119.917", rotated, "x"),
      aNCA:::.label_extents_cm("119.917", base_plot, "y")
    )
  })

  it("should return zero when the axis text is blank", {
    blank <- base_plot + theme(axis.text.x = element_blank())

    expect_equal(aNCA:::.label_extents_cm(c("1", "119.917"), blank, "x"), c(0, 0))
  })

  it("should fall back to the default theme when the plot theme is incomplete", {
    bare <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_blank()

    expect_gt(aNCA:::.label_extents_cm("119.917", bare, "x"), 0)
  })
})
