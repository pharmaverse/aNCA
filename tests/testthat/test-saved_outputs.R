# Source the saved_outputs module to test pure utility functions
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "modules", "tab_explore", "saved_outputs.R"
  ),
  local = TRUE
)

describe("empty_saved_plots_metadata", {
  it("returns a zero-row data.frame with correct columns", {
    meta <- empty_saved_plots_metadata()
    expect_s3_class(meta, "data.frame")
    expect_equal(nrow(meta), 0)
    expect_equal(names(meta), c("name", "type", "timestamp"))
  })
})

describe("upsert_saved_plot", {
  it("adds a new row to empty metadata", {
    meta <- empty_saved_plots_metadata()
    result <- upsert_saved_plot(meta, "plot1", "Individual", "2026-01-01 12:00:00")
    expect_equal(nrow(result), 1)
    expect_equal(result$name, "plot1")
    expect_equal(result$type, "Individual")
    expect_equal(result$timestamp, "2026-01-01 12:00:00")
  })

  it("adds multiple distinct plots", {
    meta <- empty_saved_plots_metadata()
    meta <- upsert_saved_plot(meta, "plot1", "Individual", "2026-01-01 12:00:00")
    meta <- upsert_saved_plot(meta, "plot2", "Mean", "2026-01-01 12:01:00")
    meta <- upsert_saved_plot(meta, "plot3", "QC", "2026-01-01 12:02:00")
    expect_equal(nrow(meta), 3)
    expect_equal(meta$name, c("plot1", "plot2", "plot3"))
    expect_equal(meta$type, c("Individual", "Mean", "QC"))
  })

  it("updates timestamp and type on overwrite", {
    meta <- empty_saved_plots_metadata()
    meta <- upsert_saved_plot(meta, "plot1", "Individual", "2026-01-01 12:00:00")
    meta <- upsert_saved_plot(meta, "plot2", "Mean", "2026-01-01 12:01:00")

    # Overwrite plot1 with new type and timestamp
    meta <- upsert_saved_plot(meta, "plot1", "QC", "2026-01-01 13:00:00")
    expect_equal(nrow(meta), 2)
    expect_equal(meta$timestamp[meta$name == "plot1"], "2026-01-01 13:00:00")
    expect_equal(meta$type[meta$name == "plot1"], "QC")

    # plot2 unchanged
    expect_equal(meta$timestamp[meta$name == "plot2"], "2026-01-01 12:01:00")
    expect_equal(meta$type[meta$name == "plot2"], "Mean")
  })

  it("does not duplicate rows on overwrite", {
    meta <- empty_saved_plots_metadata()
    meta <- upsert_saved_plot(meta, "myplot", "Individual", "t1")
    meta <- upsert_saved_plot(meta, "myplot", "Individual", "t2")
    meta <- upsert_saved_plot(meta, "myplot", "Mean", "t3")
    expect_equal(nrow(meta), 1)
    expect_equal(meta$name, "myplot")
    expect_equal(meta$type, "Mean")
    expect_equal(meta$timestamp, "t3")
  })
})

describe("remove_saved_plot", {
  it("removes an existing plot by name", {
    meta <- empty_saved_plots_metadata()
    meta <- upsert_saved_plot(meta, "plot1", "Individual", "t1")
    meta <- upsert_saved_plot(meta, "plot2", "Mean", "t2")

    result <- remove_saved_plot(meta, "plot1")
    expect_equal(nrow(result), 1)
    expect_equal(result$name, "plot2")
  })

  it("returns empty data.frame when removing the last plot", {
    meta <- empty_saved_plots_metadata()
    meta <- upsert_saved_plot(meta, "only_plot", "QC", "t1")

    result <- remove_saved_plot(meta, "only_plot")
    expect_equal(nrow(result), 0)
    expect_equal(names(result), c("name", "type", "timestamp"))
  })

  it("is a no-op when plot name does not exist", {
    meta <- empty_saved_plots_metadata()
    meta <- upsert_saved_plot(meta, "plot1", "Individual", "t1")

    result <- remove_saved_plot(meta, "nonexistent")
    expect_equal(nrow(result), 1)
    expect_equal(result$name, "plot1")
  })

  it("handles removal from empty metadata", {
    meta <- empty_saved_plots_metadata()
    result <- remove_saved_plot(meta, "anything")
    expect_equal(nrow(result), 0)
  })
})

describe("metadata reset workflow", {
  it("simulates full add-overwrite-remove-reset cycle", {
    # Start fresh
    meta <- empty_saved_plots_metadata()

    # Add plots
    meta <- upsert_saved_plot(meta, "ind1", "Individual", "t1")
    meta <- upsert_saved_plot(meta, "mean1", "Mean", "t2")
    expect_equal(nrow(meta), 2)

    # Overwrite ind1
    meta <- upsert_saved_plot(meta, "ind1", "Individual", "t3")
    expect_equal(nrow(meta), 2)
    expect_equal(meta$timestamp[meta$name == "ind1"], "t3")

    # Remove mean1
    meta <- remove_saved_plot(meta, "mean1")
    expect_equal(nrow(meta), 1)
    expect_equal(meta$name, "ind1")

    # Reset (simulates new data upload)
    meta <- empty_saved_plots_metadata()
    expect_equal(nrow(meta), 0)
  })
})
