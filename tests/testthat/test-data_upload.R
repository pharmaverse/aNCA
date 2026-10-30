describe("versioned settings upload in an installed app", {
  it("restores single and selected versions without private objects on the search path", {
    env <- source_app_exports("modules/tab_data/data_upload.R")
    env$showNotification <- function(...) NULL
    env$removeModal <- function(...) NULL
    version <- create_settings_version(
      list(settings = list(method = "linear", min_hl_points = 4), nca_ran = TRUE),
      comment = "Replay", tab = "nca"
    )
    content <- list(settings = version$settings)
    attr(content, "versioned") <- list(versions = list(version))
    restored <- NULL
    saved_versions <- NULL
    pending <- list(versions = list(version, version))
    session <- list(userData = list(settings_versions = function(x) saved_versions <<- x))
    override <- function(x) restored <<- x
    errors <- env$.apply_uploaded_settings(
      list(list(type = "settings", name = "settings.yaml", content = content)),
      character(), override, function(x) NULL, session, identity
    )
    expect_length(errors, 0L)
    expect_equal(restored$settings$method, "linear")
    expect_equal(restored$settings$min_hl_points, 4)
    expect_true(restored$nca_ran)
    expect_equal(restored$tab, "nca")
    expect_equal(saved_versions, list(version))

    restored <- NULL
    env$.handle_version_restore(
      function(x) {
        if (missing(x)) pending else pending <<- x
      },
      function() 2L, override, session
    )
    expect_equal(restored$settings$method, "linear")
    expect_equal(restored$settings$min_hl_points, 4)
    expect_true(restored$nca_ran)
    expect_equal(restored$tab, "nca")
    expect_null(pending)
    expect_length(saved_versions, 2L)
  })
})
