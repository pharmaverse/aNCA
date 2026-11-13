local({
  library(shiny)
  library(logger)
  env <- new.env(parent = environment())
  source(system.file("shiny/modules/tab_data.R", package = "aNCA"), local = env)
  env$loading_popup <- function(...) NULL

  describe("repeated session restore completion", {
    it("signals every completed restore and resets readiness while restoring", {
      state <- new.env()
      state$completed <- 0L
      probe <- function(id) {
        moduleServer(id, function(input, output, session) {
          settings <- reactiveVal(NULL)
          data <- reactiveVal(NULL)
          replay <- reactiveVal(FALSE)
          ready <- env$.setup_auto_replay(
            list(settings_override = settings), replay, reactiveVal("upload"),
            reactiveVal(0), reactive(NULL), data, session
          )
          observeEvent(ready(), {
            req(ready())
            state$completed <- state$completed + 1L
          })
        })
      }
      testServer(probe, {
        session$flushReact()
        for (i in 1:3) {
          settings(list(mapping = list(AVAL = "AVAL"), tab = "nca", version = i))
          session$flushReact()
          expect_false(ready())
          expect_true(replay())
          expect_identical(state$completed, i - 1L)
          data(list(version = i))
          session$flushReact()
          expect_true(ready())
          expect_false(replay())
          expect_identical(state$completed, i)
        }
        expect_false(session$isClosed())
      })
    })
  })
})
