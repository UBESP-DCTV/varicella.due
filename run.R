{
  function(proceed = TRUE, save_all = TRUE) {
    if (interactive()) {
      if (requireNamespace("rstudioapi") && rstudioapi::isAvailable() && save_all) {
        rstudioapi::documentSaveAll()
      }
      targets::tar_visnetwork() |>
        print()

      proceed <- usethis::ui_yeah(
        "Do you want to proceed with {usethis::ui_code('tar_make')}?"
      )
    }

    if (proceed) {
      withr::with_envvar(
        list(RSTUDIO_VERSION = "2021.09.0"), {
         # devto3ols::test(stop_on_failure = TRUE)
         targets::tar_make_future(workers = 8)
        }
      )
      targets::tar_visnetwork(targets_only = TRUE) |>
        print()
    }
  }
}()

