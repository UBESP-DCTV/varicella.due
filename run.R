{
  function(proceed = TRUE, save_all = TRUE) {
    if (interactive()) {
      if (requireNamespace("rstudioapi") && save_all) {
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
        list(RSTUDIO_VERSION = "2021.09.0"),
        targets::tar_make()
      )
      targets::tar_visnetwork() |>
        print()
    }
  }
}()

