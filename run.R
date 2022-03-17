{
  function(proceed = TRUE, save_all = TRUE) {
    if (interactive()) {
      if (
        requireNamespace("rstudioapi") &&
        rstudioapi::isAvailable() &&
        save_all
      ) {
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
         # devtools::test(stop_on_failure = TRUE)
          targets::tar_make()
          # targets::tar_make_future(workers = 2)
        }
      )
      targets::tar_visnetwork(
        targets_only = TRUE,
        degree_from = 5,
        degree_to = 5
      ) |>
        print()
    }

    # if (Sys.getenv("R_telegram_bot_name") != "") {
    #   depigner::start_bot_for_chat("varicella.due")
    #
    #   targets::tar_visnetwork(
    #     targets_only = TRUE,
    #     outdated = FALSE
    #   ) |>
    #     visNetwork::visSave(file = here::here("inst/tar_visnetwork.html"))
    #
    #   depigner::send_to_telegram(
    #     "New run finished. Here below final DAG."
    #   )
    #   webshot::webshot(
    #     here::here("inst/tar_visnetwork.html"),
    #     here::here("inst/tar_visnetwork.png"),
    #     delay = 0.1
    #   )[[1]] |>
    #     depigner::send_to_telegram(type = "photo")
    # }



  }
}()

