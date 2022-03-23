#' Custom ggplot for keras' histories
#'
#' @param keras_plot a keras plot object
#' @param history a [keras]{history} object
#' @param params output of [setup_input_data]
#'
#' @return a [ggplot2]{ggplot} object
#' @export
gg_history <- function(
    keras_plot,
    history,
    params
) {

  # when trained for test, the stopping rule are decided by
  # previously runs
  if (params$is_test) {
    stoped_epoch <- length(history[["metrics"]][["loss"]])
  } else {
    val_acc <- history[["metrics"]][["val_accuracy"]]
    # we need to consider the first epoch that reached the maximum
    # values of the validation accuracy
    stoped_epoch <- which.max(val_acc)
  }


  data_boxplot <- as.data.frame(history) %>%
    dplyr::mutate(cut = 12.5 * epoch %/% 25)

  notes_db <- as.data.frame(history) %>%
    dplyr::filter(epoch == stoped_epoch) %>%
    dplyr::mutate_if(is.numeric, round, 4) %>%
    dplyr::mutate(cut = 12.5 * epoch %/% 25)

  keras_plot +
    ggplot2::geom_boxplot(
      data = dplyr::filter(data_boxplot, data == "validation"),
      ggplot2::aes(group = cut, fill = NULL),
      alpha = 0.5
    ) +
    ggplot2::xlab("Epoch") +
    ggplot2::ggtitle(
      "Loss and categorical accuracy by epochs for the Varicella's training and validation set.",
      subtitle = glue::glue(
        "Name: {params$run_name}\n",
        "Architecture: {params$architecture}\n",
        "Start time: {params$start_time}\n",
        "Testing phase: {as.character(params$is_test)}\n",
        "Records considered: ({params$train$train_len + params$validation$validation_len} observation overall)\n",
        "Training set ss: {params$train$train_len}\n",
        "Train sequence lengths: {paste(paste0(names(params$train$train_dist), ': ', round(params$train$train_dist)), collapse = ', ')}\n",
        "Average training sequence length: {round(params$train$train_mean_len, 2)}\n",
        "Validation set ss: {params$validation$validation_len}\n",
        "Validation sequence lengths: {paste(paste0(names(params$validation$validation_dist), ': ', round(params$validation$validation_dist)), collapse = ', ')}\n",
        "Average validation sequence length: {round(params$validation$validation_mean_len, 2)}\n",
        "Words in the full embedding dictionary: {params$pedia_dict_size}\n",
        "Words in the corpus: {params$corpus_dict_size}\n",
        "Embedding used (dictionary = words + OOV + PAD, features dimension): ({params$max_words}, {params$embedding_dim})\n",
        "Input layer size (words considerd): {params$max_len} (padded/truncated at/from the end)\n",
        "Batch size: {params$batch_size}\n",
        "Max epochs: {params$epochs} (best after {stoped_epoch}, patience = 100)\n",
        "Loss: {stringr::str_replace(params$loss, '[^a-zA-Z]', ' ') %>% stringr::str_to_title()}\n",
        "Optimizer: {stringr::str_replace(params$optimizer, '[^a-zA-Z]', ' ') %>% stringr::str_to_title()}\n",
        "Optimization metric: {stringr::str_replace(params$metrics, '[^a-zA-Z]', ' ') %>% stringr::str_to_title()}\n",
        "Overall time ellapsed (fit-only): {round(params$train_time, 2)} {attr(params$train_time, 'units')}"
      )
    ) +
    ggplot2::geom_text(
      data = notes_db,
      ggplot2::aes(
        x = epoch + 1,
        y = value + 0.04,
        label = glue::glue("{100*value} [{stoped_epoch}]")
      )
    )
}
