eval_model <- function(
  model_object,
  batch_size = NULL,
  verbose = 0
) {
  model <- keras::unserialize_model(model_object[["serialized_model"]])
  new_data <- model_object[["params"]][["validation"]][["validation_x"]]

  res <- model |>
    predict(new_data, batch_size = batch_size, verbose = verbose)

  res[, 1L, drop = TRUE]
}
