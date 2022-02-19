get_current_parameter <- function(
    max_words,
    batch_size = 8L,
    epochs = 15L,
    loss      = "categorical_crossentropy",
    metrics   = "categorical_accuracy",
    optimizer = "adam"
) {
  list(
    max_words = max_words + 2L,
    batch_size = batch_size,
    epochs = epochs,
    loss      = loss,
    metrics   = metrics,
    optimizer = optimizer
  )
}
