`%||%` <- function(x, y) {
  if (is.null(x)) {
    usethis::ui_info("{usethis::ui_code('x')} is NULL, default is used")
    y
  } else {
    x
  }
}
