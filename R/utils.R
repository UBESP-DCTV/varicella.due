`%||%` <- function(x, y) {
  if (is.null(x)) {
    usethis::ui_info("{usethis::ui_code('x')} is NULL, default is used")
    y
  } else {
    x
  }
}


add_endoffield <- function(x) c(x, "__EOF__")


add_endofrecord <- function(x) c(x, "__EOR__")
