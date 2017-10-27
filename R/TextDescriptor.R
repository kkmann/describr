TextDescriptor <- function(f, label) {
  res <- structure(
    function(select_statement) {
      attr(res, "select_statement") <- sys.call()
      return(res)
    },
    class = c("TextDescriptor", "Descriptor")
  )
  attr(res, "select_statement") <- NULL
  attr(res, "label") <- label
  attr(res, "f")     <- f
  return(res)
}


label_dimensions <- function(d, data, ...) {
  UseMethod("label_dimensions", d)
}

label_dimensions.TextDescriptor <- function(
  d, data, fontsize, lineheight, ...
) { # in pt
  val   <- attr(d, "f")(data)
  return(
    unit(length(val)*lineheight, "pt"),
    unit(max(sapply(val, nchar))*fontsize, "pt")
  )
}


labelGrob <- function(td, data, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("labelGrob", td)
}

valueGrob <- function(td, data, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("valueGrob", td)
}

labelGrob.TextDescriptor <- function(td, data, name = NULL, gp = NULL, vp = NULL, ...) {
  lbl <- attr(td, "label")(data)
  g   <- gridExtra::tableGrob(lbl, cols = NULL, rows=NULL, theme = gridExtra::ttheme_minimal(base_size = 15), vp = vp)
  g   <- justify(g, "left", "top")
  return(g)
}

valueGrob.TextDescriptor <- function(td, data, name = NULL, gp = NULL, vp = NULL, ...) {
  val <- attr(td, "f")(data)
  g   <- gridExtra::tableGrob(val, cols = NULL, rows = NULL, vp = vp, theme = gridExtra::ttheme_minimal(base_size = 15, core=list(bg_params = list(fill = "white"))))
  g   <- justify(g, "left", "top")
  return(g)
}
