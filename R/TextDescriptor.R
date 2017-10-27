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


labelGrob <- function(td, data, width = NULL, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("labelGrob", td)
}

valueGrob <- function(td, data, width = NULL, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("valueGrob", td)
}

labelGrob.TextDescriptor <- function(td, data, width = unit(1, "in"), name = NULL, gp = gpar(), vp = NULL, ...) {
  lbl         <- attr(td, "label")(data)
  n_row       <- length(lbl)
  lblGrobList <- list()
  for (i in 1:n_row) {
    lblGrobList <- c(lblGrobList, list(fixedWidthTextGrob(lbl[i], width = width, gp = gp, just = c("right", "top"), x = unit(1, "npc"), y = unit(1, "npc"))))
  }
  g <- gtable(
    widths  = width,
    heights = unit(sapply(lblGrobList, function(x) 1.5*convertHeight(grobHeight(x), unitTo = "in", valueOnly = TRUE)), "in")
  )
  for (i in 1:n_row) {
    g <- gtable_add_grob(g,
      lblGrobList[i], i, 1, i, 1
    )
  }
  return(g)
}

valueGrob.TextDescriptor <- function(td, data, width = unit(1, "in"), name = NULL, gp = NULL, vp = NULL, ...) {
  val <- attr(td, "f")(data)
  n_row       <- length(val)
  lblGrobList <- list()
  for (i in 1:n_row) {
    lblGrobList <- c(lblGrobList, list(fixedWidthTextGrob(val[i], width = width, gp = gp, just = c("right", "top"), x = unit(1, "npc"), y = unit(1, "npc"))))
  }
  g <- gtable(
    widths  = width,
    heights = unit(sapply(lblGrobList, function(x) 1.5*convertHeight(grobHeight(x), unitTo = "in", valueOnly = TRUE)), "in")
  )
  for (i in 1:n_row) {
    g <- gtable_add_grob(g,
                         lblGrobList[i], i, 1, i, 1
    )
  }
  return(g)
}
