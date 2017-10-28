TextDescriptor <- function(text, label) {

  res <- structure(
    list(text = text, label = label),
    class = c("TextDescriptor", "Descriptor")
  )

  return(res)

}



labelGrob.TextDescriptor <- function(td, data, width = unit(1, "in"), name = NULL, gp = gpar(), vp = NULL, ...) {
  lbl         <- td$label(data)
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
  val <- td$text(data)
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
