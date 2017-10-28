PlotDescriptor <- function(call, label, minplotheight = unit(1, "cm")) {

  res <- structure(
    list(call = substitute(call), label = label, minplotheight = minplotheight),
    class = c("PlotDescriptor", "Descriptor")
  )

  return(res)

}



labelGrob.PlotDescriptor <- function(td, data, width = unit(1, "in"), name = NULL, gp = gpar(), vp = NULL, ...) {
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

valueGrob.PlotDescriptor <- function(td, data, width = unit(1, "in"), name = NULL, gp = NULL, vp = NULL, ...) {

  df <- data_frame(x = data)

  plot_expr <- parse(text = paste0(
    "df %>% ",
    deparse(td$call),
    " + theme_void()"
  ))
  plot <- ggplotGrob(eval(plot_expr))
  g <- gtable(
    widths  = width,
    heights = td$minplotheight
  )
  g <- gtable_add_grob(g, plot, 1, 1, 1, 1)
  return(g)
}
