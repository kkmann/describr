PlotDescriptor <- function(call, label, minplotheight = unit(1.5, "cm")) {

  res <- structure(
    list(call = substitute(call), label = label, minplotheight = minplotheight),
    class = c("PlotDescriptor", "Descriptor")
  )

  return(res)

}



as.grob.PlotDescriptor <- function(pd, dscr, variable, group) {

  pd <- list()
  pd$call <- substitute(ggplot(aes(variable)) + geom_histogram())
  pd$label <- function(data) "blaaa"

  theme <- dscr$theme_new

  lbl <- element_table_grob(
    theme$body$descriptor$style$label_cell,
    label = pd$label(variable),
    width = theme$colwidths$descriptors,
    name  = sprintf("%s_%i", colnames(df_text)[j], i)
  )

  width  <- theme$colwidths$levels

  df <- data_frame(variable = variable, group = group)

  get_plot <- function(level) {

    str <- "df %>% "

    if (level != "__total__") {

      str <- paste0(str, sprintf("filter(group == '%s') %%>%% ", level))

    }

    str <- paste0(str, paste0(deparse(pd$call), collapse = ""))

    # find scale of variable and unify across all levels
    tmp <- eval(parse(text = str))
    mappings <- lapply(tmp$mapping, as.character)
    if ("x" %in% names(mappings)) {
      if (mappings$x == "variable") {
        str <- paste0(str, " + xlim(range(df$variable))")
      }
    }
    if ("y" %in% names(mappings)) {
      if (mappings$y == "variable") {
        str <- paste0(str, " + ylim(range(df$variable))")
      }
    }

    return(eval(parse(text = str)))

  }

  total <- element_table_grob(
    theme$body$descriptor$style$plot_cell,
    plot  = get_plot("__total__"),
    width = width,
    name  = "__total__"
  )

  # all plots have same height, unify with label table
  height        <- unit.pmax(lbl$heights, total$heights)
  lbl$heights   <- height
  total$heights <- height

  res <- list(
    `__label__` = lbl,
    `__total__` = total,
    levels      = list()
  )
  for (i in 1:length(levels(group))) {
    level <- levels(group)[i]
    new_element <- list(
      element_table_grob(
        theme$body$descriptor$style$plot_cell,
        plot  = get_plot(level),
        width = width,
        name  = level
      )
    )
    new_element[[1]]$heights <- height
    names(new_element)       <- level
    res$levels  <- c(res$levels, new_element)
  }

  return(res)

}





labelGrob.PlotDescriptor <- function(td, data_complete, width = unit(1, "in"), ...) {
  lbl         <- td$label(data_complete)
  n_row       <- length(lbl)
  lblGrobList <- list()
  for (i in 1:n_row) {
    lblGrobList <- c(lblGrobList,
      list(fixedWidthTextGrob(lbl[i], width = width, just = c("right", "top"), x = unit(1, "npc"), y = unit(1, "npc"))))
  }
  g <- gtable(
    widths  = width,
    heights = unit(sapply(lblGrobList,
      function(x) 1.5*convertHeight(grobHeight(x), unitTo = "in", valueOnly = TRUE)), "in")
  )
  for (i in 1:n_row) {
    g <- gtable_add_grob(g,
      lblGrobList[i], i, 1, i, 1
    )
  }
  return(justify(g, hjust = "right", vjust = "top"))
}

valueGrob.PlotDescriptor <- function(td, data_subset, data_complete, width = unit(1, "in"), ...) {

  df <- data_frame(x = data_subset)

  plot_expr <- parse(text = paste0(
    "df %>% ",
    paste0(deparse(td$call), collapse = ""),
    " + theme_void() + theme(
      panel.background = element_rect(fill = 'lightgrey')
    )"
  ))
  plot <- ggplotGrob(eval(plot_expr))
  g <- gtable(
    widths  = width,
    heights = td$minplotheight
  )
  g <- gtable_add_grob(g, plot, 1, 1, 1, 1)
  return(g)
}
