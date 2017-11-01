PlotDescriptor <- function(call, label, pvalue, minplotheight = unit(1.5, "cm")) {

  res <- structure(
    list(
      call   = substitute(call),
      label  = label,
      pvalue = pvalue,
      minplotheight = minplotheight
    ),
    class = c("PlotDescriptor", "Descriptor")
  )

  return(res)

}



as.grob.PlotDescriptor <- function(pd, dscr, variable, group) {

  theme <- dscr$theme_new

  lbl <- element_table_grob(
    theme$body$descriptor$style$label_cell,
    label = pd$label(variable),
    width = theme$colwidths$descriptors,
    name  = "label"
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
  height        <- convertHeight(max(lbl$heights, total$heights), "pt")
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
