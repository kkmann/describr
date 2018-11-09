PlotDescriptor <- function(call, label, pvalue, minplotheight = unit(1.5, "cm")) {

  res <- structure(
    list(
      call          = substitute(call),
      label         = label,
      pvalue        = pvalue,
      minplotheight = minplotheight
    ),
    class = c("PlotDescriptor", "Descriptor")
  )

  return(res)

}



get_call <- function(d, ...) {
  UseMethod("get_call", d)
}


as_gtable.PlotDescriptor <- function(pd, dscr, variable, group) {

  pd <- setup(pd, variable, group)

  theme <- dscr$theme_new

  lbl <- element_table_grob(
    theme$body$descriptor$style$label_cell,
    label = get_label(pd, variable),
    width = theme$colwidths$descriptors,
    name  = "label",
    dscr = dscr,
    colname = "__descriptors__"
  )

  width  <- theme$colwidths$levels

  df <- data_frame(variable = variable, group = group)

  get_plot <- function(level) {

    plot <- tryCatch({

        str <- "df %>% "

        if (level != "__total__") {

          str <- paste0(str, sprintf("filter(group == '%s') %%>%% ", level))

        }

        str <- paste0(str, paste0(deparse(get_call(pd)), collapse = ""))

        # find scale of variable and unify across all levels
        tmp <- eval(parse(text = str))
        mappings <- lapply(tmp$mapping, as.character)
        if ("x" %in% names(mappings)) {
          if ("variable" %in% mappings$x & is.numeric(variable)) {
            str <- paste0(str, " + xlim(range(df$variable))")
          }
        }
        if ("y" %in% names(mappings)) {
          if (mappings$y == "variable" & is.numeric(variable)) {
            str <- paste0(str, " + ylim(range(df$variable))")
          }
        }

        eval(parse(text = str))
      },
      error = function(e) {
        print(e)
        p <- ggplot(data.frame(1)) # empty plot
        return(p)
      }
    )
    return(plot)
  }

  total <- element_table_grob(
    theme$body$descriptor$style$plot_cell,
    pd    = pd,
    plot  = get_plot("__total__"),
    width = width,
    name  = "__total__",
    dscr  = dscr,
    colname = "__total__"
  )

  # all plots have same height, unify with label table
  #height        <- convertHeight(max(lbl$heights, total$heights), "pt")
  #lbl$heights   <- height
  #total$heights <- height

  res <- list(
    `__label__` = lbl,
    `__total__` = total,
    levels      = list()
  )
  if (is.stratified(dscr)) {
    for (i in 1:length(levels(group))) {
      level <- levels(group)[i]
      new_element <- list(
        element_table_grob(
          theme$body$descriptor$style$plot_cell,
          pd      = pd,
          plot    = get_plot(level),
          width   = width,
          name    = level,
          dscr    = dscr,
          colname = sprintf("__level__%s", level)
        )
      )
      #new_element[[1]]$heights <- height
      names(new_element)       <- level
      res$levels  <- c(res$levels, new_element)
    }
  }

  return(res)

}
