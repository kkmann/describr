TextDescriptor <- function(text, label) {

  res <- structure(
    list(text = text, label = label),
    class = c("TextDescriptor", "Descriptor")
  )

  return(res)

}





as.gtable.TextDescriptor <- function(td, dscr, variable, group) {

  theme   <- dscr$theme

  td<-list()
  td$text <- function(x) {
    if (rnorm(1) > 0) {
      c(sprintf("%.2f, %.1f", mean(x), sd(x)), "bla")
    } else {
      "blubb"
    }
  }
  td$label <- function(x) "label"

  variable <- iris$Sepal.Length
  group    <- iris$Species

  df_text <- data_frame(
    variable = variable,
    group    = group
  ) %>%
    group_by(
      group
    ) %>%
    do(
      value_label = td$text(.$variable)
    ) %>%
    ungroup()

  df_text <- rbind(
    df_text,
    data_frame(group = "__total__", value_label = list(td$text(variable)))
  )

  df_text <- rbind(
    df_text,
    data_frame(group = "__label__", value_label = list(td$label(variable)))
  )

  df_text %>%
    mutate(
      max_length = max(sapply(value_label, length))
    ) %>%
    group_by(group) %>%
    do(
      value_label = c(.$value_label[[1]], rep("", .$max_length - length(.$value_label[[1]])))
    ) %>%
    ungroup() %>%
    spread(group, value_label) %>%
    unnest() ->
  df_text

  # make actual grob, cannot use tableGrob as we need to control widths!
  gt <- gtable(
    widths = rep(unit(0, "in"), ncol(df_text))
  )
  for (i in 1:nrow(df_text)) {

    gt_row <- gtable(heights = unit(0, "in")) # automatically increased

    for (j in 1:ncol(df_text)) {

      width <- theme$colwidths$levels
      style <- theme$body$descriptor$style$value_cell
      if (colnames(df_text)[j] == "__label__") {
        width <- theme$colwidths$descriptors
        style <- theme$body$descriptor$style$label_cell
      }

      gt_row <- cbind(
        gt_row,
        element_table_grob(
          style,
          label = df_text[[j]][i],
          width = width,
          name  = sprintf("%s_%i", colnames(df_text)[j], i)
        )
      )

    }

    gt <- rbind(gt, gt_row)

  }

  colnames(gt) <- colnames(df_text)

  res <- list(
    `__label__` = gt[ , "__label__"],
    `__total__` = gt[ , "__total__"],
    levels      = list()
  )
  for (i in 1:(ncol(gt) - 2)) {
    new_element <- list(gt[ , i])
    names(new_element) <- colnames(gt)[i]
    res$levels  <- c(res$levels, new_element)
  }

  return(res)

}


labelGrob.TextDescriptor <- function(td, data_complete, width = unit(1, "in"), ...) {
  lbl         <- td$label(data_complete)
  n_row       <- length(lbl)
  lblGrobList <- list()
  for (i in 1:n_row) {
    lblGrobList <- c(lblGrobList, list(fixedWidthTextGrob(lbl[i], width = width, just = c("right", "top"), x = unit(1, "npc"), y = unit(1, "npc"))))
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

valueGrob.TextDescriptor <- function(td, data_subset, data_complete, width = unit(1, "in"), ...) {
  val <- td$text(data_subset, data_complete)
  n_row       <- length(val)
  lblGrobList <- list()
  for (i in 1:n_row) {
    lblGrobList <- c(lblGrobList, list(fixedWidthTextGrob(val[i], width = width, just = c("right", "top"), x = unit(1, "npc"), y = unit(1, "npc"))))
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
