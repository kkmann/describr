TextDescriptor <- function(text, label, pvalue) {

  res <- structure(
    list(
      text   = text,
      label  = label,
      pvalue = pvalue
    ),
    class = c("TextDescriptor", "Descriptor")
  )

  return(res)

}





as.grob.TextDescriptor <- function(td, dscr, variable, group) {

  theme   <- dscr$theme

  df_text <- data_frame(
    variable = variable,
    group    = group
  ) %>%
    group_by(
      group
    ) %>%
    do(
      value_label = td$text(.$variable, variable)
    ) %>%
    ungroup()

  df_text <- rbind(
    df_text,
    data_frame(group = "__total__", value_label = list(td$text(variable, variable)))
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
