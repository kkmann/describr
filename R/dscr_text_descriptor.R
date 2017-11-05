dscr_text_descriptor <- function(text, label, pvalue) {

  res <- structure(
    list(
      text   = text,
      label  = label,
      pvalue = pvalue
    ),
    class = c("dscr_text_descriptor", "dscr_descriptor")
  )

  return(res)

}





as_data_frame.dscr_text_descriptor <- function(td, dscr, varname, ...) {

  variable <- dscr$df[[varname]]
  group    <- dscr$df[[dscr$by]]

  df_text <- data_frame( # start with labels
    group       = "__descriptors__",
    value_label = label(td, variable) %>% list()
  ) %>% rbind(data_frame(
    group       = "__total__",
    value_label = text(td, variable) %>% list()
  ))

  # only compute rest if stratified
  if (is.stratified(dscr)) {

    df_text <- df_text %>%
      rbind(data_frame(
        group       = "__pvalues__",
        value_label = compute_pvalues(td, variable, group)
      )) %>%
      rbind(
        data_frame(variable = variable, group = group) %>% group_by(group) %>%
        do(value_label = text(td, .$variable)) %>% ungroup()
      )

  }

  df_text <- df_text %>%
    mutate(
      max_length = max(sapply(value_label, length))
    ) %>% group_by(group) %>%
    do( # fill empty rows with spaces
      value_label = c(
        .$value_label[[1]],
        rep("", .$max_length - length(.$value_label[[1]]))
      )
    ) %>% ungroup() %>%
    spread(group, value_label) %>%
    unnest()

  return(df_text)

}





as_grob_list.dscr_text_descriptor <- function(td, dscr, varname, ...) {

  theme   <- dscr$theme_new

  df_text <- as_data_frame(td, dscr, varname, ...)

  # make actual grob, cannot use tableGrob as we need to control widths!
  # start with dummy gtable
  gt <- gtable( widths = rep(unit(0, "in"), ncol(df_text)))

  for (i in 1:nrow(df_text)) {

    gt_row <- gtable(heights = unit(0, "in")) # automatically increased

    for (j in 1:ncol(df_text)) {

      width   <- theme$colwidths$levels %>% to_inches()
      style   <- theme$body$descriptor$style$value_cell
      colname <- sprintf("__level__%s", names(df_text)[j])
      if (colnames(df_text)[j] == "__descriptors__") {
        width   <- theme$colwidths$descriptors %>% to_inches()
        style   <- theme$body$descriptor$style$label_cell
        colname <- "__descriptors__"
      }
      if (colnames(df_text)[j] == "__total__") {
        colname <- "__total__"
      }

      gt_row <- cbind(
        gt_row,
        element_table_grob(
          style,
          label   = df_text[[j]][i],
          width   = width,
          name    = sprintf("%s_%i", colnames(df_text)[j], i),
          dscr    = dscr,
          colname = colname
        )
      )

    }

    gt <- rbind(gt, gt_row)

  }

  colnames(gt) <- colnames(df_text)

  res <- list(
    `__label__` = gt[ , "__descriptors__"],
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
