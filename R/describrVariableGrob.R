variableGrob <- function(dscr, varname) {

  theme <- dscr$theme_new

  descriptor_list <- dscr$core[[varname]]

  # create gtables
  gtable_list <- lapply(
    descriptor_list,
    function(d) descriptorGrob(d, dscr, varname)
  )

  widths <- convertWidth(gtable_list[[1]]$widths, "in") # must be constant for all descriptors
  gt     <- gtable(widths = widths)

  # add separators
  for (i in 1:length(gtable_list)) {

    gt <- rbind(gt, gtable_list[[i]])

    if (i < length(gtable_list)) {
      gt <- gtable_add_rows(gt, theme$body$descriptor$style$separator$separator_height, pos = -1)
      gt <- gtable_add_grob(gt,
        element_table_grob(
          theme$body$descriptor$style$separator,
          widths = widths
        ),
        nrow(gt), 1, nrow(gt), ncol(gt)
      )
    }

  }

  # add columns to the left for variable label and separator
  gt <- gtable_add_cols(
    gt,
    widths = unit.c(
      theme$colwidths$variables,
      theme$colwidths$seperators
    ),
    pos = 0
  )

  gt <- gtable_add_grob(
    gt,
    element_table_grob(
      theme$body$style$variables,
      varname,
      theme$colwidths$variables,
      name = sprintf("%s_variable_label", varname),
      dscr = dscr,
      colname = "__variables__"
    ),
    1, 1, nrow(gt), 1,
    name = sprintf("%s_variable_label", varname)
  )

  return(gt)

}
