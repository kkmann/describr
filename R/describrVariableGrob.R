variableGrob <- function(dscr, varname) {

  theme <- dscr$theme_new

  descriptor_list <- dscr$core[[varname]]

  # create gtables
  gtable_list <- lapply(
    descriptor_list,
    function(d) descriptorGrob(d, dscr, varname)
  )

  # add separators
  i <- 1
  widths <- gtable_list[[1]]$widths # must be constant for all descriptors
  while (i < length(gtable_list)) {
    gtable_list <- append(
      gtable_list,
      list(element_table_grob(
        theme$body$descriptor$style$separator,
        widths = widths
      )),
      i
    )
    i <- i + 2
  }

  # bind rows
  gt <- do.call(rbind, args = gtable_list)

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
    justify(element_table_grob(
        theme$body$style$variables,
        varname,
        theme$colwidths$variables,
        name = sprintf("%s_variable_label", varname)
      ), "left", "top"
    ),
    1, 1, nrow(gt), 1,
    name = sprintf("%s_variable_label", varname)
  )

  return(gt)

}
