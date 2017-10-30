headerGrob <- function(dscr) { # print the header row of the table

  theme <- dscr$theme_new

  add_col_header <- function(gt, cell_grob, width) {

    gt <- gtable_add_cols(gt, width, pos = -1)
    gt <- gtable_add_grob(gt, cell_grob, 1, ncol(gt), 1, ncol(gt), name = cell_grob$name)
    gt$heights <- convertHeight(
      unit.pmax(gt$heights, grobHeight(cell_grob)),
      unitTo = "in"
    )

    return(gt)

  }

  gt <- gtable(heights = unit(0, "in"))

  # variables column
  variables_cell <- element_table_grob(
    theme$header$style$variables,
    theme$header$labels$variables,
    theme$colwidths$variables,
    "header_variables_cell"
  )
  gt <- add_col_header(gt, variables_cell, theme$colwidths$variables)

  # descriptors column
  descriptors_cell <- element_table_grob(
    theme$header$style$descriptors,
    theme$header$labels$descriptors,
    theme$colwidths$descriptors,
    "header_descriptors_cell"
  )
  gt <- add_col_header(gt, zeroGrob(), theme$colwidths$seperators)
  gt <- add_col_header(gt, descriptors_cell, theme$colwidths$descriptors)

  # total column
  if (!(is.stratified.describr(dscr) & !dscr$totals)) { # total column always except opt out

    totals_cell <- element_table_grob(
      theme$header$style$levels,
      theme$header$labels$total,
      theme$colwidths$levels,
      "header_totals_cell"
    )
    gt <- add_col_header(gt, zeroGrob(), theme$colwidths$seperators)
    gt <- add_col_header(gt, totals_cell, theme$colwidths$levels)

  }

  # levels columns
  if (is.stratified(dscr)) {

    lvls <- levels(dscr$df[[dscr$by]])

    for (i in 1:length(lvls)) {

      lvl_cell <- element_table_grob(
        theme$header$style$levels,
        lvls[i],
        theme$colwidths$levels,
        sprintf("header_level_%s_cell", lvls[i])
      )
      gt <- add_col_header(gt, zeroGrob(), theme$colwidths$seperators)
      gt <- add_col_header(gt, lvl_cell, theme$colwidths$levels)

    }

  }

  # pvalue columns
  if (dscr$pvalues & is.stratified(dscr)) {

    pvalues_cell <- element_table_grob(
      theme$header$style$pvalues,
      theme$header$labels$pvalues,
      theme$colwidths$pvalues,
      "header_pvalues_cell"
    )
    gt <- add_col_header(gt, zeroGrob(), theme$colwidths$seperators)
    gt <- add_col_header(gt, pvalues_cell, theme$colwidths$pvalues)

  }

  # add seperator and grouping variable
  if (is.stratified(dscr)) {

    gt <- gtable_add_rows(gt, heights = theme$header$style$separator$separator_height, pos = 0)
    colFrom <- grep("totals|level", gt$layout$name)[1]
    colTo   <- tail(grep("totals|level", gt$layout$name), 1)
    gt <- gtable_add_grob(gt,
      element_table_grob(theme$header$style$separator),
      1, colFrom, 1, colTo, name = "header_separator"
    )

    grouping_cell <- element_table_grob(
      theme$header$style$grouping,
      dscr$by,
      sum(gt$widths[colFrom, colTo]),
      "header_grouping_cell"
    )
    gt <- gtable_add_rows(gt, heights = convertUnit(grobHeight(grouping_cell), "in"), pos = 0)
    gt <- gtable_add_grob(gt, grouping_cell, 1, colFrom, 1, colTo, name = "header_grouping_cell")

  }

  return(gt)

}