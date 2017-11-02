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
    "header_variables_cell",
    dscr = dscr,
    colname = "__variables__"
  )
  gt <- add_col_header(gt, variables_cell, theme$colwidths$variables)

  # descriptors column
  descriptors_cell <- element_table_grob(
    theme$header$style$descriptors,
    theme$header$labels$descriptors,
    theme$colwidths$descriptors,
    "header_descriptors_cell",
    dscr = dscr,
    colname = "__descriptors__"
  )
  gt <- add_col_header(gt, zeroGrob(), theme$colwidths$seperators)
  gt <- add_col_header(gt, descriptors_cell, theme$colwidths$descriptors)

  # total column
  if (!(is.stratified.describr(dscr) & !dscr$totals)) { # total column always except opt out

    totals_cell <- element_table_grob(
      theme$header$style$levels,
      theme$header$labels$total,
      theme$colwidths$levels,
      "header_totals_cell",
      dscr = dscr,
      colname = "__total__"
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
        sprintf("header_level_%s_cell", lvls[i]),
        dscr = dscr,
        colname = sprintf("__level__%s", lvls[i])
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
      convertUnit(theme$colwidths$pvalues + theme$colwidths$pvalues_idx, "in"),
      "header_pvalues_cell",
      dscr = dscr,
      colname = "__pvalues__"
    )
    gt <- add_col_header(gt, zeroGrob(), theme$colwidths$seperators)
    # manual as spanning two columns!
    gt <- gtable_add_cols(gt,
      widths  = unit.c(theme$colwidths$pvalues, theme$colwidths$pvalues_idx),
      pos = -1
    )
    gt$heights <- convertUnit(max(gt$heights, grobHeight(pvalues_cell)), "in")
    gt <- gtable_add_grob(gt, pvalues_cell, 1, ncol(gt) - 1, 1, ncol(gt), name = "pvalue")

  }

  colnames(gt) <- create_colnames(dscr)

  # add seperator and grouping variable
  if (is.stratified(dscr)) {

    # add n descriptor

    gt <- rbind(gt, variableGrob(dscr, dscr$by))

    gt <- gtable_add_rows(gt, heights = theme$header$style$separator$separator_height, pos = 0)
    colFrom <- grep("__total__|__level__", colnames(gt))[2] # + 1 for seperator
    colTo   <- tail(grep("__total__|__level__", colnames(gt)), 1)
    gt <- gtable_add_grob(gt,
      element_table_grob(theme$header$style$separator),
      1, colFrom, 1, colTo, name = "header_separator"
    )

    grouping_cell <- element_table_grob(
      theme$header$style$grouping,
      dscr$by,
      convertWidth(sum(gt$widths[colFrom:colTo]), "in"),
      "header_grouping_cell"
    )
    gt <- gtable_add_rows(gt, heights = convertUnit(grobHeight(grouping_cell), "in"), pos = 0)
    gt <- gtable_add_grob(gt, grouping_cell, 1, colFrom, 1, colTo, name = "header_grouping_cell")

  }

  return(gt)

}