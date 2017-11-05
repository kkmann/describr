descriptor <- function() {

  res <- structure(
    list(),
    class = c("dscr_descriptor")
  )

  return(res)

}




as_grob_list <- function(d, dscr, varname, ...) {
  UseMethod("as_grob_list", d)
}

as_grob_list.dscr_descriptor <- function(d, dscr, varname, ...) {
  stop("Not implemented")
}





as_gtable.dscr_descriptor <- function(d, dscr, varname, ...) {

  theme    <- dscr$theme_new

  group    <- dscr$df[[dscr$by]]
  variable <- dscr$df[[varname]]

  grobs    <- as_grob_list(d, dscr, varname)

  # start with descriptor label column
  gt <- gtable(
      widths  = theme$colwidths$descriptors %>% to_inches(),
      heights = grobHeight(grobs$`__label__`) %>% to_inches()
    ) %>%
    gtable_add_grob(
      grobs$`__label__`,
      1, 1, name = "label"
    )

  # total column
  if (!is.stratified(dscr) | dscr$totals) { # total column always except opt out

    # add separator spacing
    gt <- gtable_add_cols(gt,
        unit.c(theme$colwidths$seperators, theme$colwidths$levels) %>% to_inches(),
        pos = -1
      ) %>%
      gtable_add_grob(
        grobs$`__total__`,
        1, ncol(.), name = "total"
      )
    gt$heights <- max(gt$heights, grobHeight(grobs$`__total__`)) %>% to_inches()

  }

  # levels columns
  if (is.stratified(dscr)) {

    for (lvl in levels(group)) {

      # add separator spacing
      gt <- gtable_add_cols(gt,
          unit.c(theme$colwidths$seperators, theme$colwidths$levels) %>% to_inches(),
          pos = -1
        ) %>%
        gtable_add_grob(
          grobs$levels[[lvl]],
          1, ncol(.), name = lvl
        )
      gt$heights <- max(gt$heights, grobHeight(grobs$levels[[lvl]])) %>% to_inches()

    }

  }

  # pvalue columns
  if (dscr$pvalues & is.stratified(dscr)) {

    # add separator spacing
    gt <- gtable_add_cols(gt, theme$colwidths$seperators %>% to_inches(), pos = -1)

    # compute pvalue (actual number)
    p       <- d$pvalue(variable, group)
    p_label <- attr(d$pvalue, "label")
    dscr$register_pvalue(p_label)

    new_element <- element_table_grob(
      theme$body$descriptor$style$pvalues,
      label = sprintf("%.3f", p),
      width = theme$colwidths$pvalues,
      name  = "pvalue",
      dscr = dscr,
      colname = "__pvalues__"
    )
    gt <- gtable_add_cols(gt, widths = theme$colwidths$pvalues, pos = -1)
    gt <- gtable_add_grob(gt,
      new_element, t = 1, ncol(gt), nrow(gt), ncol(gt)
    )


    new_element <- element_table_grob(
      theme$body$descriptor$style$pval_idx,
      label   = sprintf("(%i)", dscr$register_pvalue(p_label)),
      width   = theme$colwidths$pvalues_idx,
      name    = "pvalue_index",
      dscr    = dscr,
      colname = "__pvalues_idx__"
    )
    gt <- gtable_add_cols(gt, widths = theme$colwidths$pvalues_idx, pos = -1)
    gt <- gtable_add_grob(gt,
      new_element, t = 1, ncol(gt), nrow(gt), ncol(gt)
    )

  }

  # gt <- gtable_add_grob(gt,
  #   rectGrob(gp = gpar(alpha = 0.25)), 1, 1, nrow(gt), ncol(gt), z = Inf
  # )

  return(gt)

}
