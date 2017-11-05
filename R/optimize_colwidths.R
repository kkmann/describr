optimize_columnwidths <- function(dscr_gtable, maxwidth  = unit(6, "in"), ...) {
  UseMethod("optimize_columnwidths", dscr_gtable)
}

optimize_columnwidths.describr <- function(dscr, maxwidth  = unit(6, "in"), ...) {

  dscr %>%
    make_plottable() %>%
    optimize_columnwidths(maxwidth = maxwidth)

}



optimize_columnwidths.describrGtable <- function(
  dscr_gtable, maxwidth  = unit(6, "in"), ...
) {

  # retrieve original describer object
  dscr      <- attr(dscr_gtable, "describr")

  col_names <- colnames(dscr_gtable)

  maxwidth  <- convertUnit(maxwidth, "in", valueOnly = TRUE)

  col_names <- colnames(dscr_gtable)

  required_colwidths <- dscr$col_widths_tracker(0, 0, TRUE)
  required_colwidths_in <- sapply(
    required_colwidths,
    function(x) convertUnit(x, "in", valueOnly = TRUE)
  )

  if (sum(required_colwidths_in) <= maxwidth) {

    dscr$theme_new$colwidths$variables <- required_colwidths$`__variables__`

    dscr$theme_new$colwidths$descriptors <- required_colwidths$`__descriptors__`

    dscr$theme_new$colwidths$pvalues <- required_colwidths$`__pvalues__`

    dscr$theme_new$colwidths$pvalues_idx <- required_colwidths$`__pvalues_idx__`

    # determine maximal level width
    max_level_width <- unit(0, "in")
    for (colname in names(required_colwidths)) {
      if (length(grep("__level__", colname)) == 1 | colname == "__total__") {
        max_level_width <- convertWidth(
          max(required_colwidths[[colname]], max_level_width),
          "in"
        )
      }

    }

    dscr$theme_new$colwidths$levels <- max_level_width

  } else {

    # need to find a compromise

    l1_penalty_weights <- rep(1, length(col_names))
    l0_penalty_weights <- rep(1, length(col_names))
    # need to exclude seperators from l1 penalty
    for (i in 1:length(col_names)) {
      if (
        length(grep("__level", col_names[i])) == 1 |
          length(grep("__total__", col_names[i])) == 1 |
          length(grep("__pvalues", col_names[i])) == 1
      ) {
        l1_penalty_weights[i] <- 1000
      }
      if (
        length(grep("__descriptors__", col_names[i])) == 1
      ) {
        l1_penalty_weights[i] <- 5
      }
      if (length(grep("__separator", col_names[i])) == 1) {
        l0_penalty_weights[i] <- 0
        l1_penalty_weights[i] <- 0
      }
    }

    separator_inds <- grep("__separator", col_names)
    level_inds <- setdiff(
      c(grep("__level__", col_names), grep("__total__", col_names)),
      separator_inds
    )

    lambda   <- 0.25

    MIPModel() %>%
      add_variable(
        col_widths[i], i = 1:length(col_names),
        lb = 0,
        ub = maxwidth
      ) %>%
      # add_constraint(
      #   col_widths[i] <= required_colwidths_in[i],
      #   i = 1:length(col_names)
      # ) %>%
      add_constraint(
        sum_expr(
          col_widths[i],
          i = 1:length(col_names)
        ) == maxwidth
      ) %>%
      add_constraint(
        col_widths[1] >= min(maxwidth / 8, required_colwidths_in[1] / 2)
      ) %>%
      add_constraint(
        col_widths[3] >= min(maxwidth / 8, required_colwidths_in[3] / 2)
      ) %>%
      {
        m <- .
        if (length(separator_inds) > 1) {
          for (i in 2:length(separator_inds)) {
            m <- add_constraint(m, col_widths[separator_inds[i]] == col_widths[separator_inds[i - 1]])
          }
        }
        for (i in 1:length(separator_inds)) {
          m <- add_constraint(m, col_widths[separator_inds[i]] >= required_colwidths_in[separator_inds[i]] / 5)
        }
        if (length(level_inds) > 1) {
          for (i in 2:length(level_inds)) {
            m <- add_constraint(m, col_widths[level_inds[i]] == col_widths[level_inds[i - 1]])
          }
        }
        return(m)
      } %>%
      add_variable(
        is_too_small[i],
        i = 1:length(col_names),
        type = "binary"
      ) %>%
      add_constraint(
        col_widths[i] - required_colwidths_in[i] +
          2 * max(required_colwidths_in) * is_too_small[i] >= 0,
        i = 1:length(col_names)
        # this constraint ensures that is_too_small[i] == 1 if the new_colwidth
        # is smaller than the required one
      ) %>%
      set_objective(
        sum_expr(
          l1_penalty_weights[i] * (required_colwidths_in[i] - col_widths[i]),
          i = 1:length(col_names)
        ) +
          sum_expr(
            lambda * l0_penalty_weights[i] * is_too_small[i],
            i = 1:length(col_names)
          ),
        sense = "min"
      ) %>%
      solve_model(with_ROI(solver = "glpk")) ->
      result

    col_widths_opt <- unit(get_solution(result, col_widths[i])$value, "in")

    dscr$theme_new$colwidths$variables   <- col_widths_opt[grep("__variables__", col_names)[1]]
    dscr$theme_new$colwidths$descriptors <- col_widths_opt[grep("__descriptors__", col_names)[2]]
    dscr$theme_new$colwidths$pvalues     <- col_widths_opt[grep("__pvalues__", col_names)[2]]
    dscr$theme_new$colwidths$pvalues_idx <- col_widths_opt[grep("__pvalues_idx__", col_names)[1]]

    # determine maximal level width
    max_level_width <- convertUnit(max(col_widths_opt[level_inds]), "in")

    dscr$theme_new$colwidths$levels <- max_level_width

    # determine minimal separator width
    min_sep_width <- convertUnit(min(col_widths_opt[separator_inds]), "in")

    dscr$theme_new$colwidths$seperators <- min_sep_width

  }

  adjusted_dt <- make_plottable(dscr)

  return(adjusted_dt)

}