dtable <- function(
  df,
  by = NULL,
  theme_new = theme_default_tmp(),
  pvalues = FALSE,
  totals = TRUE,
  maxwidth = 1.5*unit(8.27 - 2.5, "in") # 1.5 times maximal actual plotting
   # width on standard portrait a4: larger would be unrealistic scaling
) {

  by     = as.character(substitute(by))

  if (length(by) > 1) {
    stop("currently only single stratum supported")
  }
  if (length(by) == 1 & !(by %in% names(df))) {
    stop(sprintf("stratum 'by = %s' not found"))
  }

  core <- list()
  for (i in 1:ncol(df)) {
    core[names(df)[i]] <- NULL
  }

  register_pvalue <- function() { # counter!

    pvalue_labels <- character(0)

    function(label, return = FALSE) {
      if (return) {
        return(pvalue_labels)
      }

      if (label %in% pvalue_labels) {
        return(which(label == pvalue_labels))
      } else {
        pvalue_labels <<- cbind(pvalue_labels, label)
        return(length(pvalue_labels))
      }
    }

  }

  res <- structure(
    list(
      df              = df,
      by              = by,
      core            = core,
      group_descriptors = list(dscr_n_perc),
      pvalues         = pvalues,
      totals          = totals,
      register_pvalue = register_pvalue(),
      theme_new       = theme_new,
      maxwidth        = maxwidth
    ),
    class = "describr"
  )

  return(res)

}



is.stratified <- function(dscr, ...) {
  UseMethod("is.stratified", dscr)
}



is.stratified.describr <- function(dscr, ...) length(dscr$by) == 1





dtableGrob <- function(dscr, col_widths_tracker = NULL, padding_fct = 1.1) {

  # create preferred-column-width tracker
  create_col_widths_tracker  <- function(dscr) { # counter!

    col_names      <- create_colnames(dscr)
    max_col_widths <- list()
    for (i in 1:length(col_names)) {
      max_col_widths[[col_names[i]]] <- unit(0, "in")
      if (length(grep("__separator", col_names[i])) == 1) {
        max_col_widths[[col_names[i]]] <- dscr$theme_new$colwidths$seperators
      }
    }

    function(width, colname, return = FALSE) {
      if (return) {
        return(max_col_widths)
      }

      if (colname %in% names(max_col_widths)) {
        max_col_widths[[colname]] <<- convertWidth(
          max(
            max_col_widths[[colname]],
            convertWidth(width * padding_fct, "in")
          ),
        "in")
        return(NULL)
      } else {
        stop(sprintf("colname '%s' unknown", colname))
      }
    }

  }

  dscr$col_widths_tracker <- create_col_widths_tracker(dscr)

  theme <- dscr$theme_new

  # header and header separator
  gt <- headerGrob(dscr)

  gt <- rbind(
    gt,
    element_table_grob(theme$header$style$separator_bottom, widths = gt$widths)
  )

  # variable rows and separators
  for (varname in names(dscr$core)) {

    gt <- rbind(gt, variableGrob(dscr, varname))

    if (which(varname == names(dscr$core)) < length(names(dscr$core))) { # only if not last row
      gt <- rbind(
        gt,
        element_table_grob(theme$body$style$separator_variables, widths = gt$widths)
      )
    }

  }

  # footer
  footer <- bottomGrob(dscr, gt)
  gt <- gtable_add_rows(gt, heights = convertUnit(grobHeight(footer), "in"))
  gt <- gtable_add_grob(gt, footer, nrow(gt), 1, nrow(gt), ncol(gt), name = "footer")

  attr(gt, "describr")  <- dscr
  class(gt) <- c("describr_gtable", class(gt))

  return(gt)

}




optimize_columnwidths <- function(dscr_gtable) {

  dscr <- attr(dscr_gtable, "describr")

  col_names <- colnames(dscr_gtable)

  maxwidth <- convertUnit(dscr$maxwidth, "in", valueOnly = TRUE)

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
        l1_penalty_weights[i] <- 100
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

    lambda   <- 0.1

    MIPModel() %>%
      add_variable(
        col_widths[i], i = 1:length(col_names),
        lb = 0,
        ub = maxwidth
      ) %>%
      add_constraint(
        col_widths[i] <= required_colwidths_in[i],
        i = 1:length(col_names)
      ) %>%
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
          m <- add_constraint(m, col_widths[separator_inds[i]] >= required_colwidths_in[separator_inds[i]] / 10)
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

  }

  return(dtableGrob(dscr))

}





create_colnames <- function(dscr) {

  theme <- dscr$theme_new

  colnames <- "__variables__"

  colnames <- c(colnames, "__descriptors__separator", "__descriptors__")

  # total column
  if (!(is.stratified.describr(dscr) & !dscr$totals)) { # total column always except opt out

    colnames <- c(colnames, "__total__separator", "__total__")

  }

  # levels columns
  if (is.stratified(dscr)) {

    lvls <- levels(dscr$df[[dscr$by]])

    for (i in 1:length(lvls)) {

      colnames <- c(colnames,
        sprintf("__level__%s__separator", lvls[i]),
        sprintf("__level__%s", lvls[i])
      )

    }

  }

  # pvalue columns
  if (dscr$pvalues & is.stratified(dscr)) {

    colnames <- c(colnames, "__pvalues__separator", "__pvalues__", "__pvalues_idx__")

  }

  return(colnames)

}
