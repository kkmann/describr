#' Create a description object for a data frame
#'
#' @param df a data frame.
#' @param by unquoted name of optional stratifying factor
#' @param theme_new theme to use
#' @param pvalus Flag indicating whether p-values for homogeneity null hypothesis
#'   should be printed if available.
#' @param total Flag indicating whether all descriptors should also be given
#'   for the total sample.
#' @export
describr <- function(
  df,
  by        = NULL,
  theme_new = theme_default_tmp(),
  pvalues   = FALSE,
  totals    = TRUE
) {

  # capture stratum using non-standard evaluation
  by = as.character(substitute(by))

  # ensure stratum fits
  if (length(by) > 1)
    stop("currently only single stratum supported")
  if (!(by %in% names(df)))
    stop(sprintf("stratum 'by = %s' not found"))
  if (!(is.factor(df[[by]])))
    stop(sprintf("'by' must be factor"))

  # create list of descriptors for stratum
  group_descriptors <- list(dscr_n_perc)

  # create list of to hold variabe descrptors
  core <- list()

  # collect all items and add class label
  res <- structure(
    list(
      df                = df,
      by                = by,
      core              = core,
      group_descriptors = group_descriptors,
      pvalues           = pvalues,
      totals            = totals,
      theme_new         = theme_new
    ),
    class = c("describr", "list")
  )

  return(res)

}





#' Is the describr object stratiied (non-NULL by object)
is.stratified <- function(dscr, ...) {
  UseMethod("is.stratified", dscr)
}

is.stratified.describr <- function(dscr, ...) length(dscr$by) == 1






#' Print a describr object
#'
#' By redirecting print to constructing a grob and calling grid. draw
#' immediately, tables can be plotted without additional uder interaction.
#' @export
print.describr <- function(x, maxwidth = unit(6, "in"), ...) {

  describrGrob(x) %>%
    optimize_columnwidths(maxwidth = maxwidth) %>%
    grid.draw()

}

print.describrGtable <- function(x, maxwidth = unit(6, "in"), ...) {

  grid.draw(x)

}





to_pdf <- function(x, name, ...) {
  UseMethod("to_pdf", x)
}

to_pdf.describr <- function(x, name, maxwidth = 6.5, maxheight = Inf, ...) {

  describrGrob(x) %>%
    optimize_columnwidths(maxwidth = unit(maxwidth, "in")) %>%
    split_pages(maxheight = unit(maxheight, "in")) ->
  grob_list

  for (i in 1:length(grob_list)) {

    pdf(
      sprintf("%s_%i.pdf", name, i),
      width  = convertUnit(sum(grob_list[[i]]$widths), "in", valueOnly = TRUE),
      height = convertUnit(sum(grob_list[[i]]$heights), "in", valueOnly = TRUE)
    )

    grid.draw(grob_list[[i]])

    dev.off()

  }

}




to_pdf.describrGtable <- function(x, name, maxwidth = 6.5, maxheight = Inf, ...) {

  width <- convertUnit(x, "in", valueOnly = TRUE)

  if (width <= maxwidth) {
    x %>%
      split_pages(maxheight = unit(maxheight, "in")) ->
      grob_list
  } else {
    x %>%
      optimize_columnwidths(maxwidth = unit(maxwidth, "in")) %>%
      split_pages(maxheight = unit(maxheight, "in")) ->
      grob_list
  }

  for (i in 1:length(grob_list)) {

    pdf(
      sprintf("%s_%i.pdf", name, i),
      width  = convertUnit(sum(grob_list[[i]]$widths), "in", valueOnly = TRUE),
      height = convertUnit(sum(grob_list[[i]]$heights), "in", valueOnly = TRUE)
    )

    grid.draw(grob_list[[i]])

    dev.off()

  }

}




#' Turn a 'describr' object in a plottable grob
describrGrob <- function(dscr, widths_padding_fct = 1.05) {

  if (!inherits(dscr, "describr"))
    stop("dscr must be of class 'describr'")

  # create preferred-column-width tracker which can be used by table
  # elements to report their preferred widths
  # can be used to determine optimal column widths in a second pass
  dscr$col_widths_tracker <- create_col_widths_tracker(dscr, widths_padding_fct)

  # this counter tracks the used p-values to be noted in the footer
  dscr$register_pvalue <- register_pvalue()

  # abbrevite the theme
  theme <- dscr$theme_new

  # start with header and separator
  gt <- headerGrob(dscr)
  gt <- rbind(gt, element_table_grob(
    theme$header$style$separator_bottom, widths = gt$widths)
  )
  rownames(gt) <- sapply(1:nrow(gt), function(i) sprintf("__header__%i", i))

  # variable rows and separators
  for (varname in names(dscr$core)) {

    vargrob <- variableGrob(dscr, varname)
    rownames(vargrob) <- sapply(1:nrow(vargrob), function(i) sprintf("__variable__%s_%i", varname, i))
    gt <- rbind(gt, vargrob)

    if (which(varname == names(dscr$core)) < length(names(dscr$core))) { # only if not last row
      gt <- rbind(gt,
        element_table_grob(theme$body$style$separator_variables, widths = gt$widths)
      )
    }
    rownames(gt) <- c(rownames(gt), sprintf("__variable__%s_separator", varname))

  }

  # footer
  footer <- bottomGrob(dscr, gt)
  rownames(footer) <- sapply(1:nrow(footer), function(i) sprintf("__footer__%i", i))
  gt <- gtable_add_rows(gt, heights = convertUnit(grobHeight(footer), "in"))
  gt <- gtable_add_grob(gt, footer, nrow(gt), 1, nrow(gt), ncol(gt), name = "footer")


  # store the initial describer object as attribute
  attr(gt, "describr")  <- dscr
  class(gt) <- c("describrGtable", class(gt))

  return(gt)

}






optimize_columnwidths <- function(dscr_gtable, maxwidth  = unit(6, "in"), ...) {
  UseMethod("optimize_columnwidths", dscr_gtable)
}

optimize_columnwidths.describr <- function(dscr, maxwidth  = unit(6, "in"), ...) {
  dscr %>%
    describrGrob() %>%
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

  adjusted_dt <- describrGrob(dscr)

  return(adjusted_dt)

}



split_pages <- function(dscr_gtable, maxheight = unit(11.69 - 3, "in")) {

  # very crude

  dscr <- attr(dscr_gtable, "describr")

  pages          <- list()

  heights_in     <- convertUnit(dscr_gtable$heights, "in", valueOnly = TRUE)

  gt_tmp         <- dscr_gtable[grep("__header__", rownames(dscr_gtable)), ]
  current_height <- sum(heights_in[grep("__header__", rownames(dscr_gtable))])

  varnames       <- names(dscr$core)

  for (varname in varnames) {

    var_inds <- grep(sprintf("__variable__%s_", varname), rownames(dscr_gtable))

    new_height <- current_height + sum(heights_in[var_inds])

    if (new_height <= convertUnit(maxheight, "in", valueOnly = TRUE)) {

      gt_tmp <- rbind(gt_tmp, dscr_gtable[var_inds, ])

      current_height <- new_height

    } else {

      pages <- c(pages, list(gt_tmp))

      gt_tmp         <- dscr_gtable[grep("__header__", rownames(dscr_gtable)), ]

      gt_tmp         <- rbind(gt_tmp, dscr_gtable[var_inds, ])

      current_height <- sum(heights_in[grep("__header__", rownames(dscr_gtable))]) + sum(heights_in[var_inds])

    }

  }

  pages <- c(pages, list(gt_tmp))

  return(pages)

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





create_col_widths_tracker  <- function(dscr, widths_padding_fct) { # counter!

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
          convertWidth(width * widths_padding_fct, "in")
        ),
        "in")
      return(NULL)
    } else {
      stop(sprintf("colname '%s' unknown", colname))
    }
  }

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
