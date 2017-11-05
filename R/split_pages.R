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
