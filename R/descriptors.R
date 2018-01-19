# Group counts and percent =====================================================
dscr_n_perc <- function(
  format = "%3i (%5.1f%%)",
  pvalues = list(
    dscr_one_sample_chisq()
  )
) {

  structure(
    list(
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_n_perc", "TextDescriptor", "Descriptor")
  )

}


get_label.dscr_n_perc <- function(td, variable_all) c("n (%)")


get_description.dscr_n_perc <- function(td, variable_group, variable_all) {

  sprintf(td$format,
    length(variable_group),
    100*length(variable_group)/length(variable_all)
  )

}



# Group counts and percent =====================================================
dscr_n_na <- function(
  format = "%3i (%5.1f%%)",
  pvalues = list(
    dscr_no_test()
  )
) {

  structure(
    list(
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_n_na", "TextDescriptor", "Descriptor")
  )

}


get_label.dscr_n_na <- function(td, variable_all) c("#na (%)")


get_description.dscr_n_na <- function(td, variable_group, variable_all) {

  sprintf(td$format,
    sum(is.na(variable_group)),
    100*sum(is.na(variable_group))/length(variable_all)
  )

}



# Tabulate factor ==============================================================
dscr_freq <- function(
  format = "%3i (%5.1f%%)",
  pvalues = list(dscr_cross_table_chisq())
) {

  structure(
    list(
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_freq", "TextDescriptor", "Descriptor")
  )

}



get_label.dscr_freq <- function(td, variable_all) {

  tbl    <- variable_all %>% table(useNA = "always")
  names  <- c("n (%)", names(tbl))

  return(names)

}

get_description.dscr_freq <- function(td, variable_group, variable_all) {

  tbl    <- variable_group %>% table(useNA = "always")
  counts <- as.numeric(tbl)
  freqs  <- counts / sum(counts)
  res    <- c(
    "", # first blank column is to align with "n (row %)" label
    sapply(1:length(counts), function(i) sprintf(td$format, counts[i], 100*freqs[i]))
  )

  return(res)

}






# Mean =========================================================================
dscr_mean <- function(
  label   = "Mean",
  format  = "%.2f",
  pvalues = list(dscr_Welch_ANOVA())
) {

  structure(
    list(
      label   = label,
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_mean", "TextDescriptor", "Descriptor")
  )

}


get_label.dscr_mean <- function(td, variable_all) td$label


get_description.dscr_mean <- function(td, variable_group, variable_all) {

  sprintf(td$format, mean(variable_group, na.rm = TRUE))

}



# Standard deviation ===========================================================
dscr_sd <- function(
  label   = "Standard deviation",
  format  = "%.2f",
  pvalues = list(dscr_levene())
) {

  structure(
    list(
      label   = label,
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_sd", "TextDescriptor", "Descriptor")
  )

}


get_label.dscr_sd <- function(td, variable_all) td$label


get_description.dscr_sd <- function(td, variable_group, variable_all) {

  sprintf(td$format, sd(variable_group, na.rm = TRUE))

}



# Mean (SD) ====================================================================
dscr_mean_sd <- function(
  label   = "Mean (SD)",
  format  = "%.2f (%.2f)",
  pvalues = list(dscr_Welch_ANOVA())
) {

  structure(
    list(
      label   = label,
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_mean_sd", "TextDescriptor", "Descriptor")
  )

}


get_label.dscr_mean_sd <- function(td, variable_all) td$label


get_description.dscr_mean_sd <- function(td, variable_group, variable_all) {

  sprintf(
    td$format,
    mean(variable_group, na.rm = TRUE),
    sd(variable_group, na.rm = TRUE)
  )

}



# Median (IQR) ====================================================================
dscr_median_q1_q3 <- function(
  label   = "Median (Q1, Q3)",
  format  = "%.2f (%.2f, %.2f)",
  pvalues = list(dscr_Kruskal())
) {

  structure(
    list(
      label   = label,
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_median_q1_q3", "TextDescriptor", "Descriptor")
  )

}


get_label.dscr_median_q1_q3 <- function(td, variable_all) td$label


get_description.dscr_median_q1_q3 <- function(td, variable_group, variable_all) {

  sprintf(
    td$format,
    median(variable_group, na.rm = TRUE),
    quantile(variable_group, probs = .25, na.rm = TRUE),
    quantile(variable_group, probs = .75, na.rm = TRUE)
  )

}




# Min/Max ======================================================================
dscr_min_max <- function(
  label   = "[min, max]",
  format  = "[%.2f, %.2f]",
  pvalues = list(dscr_no_test())
) {

  structure(
    list(
      label   = label,
      format  = format,
      pvalues = pvalues
    ),
    class = c("dscr_min_max", "TextDescriptor", "Descriptor")
  )

}


get_label.dscr_min_max <- function(td, variable_all) td$label

get_description.dscr_min_max <- function(td, variable_group, variable_all) {

  sprintf(td$format, min(variable_group, na.rm = TRUE), max(variable_group, na.rm = TRUE))

}




# Histogram ====================================================================
dscr_histogram <- function(
  label = "Histogram",
  nbins  = 10,
  pvalues   = list(dscr_anderson_darling()),
  minheight = unit(1.5, "cm"),
  minwidth  = unit(2, "cm")
) {

  structure(
    list(
      label = label,
      nbins = nbins,
      breaks = NA,
      pvalues = pvalues,
      minheight = minheight,
      minwidth = minwidth,
      xlim = NA
    ),
    class = c("dscr_histogram", "PlotDescriptor", "Descriptor")
  )

}

setup.dscr_histogram <- function(d, variable, group, ...) {

  # save global range
  d$xlim   <- range(variable, na.rm = TRUE)
  rng <- range(variable, na.rm = TRUE)
  delta <- rng[2] - rng[1]
  d$breaks <- seq(rng[1] + .025*delta, rng[2] + .025*delta, length.out = d$nbins)
  return(d)

}


get_call.dscr_histogram <- function(pd, ...) {

  nbins <- pd$nbins

  if (any(is.na(pd$breaks)) | any(is.na(pd$xlim))) stop("call setup() first")

  substitute(
    ggplot(aes(variable)) +
      geom_histogram(
        aes(y = ..count../sum(..count..)),
        breaks = tmp
      ),
    list(tmp = pd$breaks)
  )
}




# Boxplot ====================================================================
dscr_boxplot <- function(
  label = "Boxplot",
  pvalues = list(dscr_anderson_darling()),
  minheight = unit(1.5, "cm"),
  minwidth = unit(2, "cm")
) {

  structure(
    list(
      label   = label,
      pvalues = pvalues,
      minheight = minheight,
      minwidth = minwidth
    ),
    class = c("dscr_boxplot", "PlotDescriptor", "Descriptor")
  )

}

get_call.dscr_boxplot <- function(pd, ...) {

  return(substitute(
    ggplot(aes(x = 1, y = variable)) + stat_boxplot() + coord_flip()
  ))

}
