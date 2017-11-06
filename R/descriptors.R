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
  names  <- c("n (row %)", names(tbl))

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
dscr_histogram <- PlotDescriptor(

  ggplot(aes(variable)) + geom_histogram(bins = 10),

  function(data) "Histogram",

  pvalue = structure(
    function(data, group) {
      tryCatch(
        {
          tmp <- kSamples::ad.test(data ~ group, data = data_frame(data, group), method = "asymptotic")
          return(tmp$ad[1, " asympt. P-value"])
        },
        error = function(e) NA
      )
    },
    label = "asymptotic Anderson-Darling k-sample test (Version 1)"
  )

)





dscr_boxplot <- PlotDescriptor(

  ggplot(aes(x = 1, y = variable)) + stat_boxplot() + coord_flip(),

  function(data) "Boxplot",

  pvalue = structure(
    function(data, group) {
      tryCatch(
        {
          tmp <- kSamples::ad.test(data ~ group, data = data_frame(data, group), method = "asymptotic")
          return(tmp$ad[1, " asympt. P-value"])
        },
        error = function(e) NA
      )
    },
    label = "asymptotic Anderson-Darling k-sample test (Version 1)"
  )

)

