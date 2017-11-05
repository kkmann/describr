dscr_n_perc <- TextDescriptor(

  text   = function(data_sub, data_cpl) sprintf("%i (%5.1f%%)", length(data_sub), 100*length(data_sub)/length(data_cpl)),

  label  = function(data) c("n (%)"),

  pvalue = structure(
    function(data, group) {
      chisq.test(group %>% table(), correct = FALSE)$p.value
    },
    label = "Chi-Square"
  )

)





# Tabulate factor ==============================================================
dscr_freq <- TextDescriptor(

  text = function(data_sub, data_cpl) {
    tbl    <- data_sub %>% table(useNA = "always")
    counts <- as.numeric(tbl)
    freqs  <- counts / sum(counts)
    res    <- c("",
      sapply(1:length(counts), function(i) sprintf("%i (%.1f%%)", counts[i], 100*freqs[i]))
    )
    return(res)
  },

  label = function(data_cpl) {
    tbl    <- data_cpl %>% table(useNA = "always")
    names  <- c("n (row %)", names(tbl))
    return(names)
  },

  pvalue = structure(
    function(data, group) {
      chisq.test(data_frame(data, group) %>% table(), correct = FALSE)$p.value
    },
    label = "Chi-Square"
  )

)


get_label.dscr_freq <- function(td, variable_group, variable_all) {

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
    sapply(1:length(counts), function(i) sprintf("%i (%.1f%%)", counts[i], 100*freqs[i]))
  )

  return(res)

}






# ========================







dscr_mean_sd <- TextDescriptor(

  text   = function(data_sub, data_cpl) sprintf("%.2f (%.2f)", mean(data_sub, na.rm = TRUE), sd(data_sub, na.rm = TRUE)),

  label  = function(data) c("Mean (SD)"),

  pvalue = structure(
    function(data, group) {
      tryCatch(
        oneway.test(data ~ group, data = data_frame(data, group), var.equal = FALSE)$p.value,
        error = function(e) NA
      )
    },
    label = "Welch-ANOVA"
  )

)



dscr_median_iqr <- TextDescriptor(

  text   = function(data_sub, data_cpl) {
    sprintf(
      "%.2f (%.2f)",
      median(data_sub, na.rm = TRUE),
      quantile(data_sub, c(.25, .75)) %*% c(-1, 1) %>% .[1, 1]
    )
  },

  label  = function(data) c("Median (IQR)"),

  pvalue = structure(
    function(data, group) {
      tryCatch(
        kruskal.test(data ~ group, data = data_frame(data, group))$p.value,
        error = function(e) NA
      )
    },
    label = "Kruskal-Wallis test"
  )

)



dscr_range <- TextDescriptor(

  text   = function(data_sub, data_cpl) {
    sprintf(
      "[%.2f, %.2f]",
      min(data_sub), max(data_sub)
    )
  },

  label  = function(data) c("[min, max]"),

  pvalue = structure(
    function(data, group) {
      NA
    },
    label = "no test for equality of ranges conducted"
  )

)







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




dscr_violin <- PlotDescriptor(

  ggplot(aes(x = 1, y = variable)) + geom_violin() + coord_flip(),

  function(data) "Violin-plot",

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



dscr_qqnorm <- PlotDescriptor(

  ggplot(aes(sample = variable)) +
    geom_qq(distribution = qnorm) +
    coord_flip(),

  function(data) "QQ-plot\n (Normal)",

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

