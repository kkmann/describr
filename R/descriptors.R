dscr_n_perc <- TextDescriptor(
  text   = function(data_sub, data_cpl) sprintf("%i (%.1f%%)", length(data_sub), 100*length(data_sub)/length(data_cpl)),
  label  = function(data) c("n (%)"),
  pvalue = structure(function(data, group) 0.06, label = "dummy")
)

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
    names  <- c("n (%)", names(tbl))
    return(names)
  },
  pvalue = structure(function(data, group) 0.06, label = "dummy")
)

dscr_mean_sd <- TextDescriptor(
  function(data_sub, data_cpl) sprintf("%.2f (%.2f)", mean(data_sub), sd(data_sub)),
  function(data) c("mean (sd)"),
  pvalue = structure(function(data, group) 0.06, label = "dummy")
)

dscr_histogram <- PlotDescriptor(
  ggplot(aes(variable)) + geom_histogram(bins = 10),
  function(data) "histogram",
  pvalue = structure(function(data, group) 0.06, label = "dummy")
)

dscr_boxplot <- PlotDescriptor(
  ggplot(aes(x = 1, y = variable)) + stat_boxplot() + coord_flip(),
  function(data) "boxplot",
  pvalue = structure(function(data, group) 0.06, label = "dummy")
)
