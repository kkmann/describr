dscr_n_perc <- TextDescriptor(
  function(data_sub, data_cpl) sprintf("%i (%.1f%%)", length(data_sub), 100*length(data_sub)/length(data_cpl)),
  function(data) c("n (%)")
)

dscr_freq <- TextDescriptor(
  function(data_sub, data_cpl) {
    tbl    <- data_sub %>% table(useNA = "always")
    counts <- as.numeric(tbl)
    freqs  <- counts / sum(counts)
    res    <- c("",
      sapply(1:length(counts), function(i) sprintf("%i (%.1f%%)", counts[i], 100*freqs[i]))
    )
    return(res)
  },
  function(data_cpl) {
    tbl    <- data_cpl %>% table(useNA = "always")
    names  <- c("n (%)", names(tbl))
    return(names)
  }
)

dscr_mean_sd <- TextDescriptor(
  function(data_sub, data_cpl) sprintf("%.2f (%.2f)", mean(data_sub), sd(data_sub)),
  function(data) c("mean (sd)")
)

dscr_histogram <- PlotDescriptor(
  ggplot(aes(x)) + geom_histogram(bins = 10) +
    scale_x_continuous(breaks = range(data_complete)) +
    coord_cartesian(xlim = range(data_complete), expand = FALSE),
  function(data) "histogram"
)

dscr_boxplot <- PlotDescriptor(
  ggplot(aes(x = 1, y = x)) + stat_boxplot() +
    scale_y_continuous(breaks = range(data_complete), limits = range(data_complete)) +
    coord_cartesian(ylim = range(data_complete), expand = FALSE) + coord_flip(),
  function(data) "boxplot"
)
