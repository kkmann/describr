library(describr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)

iris %>%
  mutate(
    ichbinsvenjasdaemlicherzufallsfaktor = factor(
      sample(1:4, nrow(iris), replace = TRUE),
      labels = c("hi", "ho", "xXx", "ich bin kein level")
    )
  ) %>%
dtable(by = Species, pvalue = TRUE, theme_new = theme_debug(10)) %>%
  describe_if(
    is.numeric,
    with = list(dscr_mean_sd, dscr_median_iqr, dscr_range)
  ) %>%
  describe_if(
    is.factor,
    with = dscr_freq
  ) %>%
  describe(
    with = list(dscr_histogram, dscr_boxplot, dscr_violin, dscr_qqnorm),
    Sepal.Width
  ) ->
dt

# dt$theme_new$colwidths$variables <- unit(1, "in")

g <- dtableGrob(dt)

g2 <- optimize_columnwidths(g)
grid.newpage()
grid.draw(g2)

# dt2 <- attr(g2, "describr")
#
# gg <- descriptorGrob(dscr_boxplot, dt2, "Sepal.Width")
# grid.newpage()
# grid.draw(gg)

h <- convertHeight(grobHeight(g2), "in", valueOnly = TRUE)
w <- convertWidth(grobWidth(g2), "in", valueOnly = TRUE)

pdf("test.pdf", 1.1*w, h)
grid.draw(g2)
dev.off()
