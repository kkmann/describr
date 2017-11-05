library(describr)
library(tidyverse)

# create dataset with dummy factor
iris %>%
  mutate(
    `I am a facto with very long variable-name` = factor(
      sample(1:4, nrow(iris), replace = TRUE),
      labels = c("hi", "ho", "xXx", "I am a level with long level-name")
    )
  ) ->
df_iris_test



# plot immediately
df_iris_test %>%
  describr(by = Species, pvalues = TRUE,
    theme_new = theme_default(text_size = 9) # pt
  ) %>%
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
  ) %>%
  to_pdf(name = "iris_test_page")
