# describr - publication quality descriptive tables with R

Tired of fiddling with boring 'univariat descriptive tables' that everybody (should) want but nobody wants to do - `describr` (hopefully) resolves this issue and makes generating nice-looking descriptive tables (almost) fun!

## Installation

Make sure that the `devtools` package is installed and run

    devtools::install_github('kkmann/describr')
    
to install the newest release. 

## Intro

There is currently zero documentation (which limits practical usability ;) ) but if you want to play around:

    library(describr)
    library(tidyverse)
    library(gridExtra)
    library(gtable)
    library(grid)

    # create dataset with dummy factor

    iris %>%
      mutate(
        `I am a factor with very long variable-name` = factor(
          sample(1:4, nrow(iris), replace = TRUE),
          labels = c("hi", "ho", "xXx", "I am a level with long level-name")
        )
      ) ->
    df_iris_test



    # create descriptive table object

    df_iris_test %>%
      dtable(
        by = Species,
        pvalue = TRUE,
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
      ) ->
    dt




    # optimize to normal page width and split by normal page length

    dt %>%
      dtableGrob() %>%
      optimize_columnwidths() ->
    dt_grob



    # draw entire table

    pdf(
      "iris_test_onepage.pdf",
      width  = 8.27 - 2.5,
      height = convertUnit(sum(dt_grob$heights), "in", valueOnly = TRUE)
    )
    grid.draw(dt_grob)
    dev.off()




    # print individual table pieces to pdf file

    dt_grob %>%
    split_pages(maxheight = unit(11.69 - 4, "in")) ->
      dt_grob_list

    for (i in 1:length(dt_grob_list)) {

      pdf(
        sprintf("iris_test_page_%i.pdf", i),
        width  = 8.27 - 2.5,
        height = convertUnit(sum(dt_grob_list[[i]]$heights), "in", valueOnly = TRUE)
      ) # both in inches ( - normal margins)
      grid.draw(dt_grob_list[[i]])
      dev.off()

    }



