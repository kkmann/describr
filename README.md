# describr - publication quality descriptive tables with R

[![Travis-CI Build Status](https://travis-ci.org/kkmann/describr.svg?branch=master)](https://travis-ci.org/kkmann/describr)



## Why describr?

Tired of fiddling with boring 'univariat descriptive tables' that most people 
(should) have in their papers but nobody wants to do - 
describr (hopefully) resolves this by making generating nice-looking 
descriptive tables (almost) fun!

The main concept is to use ggplot2-like syntax for defining a table and to 
create the table as a graphics object.
The latter allows graphs for describing variables alongside simple statistics 
and (eventually) finer control over the layout of a table.
This is necessary as the primary objective is to create tables for pdf reports 
which need to take spaceconstraints into account.

In contrast to other packages providing descriptive tables functionality in
R, describr is less flexible in the output format (always graphics).
Yet this opens up new possibilities (graphs in tables) and finer layout
control which would not be possible for a Markdown/pandoc approach.

Another core principle is extensibility. 
Users should be able to add new descriptors (stats or graphs) in a fairly
easy way.

Other packages to look at:

- [reporttools](https://cran.r-project.org/web/packages/reporttools/index.html)

- [Hmisc](https://cran.r-project.org/web/packages/Hmisc/index.html)

- [tangram](https://github.com/spgarbet/tangram)


Please note that this work is very much in an alpha stage. 
The API and core functionality might still change and the code-base is extremely untidy.
Please feel free to open issues for bugs and further development ideas.



## Installation

Make sure that the `devtools` package is installed and run

    devtools::install_github('kkmann/describr')
    
to install the most recent version (might be unstable). 



## Documentation

Please refer to [Getting Started](https://kkmann.github.io/describr/articles/describr.html) for 
a brief introduction and the [Function Reference](https://kkmann.github.io/describr/reference/index.html)
for details.



## ToDo

1. Rework the theming system
2. Rework graphics code to be more robust/readable 
3. Document interface for custom descriptors
4. Better column width optimization
5. Bugs, bugs, bugs...
