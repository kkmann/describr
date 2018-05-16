# describr - publication quality descriptive tables with R

[![Travis-CI Build Status](https://travis-ci.org/kkmann/describr.svg?branch=master)](https://travis-ci.org/kkmann/describr)

Tired of fiddling with boring 'univariat descriptive tables' that everybody (should) want but nobody wants to do - `describr` (hopefully) resolves this issue and makes generating nice-looking descriptive tables (almost) fun!
The main concept is to use ggplot2-like syntax for defining a table and to create the table as a graphics object.
The latter allows graphs for describing variables alongside simple statistics and (eventually) finer control
over the layout of a table.
This is necessary as the primary objective is to create tables for pdf reports which need to take space 
constraints into account.
Please note that this work is very much in an alpha stage. 
The API and core functionality might still change and the code-base is extremely untidy.



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
