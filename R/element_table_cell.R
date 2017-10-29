element_table_cell_text <- function(size, lineheight = 1.2*size) {
  structure(list(
      size       = size,
      lineheight = lineheight
    ),
    class = c("element_cell_text", "element_table_cell", "element_table")
  )
}

element_table_horizontal_seperator <- function(height, linesize) {
  structure(list(
    height = height,
    linesize = linesize
  ),
  class = c("element_table_horizontal_seperator", "element_table_seperator", "element_table")
  )
}

element_table_vertical_seperator <- function(width, linesize) {
  structure(list(
    width = width,
    linesize = linesize
  ),
  class = c("element_table_vertical_seperator", "element_table_seperator", "element_table")
  )
}

element_table_cell_plot <- function(width, height) {
  structure(list(
    height = height,
    width  = width
  ),
  class = c("element_table_cell_plot", "element_table_cell", "element_table")
  )
}
