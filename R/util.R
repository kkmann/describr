justify <- function(x, hjust="center", vjust="center"){
  w <- sum(x$widths)
  h <- sum(x$heights)
  xj <- switch(hjust,
               center = 0.5,
               left = 0.5*w,
               right=grid::unit(1,"npc") - 0.5*w)
  yj <- switch(vjust,
               center = 0.5,
               bottom = 0.5*h,
               top=grid::unit(1,"npc") - 0.5*h)
  x$vp <- grid::viewport(x=xj, y=yj)
  return(x)
}
