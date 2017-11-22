gglocator <- function (n = 1, message = FALSE, xexpand = c(0.05, 0), yexpand = c(0.05,  0))
# from ggmap_2.3 package  
{
  warning("gglocator() is not accurate in RStudio GUI...")
  if (n > 1) {
    df <- NULL
    for (k in 1:n) {
      df <- rbind(df, gglocator(message = message, xexpand = xexpand, 
                                yexpand = yexpand))
    }
    return(df)
  }
  x <- grid.ls(print = message)$name
  x <- x[grep("panel.", x)][1]
  seekViewport(x)
  loc <- as.numeric(grid.locator("npc"))
  object <- last_plot()
  xrng <- with(object, range(data[, deparse(mapping$x)]))
  yrng <- with(object, range(data[, deparse(mapping$y)]))
  xrng <- scales::expand_range(range = xrng, mul = xexpand[1], 
                               add = xexpand[2])
  yrng <- scales::expand_range(range = yrng, mul = yexpand[1], 
                               add = yexpand[2])
  point <- data.frame(xrng[1] + loc[1] * diff(xrng), yrng[1] + 
                        loc[2] * diff(yrng))
  names(point) <- with(object, c(deparse(mapping$x), deparse(mapping$y)))
  point
}
