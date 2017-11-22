# library(ggmap)
library(dplyr)

ggpointlabel <- function(.pl, .label_name) {
# ggpointlabel draws a ggplot, ask the user to click on a point and draw a label
# showing the value of column 'label_name'

# Minimal example:
# library(ggplot2)
# my_df <- data.frame(x=runif(1e2, -40, -20), y=rnorm(1e2, 10), t=rep(letters, length.out=100), id=1:100)
# pl <- qplot(x, y , data=my_df)
# ggpointlabel(pl, 't')

  # find click coords
  print(.pl)
  .coords <- gglocator()

  # compute the matrix distance of all points, and the clicked point
  .df <- .pl$data # get the data from the plot
  .x <- .pl$mapping$x
  .y <- .pl$mapping$y
  .dists <- .df %>%
    select_(.x, .y) %>%
    rbind(.coords) %>%
    dist %>% as.matrix
  # keep the relevant distance vector (dists to the clicked point) and find its min
  .l <- dim(.dists)[1]
  .dists <- .dists[1:.l-1, .l]
  .row <- which.min(.dists)

  # update the plot
  .pl +
    geom_textbox(aes_string(label=.label_name), data=.df[.row,])
}


# create a new geom for boxed labels (modified from http://stackoverflow.com/a/9814794/576684)
library(grid)
library(proto)

btextGrob <- function (label,x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
                       default.units = "npc", name = NULL, gp = gpar(), vp = NULL, expand_w, expand_h, box_gp = gpar()) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(label = label, x = x, y = y, just = just, hjust = hjust,
       vjust = vjust, rot = rot, check.overlap = check.overlap,
       name = name, gp = gp, vp = vp, cl = "text")
  tg <- textGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                 vjust = vjust, rot = rot, check.overlap = check.overlap)
  w <- unit(rep(1, length(label)), "strwidth", as.list(label))
  h <- unit(rep(1, length(label)), "strheight", as.list(label))
  rg <- rectGrob(x=x, y=y, width=expand_w*w, height=expand_h*h,
                 gp=box_gp)

  gTree(children=gList(rg, tg), vp=vp, gp=gp, name=name)
}

GeomTextbox <- proto(ggplot2:::GeomText, {
  objname <- "textbox"

  draw <- function(., data, scales, coordinates, ..., parse = FALSE, na.rm = FALSE,
                   expand_w = 1.2, expand_h = 2, bgcol = "grey50", bgfill = "white", bgalpha = 1) {
    data <- remove_missing(data, na.rm,
                           c("x", "y", "label"), name = "geom_textbox")
    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    with(coord_transform(coordinates, data, scales),
         btextGrob(lab, x, y, default.units="native",
                   hjust=hjust, vjust=vjust, rot=angle,
                   gp = gpar(col = alpha(colour, alpha), fontsize = size * .pt,
                             fontfamily = family, fontface = fontface, lineheight = lineheight),
                   box_gp = gpar(fill = bgfill, alpha = bgalpha, col = bgcol),
                   expand_w = expand_w, expand_h = expand_h)
    )
  }

})

geom_textbox <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          parse = FALSE,  ...) {
  GeomTextbox$new(mapping = mapping, data = data, stat = stat,position = position,
                  parse = parse, ...)
}
