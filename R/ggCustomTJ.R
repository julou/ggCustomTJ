.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ggCustomTJ <- list(
    ggCustomTJ.path = "~/Documents/Biozentrum/Tools/ggCustomTJ/",
    ggCustomTJ.install.args = "",
    ggCustomTJ.name = "Thomas Julou",
    ggCustomTJ.desc.author = '"Thomas Julou <thomas.julou@normalesup.org> [aut, cre]"',
    # ggCustomTJ.desc.license = "What license is it under?",
    ggCustomTJ.desc.suggests = NULL,
    ggCustomTJ.desc = list()
  )
  toset <- !(names(op.ggCustomTJ) %in% names(op))
  if(any(toset)) options(op.ggCustomTJ[toset])
  
  ggplot2::theme_set(ggplot2::theme_bw()) # override default theme
  
  invisible()
}

# qualitative color palette from "Color Universal Design" by Okabe and Ito, http://jfly.iam.u-tokyo.ac.jp/color/
qual_cols <- c("#0072B2", "#D55E00", "#009E73", "#F0E442", "#56B4E9", "#E69F00", "#CC79A7")
scale_colour_discrete <- function(...) ggplot2::scale_colour_manual(..., values=qual_cols, na.value='gray50')
scale_fill_discrete <- function(...) ggplot2::scale_fill_manual(..., values=qual_cols, na.value='gray50')
# to use the default ggplot2 discrete colour scale, use: + ggplot2::scale_colour_discrete()
#  or ggplot2::scale_colour_hue()
scale_colour_periodic <- function(..., .n=4) 
  ggplot2::scale_colour_manual(..., values = rep(qual_cols[1:.n], 1e4), na.value='gray50')
scale_fill_periodic <- function(..., .n=4) 
  ggplot2::scale_fill_manual(..., values = rep(qual_cols[1:.n], 1e4), na.value='gray50')


brewer_cols <- c(RColorBrewer::brewer.pal(4, 'Set1'), 'gray42')
brewer_all_cols <- RColorBrewer::brewer.pal(9, 'Set1')
brewer_paired_cols <- RColorBrewer::brewer.pal(12, "Paired")
scale_colour_periodic_brewer <- function(..., .n=4) 
  ggplot2::scale_colour_manual(..., values = rep(c(brewer_all_cols[1:.n], 'gray42'), 1e4), na.value='gray25')
scale_fill_periodic_brewer <- function(..., .n=4) 
  ggplot2::scale_fill_manual(..., values = rep(c(brewer_all_cols[1:.n], 'gray42'), 1e4), na.value='gray25')
scale_shape_periodic <- function(...) 
  ggplot2::scale_shape_manual(..., values = rep(15:18, 5))

formatter_sec_to_h <- function(.x) .x/3600 #%>% format(digits=2)
scale_x_hours <- function(.dh=6, ...) {
  .f_breaks <- function(.lims) 
    seq(.lims[1] %/% (.dh*3600) *.dh*3600, (.lims[2] %/% (.dh*3600) + 1) *.dh*3600, .dh*3600)
  .call <- as.list(match.call()) # avoid pipe
  if("name" %in% names(.call)) {
    ggplot2::scale_x_continuous(..., labels=formatter_sec_to_h, breaks=.f_breaks)
  } else {
    ggplot2::scale_x_continuous(..., name="time (h)", labels=formatter_sec_to_h, breaks=.f_breaks)
  }
}
scale_y_hours <- function(.dh=6, ...) {
  .f_breaks <- function(.lims) 
    seq(.lims[1] %/% (.dh*3600) *.dh*3600, (.lims[2] %/% (.dh*3600) + 1) *.dh*3600, .dh*3600)
  .call <- as.list(match.call()) # avoid pipe
  if("name" %in% names(.call)) {
    ggplot2::scale_y_continuous(..., labels=formatter_sec_to_h, breaks=.f_breaks)
  } else {
    ggplot2::scale_y_continuous(..., name="time (h)", labels=formatter_sec_to_h, breaks=.f_breaks)
  }
}
sec_to_h_trans <- function() scales::trans_new("sec_to_h", function(.x) .x/3600, function(.x) .x*3600)

# utility functions to slice and sample dataframes by groups
slice_groups <- function(.data, ...) {
  .groups <- groups(.data)
  .data %>% nest() %>% slice(...) %>% unnest() %>% group_by(!!!.groups)
}

sample_n_groups <- function(.data, ...) {
  .groups <- groups(.data)
  .data %>% nest() %>% sample_n(...) %>% unnest() %>% group_by(!!!.groups)
}

