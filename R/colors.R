# code inspired by color.js from LESS, based on http://stackoverflow.com/a/30220605/576684

col_saturation_adjust <- function(cols, amount, method='relative') {
  # amount: -1 to 1 (negative desaturates, positive saturates)
  cols <- as(cols, "HLS")
  if (method == "relative") {
    cols@coords[, "S"] <- cols@coords[, "S"] * (1 + amount)
  }
  else if (method == "additive") {
    cols@coords[, "S"] <- cols@coords[, "S"] + amount
  }
  else {
    warning('saturation adjustment method not supported')
  }
  cols@coords[, "S"] <- pmax(0, cols@coords[, "S"])
  cols@coords[, "S"] <- pmin(1, cols@coords[, "S"])
  return(cols);
}
hex_saturate <- function(cols, amount, method='relative')
  cols %>% (colorspace::hex2RGB) %>% col_saturation_adjust(amount, method) %>% (colorspace::hex)
hex_desaturate <- function(cols, amount, method='relative')
  cols %>% (colorspace::hex2RGB) %>% col_saturation_adjust(-amount, method) %>% (colorspace::hex)

col_lightness_adjust <- function(cols, amount, method='relative') {
  # amount: -1 to 1 (negative darkens, positive lightens)
  cols <- as(cols, "HLS")
  if (method == "relative") {
    cols@coords[, "L"] <- cols@coords[, "L"] * (1 + amount)
  }
  else if (method == "additive") {
    cols@coords[, "L"] <- cols@coords[, "L"] + amount
  }
  else {
    warning('lightness adjustment method not supported')
  }
  cols@coords[, "L"] <- pmax(0, cols@coords[, "L"])
  cols@coords[, "L"] <- pmin(1, cols@coords[, "L"])
  return(cols);
}
hex_lighten <- function(cols, amount, method='relative')
  cols %>% (colorspace::hex2RGB) %>% col_lightness_adjust(amount, method) %>% (colorspace::hex)
hex_darken <- function(cols, amount, method='relative')
  cols %>% (colorspace::hex2RGB) %>% col_lightness_adjust(-amount, method) %>% (colorspace::hex)

# hex_fade <- function(cols, amount)
# hex_fadein <- function(cols, amount, method='relative')
# hex_fadeout <- function(cols, amount, method='relative')
