# Helper functions

# scale_values: scale values between new min and max ----
scale_values <- function(
    x,
    new_min,
    new_max
) {
  xmin <- min(x)
  xmax <- max(x)
  x <- ((new_max - new_min) * (x - xmin)) / (xmax - xmin)
  x <- x + new_min
  return(x)
}