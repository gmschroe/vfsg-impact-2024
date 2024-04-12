# Functions for generating nested polygons 

# Generate data for nested polygons -----
gen_nested_polygon_data <- function(
    n_sides, # number of sides in polygon
    n_rep, # number of repetitions
    r_max # radius of outer polygon 
) {
  
  # Compute angles 
  theta_delta <- (2*pi) / (n_sides * 2)
  theta <- seq(from = theta_delta, to = 2*pi, by = theta_delta) + pi/2
  
  # Compute radii
  r <- numeric(n_rep)
  r[1] <- r_max
  for (i in 1:(n_rep - 1)) {
    r[i + 1] = r[i] * cos(theta_delta)
  }
  
  # Create tibble with r and theta values
  polygon_data = list()
  for (i in 1:n_rep) {
    id <- rep(1:2, n_sides) + 2 * (i - 1)
    
    polygon_data[[i]] <- tibble(
      id = id,
      theta = theta,
      r = r[i]
    )
  }
  polygon_data <- bind_rows(polygon_data, .id = 'r_num')
  
  # Compute x and y coordinates 
  polygon_data <- polygon_data |>
    mutate(
      x = r * cos(theta),
      y = r * sin(theta)
    )
  
  return(polygon_data)
}