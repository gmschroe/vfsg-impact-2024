# Functions for generating "beacons"

# Make one ray of beacon ----
make_ray <- function(
    r1, # inner (start) radius
    r2, # outer (stop) radius
    theta, # width angle, in radians
    phi # rotation angle (clockwise), in radians
) {

  # compute x coordinates from width angle
  x1 <- r1 * tan(theta/2)
  x2 <- r2 * tan(theta/2)
  
  # (x, y) coordinates (ordered for geom_polygon)
  x <- c(x1, x2, -x2, -x1)
  y <- c(r1, r2, r2, r1)
  
  # rotate
  # first multiply by -1 to rotate clockwise instead of counterclockwise
  phi <- -phi
  x_rot <- x * cos(phi) - y * sin(phi)
  y_rot <- y * cos(phi) + x * sin(phi)
  
  # as tibble
  ray_data <- tibble(x = x_rot, y = y_rot)
  return(ray_data)
}

# Generate rotation angles for rays----
gen_ray_angles <- function(
    data, # tibble with "theta" variable for ray widths
    seed = 0, # seed for generating angles
    dist_type = 'normal',
    base_gap = 1 # baseline value for gaps - increasing base_gap decreases variability
    ) {
  
  set.seed(seed)
  
  # number of rays
  n_ray <- nrow(data)
  
  # draw gaps between rays from distribution 
  if (dist_type == 'uniform') {
    gap_size <- runif(n_ray, 0, 1) + base_gap
  } else if (dist_type == "normal") {
    gap_size <- abs(rnorm(n_ray, mean = 0, sd = 1)) + base_gap
  }
  
  # normalise gaps to fill remaining space (in radians) not used by ray widths
  space_remaining <- 2*pi - sum(data$theta) 
  gap_size <- (gap_size / sum(gap_size)) * space_remaining
  
  # Shift first gap so last ray isn't at pi/2
  gap_shifted <- runif(1, 0, gap_size[1])
  gap_size[1] <- gap_shifted
  
  # Convert to rotation angle by computing cumulative gap and ray widths
  # do not include first ray width in sum
  rolling_theta <- stats::filter(
      c(data$theta, data$theta[n_ray]),
      rep(1 / 2, 2), sides = 2
    )
  rolling_theta <- rolling_theta[c(n_ray, 1:n_ray - 1)]
  phi <- cumsum(gap_size + rolling_theta) 
  
  
  # Add to rotation angle phi to the data
  data <- data |> 
    mutate(phi = phi)
  return(data)
}

# Make all rays ("beacon") ----
make_all_rays <- function(
    data # tibble with r1, r2, theta, and phi for specifying ray lengths and angles
) {
  n_ray <- nrow(data)
  ray_list <- vector(mode = 'list', length = n_ray)
  for (i in 1:n_ray) {
    ray_list[[i]] <- make_ray(data$r1[i], data$r2[i], data$theta[i], data$phi[i])
  }
  ray_data <- bind_rows(ray_list, .id = 'id')
  return(ray_data)
}


