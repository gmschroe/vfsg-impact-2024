# Plotting and plotting-related functions
library('magick')
library('ggfx')
library('ggplot2')

# Stretch colour palette ----
stretch_colour_palette <- function(clrs) {
  n_clrs <- length(clrs)
  clrs_stretched <- c()
  for (i in 1:n_clrs) {
    clrs_stretched <- c(clrs_stretched, rep(clrs[i], i))
  }
  return(clrs_stretched)
}

# Visualise data for one charity ----
charity_data_art <- function(
    project, # project id to plot
    data_charities, # charity data, cleaned
    data_submissions, # submissions data, cleaned
    plot_clrs, # named list with $rays and $bg
    polygon_clrs, # colors for polygons (at least 3)
    axis_lim = NA,
    x_shift = 0, # for shifting x coordinates
    y_shift = 0, # for shifting y coordinates
    x_margins = 0.5,
    y_margins = 1.5,
    submissions_r1 = 0,
    submissions_base_theta = (2*pi)/160,
    submissions_theta_present_multiplier = 5,
    polygon_r_max = 1.75,
    polygon_lw = 0.25,
    polygon_alpha = 1,
    polygon_dither_map_size = 8,
    polygon_dither_levels = 50,
    rays_alpha = 1,
    rays_glow_sigma = 0.6,
    interpolate_src_perc = 30
  ) {
  
  
  # -- Charities layer --
  
  # colors for polygons
  n_clrs <- length(polygon_clrs)
  clrs <- list()
  clr_palette <- list()
  clrs$fill <- stretch_colour_palette(polygon_clrs[3:n_clrs])
  clr_palette$fill <- colorRampPalette(clrs$fill)
  clrs$lines <- stretch_colour_palette(polygon_clrs[1:(n_clrs - 1)])
  clr_palette$lines <- colorRampPalette(clrs$lines) 
  
  # make polygons
  n_sides <- data_charities$n_sdg[project] + 3
  polygon_data <- gen_nested_polygon_data(
    n_sides = n_sides,
    n_rep = n_sides * 4,
    r_max = polygon_r_max
  )
  
  # polygon geom
  p_polygon <- geom_polygon(
    data = polygon_data, 
    mapping = aes(
      x = x + x_shift, y = y + y_shift, group = id, fill = id, color = id
    ),
    linewidth = polygon_lw, 
    alpha = polygon_alpha,
    show.legend = FALSE
  )
  
  # dither polygons
  p_polygon <- with_ordered_dither(
    p_polygon, 
    map_size = polygon_dither_map_size, 
    levels = polygon_dither_levels
  )
  
  
  # -- Submission layer --
  
  # add radii and thetas to submissions data
  data_submissions <- prep_submissions_data(
    data_submissions, 
    r1 = submissions_r1,
    base_theta = submissions_base_theta, 
    theta_present_multiplier = submissions_theta_present_multiplier
  )
  
  data_for_plot <- data_submissions |>
    filter(project_id == project)
  
  # add angles
  data_for_plot <- gen_ray_angles(data_for_plot, seed = project)
  ray_data <- make_all_rays(data_for_plot)
  
  # rays geom
  p_rays <- geom_polygon(
    data = ray_data, 
    mapping = aes(x = x + x_shift, y = y + y_shift, group = id),
    fill = plot_clrs$rays, 
    alpha = rays_alpha
  ) 
  
  # add outer glow
  p_rays <- with_outer_glow(
    p_rays, 
    colour = plot_clrs$rays, 
    sigma = rays_glow_sigma
  )
  
  # -- Compute axis limits --
  
  if (sum(is.na(axis_lim)) == 0) {
    x_min = axis_lim$x_min
    x_max = axis_lim$x_max
    y_min = axis_lim$y_min
    y_max = axis_lim$y_max
  } else {
    x_min <- min(c(ray_data$x, polygon_data$x))
    x_max <- max(c(ray_data$x, polygon_data$x))
    y_min <- min(c(ray_data$y, polygon_data$y))
    y_max <- max(c(ray_data$y, polygon_data$y))
  }

  # -- Background --
  bg_data <- tibble(
    x = c(x_min, x_max, x_max, x_min),
    y = c(y_min, y_min, y_max, y_max)
  )
  p_bg <- geom_polygon(
    data = bg_data,
    mapping = aes(x = x + x_shift, y = y + y_shift),
    fill = plot_clrs$bg,
    colour = NA,
    inherit_aes = FALSE
  )
  
  # -- Plot --
  art <- list()
  art$p <- ggplot() +
    p_bg +
    as_reference(p_polygon, id = 'polygon') +
    with_interpolate(
      p_rays,
      'polygon', blend_type = "overlay", src_percent = interpolate_src_perc) +
    with_blend(
      p_rays,
      'polygon', blend_type = "overlay", alpha = 1) +
    coord_fixed(clip = 'off') +
    theme_void() +
    scale_fill_gradientn(colours = clr_palette$fill(100)) +
    scale_colour_gradientn(colours = clr_palette$lines(100)) +
    theme(
      plot.background = element_rect(
        fill = plot_clrs$margins, 
        colour = plot_clrs$marings)
    ) +
    scale_x_continuous(limits = c(x_min + x_shift - x_margins, x_max + x_shift + x_margins),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min + y_shift - y_margins/2, y_max + y_shift + y_margins),
                       expand = c(0, 0))
  
  art$x_min = x_min
  art$x_max = x_max
  art$y_min = y_min
  art$y_max = y_max
  return(art)
}