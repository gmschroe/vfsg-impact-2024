# Visualise submissions and charity data for each charity (one plot per charity)

# Set up----
rm(list = ls())
library(dplyr)
library(readxl)
library(janitor)
library(tidyr)
library(stringr)
library(magick)
library(ggfx)
library(ggplot2)
library(ggtext)
library(systemfonts)
library(glue)
library(png)
library(lubridate)

source('lib/lib_data_prep.R')
source('lib/lib_polygons.R')
source('lib/lib_beacons.R')
source('lib/lib_plotting.R')
source('lib/lib_colours.R')

# Load data ----

# charities
file_sdg <- file.path('data', 'Charity - SDG.xlsx')
file_submissions <- file.path('data', 'vfsg_all_submissions_clean.xlsx')
data_charities <- get_charity_topics_and_sdg(
  file_submissions = file_submissions,
  file_sdg = file_sdg
)

# submissions
file_submissions <- file.path('data', 'vfsg_all_submissions_clean.xlsx')
data_submissions <- read_xlsx(file_submissions)

# Colors ----

plot_clrs <- get_plot_clrs()

# Get data limits for each plot ----

n_projects <- max(data_submissions$project_id)
# x_min <- numeric(n_projects)
# y_min <- numeric(n_projects)
# x_max <- numeric(n_projects)
# y_max <- numeric(n_projects)
# 
# for (i in 46) {
#   art <- charity_data_art(
#     project = i,
#     data_charities,
#     data_submissions,
#     plot_clrs
#   )
#   
#   x_min[i] <- art$x_min
#   y_min[i] <- art$y_min
#   x_max[i] <- art$x_max
#   y_max[i] <- art$y_max
# }

# Set same limits for all plots and plot ----
x_margins <- 0.5 
y_margins <- 1.75

axis_lim <- list()
axis_lim$x_min <- -3.1
axis_lim$x_max <- 3.1

x_size <- (axis_lim$x_max - axis_lim$x_min) + (x_margins * 2)
y_size <- x_size * (297/210) # A paper ratio
axis_lim$y_max <- 3.25
axis_lim$y_min <- -1 * (y_size - (y_margins*1.5) - axis_lim$y_max)

for (i in 1:n_projects) {
  art <- charity_data_art(
    project = i,
    data_charities,
    data_submissions,
    plot_clrs,
    axis_lim = axis_lim,
    x_margins = x_margins,
    y_margins = y_margins
  )

  # Text ---
  project <- i
  sz1 <- 32
  sz2 <- 18
  sz3 <- 12
  sz4 <- 22
  
  font_family <- "Cooper Hewitt R"
  register_font(
    name = font_family,
    plain = '/Users/gmschroe/Library/Fonts/CooperHewitt-Light.otf',
    bold = '/Users/gmschroe/Library/Fonts/CooperHewitt-Book.otf',
    bolditalic = '/Users/gmschroe/Library/Fonts/CooperHewitt-MediumItalic.otf'
  )
  
  name <- data_charities$charity_name[project]
  details <- glue(
    '<span style="font-size:{sz3}pt;">',
    'Designed by <b>Gabrielle M. Schroeder</b><br>for Viz For Social Good<br>Data: VFSG')
  
  name_data <- tibble(
    x = art$x_min,
    y = art$y_max + 0.1,
    label = glue(
      '<span style="font-size:{sz2}pt;">',
      'Project #{project}</span><br>',
      '<b style="font-size:{sz1}pt;">{name}</b>'
      )
  )
  
  details_data <- tibble(
    x = art$x_max - 0.65,
    y = art$y_min - 0.1,
    label = details
  )
  
  date_data <- tibble(
    x = art$x_min,
    y = art$y_min - 0.1,
    label = glue('<b style="font-size:{sz2}pt;">*{data_charities$date_string[project]}*</b>')
  )
  
  vfsg_width <- 0.525
  vfsg_y_shift <- 0.07
  
  p <- art$p
  p_with_text <- p + 
    geom_textbox(name_data, mapping = aes(x = x, y = y, label = label),
                 family = font_family, 
                 colour = plot_clrs$bg,
                 inherit.aes = FALSE,
                 box.colour = NA, fill = NA,     
                 width = unit(16, 'cm'),
                 box.padding = unit(rep(0, 4), 'pt'),
                 hjust = 0,
                 vjust = 0,
                 lineheight = 3.35
                 ) +
    geom_textbox(details_data, mapping = aes(x = x, y = y, label = label),
                 family = font_family, 
                 colour = plot_clrs$bg,
                 inherit.aes = FALSE,
                 box.colour = NA, fill = NA,     
                 width = unit(10, 'cm'),
                 box.padding = unit(rep(0, 4), 'pt'),
                 vjust = 1, valign = 1, hjust = 1, halign = 1,
                 lineheight = 1.4
                 ) +
    geom_textbox(date_data, mapping = aes(x = x, y = y, label = label),
                 family = font_family, 
                 colour = plot_clrs$bg,
                 inherit.aes = FALSE,
                 box.colour = NA, fill = NA,     
                 width = unit(10, 'cm'),
                 box.padding = unit(rep(0, 4), 'pt'),
                 vjust = 1, valign = 1, hjust = 0, halign = 0,
                 lineheight = 1.4) +
    # vfsg_logo
    vfsg_logo_layer(
      file.path('data','vfsg_logo.png'),
      ymin = art$y_min - vfsg_y_shift - vfsg_width,
      ymax = art$y_min - vfsg_y_shift,
      xmin = art$x_max - vfsg_width,
      xmax = art$x_max
    )
  
  # Save ---
  plot_dir <- 'vfsg_plots'
  save_charity_plot(plot_dir, p_with_text, i, res = 100) 
}