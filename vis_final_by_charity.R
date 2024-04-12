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

# Choose project ----

project <- 39#46#39


# Colors ----

plot_clrs <- get_plot_clrs()

clrs <- c(
  '#392566',
          '#4e235c',
           '#5a2352',
           '#622649',
           '#662c42'
)

# Plot ----

x_margins <- 0.5
y_margins <- 1.5

axis_lim <- list()
axis_lim$x_min <- -3.1
axis_lim$x_max <- 3.1

x_size <- (axis_lim$x_max - axis_lim$x_min) + (x_margins * 2)
y_size <- x_size * 1.41
axis_lim$y_max <- 3.25
axis_lim$y_min <- -1 * (y_size - (y_margins*1.5) - axis_lim$y_max)

n_projects <- max(data_submissions$project_id)
# x_min <- numeric(n_projects)
# y_min <- numeric(n_projects)
# x_max <- numeric(n_projects)
# y_max <- numeric(n_projects)

for (i in 46) {
  art <- charity_data_art(
    project = i,
    data_charities,
    data_submissions,
    plot_clrs,
    clrs,
    axis_lim = axis_lim,
    x_margins = x_margins,
    y_margins = y_margins
  )
  # x_min[i] <- art$x_min
  # y_min[i] <- art$y_min
  # x_max[i] <- art$x_max
  # y_max[i] <- art$y_max
  
  art$p
}
# ----

sz1 <- 24
sz2 <- 12
sz3 <- 8

name <- data_charities$charity_name[i]#'Viz For Social Good'
details <- glue(
  '<span style="font-size:{sz3}pt;">',
  'Designed by <b>Gabrielle M. Schroeder</b><br>for Viz For Social Good<br>Data: VFSG')

text_x = 0
name_data <- tibble(
  x = art$x_min + text_x ,
  y = art$y_max + 0.1,
  label = glue(
    '<span style="font-size:{sz2}pt;">',
    'Project #{project}</span><br>',
    '<b style="font-size:{sz1}pt;">{name}</b>'
    )
)

details_data <- tibble(
  x = art$x_max - text_x ,
  y = art$y_min - 0.1,
  label = details
)

font_family <- "Cooper Hewitt R"
register_font(
  name = font_family,
  plain = '/Users/gmschroe/Library/Fonts/CooperHewitt-Light.otf',
  bold = '/Users/gmschroe/Library/Fonts/CooperHewitt-Book.otf'
)
p <- art$p
p + 
  geom_textbox(name_data, mapping = aes(x = x, y = y, label = label),
               family = font_family, 
               colour = plot_clrs$bg,
               inherit.aes = FALSE,
               box.colour = NA, fill = NA,     
               width = unit(4, 'inch'),
               box.padding = unit(rep(0, 4), 'pt'),
               hjust = 0,
               vjust = 0,
               lineheight = 2.5
               ) +
  geom_textbox(details_data, mapping = aes(x = x, y = y, label = label),
               family = font_family, 
               colour = plot_clrs$bg,
               size = 1.5,
               inherit.aes = FALSE,
               box.colour = NA, fill = NA,     
               width = unit(2, 'inch'),
               box.padding = unit(rep(0, 4), 'pt'),
               vjust=1, valign=1, hjust=1, halign=1
               )
