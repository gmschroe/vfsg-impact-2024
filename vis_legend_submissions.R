# Legend for submissions data for charity plots
# Rough code, will be refactored

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

# Synthetic data to demo encoding ----
n_submissions <- 14
example_data <- tibble(
  id = seq(1:n_submissions),
  project_id = 1,
  name_of_charity_project = 'Volunteer Submissions',
  name_of_volunteer_clean = 'demo',
  n_submissions_incl_collaboration = c(
    rep(1, 6), 2, 3, 5, 15, 25, 3, 1, 10
  ),
  selected_to_present_flag = c(
    rep(FALSE, n_submissions - 3), rep(TRUE, 3)
  )
)

example_data <- prep_submissions_data(example_data)
example_data <- gen_ray_angles(example_data, seed = 1)
example_data$phi <- example_data$phi - pi/2 #  shift so easier to label
ray_data <- make_all_rays(example_data)

# Colors ---
plot_clrs <- get_plot_clrs()

# Limits ---
x_margins <- 0.5 
y_margins <- 1.75

axis_lim <- list()
axis_lim$x_min <- -3.1
axis_lim$x_max <- 3.1

x_size <- (axis_lim$x_max - axis_lim$x_min) + (x_margins * 2)
y_size <- x_size * (297/210) # A paper ratio
axis_lim$y_max <- 3.25
axis_lim$y_min <- -1 * (y_size - (y_margins*1.5) - axis_lim$y_max)

# Plot submissions legend ---

project <- 1

x_min = axis_lim$x_min
x_max = axis_lim$x_max
y_min = axis_lim$y_min
y_max = axis_lim$y_max

# ray layer using geom_polygon
p_rays <- geom_polygon(
  data = ray_data, 
  mapping = aes(x = x, y = y, group = id),
  fill = plot_clrs$rays, 
  alpha = 0.5
) 

# add outer glow
p_rays <- with_outer_glow(
  p_rays, 
  colour = plot_clrs$rays, 
  sigma = 0.6
)

# background
bg_data <- tibble(
  x = c(x_min, x_max, x_max, x_min),
  y = c(y_min, y_min, y_max, y_max)
)
p_bg <- geom_polygon(
  data = bg_data,
  mapping = aes(x = x, y = y),
  fill = plot_clrs$bg,
  colour = NA
)

# make full plot
art <- list()
art$p <- ggplot() +
  p_bg +
  p_rays +
  coord_fixed(clip = 'off') +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = plot_clrs$margins, 
      colour = plot_clrs$margins)
  ) +
  scale_x_continuous(limits = c(x_min - x_margins, x_max + x_margins),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(y_min - y_margins/2, y_max + y_margins),
                     expand = c(0, 0))

art$p

art$x_min = x_min
art$x_max = x_max
art$y_min = y_min
art$y_max = y_max
# Annotations ----
sz1 <- 32
sz2 <- 16
sz2b <- 14
sz3 <- 12
sz4 <- 10

# longer annotations
text_overview <- glue(
  "<span style='font-size:{sz2}pt;'>Each <b>ray</b> is a <b>data visualization</b> ",
                        "submitted by a VFSG volunteer. ",
                        "Together, a project's visualizations illuminate ",
                        "a non-profit's mission and impact.</span>")
text_n_vis <- glue("<span style='font-size:{sz2b}pt;'>",
                     "The <b>length</b> of a ray corresponds to ",
                     "the volunteer's total <br><b>number of<br>submissions</b><br>thus far.</span>")

text_new <- glue("<span style='font-size:{sz3}pt;'>", 
                   "*New volunteers<br>(1 submission)*</span>")
text_returning <- glue("<span style='font-size:{sz3}pt;'>",
                       "*Returning<br>volunteers*</span>")

text_present <- glue("<span style='font-size:{sz2b}pt;'>",
                     "<b>Thicker</b> rays are visualizations that the non-profit ",
                        "selected to <b>present</b> at the project's wrap up.</span>")

text_main <- tibble(
  x = -2.9,
  y = 3,
  label = text_overview
)
text_data <- tibble(
  x = c(1.15, -0.5, 1.7, -2.9),
  y = c(2, 1.15, -2.8, -0.7),
  label = c(text_n_vis, text_new, text_returning, text_present)
)

# labels for number of submissions > 1
vis_bool <- example_data$n_submissions_incl_collaboration > 1 & example_data$selected_to_present_flag == FALSE
n_vis <- example_data$n_submissions_incl_collaboration[vis_bool]
n_vis_data <- tibble(
  n_vis = n_vis,
  x = c(1.15, 1.5, 1.7, 2.1, 0.9),
  y = c(0.45, -0.1, -0.8, -1.9, -3.2)
)
n_vis_data <- n_vis_data |>
  mutate(
    label = glue("<span style='font-size:{sz3}pt;'>",
                      "*{n_vis}*</span>")
  )
n_vis_data$label[1] <- glue("<span style='font-size:{sz3}pt;'>",
                        "*2 submissions*</span>")


font_family <- "Cooper Hewitt R"
register_font(
  name = font_family,
  plain = '/Users/gmschroe/Library/Fonts/CooperHewitt-Book.otf',
  bold = '/Users/gmschroe/Library/Fonts/CooperHewitt-Medium.otf',
  bolditalic = '/Users/gmschroe/Library/Fonts/CooperHewitt-MediumItalic.otf'
)

font_family_annotations <- "Roboto R"
register_font(
  name = font_family_annotations,
  plain = '/Users/gmschroe/Library/Fonts/Roboto-Light.ttf',
  bold = '/Users/gmschroe/Library/Fonts/Roboto-Medium.ttf',
  italic = '/Users/gmschroe/Library/Fonts/Roboto-Italic.ttf',
)

details <- glue(
  '<span style="font-size:{sz3}pt;">',
  'Designed by <b>Gabrielle M. Schroeder</b><br>for Viz For Social Good<br>Data: VFSG')

name_data <- tibble(
  x = art$x_min,
  y = art$y_max + 0.1,
  label = glue(
    '<span style="font-size:{sz2}pt;">',
    'How to Read</span><br>',
    '<b style="font-size:{sz1}pt;">Viz For Social Good<br>Volunteer Submissions</b>'
  )
)

details_data <- tibble(
  x = art$x_max - 0.65,
  y = art$y_min - 0.1,
  label = details
)

vfsg_width <- 0.535
vfsg_y_shift <- 0.07

# Add text to plot --
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
  # annotations for rays
  geom_textbox(text_main, mapping = aes(x = x, y = y, label = label),
               family = font_family_annotations, 
               colour = plot_clrs$rays,
               inherit.aes = FALSE,
               box.colour = NA, fill = NA,     
               width = unit(16, 'cm'),
               box.padding = unit(rep(0, 4), 'pt'),
               hjust = 0, halign = 0,
               vjust = 1, valign = 1, 
               lineheight = 2,
               alpha = 0.9
  ) +
  geom_textbox(text_data, mapping = aes(x = x, y = y, label = label),
               family = font_family_annotations, 
               colour = plot_clrs$rays,
               inherit.aes = FALSE,
               box.colour = NA, fill = NA,     
               width = unit(6, 'cm'),
               box.padding = unit(rep(0, 4), 'pt'),
               hjust = 0, halign = 0,
               vjust = 1, valign = 1, 
               lineheight = 1.5,
               alpha = 0.8
  ) +
  geom_textbox(n_vis_data, mapping = aes(x = x, y = y, label = label),
               family = font_family_annotations, 
               colour = plot_clrs$rays,
               inherit.aes = FALSE,
               box.colour = NA, fill = NA,     
               width = unit(6, 'cm'),
               box.padding = unit(rep(0, 4), 'pt'),
               hjust = 0, halign = 0,
               vjust = 1, valign = 1,
               alpha = 0.7
  ) +
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
ggsave(
  file.path(plot_dir,glue::glue('legend_submissions.png')), 
  width = 21, 
  height = 29.7, 
  units = "cm", dpi = 300
)
    

