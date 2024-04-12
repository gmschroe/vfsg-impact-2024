# Functions for preparing submissions and charity data for visualisation

library('dplyr')
library('readxl')
library('janitor')
library('tidyr')
library('stringr')

source('lib/lib_colours.R')
source('lib/lib_helper.R')

# Load and organise charity data
prep_charity_data <- function(file_submissions, file_sdg) {
  
  # Load SDG data
  data_sdg <- read_xlsx(file_sdg)
  data_sdg <- clean_names(data_sdg)
  
  # Separate SDG numbers from names
  data_sdg <- data_sdg |>
    separate_wider_delim(sdg_goals, ': ', names = c('goals_num', 'goals_name')) |>
    mutate(goals_num = as.numeric(str_replace(goals_num, 'GOAL ', '')))
  
  # Compute number of SDG for each project
  # Also get and format number of followers
  data_n_sdg <- data_sdg |>
    select(project_id, name_of_charity_project, goals_num, n_twitter_followers) |>
    group_by(project_id, name_of_charity_project, n_twitter_followers) |>
    summarise(goals_num = list(goals_num)) |>
    mutate(n_sdg = unlist(lapply(goals_num, length))) |>
    # Log number of followers
    mutate(log_n_followers = log(n_twitter_followers + 10))
  
  # Scale and round number of followers
  data_n_sdg$n_followers_scaled <- round(scale_values(
    data_n_sdg$log_n_followers, new_min = 4, new_max = 10
  ))
  
  # Load submissions data (note: use clean submissions data)
  data_submissions <- read_xlsx(file_submissions)
  
  # Get topic of each project from submissions data; clean and organise topics;
  # get formatted project date
  data_topic <- data_submissions |>
    # remove "2" from names of charities with repeat projects
    # (esp. since added inconsistently for some projects)
    mutate(
      name_of_charity_project = str_replace(name_of_charity_project, ' 2', '')
    ) |>
    # Sunny Street topic = Health (removes Health/Healthcare difference and probably Education error)
    # Crowd2Map = Health (remove "56" error)
    mutate(
      topic = case_when(
        name_of_charity_project == "Sunny Street" ~ "Health",
        name_of_charity_project == "Crowd2Map" ~ "Health",
        .default = topic
      ) 
    ) |>
    # Broader categories for topics
    # Energy, Energy/ Sustainability --> Energy and Sustainability
    # Collapse "Environmental Impact" and "Conservation" into "Environment and Conservation"
    mutate(
      topic_general = case_when(
        topic %in% c(
          'Energy/ Sustainability', 'Energy', 'Sanitation', 'Water management', 
          'Infrastructures', 'Sustainable Development'
        ) ~ 'Infrastructure, resources, and sustainability',
        topic %in% c(
          'Environmental Impact', 'Conservation'
        ) ~ "Environment and conservation",
        topic %in% c(
          'Homelessness', 'Financial support', 'Human Rights', 'Gender Equality',
          'Recycling', 'Crisis', 'Children and Youth'
        ) ~ 'Welfare, rights, and equality',
        name_of_charity_project == "Fondation Follereau" ~ 'Welfare, rights, and equality',
        name_of_charity_project %in% c(
          "Video Volunteers", "Viz For Social Good"
          ) ~ 'Amplifying voices',
        .default = topic
      )
    ) |>
    # Add date as name of month and year
    mutate(
      date_string = format(date_of_project, "%B %Y")
    ) |>
    distinct(
      project_id, name_of_charity_project, date_string, topic, topic_general
    )
  
  # Join tibbles
  data_charities <- left_join(data_n_sdg, data_topic, by = 'project_id')
  data_charities <- data_charities |>
    mutate(charity_name = name_of_charity_project.y) |>
    select(
      project_id, charity_name, date_string, goals_num, n_sdg, topic, topic_general,
      n_twitter_followers, n_followers_scaled
    )
  
  # Clean up charity names; add breaks for plotting
  data_charities <- data_charities |>
    mutate(
      charity_name = case_when(
        charity_name == "Centro de Pensamiento Estratégico Internacional (Cepei)" ~
          'Centro de Pensamiento Estratégico Internacional',
        charity_name == "Gord Downie & Chanie Wenjack Fund" ~ 
          "Gord Downie &<br>Chanie Wenjack Fund",
        charity_name == "Inter-American Development Bank" ~
          "Inter-American<br>Development Bank",
        charity_name == "African Youth Mentorship Network" ~
          "African Youth<br>Mentorship Network",
        charity_name == "United Nations in Papua New Guinea" ~
          "United Nations in<br>Papua New Guinea",
        charity_name == "Tap Elderly Women's Wisdom for Youth" ~
          "Tap Elderly Women's<br>Wisdom for Youth",
        charity_name == "Physicalizing Data for a Better World" ~
          "VFSG: Physicalizing<br>Data for a Better World",
        .default = charity_name
      )
    )
  
  # Add colors based on topic
  clrs <- get_polygon_clrs()
  data_charities <- data_charities |>
    mutate(
      clrs  = case_when(
        topic_general == 'Health' ~ list(clrs$health),
        topic_general == 'Education' ~ list(clrs$education),
        topic_general == 'Amplifying voices' ~ list(clrs$voices),
        topic_general == 'Infrastructure, resources, and sustainability' ~ list(clrs$sustainability),
        topic_general == 'Welfare, rights, and equality' ~ list(clrs$rights),
        topic_general == "Environment and conservation" ~ list(clrs$environment),
        .default = NA
        
      )
    )
  return(data_charities)
  
}

# Prep submissions data for plotting by adding r and theta values ----
prep_submissions_data <- function(
    data_submissions, 
    r1 = 0, 
    base_theta = (2*pi)/120,
    theta_present_multiplier = 5
  ) {
  
  # Fill in "FALSE" selected_to_present_flag values
  data_submissions <- data_submissions |>
    mutate(
      selected_to_present_flag = case_when(
        selected_to_present_flag == TRUE ~ TRUE,
        .default = FALSE
      )
    )
  
  # Add r and theta
  data_for_plot <- data_submissions |>
    mutate(
      r1 = r1,
      r2 = log(n_submissions_incl_collaboration + 1) + r1,
      theta = case_when(
        selected_to_present_flag == TRUE ~ base_theta * theta_present_multiplier,
        .default = base_theta
      )
    ) |> 
    select(c(id, project_id, name_of_charity_project, selected_to_present_flag,
             name_of_volunteer_clean, n_submissions_incl_collaboration, 
             r1, r2, theta))
  
  
  return(data_for_plot)
}