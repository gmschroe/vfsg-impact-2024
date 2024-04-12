# Functions for preparing submissions and charity data for visualisation

library('dplyr')
library('readxl')
library('janitor')
library('tidyr')
library('stringr')

# Load and organise charity data
get_charity_topics_and_sdg <- function(file_submissions, file_sdg) {
  
  # Load SDG data
  data_sdg <- read_xlsx(file_sdg)
  data_sdg <- clean_names(data_sdg)
  
  # Separate SDG numbers from names
  data_sdg <- data_sdg |>
    separate_wider_delim(sdg_goals, ': ', names = c('goals_num', 'goals_name')) |>
    mutate(goals_num = as.numeric(str_replace(goals_num, 'GOAL ', '')))
  
  # Compute number of SDG for each project
  data_n_sdg <- data_sdg |>
    select(project_id, name_of_charity_project, goals_num) |>
    group_by(project_id, name_of_charity_project) |>
    summarise(goals_num = list(goals_num)) |>
    mutate(n_sdg = unlist(lapply(goals_num, length)))
  
  # Load submissions data (note: use clean submissions data)
  data_submissions <- read_xlsx(file_submissions)
  
  # Get topic of each project from submissions data; clean and organise topics
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
        (topic == 'Energy') | (topic == 'Energy/ Sustainability') |
          (topic == 'Sanitation') | (topic == 'Water management') | 
          (topic == 'Infrastructures') ~ 'Infrastructure and Resources',
        (topic == 'Environmental Impact') | 
          (topic == 'Conservation') ~ "Environment and Conservation",
        (topic == 'Homelessness') | (topic == 'Financial support') | 
          (topic == 'Human Rights') | (topic == 'Gender Equality') | 
          (topic == 'Recycling') | 
          (topic == 'Crisis') ~ 'Welfare, Rights, and Equality',
        (topic == 'Data') | (topic == 'Sustainable Development') ~ 'Supporting Goals',
        .default = topic
      )
    ) |>
    distinct(project_id, name_of_charity_project, topic, topic_general)
  
  # Join tibbles
  data_charities <- left_join(data_n_sdg, data_topic, by = 'project_id')
  data_charities <- data_charities |>
    mutate(charity_name = name_of_charity_project.y) |>
    select(project_id, charity_name, goals_num, n_sdg, topic, topic_general)
  
  # Clean up charity names
  data_charities <- data_charities |>
    mutate(
      charity_name = case_when(
        charity_name == "Centro de Pensamiento Estratégico Internacional (Cepei)" ~
          'Centro de Pensamiento Estratégico Internacional',
        .default = charity_name
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