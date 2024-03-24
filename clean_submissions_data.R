# Amended version of "VFSG All Submissions" data.
# Volunteer names have been cleaned up and additional measures have been 
# computed.
#
# CLEANING STEPS:
#   - "Summary Viz Japan Chapter Hackathon" submission removed since it is a 
#     summary of other submissions.
#   - Volunteer names added for "see below" submission (extracted from the 
#     submitted vis)
#   - Converted all volunteer names to lowercase
#   - Changed volunteer name "tokiko.mizuguchi" to "tokiko mizuguchi"
#   - Column names changed to snakecase
#
# Also note that date_of_project now reveals day data (not visible in date format in Excel).
# Unclear if day data is accurate - set to first of month for some submissions, 
# suggesting that only month was originally stored for some submissions.
#
# NEW SUBMISSION-SPECIFIC MEASURES:
#   - "name_of_volunteer_clean": comma-separated and alphabetically-sorted list 
#     of names of volunteers for each submission
#   - "n_volunteers": number of volunteers that worked on the submission
#   - "is_collaboration": whether the submission was a collaboration 
#     (n_volunteers > 1)
#   - "n_submissions": number of submissions that the submission's volunteer or 
#     group of volunteers had submitted for VFSG (at the time of the specified 
#     submission). E.g., n_submissions = 1 means that that is the volunteer's 
#     first submission; n_submissions = 2 means that that volunteer had 
#     previously submitted one other vis. For this measure, collaborations are 
#     treated as a completely separate "volunteer" and do not increment the
#     corresponding individuals' solo submission counts.
#   - "n_submissions_incl_collaboration": same as n_submissions, but 
#     collaborations do count towards any of the volunteer's future solo 
#     submissions. For collaborative projects, this measure is the same as 
#     n_submissions.
#   - "is_first_submission": whether the submission was the volunteer's (or 
#     specific combination of volunteers') first VFSG submission (i.e., whether
#     n_submissions == 1). For non-collaborative projects, a volunteer's first 
#     solo submission is considered their first submission, even if they had
#     previously submitted a collaborative vis.
#   - "is_first_submission_incl_collaboration": same as is_first_submission, but
#     uses n_submissions_incl_collaboration (i.e., whether
#     n_submissions_incl_collaboration == 1). Thus, if a volunteer's first 
#     submission is collaborative, a subsequent solo submission is NOT 
#     considered their first submission.
#
# NEW VOLUNTEER-SPECIFIC MEASURES: 
# (These measures are the same for each submission that has the same 
# "name_of_volunteer_clean")
#   - "volunteer_first_project_date": date of the project of the volunteer's (or 
#     group of volunteers') first submission. For non-collaborative projects, a 
#     volunteer's first solo submission is considered their first submission.
#   - "volunteer_first_project_date_incl_collaboration": same as 
#     volunteer_first_project_date, but counts collaborative submissions when
#     determining the first project date of the corresponding individual 
#     volunteers.
#
# See solo and collaborative projects of "adam crahen" and "pooja gandhi" as a 
# good example of how solo and collaborative projects impact n_submissions vs. 
# n_submissions_incl_collaboration

# Note that the max n_submissions_incl_collaborative does NOT give the total
# the total number of vis (solo and collaborative) a volunteer has 
# submitted if the volunteer's last (or only) projects were collaborative 
# projects. As such, n_submissions_incl_collaborative should NOT be used to 
# compare the total number of submissions of different volunteers.
#
# However, the max n_submissions of each volunteer does give the total number of
# SOLO vis submitted by each volunteer (or, for collaborative projects, the 
# number of vis submitted by that specific combination of volunteers).
#
# For first submission and first project date measures: In this dataset, the 
# "incl_collaboration" versions only differ for "sumeet bedekar", whose first 
# VFSG submission was a collaboration. 
#
# NOT INCLUDED in this dataset:
#   - Stats (e.g., first project dates) for any volunteers who ONLY participated 
#     in collaborative projects
#   - Total number of submissions (incl. collaborative vis) of each volunteer
#
#
# Data from Viz For Social Good
# Additional cleaning and computations by Gabrielle M. Schroeder
# 24 March, 2024


# Set up ----

rm(list = ls())
library('dplyr')
library('readxl')
library('janitor')
library('stringr')
library('purrr')
library('writexl')

# Load data ----

file_submissions <- file.path('data', 'VFSG All Submissions.xlsx')
data_submissions <- read_xlsx(file_submissions)
data_submissions <- clean_names(data_submissions)

# Sorted by project date (shifts a few submissions)
data_submissions <- data_submissions |>
  arrange(date_of_project)

# Data cleaning ----

# Remove "Summary Viz Japan Chapter Hackathon" entry 
# (is a summary vis of other submissions, most of which are listed separately)
data_submissions <- 
  data_submissions[!data_submissions$name_of_volunteer == "Summary Viz Japan Chapter Hackathon",]

# Add volunteer names for "see below" entry (extracted from submitted vis)
data_submissions <- data_submissions |>
  mutate(
    name_of_volunteer = 
      sub('see below', 
          'Andrew Ho, Antoneely Calizaya, Ayda Akbarzadeh, Martin Ding, Terry Zhou',
          name_of_volunteer)
    )

# Convert all volunteer names to lowercase
# (case isn't always consistent for same volunteer)
data_submissions <- data_submissions |>
  mutate(name_of_volunteer = str_to_lower(name_of_volunteer))

# Remove period from name 
data_submissions <- data_submissions |>
  mutate(
    name_of_volunteer = 
      sub('tokiko.mizuguchi', 
          'tokiko mizuguchi',
          name_of_volunteer)
  )

# Find collaborative projects and compute associated stats ----

data_submissions <- data_submissions |>
  mutate(
    # Extract individual names (separated by a comma, &, +, or "and")
    volunteers_list = str_split(name_of_volunteer,',|&|\\+| and '),
    # Trim extra white space
    volunteers_list = lapply(volunteers_list, str_squish),
    # Sort alphabetically
    volunteers_list = lapply(volunteers_list, sort),
    # Number of volunteers
    n_volunteers = unlist(lapply(volunteers_list, length)),
    # Collaborations
    is_collaboration = n_volunteers > 1,
    # Create clean version of volunteer names.
    # Returns sorted, comma separated names of all volunteers for each project.
    # Allows easy identification of repeat collaborations.
    name_of_volunteer_clean = unlist(lapply(volunteers_list, paste, collapse = ', '))
  )

# Check number of distinct names (fewer after names have been cleaned)
n_distinct(data_submissions$name_of_volunteer)
n_distinct(data_submissions$name_of_volunteer_clean)

# Compute submission number for each volunteer/collaboration ----

# n_submissions (solo and collaborative projects treated as completely separate 
# "volunteers")
data_submissions <- data_submissions |>
  group_by(name_of_volunteer_clean) |>
  mutate(n_submissions = 1:n() ) |>
  ungroup()

# n_submissions_incl_collaboration (collaborative projects also increment each
# individual volunteer's number of submissions)
# (suggestions for how to do this computation without a loop would be welcome!)
data_submissions <- data_submissions |>
  mutate(n_submissions_incl_collaboration = n_submissions)
n_projects <- nrow(data_submissions)
for (i in 1:n_projects) {
  # If not a collaborative project, count number of times that the volunteer 
  # name has occurred in the list of volunteer names up to and including the 
  # time of that project
  if (!data_submissions$is_collaboration[i]) {
    data_submissions$n_submissions_incl_collaboration[i] = 
      sum(
        map_lgl(
          data_submissions$volunteers_list[1:i], ~data_submissions$name_of_volunteer_clean[i] %in% .x
          )
      )
  }
}

# Find first-time submissions for each volunteer or combination of volunteers ----

data_submissions <- data_submissions |>
  mutate(
    # First submission if n_submissions == 1
    is_first_submission = (n_submissions == 1),
    # First submission if n_submissions_incl_collaboration == 1
    # (means that if a volunteer's first submission is collaborative, their first
    # solo submission will NOT be marked as their first submission)
    is_first_submission_incl_collaboration = (n_submissions_incl_collaboration == 1)
  )

# View submissions where is_first_submission differs from 
# is_first_submission_incl_collaboration
View(
  data_submissions |>
    filter(is_first_submission != is_first_submission_incl_collaboration)
)

# Identify date of first project for each volunteer or collaboration ----

# Treating collaborations completely separately
data_submissions <- data_submissions |>
  group_by(name_of_volunteer_clean) |>
  mutate(
    volunteer_first_project_date = date_of_project[is_first_submission]
  ) |>
  ungroup()

# Including collaborative submissions towards each individual volunteer's
# start date (will only impact one volunteer in this dataset)
data_submissions <- data_submissions |>
  mutate(volunteer_first_project_date_incl_collaboration = volunteer_first_project_date) 
for (i in 1:n_projects) {
  # If not a collaborative project, find date of first project that the 
  # volunteer was involved in (solo or collaborative submission)
  if (!data_submissions$is_collaboration[i]) {
    idx <- which(
      map_lgl(data_submissions$volunteers_list, ~data_submissions$name_of_volunteer_clean[i] %in% .x)
      )
    idx <- idx[1]
    data_submissions$volunteer_first_project_date_incl_collaboration[i] = 
      data_submissions$date_of_project[idx]
  }
}

# View submissions where volunteer_first_project_date differs from 
# volunteer_first_project_date_incl_collaboration
View(
  data_submissions |>
    filter(volunteer_first_project_date != volunteer_first_project_date_incl_collaboration)
)

# Remove volunteers list ----
data_submissions <- data_submissions |>
  select(!volunteers_list)

# View new measures ----

var_list <- c(
  'project_id', 
  'date_of_project',
  'name_of_volunteer', 
  'name_of_volunteer_clean',
  'is_collaboration',
  'n_volunteers',
  'n_submissions', 
  'n_submissions_incl_collaboration',
  'is_first_submission',
  'is_first_submission_incl_collaboration',
  'volunteer_first_project_date',
  'volunteer_first_project_date_incl_collaboration'
)

# All
View(
  data_submissions |> 
    arrange(name_of_volunteer_clean) |>
    select(var_list)
)

# Entries where clean name does not match original name
# (includes some solo submissions due to white space changes)
View(
  data_submissions |> 
    filter(name_of_volunteer != name_of_volunteer_clean) |>
    arrange(name_of_volunteer_clean) |>
    select(var_list)
)

# Collaborative projects only
# check that any repeat collaborations are correctly identified
View(
  data_submissions |> 
    filter(is_collaboration == TRUE) |>
    arrange(name_of_volunteer_clean) |>
    select(var_list)
)  

# Complete table
View(data_submissions)

# Some basic stats ----

# Number of projects
n_distinct(data_submissions$project_id)

# Number of volunteers
n_distinct(data_submissions$name_of_volunteer_clean)

# Number of submissions per project 
n_submissions_per_project <- count(data_submissions, project_id, sort = TRUE)
View(n_submissions_per_project)

# Save ----
save_path <- file.path('data', 'vfsg_all_submissions_clean.xlsx')
write_xlsx(data_submissions, save_path)

# Check import ----
data_submissions_imported <- read_xlsx(save_path)
View(data_submissions_imported)
all.equal(data_submissions, data_submissions_imported)

