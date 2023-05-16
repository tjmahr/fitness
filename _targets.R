# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.


# Replace the target list below with your own:
list(
  tar_file_read(
    data_exercise,
    "data/exercise-minutes.csv",
    readr::read_csv(!! .x)
  ),
  tar_file_read(
    data_intervals,
    "data/intervals.csv",
    readr::read_csv(!! .x)
  ),
  tar_file_read(
    data_bodyweight,
    "data/bodyweight.csv",
    readr::read_csv(!! .x)
  ),
  tar_file_read(
    data_miles_to_school,
    "data/miles-to-school.csv",
    readr::read_csv(!! .x)
  ),
  tar_render(readme, "README.Rmd")
)
