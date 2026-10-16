library(DBI)
library(RPostgres)
library(tidyverse)
library(glue)
library(httr)
library(jsonlite)


source("backend/data_extract_functions/extract_data_from_database.r")
source("backend/data_transform_functions/pitcher_scouting_report_builder_functions.r")
source("backend/data_load_functions/load_data_to_database.r")
source("backend/data_pipelines/pitcher_scouting_report_builder_pipeline.r")

run_pitcher_scouting_report_builder()
