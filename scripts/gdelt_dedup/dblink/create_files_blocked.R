## load libraries
library(readr) # files manip
library(dplyr) # data manip

## load gdelt data
gdelt <- read_csv("data/gdelt/westafrica_conflict_evts_1979-2019_gdelt.csv")
gdelt <- gdelt %>% filter(Year != "Year")

## load conf and run file templates
conf <- read_file("scripts/gdelt_dedup/dblink/template.conf")
run <- read_file("scripts/gdelt_dedup/dblink/run_template.sh")

## perform blocking -- split data by years
gdelt_years <- split(gdelt, gdelt$Year)

## for each year, make files
for(year in names(gdelt_years)) {
  # get nrow
  n <- nrow(gdelt_years[[year]])

  # add row id
  gdelt_years[[year]]$row_id <- seq_len(n)

  # get num partitions/nodes
  n_levels <- min(ceiling(log(n/200, 2)), 6) # max num partitions = 64
  n_nodes <- 2^n_levels

  # get time
  n_hours <- min(ceiling(n/150), 168) # max time = 1 week

  # save each year as its own csv
  write_csv(gdelt_years[[year]], paste0("data/gdelt/blocked/", year, ".csv"))

  # create and write the .conf file
  conf <- gsub("REPLACE_DATAFILE", paste0(year, ".csv"), conf)
  conf <- gsub("REPLACE_NROW", n, conf)
  conf <- gsub("REPLACE_NLEVELS", n_levels, conf)
  conf <- gsub("REPLACE_RESULTSDIR", year, conf)
  write_file(conf, paste0("scripts/gdelt_dedup/dblink/", year, ".conf"))

  # create and write the run .sh file
  run <- gsub("REPLACE_NNODES", n_levels, run)
  run <- gsub("REPLACE_NAME", year, run)
  run <- gsub("REPLACE_TIME", paste0(n_hours, ":00:00"), run)
  run <- gsub("REPLACE_CONF", paste0(year, ".conf"), run)
  write_file(run, paste0("scripts/gdelt_dedup/dblink/run_", year, ".sh"))
  
  # make results folders
  dir.create(file.path("results/dblink/", year))
}
