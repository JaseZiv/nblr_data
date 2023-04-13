


library(dplyr)
library(piggyback)

# function to call to save to GitHub Releases
save_to_rel <- function(df, file_name, ext, release_tag) {
  
  Sys.setenv(TZ = "Australia/Melbourne")
  
  temp_dir <- tempdir(check = TRUE)
  .f_name <- paste0(file_name,".", ext)
  
  if(ext == "csv") {
    write.csv(df, file.path(temp_dir, .f_name), row.names = F)
  } else {
    saveRDS(df, file.path(temp_dir, .f_name))
  }
  
  piggyback::pb_upload(file.path(temp_dir, .f_name),
                       repo = "JaseZiv/nblr_data",
                       tag = release_tag
  )
  
}


# function to read in data from releases:
read_from_rel <- function(file_name, ext, release_tag) {
  
  piggyback::pb_download(paste0(file_name, ".", ext),
                         repo = "JaseZiv/nblr_data",
                         tag = release_tag,
                         dest = tempdir())
  if(ext == "csv") {
    read.csv(paste0(tempdir(), "/", file_name, ".csv"))
  } else {
    readRDS(paste0(tempdir(), "/", file_name, ".rds"))
  }
  
  
}


team_box <- read_from_rel(file_name = "box_team", ext = "rds", release_tag = "box_team")
save_to_rel(df=team_box, file_name = "box_team", ext = "csv", release_tag = "box_team")


player_box <- read_from_rel(file_name = "box_player", ext = "rds", release_tag = "box_player")
save_to_rel(df=player_box, file_name = "box_player", ext = "csv", release_tag = "box_player")


pbp <- read_from_rel(file_name = "pbp", ext = "rds", release_tag = "pbp")
save_to_rel(df=player_box, file_name = "pbp", ext = "csv", release_tag = "pbp")

results <- read_from_rel(file_name = "results_wide", ext = "rds", release_tag = "match_results")
save_to_rel(df=results, file_name = "results_wide", ext = "csv", release_tag = "match_results")


shots <- read_from_rel(file_name = "shots", ext = "rds", release_tag = "shots")
save_to_rel(df=shots, file_name = "shots", ext = "csv", release_tag = "shots")




