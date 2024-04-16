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