#' @importFrom purrr safely
#' @importFrom jsonlite fromJSON
.safely_from_json <- purrr::safely(jsonlite::fromJSON, otherwise = NA, quiet = TRUE)



#' Save files to nblr_data release
#'
#' This functions attaches nblr_data attributes like type and timestamp, saves
#' data to a temporary directory in rds format and then uploads to nblr_data
#' repository for a specified release tag.
#'
#' @param df data_frame to save
#' @param file_name file_name to upload as, without the file extension
#' @param release_tag name of release to upload to
#'
#' @export
save_nblr <- function(df, file_name, release_tag) {
  
  Sys.setenv(TZ = "Australia/Melbourne")
  attr(df,"nblr_timestamp") <- Sys.time()
  
  temp_dir <- tempdir(check = TRUE)
  .f_name <- paste0(file_name,".rds")
  saveRDS(df, file.path(temp_dir, .f_name))
  
  piggyback::pb_upload(file.path(temp_dir, .f_name),
                       repo = "JaseZiv/nblr_data",
                       tag = release_tag
                       )
  
}


#' Parse scraped gae list for player names
#'
#' This functions parses scraped NBL match jason for player identifying
#' details including, name, surname, shirt number, etc
#'
#' @param resp list element from scraped json data
#'
#' @export
parse_players <- function(resp) {
  # print(paste0("match id ", resp[["match_id"]]))
  pl_home <- resp[["tm"]][["1"]][["pl"]]
  
  if(rlang::is_null(pl_home) | rlang::is_empty(pl_home)) {
    pl_df <- data.frame()
  } else {
    
    pl_home_df <- purrr::map_df(lapply(pl_home, function(x) data.frame(lapply(x, as.character))), dplyr::bind_rows, .id = "pno")
    
    pl_home_df$match_id <- resp[["match_id"]]
    pl_home_df$home_away <- 1
    
    pl_away <- resp[["tm"]][["2"]][["pl"]]
    pl_away_df <- purrr::map_df(lapply(pl_away, function(x) data.frame(lapply(x, as.character))), dplyr::bind_rows, .id = "pno")
    pl_away_df$match_id <- resp[["match_id"]]
    pl_away_df$home_away <- 2
    
    pl_df <- dplyr::bind_rows(pl_home_df, pl_away_df)
    pl_df <- pl_df %>% 
      janitor::clean_names() %>% 
      dplyr::select(tidyselect::any_of(c("match_id", "home_away", "pno", "first_name", "family_name", "scoreboard_name", 
                      "shirt_number")))
    
  }
  return(pl_df)
}

