

check_status <- function(res) {
  
  x = httr::status_code(res)
  
  if(x != 200) stop("The API returned an error", call. = FALSE)
  
}

get_games <- function(match_id) {
  # print(paste0("getting match ", match_id))
  
  tryCatch({
  res <- httr::RETRY("GET", paste0("https://fibalivestats.dcd.shared.geniussports.com/data/", match_id, "/data.json"))
  
  # Check the result
  # check_status(res)
  
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  
  resp <- jsonlite::fromJSON(resp)
  resp$match_id <- match_id
  return(resp)
  }, error=function(e){})
}



match_id <- results_wide %>%
  dplyr::filter(season == "2021-2022") %>%
  dplyr::pull(match_id) %>% unique()

match_lists_2021_22 <- match_id %>%
  purrr::map(get_games)

save_nblr(df=match_lists_2015_16, file_name = "match_lists_2015_16", release_tag = "match_lists")



match_id <- results_wide %>%
  dplyr::filter(season == "2020-2021") %>%
  dplyr::pull(match_id) %>% unique()

match_lists_2020_21 <- match_id %>%
  purrr::map(get_games)

save_nblr(df=match_lists_2020_21, file_name = "match_lists_2020_21", release_tag = "match_lists")


match_id <- results_wide %>%
  dplyr::filter(season == "2019-2020") %>%
  dplyr::pull(match_id) %>% unique()

match_lists_2019_20 <- match_id %>%
  purrr::map(get_games)

save_nblr(df=match_lists_2019_20, file_name = "match_lists_2019_20", release_tag = "match_lists")


match_id <- results_wide %>%
  dplyr::filter(season == "2018-2019") %>%
  dplyr::pull(match_id) %>% unique()

match_lists_2018_19 <- match_id %>%
  purrr::map(get_games)

save_nblr(df=match_lists_2018_19, file_name = "match_lists_2018_19", release_tag = "match_lists")


match_id <- results_wide %>%
  dplyr::filter(season == "2017-2018") %>%
  dplyr::pull(match_id) %>% unique()

match_lists_20171_18 <- match_id %>%
  purrr::map(get_games)

save_nblr(df=match_lists_2017_18, file_name = "match_lists_2017_18", release_tag = "match_lists")



match_id <- results_wide %>%
  dplyr::filter(season == "2016-2017") %>%
  dplyr::pull(match_id) %>% unique()

match_lists_2016_17 <- match_id %>%
  purrr::map(get_games)

save_nblr(df=match_lists_2016_17, file_name = "match_lists_2016_17", release_tag = "match_lists")



match_id <- results_wide %>%
  dplyr::filter(season == "2015-20162") %>%
  dplyr::pull(match_id) %>% unique()

match_lists_2015_16 <- match_id %>%
  purrr::map(get_games)

save_nblr(df=match_lists_2015_16, file_name = "match_lists_2015_16", release_tag = "match_lists")
