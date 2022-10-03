
library(nblscrapeR)
library(dplyr)
library(purrr)

results_wide <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_results/results_wide.rds"))

current_season <- "2022-2023"

match_id <- results_wide %>%
  dplyr::filter(season ==current_season) %>%
  dplyr::filter(match_status == "COMPLETE") %>% 
  dplyr::pull(match_id) %>% unique()

match_lists <- match_id %>%
  purrr::map(nblscrapeR::get_games)

save_nblr(df=match_lists, file_name = "match_lists_2022_23", release_tag = "match_lists")

rm(list = ls());gc()
