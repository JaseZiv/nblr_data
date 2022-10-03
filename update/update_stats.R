library(nblscrapeR)
library(dplyr)
library(purrr)

# results_wide <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_results/results_wide.rds"))
# results_wide <- results_wide %>% select(match_id, season)

current_season <- "2022-2023"

games <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2022_23.rds"))

#==========================================================================
# Play-by-Play ------------------------------------------------------------

pbp_df <- nblscrapeR::parse_pbp(games)

save_nblr(df=pbp_df, file_name = "pbp_2022-2023", release_tag = "pbp")

rm(pbp_df)


#==========================================================================
# Player Box Scores -------------------------------------------------------

player <- nblscrapeR::parse_player_box(games)

save_nblr(df=player, file_name = "box_player_2022-2023", release_tag = "box_player")

rm(player)

#==========================================================================
# Team Box Scores ---------------------------------------------------------

team_box <- nblscrapeR::parse_team_box(games)

save_nblr(df=team_box, file_name = "box_team_2022-2023", release_tag = "box_team")

rm(team_box)


#==========================================================================
# Match Shots -------------------------------------------------------------
shots <- nblscrapeR::parse_match_shot(games)
save_nblr(df=shots, file_name = "shots_2022-2023", release_tag = "shots")
rm(shots)

