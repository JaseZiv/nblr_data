library(nblscrapeR)
library(dplyr)
library(purrr)

# results_wide <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_results/results_wide.rds"))
# results_wide <- results_wide %>% select(match_id, season)

current_season <- "2022-2023"

games <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2022_23.rds"))

#==========================================================================
# Play-by-Play ------------------------------------------------------------

# get current
pbp_df <- nblscrapeR::parse_pbp(games)

# get all existing
pbp_all <- nblR::nbl_pbp(season = c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022", "2022-2023"))

# filter out the new
pbp_previous <- pbp_all %>% filter(season != current_season)

# then add back in the new
pbp_df <- bind_rows(pbp_previous, pbp_df)

# save
save_nblr(df=pbp_df, file_name = "pbp", release_tag = "pbp")

rm(pbp_df, pbp_all, pbp_previous);gc()


#==========================================================================
# Player Box Scores -------------------------------------------------------

# get current
player <- nblscrapeR::parse_player_box(games)

# get all existing
player_all <- nblR::nbl_box_player(season = c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022", "2022-2023"))

# filter out the new
player_previous <- player_all %>% filter(season != current_season)

# then add back in the new
player <- bind_rows(player_previous, player)

save_nblr(df=player, file_name = "box_player", release_tag = "box_player")

rm(player, player_all, player_previous);gc()

#==========================================================================
# Team Box Scores ---------------------------------------------------------

# get current
team_box <- nblscrapeR::parse_team_box(games)

# get all existing
team_all <- nblR::nbl_box_team(season = c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022", "2022-2023"))

# filter out the new
team_previous <- team_all %>% filter(season != current_season)

# then add back in the new
team_box <- bind_rows(team_previous, team_box)

save_nblr(df=team_box, file_name = "box_team", release_tag = "box_team")

rm(team_box, team_all, team_previous);gc()


#==========================================================================
# Match Shots -------------------------------------------------------------
shots <- nblscrapeR::parse_match_shot(games)
save_nblr(df=shots, file_name = "shots_2022-2023", release_tag = "shots")
rm(shots)


# get current
shots <- nblscrapeR::parse_match_shot(games)

# get all existing
shots_all <- nblR::nbl_shots(season = c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022", "2022-2023"))

# filter out the new
shots_previous <- shots_all %>% filter(season != current_season)

# then add back in the new
shots <- bind_rows(shots_previous, shots)

save_nblr(df=shots, file_name = "shots", release_tag = "shots")

rm(shots, shots_previous, shots_all);gc()

