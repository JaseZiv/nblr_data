
# remotes::install_github("JaseZiv/nblscrapeR")
library(nblscrapeR)
library(dplyr)
library(purrr)
library(tidyr)



all_season_existing <- readRDS("matches_df.rds")
current_season <- "2025-2026"

already_scraped <- all_season_existing |> filter(match_status == "complete") |> pull(id)

# 
# 
# matches_df_existing <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/league/matches_df.rds"))
# matches_df_existing <- matches_df_existing %>% dplyr::filter(season != current_season)


existing_results_wide <- nblR::nbl_results("wide")



matches_df_new <- get_season_matches_df() |> 
  unnest(cols = c(home_team, away_team, venue), names_sep = "_")






new_matches <- matches_df_new |> filter(tolower(match_status) == "complete") |> pull(id)
new_matches <- new_matches[!new_matches %in% (all_season_existing |> filter(tolower(match_status) == "complete") |> pull(id))]

all_season <- data.frame()

for(each_id in 1:length(new_matches)) {
  print(paste0("scraping id ", each_id, " of ", length(new_matches)))
  each <- get_each_match(new_matches[each_id])
  all_season <- bind_rows(all_season, each)
  
}

if(any(grepl("live_match_data", names(all_season)))) {
  all_season <- all_season |> select(-live_match_data)
}



all_season <- bind_rows(
  all_season_existing |> filter(!id %in% all_season$id),
  all_season
) |> 
  arrange(as.Date(start_time_datetime))


saveRDS(all_season, "matches_df.rds")


match_meta_exploded <- all_season |> 
  # select(id, external_id, date_updated, start_time_datetime, match_type, home_score, away_score, season, venue, ) |> 
  select(-last_nexus_update, -team_match_statistics, -player_match_statistics, -contains("broadcaster"), -play_by_play, -team_summary_statistics) |> 
  unnest(season, names_sep = "_") |> 
  unnest(venue, names_sep = "_") |> 
  unnest(home_team, names_sep = "_") |> 
  unnest(away_team, names_sep = "_")



team_box <- all_season |>
  # head(1) |>
  select(id, team_match_statistics) |> mutate(missing = mapply(length, team_match_statistics)) |>
  filter(missing > 0) |> select(-missing) |>
  mutate(team_match_statistics = map(team_match_statistics, ~ select(.x, -match))) |>
  unnest(team_match_statistics, names_sep = "_") |>
  unnest()


# team_box <- purrr::map_df(team_box, unnest_each_team_box)


# we can infer the number of extra periods used from the team statgistics df:
# team_box |> 
#   group_by(id) |> 
#   summarise(extra_periods_used = max(as.numeric(team_match_statistics_period)) - 4)

extra_time <- team_box |> 
  distinct(id, team_match_statistics_minutes, team_match_statistics_period) |> 
  group_by(id) |> 
  summarise(duration = max(team_match_statistics_minutes, na.rm = T),
            extra_periods_used = ((team_match_statistics_minutes[team_match_statistics_period == "0"] / 5) - 40) / 5) |> 
  ungroup()




# now we need to make the 2025-26 season data backward compatible
matches_df_new <- match_meta_exploded |> 
  # left_join(extra_time, by = "id") |>
  mutate(match_id = as.integer(external_media_id),
         season = "2025-2026",
         round_number = round) |> 
  arrange(start_time) |> 
  mutate(match_number = row_number()) |> 
  mutate(match_status = toupper(match_status),
         match_name = NA_character_,
         match_type = toupper(match_type),
         home_team_nickname = home_team_team_nickname,
         home_score_string = as.character(home_score),
         away_team_nickname = away_team_team_nickname,
         away_score_string = as.character(away_score),
         at_neutral_venue = NA_integer_,
         # extra_periods_used = NA_integer_,
         # duration = NA_integer_,
         match_time = NA_character_,
         match_time_utc = start_time_datetime,
         attendance = NA_integer_) |> 
  select(match_id=id, 
         external_id, external_media_id, season, venue_name, round_number, match_number, match_status, match_name, match_type, home_team_id, 
         home_team_name, home_team_nickname, home_score_string, away_team_id, away_team_name, away_team_nickname, away_score_string, 
         at_neutral_venue, 
         # extra_periods_used,
         match_time, match_time_utc, attendance, 
         # duration, 
         home_team_team_logo, home_team_external_team_logo, away_team_team_logo, away_team_external_team_logo)


updated <- bind_rows(
  existing_results_wide |> mutate(match_id = as.character(match_id)) |> filter(season != current_season),
  # matches_df_new_preseason,
  matches_df_new  |> filter(season == current_season)
)


library(nblscrapeR)
save_nblr(df=updated, file_name = "results_wide", release_tag = "match_results")


current_season_df <- updated |> filter(season == current_season)

results_meta <- current_season_df %>% 
  select(match_id, season, venue_name, round_number, match_number, match_status, match_name, match_type,
         at_neutral_venue, extra_periods_used, match_time, match_time_utc, attendance, duration) %>% distinct()

results_home <- current_season_df %>% 
  dplyr::mutate(isHomeCompetitor = "1") %>%
  dplyr::select(match_id, teamId=home_team_id, teamName=home_team_name, teamNickname=home_team_nickname, scoreString=home_score_string, isHomeCompetitor) %>% 
  dplyr::bind_cols(
    current_season_df %>% 
      # dplyr::mutate(isHomeCompetitor = 0) %>% 
      dplyr::select(oppTeamId=away_team_id, oppTeamName=away_team_name, 
                    oppTeamNickname=away_team_nickname, oppScoreString=away_score_string)
  )


results_away <- current_season_df %>% 
  dplyr::mutate(isHomeCompetitor = "0") %>%
  dplyr::select(match_id, teamId=away_team_id, teamName=away_team_name, teamNickname=away_team_nickname, scoreString=away_score_string, isHomeCompetitor) %>% 
  dplyr::bind_cols(
    current_season_df %>% 
      # dplyr::mutate(isHomeCompetitor = 1) %>% 
      dplyr::select(oppTeamId=home_team_id, oppTeamName=home_team_name, 
                    oppTeamNickname=home_team_nickname, oppScoreString=home_score_string)
  )


results_long <- results_meta %>% 
  left_join(bind_rows(results_home, results_away), by = "match_id")

results_long <- results_long %>% 
  dplyr::select(match_id, season, venue_name, round_number, match_number, match_status, match_name, match_type,
                teamId, teamName, teamNickname, scoreString, isHomeCompetitor,
                oppTeamId, oppTeamName, oppTeamNickname, oppScoreString,
                at_neutral_venue, extra_periods_used, match_time, match_time_utc, attendance, duration) %>% 
  dplyr::arrange(match_time_utc, match_number)

results_long <- janitor::clean_names(results_long)


existing_results_long <- nblR::nbl_results("long")

results_long_updated <- bind_rows(
  existing_results_long |> filter(season != current_season) |> mutate(match_id = as.character(match_id)),
  results_long |> filter(season == current_season)
)


save_nblr(df=results_long_updated, file_name = "results_long", release_tag = "match_results")




