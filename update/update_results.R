
# remotes::install_github("JaseZiv/nblscrapeR")
library(nblscrapeR)
library(dplyr)
library(purrr)
library(tidyr)

current_season <- "2023-2024"


seasons <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/league/seasons.rds"))

seas <- seasons %>% 
  # dplyr::filter(season == current_season) %>%
  dplyr::pull(competitionId)


matches_df_existing <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/league/matches_df.rds"))
matches_df_existing <- matches_df_existing %>% dplyr::filter(season != current_season)


matches_df_new <- seas %>% 
  purrr::map_df(nblscrapeR::get_matches)




get_comp_meta <- function(x) {
  x |> 
    data.frame() |> 
    select(teamId, teamName, teamNickname, scoreString, isHomeCompetitor)
}


competitors_df <- matches_df_new |> 
  mutate(matchId = as.integer(matchId)) |> 
  select(matchId, competitors)


comps <- data.frame()

for(i in 1:nrow(competitors_df)) {
  id <- competitors_df$matchId[i]
  comp_df <- get_comp_meta(competitors_df$competitors[i])
  comp_df$matchId <- id
  comps <- bind_rows(comps, comp_df)
}


comps <- comps |> 
  select(matchId, teamId, teamName, teamNickname, scoreString, isHomeCompetitor) |> 
  mutate(across(c(teamId, teamName, teamNickname, scoreString, isHomeCompetitor), as.character))



matches_df_new <- matches_df_new |> 
  mutate(across(c(matchId, matchNumber, leagueId, competitionId), .fns = as.integer)) |> 
  mutate(across(c(roundNumber, matchName, matchStatus, matchTime, matchTimeUTC, matchType), .fns = as.character)) |>
  dplyr::left_join(seasons, by = c("competitionId")) |> 
  tidyr::unnest(venue) |>
  dplyr::left_join(comps, by = "matchId") |> 
  dplyr::select(matchId, season, venueName, roundNumber, matchNumber, matchStatus, matchName, matchType,
         teamId, teamName, teamNickname, scoreString, isHomeCompetitor, atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration) |> 
  mutate(across(c(venueName, atNeutralVenue), as.character)) |> 
  mutate(across(c(atNeutralVenue, extraPeriodsUsed, attendance, duration), as.integer))






matches_df <- matches_df_existing |> 
  dplyr::bind_rows(matches_df_new) %>% 
  dplyr::arrange(matchTimeUTC) |> 
  dplyr::select(matchId, season, venueName, roundNumber, matchNumber, matchStatus, matchName, matchType,
                teamId, teamName, teamNickname, scoreString, isHomeCompetitor, atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration)


nblscrapeR::save_nblr(matches_df, "matches_df", "league")


# venues <- matches_df %>% 
#   # dplyr:: select(venue) %>% tidyr::unnest(venue) %>% 
#   dplyr::select(venueId, venueName) %>% 
#   dplyr::distinct()
# 
# 
# test <- bbb |> 
#   select(-`website`, -`externalId`) |> 
#   unnest(competitors)


results <- matches_df %>% 
  # there was a match in 2006 where only one team (36ers were listed so will remove this)
  dplyr::filter(matchId != 36166) |> 
  distinct(matchId, teamName, .keep_all = T)

# saveRDS(results, "results.rds")

# temporarily, while the playoff schedules are dynamicaly being set as they progress, will need to 
# manually filter out some rows which are throwing errors with too many isHomCompetitor values == 1.
# once the finals are scheduled/played, I can remove this, but for now, dynamically filter the date:
# results <- results %>% 
#   filter(matchTime < '2023-02-15 19:30:00')

results_meta <- results %>% 
  select(matchId, season, venueName, roundNumber, matchNumber, matchStatus, matchName, matchType,
         atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration) %>% distinct()


results_home <- results %>% 
  dplyr::filter(isHomeCompetitor == 1) %>% 
  dplyr::select(matchId, teamId, teamName, teamNickname, scoreString, isHomeCompetitor) %>% 
  dplyr::bind_cols(
    results %>% 
      dplyr::filter(isHomeCompetitor == 0) %>% 
      dplyr::select(oppTeamId=teamId, oppTeamName=teamName, 
                    oppTeamNickname=teamNickname, oppScoreString=scoreString)
  )


results_away <- results %>% 
  dplyr::filter(isHomeCompetitor == 0) %>% 
  dplyr::select(matchId, teamId, teamName, teamNickname, scoreString, isHomeCompetitor) %>% 
  dplyr::bind_cols(
    results %>% 
      dplyr::filter(isHomeCompetitor == 1) %>% 
      dplyr::select(oppTeamId=teamId, oppTeamName=teamName, 
                    oppTeamNickname=teamNickname, oppScoreString=scoreString)
  )


results_long <- results_meta %>% 
  left_join(bind_rows(results_home, results_away), by = "matchId")

results_long <- results_long %>% 
  dplyr::select(matchId, season, venueName, roundNumber, matchNumber, matchStatus, matchName, matchType,
                teamId, teamName, teamNickname, scoreString, isHomeCompetitor,
                oppTeamId, oppTeamName, oppTeamNickname, oppScoreString,
                atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration) %>% 
  dplyr::arrange(matchTime, matchNumber)

results_long <- janitor::clean_names(results_long)

save_nblr(df=results_long, file_name = "results_long", release_tag = "match_results")


results_wide <- results %>% 
  dplyr::filter(isHomeCompetitor == 1) %>% 
  dplyr::select(matchId, season, venueName, roundNumber, matchNumber, matchStatus, matchName, matchType,
                homeTeamId=teamId, homeTeamName=teamName, homeTeamNickname=teamNickname, homeScoreString=scoreString,
                atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration) %>% 
  dplyr::bind_cols(
    results %>% 
      dplyr::filter(matchId != 36166) %>%
      dplyr::filter(isHomeCompetitor == 0) %>% 
      dplyr::select(awayTeamId=teamId, awayTeamName=teamName, 
                    awayTeamNickname=teamNickname, awayScoreString=scoreString)
  ) %>% 
  dplyr::select(matchId, season, venueName, roundNumber, matchNumber, matchStatus, matchName, matchType,
                homeTeamId, homeTeamName, homeTeamNickname, homeScoreString,
                awayTeamId, awayTeamName, awayTeamNickname, awayScoreString,
                atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration) %>% 
  dplyr::arrange(matchTime, matchNumber)

results_wide <- janitor::clean_names(results_wide)


save_nblr(df=results_wide, file_name = "results_wide", release_tag = "match_results")


rm(list = ls())


