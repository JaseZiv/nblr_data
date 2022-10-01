
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)


get_seasons <- function() {
  seasons <- jsonlite::fromJSON('https://apicdn.nbl.com.au/nbl/custom/api/genius?route=leagues/7/competitions&competitionName=NBL&fields=season,competitionId&limit=500&filter%5Btenant%5D=nbl')
  seasons <- seasons$data %>% data.frame()
  seasons <- seasons %>% dplyr::arrange(competitionId)
  return(seasons)
}

seasons <- get_seasons



# match data is available from the 2015-16 seasons
.get_matches <- function(season_id) {
  Sys.sleep(2)
  match <- .safely_from_json(paste0('https://apicdn.nbl.com.au/nbl/custom/api/genius?route=competitions/', season_id, '/matches'))
  match <- match$result$data %>% data.frame()
  return(match)
}

get_matches <- purrr::safely(.get_matches, otherwise = data.frame(), quiet = FALSE)


seas <- seasons %>% 
  dplyr::pull(competitionId)


matches_df <- seas %>% 
  purrr::map_df(get_matches)

matches_df <- matches_df %>% tidyr::unnest(result)
# saveRDS(matches_df, "matches_df.rds")


venues <- matches_df %>% 
  dplyr:: select(venue) %>% tidyr::unnest(venue) %>% 
  dplyr::select(venueId, venueName) %>% 
  dplyr::distinct()


results <- matches_df %>% 
  # there was a match in 2006 where only one team (36ers were listed so will remove this)
  dplyr::filter(matchId != 36166) %>% 
  dplyr::select(-externalId, -venue) %>% 
  dplyr::left_join(venues, by = "venueId") %>% 
  dplyr::select(matchId, competitionId, venueName, roundNumber, matchNumber, matchStatus, matchName,
         atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration, matchType, competitors) %>%
  tidyr::unnest(competitors) %>% 
  dplyr::select(-images) %>% 
  dplyr::left_join(seasons, by = c("competitionId")) %>% 
  dplyr::select(matchId, season, venueName, roundNumber, matchNumber, matchStatus, matchName, matchType,
                teamId, teamName, teamNickname, scoreString, isHomeCompetitor, atNeutralVenue, extraPeriodsUsed, matchTime, matchTimeUTC, attendance, duration)

# saveRDS(results, "results.rds")



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


