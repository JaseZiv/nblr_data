library(dplyr)
library(tidyr)
library(httr)
library(nblscrapeR)


# Functions ---------------------------------------------------------------

get_nbl_team <- function(season_code, team_id) {
  
  headers = c(
    `sec-ch-ua` = '"Google Chrome";v="123", "Not:A-Brand";v="8", "Chromium";v="123"',
    `Content-Type` = "application/json",
    `Referer` = "https://www.nbl.com.au/",
    `sec-ch-ua-mobile` = "?0",
    `x-quark-req-src` = "web-nbl",
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    `sec-ch-ua-platform` = '"macOS"'
  )
  
  res <- httr::GET(url = paste0("https://prod.services.nbl.com.au/api_cache/nbl/genius?route=competitions/", season_code, "/teams/", team_id, "/persons"), httr::add_headers(.headers=headers)) |> 
    httr::content()
  
  return(res)
}


parse_teams <- function(team_list) {
  
  raw_df <- cbind(team_list) |> 
    data.frame() |> 
    unnest_wider(col=1)
  
  clean_df <- raw_df |> 
    # select(personId, firstName, familyName, scoreboardName, dob, nickName, height, weight, dominantHand, 
    #        dominantFoot, nationality, current_team_id=primaryTeamId, current_team_name=primaryTeamName,
    #        competition_player, images) |> 
    unnest_wider(col = c(competition_player, images)) |> 
    unnest_wider(col = photo) |> 
    unnest_wider(c(`L1`, `M1`, `S1`), names_sep = "_") |> 
    select(playerTeamId, personId, firstName, familyName, dob, height, weight, dominantHand,
           dominantFoot, nationality, current_team_id=primaryTeamId, current_team_name=primaryTeamName,
           L1_url, M1_url, S1_url)
  
  return(clean_df)
}


# Setup -------------------------------------------------------------------

source(here::here("R", "environment.R"))

current_season <- "2023-2024"

matches_df <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/league/matches_df.rds")) |> 
  filter(season == current_season)



seasons <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/league/seasons.rds")) |> 
  filter(season == current_season)


teams <- matches_df |> 
  select(teamId, teamName, season) |> 
  distinct(.keep_all = T) |> 
  arrange(season, teamName) |> 
  left_join(
    seasons,
    by = c("season")
  )



# Scrape and Save ---------------------------------------------------------

test_df <- data.frame()


for(i in 1:nrow(teams)) {
  print(paste0("scraping ", teams$teamName[i], " for the ", teams$season[i], " season"))
  Sys.sleep(2)
  team_list <- get_nbl_team(season_code = teams$competitionId[i], team_id = teams$teamId[i])
  df <- parse_teams(team_list)
  df <- bind_cols(
    teams[i,],
    df
  )
  
  test_df <- bind_rows(
    test_df, df
  )
}



existing <- read_from_rel(file_name = "nbl_player_bios", ext = "rds", release_tag = "csvs")

existing <- existing |> 
  filter(season != current_season)


out <- bind_rows(
  test_df, existing
)


save_to_rel(df=out, file_name = "nbl_player_bios", ext = "csv", release_tag = "csvs")
save_to_rel(df=out, file_name = "nbl_player_bios", ext = "rds", release_tag = "csvs")









