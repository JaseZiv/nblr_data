


.each_player_box <- function(resp) {
  print(paste0("match id ", resp[["match_id"]]))
  pl_home <- resp[["tm"]][["1"]][["pl"]]
  
  coerce_to_char <- function(x) {
    x[1][["sMinutes"]] <- as.character(x[1][["sMinutes"]])
    return(x)
  }
  
  if(rlang::is_null(pl_home) | rlang::is_empty(pl_home)) {
    player_box <- data.frame()
  } else {
    
    pl_home <- purrr::map(pl_home, coerce_to_char)
    pl_home_df <- bind_rows(pl_home, .id = "pno")
    pl_home_df <- pl_home_df %>% dplyr::select(-tidyselect::any_of("comp")) %>% dplyr::distinct()
    pl_home_df$match_id <- resp[["match_id"]]
    pl_home_df$home_away <- 1
    # pl_home_df$minutes <- as.character(pl_home_df$minutes)
    
    pl_away <- resp[["tm"]][["2"]][["pl"]]
    pl_away <- purrr::map(pl_away, coerce_to_char)
    pl_away_df <- dplyr::bind_rows(pl_away, .id = "pno")
    pl_away_df <- pl_away_df %>% dplyr::select(-tidyselect::any_of("comp")) %>% dplyr::distinct()
    pl_away_df$match_id <- resp[["match_id"]]
    pl_away_df$home_away <- 2
    # pl_away_df$minutes <- as.character(pl_away_df$minutes)
    
    pl_df <- bind_rows(pl_home_df, pl_away_df)
    
    team_meta <- resp[["tm"]]
    
    team_meta_df <- data.frame(match_id = resp[["match_id"]], 
                               team_name = team_meta[[1]][["name"]], 
                               team_short_name = team_meta[[1]][["shortName"]],
                               home_away = 1, 
                               opp_name = team_meta[[2]][["name"]], 
                               opp_short_name = team_meta[[2]][["shortName"]]) %>% 
      dplyr::bind_rows(
        data.frame(match_id = resp[["match_id"]], 
                   team_name = team_meta[[2]][["name"]], 
                   team_short_name = team_meta[[2]][["shortName"]],
                   home_away = 2, 
                   opp_name = team_meta[[1]][["name"]], 
                   opp_short_name = team_meta[[1]][["shortName"]])
      )
    
    player_box <- team_meta_df %>% 
      left_join(pl_df, by = c("match_id", "home_away")) %>% 
      janitor::clean_names() %>% 
      select(-tidyselect::matches("^eff_"))
    
    names(player_box) <- gsub("^s_", "", names(player_box))
    
    player_box <- player_box %>% 
      select(any_of(c("match_id", "team_name", "team_short_name", "home_away", "opp_name", "opp_short_name", "first_name",
                      "family_name", "scoreboard_name", "playing_position", "starter", "shirt_number", "captain", "minutes", "points")),
             tidyselect::everything(),
             -contains("international"), -pno, -contains("initial"))
    
  }
  
  return(player_box)
  
}


results_wide <- readRDS("data/results/results_wide.rds")
results_wide <- results_wide %>% select(match_id, season)

#----- 2021-22 -----#
games_22 <- readRDS("data/games_2021_22.rds")

player22 <- games_22 %>% 
  purrr::map_df(.each_player_box)

player22 <- player22 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=player22, file_name = "box_player_2021-2022", release_tag = "box_player")


#----- 2020-21 -----#
games_21 <- readRDS("data/games_2020_21.rds")

player21 <- games_21 %>% 
  purrr::map_df(.each_player_box)

player21 <- player21 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=player21, file_name = "box_player_2020-2021", release_tag = "box_player")


#----- 2019-20 -----#
games_20 <- readRDS("data/games_2019_20.rds")

player20 <- games_20 %>% 
  purrr::map_df(.each_player_box)

player20 <- player20 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=player20, file_name = "box_player_2019-2020", release_tag = "box_player")

#----- 2018-19 -----#
games_19 <- readRDS("data/games_2018_19.rds")

player19 <- games_19 %>% 
  purrr::map_df(.each_player_box)

player19 <- player19 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=player19, file_name = "box_player_2018-2019", release_tag = "box_player")

#----- 2017-18 -----#
games_18 <- readRDS("data/games_2017_18.rds")

player18 <- games_18 %>% 
  purrr::map_df(.each_player_box)

player18 <- player18 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=player18, file_name = "box_player_2017-2018", release_tag = "box_player")



#----- 2016-17 -----#
games_17 <- readRDS("data/games_2016_17.rds")

player17 <- games_17 %>% 
  purrr::map_df(.each_player_box)

player17 <- player17 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=player17, file_name = "box_player_2016-2017", release_tag = "box_player")


#----- 2015-16 -----#
games_16 <- readRDS("data/games_2015_16.rds")

player16 <- games_16 %>% 
  purrr::map_df(.each_player_box)

player16 <- player16 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=player16, file_name = "box_player_2015-2016", release_tag = "box_player")
