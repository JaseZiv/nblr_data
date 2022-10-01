

#===================================================
#----- Play-by-Play -----#

.each_pbp <- function(resp) {
  
  # resp <- resp[[1]]
  
  pbp <- resp[["pbp"]]
  
  if(rlang::is_null(pbp) | rlang::is_empty(pbp)) {
    pbp <- data.frame()
  } else {
    
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
    
    
    pbp$match_id <- resp[["match_id"]]
    pbp <- janitor::clean_names(pbp)
    
    
    pbp <- pbp %>% 
      dplyr::left_join(team_meta_df, by = c("match_id", "tno"="home_away")) %>% 
      dplyr::mutate(home_away = dplyr::case_when(
        tno == 1 ~ "home",
        tno == 2 ~ "away",
        tno == 0 ~ NA_character_
      )) %>% 
      dplyr::select(tidyselect::any_of(c("match_id", "action_number", "team_name", "team_short_name", "home_away", "opp_name", "opp_short_name", 
                                         "period_type", "period", "gt", "s1", "s2", "lead", "pno", "first_name", "family_name", "scoreboard_name", 
                                         "shirt_number", "action_type", "sub_type", "success", "qualifier", "previous_action", "scoring")))
    
    if(any(grepl("qualifier", names(pbp)))) {
      pbp <- pbp %>% mutate(qualifier = as.list(qualifier))
    }
    
  }
  return(pbp)
  
}

results_wide <- readRDS("data/results/results_wide.rds")
results_wide <- results_wide %>% select(match_id, season)


games_22 <- readRDS("data/games_2021_22.rds")
pbp_df22 <- games_22 %>% 
  purrr::map_df(.each_pbp)

pbp_df22 <- pbp_df22 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=pbp_df22, file_name = "pbp_2021-2022", release_tag = "pbp")


games_21 <- readRDS("data/games_2020_21.rds")
pbp_df21 <- games_21 %>% 
  purrr::map_df(.each_pbp)

pbp_df21 <- pbp_df21 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=pbp_df21, file_name = "pbp_2020-2021", release_tag = "pbp")


games_20 <- readRDS("data/games_2019_20.rds")
pbp_df20 <- games_20 %>% 
  purrr::map_df(.each_pbp)

pbp_df20 <- pbp_df20 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=pbp_df20, file_name = "pbp_2019-2020", release_tag = "pbp")


games_19 <- readRDS("data/games_2018_19.rds")
pbp_df19 <- games_19 %>% 
  purrr::map_df(.each_pbp)

pbp_df19 <- pbp_df19 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=pbp_df19, file_name = "pbp_2018-2019", release_tag = "pbp")


games_18 <- readRDS("data/games_2017_18.rds")
pbp_df18 <- games_18 %>% 
  purrr::map_df(.each_pbp)

pbp_df18 <- pbp_df18 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=pbp_df18, file_name = "pbp_2017-2018", release_tag = "pbp")


games_17 <- readRDS("data/games_2016_17.rds")
pbp_df17 <- games_17 %>% 
  purrr::map_df(.each_pbp)

pbp_df17 <- pbp_df17 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=pbp_df17, file_name = "pbp_2016-2017", release_tag = "pbp")


games_2015_16 <- readRDS("data/games_2015_16.rds")
pbp_df16 <- games_2015_16 %>% 
  purrr::map_df(.each_pbp)

pbp_df16 <- pbp_df16 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())


# because 2016 didn't include player names in the pbp data, need to extract those separately:
players_16 <- games_2015_16 %>% 
  purrr::map_df(parse_players)

players_16$pno <- as.numeric(players_16$pno)

pbp_df16_full <- pbp_df16 %>% 
  select(-first_name, -family_name, -scoreboard_name, -shirt_number) %>% 
  mutate(tno = as.numeric(ifelse(home_away == "home", 1, ifelse(home_away=="away", 2, home_away)))) %>% 
  left_join(players_16, by = c("match_id", "pno", "tno"="home_away")) %>% 
  distinct() %>% 
  select(-tno)

save_nblr(df=pbp_df16_full, file_name = "pbp_2015-2016", release_tag = "pbp")


# pbp_all <- bind_rows(pbp_df22, pbp_df21, pbp_df20, pbp_df19, pbp_df18, pbp_df17, pbp_df16_full)
# saveRDS(pbp_all, "data/pbp/pbp_16-22.rds")


