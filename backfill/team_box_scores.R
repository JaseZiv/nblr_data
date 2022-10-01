

.each_team_box <- function(resp) {
  # print(paste0("match id ", resp$match_id))
  rems <- c("logoT", "logoS", "pl", "shot", "lds", "scoring")
  
  
  home <- resp[["tm"]][[1]]
  if(length(home) == 0) {
    df <- data.frame()
  } else {
    home <- home[-which(names(home) %in% rems)]
    home <- data.frame(home) 
    home <- home %>% dplyr::select(-tidyselect::matches("logo|nameInternational|codeInternational|coach|asst|scoring", perl=TRUE))
    names(home) <- gsub("tot_s", "", names(home)) %>% janitor::make_clean_names()
    tryCatch( {home$minutes <- as.character(home$minutes)}, error = function(e) {home$minutes <- NA_character_})
    
    
    away <- resp[["tm"]][[2]]
    away <- away[-which(names(away) %in% rems)]
    away <- data.frame(away) 
    away <- away %>% dplyr::select(-tidyselect::matches("logo|nameInternational|codeInternational|coach|asst", perl=TRUE))
    names(away) <- gsub("tot_s", "", names(away)) %>% janitor::make_clean_names()
    tryCatch( {away$minutes <- as.character(away$minutes)}, error = function(e) {away$minutes <- NA_character_})
    
    home$home_away <- "home"
    home$opp_name <- away$name
    home$opp_short_name <- away$short_name
    home$opp_score <- away$score
    home$opp_full_score <- away$full_score
    home$match_id <- resp$match_id
    
    
    
    away$home_away <- "away"
    away$opp_name <- home$name
    away$opp_short_name <- home$short_name
    away$opp_score <- home$score
    away$opp_full_score <- home$full_score
    away$match_id <- resp$match_id
    
    df <- dplyr::bind_rows(home, away)
    # df <- df %>% 
    #   select(match_id, home_away, name, short_name, score, full_score, opp_name, opp_short_name, opp_score, opp_full_score, tidyr::everything())
    
    # df <- df %>% 
    #   select(tidyselect::matches("match_id|home_away|name|short_name|score|full_score|opp_name|opp_short_name|opp_score|opp_full_score"), tidyr::everything()) %>% 
    #   dplyr::select(match_id, tidyr::everything())
    
    df <- df %>% 
      select(tidyselect::any_of(c("match_id", "home_away", "name", "short_name", "code", "score", "full_score", 
                                "opp_name", "opp_short_name", "opp_score", "opp_full_score")), 
             tidyselect::everything()) 
  }
  
  return(df)
}


results_wide <- readRDS("data/results/results_wide.rds")
results_wide <- results_wide %>% select(match_id, season)


games_22 <- readRDS("data/games_2021_22.rds")
df22 <- games_22 %>% 
  purrr::map_df(.each_team_box)

df22 <- df22 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=df22, file_name = "box_team_2021-2022", release_tag = "box_team")



games_21 <- readRDS("data/games_2020_21.rds")
df21 <- games_21 %>% 
  purrr::map_df(.each_team_box)

df21 <- df21 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=df21, file_name = "box_team_2020-2021", release_tag = "box_team")


games_20 <- readRDS("data/games_2019_20.rds")
df20 <- games_20 %>% 
  purrr::map_df(.each_team_box)

df20 <- df20 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=df20, file_name = "box_team_2019-2020", release_tag = "box_team")


games_19 <- readRDS("data/games_2018_19.rds")
df19 <- games_19 %>% 
  purrr::map_df(.each_team_box)

df19 <- df19 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=df19, file_name = "box_team_2018-2019", release_tag = "box_team")



games_18 <- readRDS("data/games_2017_18.rds")
df18 <- games_18 %>% 
  purrr::map_df(.each_team_box)

df18 <- df18 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=df18, file_name = "box_team_2017-2018", release_tag = "box_team")

games_17 <- readRDS("data/games_2016_17.rds")
df17 <- games_17 %>% 
  purrr::map_df(.each_team_box)

df17 <- df17 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=df17, file_name = "box_team_2016-2017", release_tag = "box_team")


games_2015_16 <- readRDS("data/games_2015_16.rds")
df16 <- games_2015_16 %>% 
  purrr::map_df(.each_team_box)

df16 <- df16 %>% distinct() %>%
  left_join(results_wide, by = "match_id") %>%
  select(match_id, season, tidyselect::everything())

save_nblr(df=df16, file_name = "box_team_2015-2016", release_tag = "box_team")



# team_box <- bind_rows(df22, df21, df20, df19, df18, df17, df16)

