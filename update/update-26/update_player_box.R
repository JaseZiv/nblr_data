
all_season <- readRDS("matches_df.rds")
current_season <- "2025-2026"


player_box <- all_season |> 
  # head(1) |> 
  select(id, player_match_statistics) |> 
  mutate(missing = mapply(length, player_match_statistics)) |> 
  filter(missing > 0) |> select(-missing) |> 
  mutate(player_match_statistics = map(player_match_statistics, ~ select(.x, -match))) |>
  mutate(player_match_statistics = map(player_match_statistics, ~ mutate(.x, field_goals_made = as.integer(field_goals_made)))) |> 
  unnest(player_match_statistics, names_sep = "_") |> 
  unnest() |> 
  select(-starter, -player_match_statistics_active)


existing_player_box <- nblR::nbl_box_player()



team_meta <- all_season |> 
  filter(id %in% player_box$id) |> 
  select(id, home_team, away_team, home_team_score=home_score, away_team_score=away_score, player_match_statistics) |> 
  mutate(player_match_statistics = map(player_match_statistics, ~ select(.x, -match))) |>
  mutate(player_match_statistics = map(player_match_statistics, ~ mutate(.x, field_goals_made = as.integer(field_goals_made)))) |> 
  unnest(c(home_team, away_team, player_match_statistics), names_sep = "_") |> 
  # filter(player_match_statistics == "0") |> 
  distinct(id, home_team_id, away_team_id, .keep_all = T)


team_meta <- team_meta |> 
  select(id, team_id=home_team_id, opp_name=away_team_name, opp_short_name=away_team_team_code, opp_score=away_team_score, opp_full_score=away_team_score) |> 
  mutate(home_away = 1) |> 
  bind_rows(
    team_meta |> 
      select(id, team_id=away_team_id, opp_name=home_team_name, opp_short_name=home_team_team_code, opp_score=home_team_score, opp_full_score=home_team_score) |> 
      mutate(home_away = 2)
  ) 



player_box <- player_box |> 
  select(-player_match_statistics_id, -player_match_statistics_external_id, -player_match_statistics_jersey_number, -player_match_statistics_playing_position)

names(player_box) <- gsub("player_match_statistics_", "", names(player_box))  



player_box_updated <- player_box |> 
  select(-external_id, -date_created, -date_updated, -sort, -user_created, -user_updated) |>
  mutate(season = current_season) |>
  left_join(
    team_meta, by = c("id", "id2" = "team_id")
  )



# player_box_updated <- player_box_updated |> 
#   filter(period == "0") |> 
#   mutate(fouls_total=personal_fouls+technical_fouls) |> 
#   select(
#     match_id = id, season, name, short_name=team_nickname, code=team_code, score=score, full_score=score, opp_name, opp_short_name,
#     opp_score, opp_full_score, p1_score, p2_score, p3_score, p4_score, fouls, minutes, field_goals_made, field_goals_attempted, field_goals_percentage,
#     three_pointers_made=three_points_made, three_pointers_attempted=three_points_attempted, three_pointers_percentage=three_points_percentage,
#     two_pointers_made=two_points_made, two_pointers_attempted=two_points_attempted, two_pointers_percentage=two_points_percentage,
#     free_throws_made, free_throws_attempted, free_throws_percentage, rebounds_defensive=defensive_rebounds, rebounds_offensive=offensive_rebounds, rebounds_total=rebounds,
#     assists, turnovers, steals, blocks, fouls_personal=personal_fouls, points=score, ot_score, fouls_total
#   )




player_box_updated <- player_box_updated |> 
  filter(period == "0") |> 
  mutate(fouls_total=personal_fouls+technical_fouls) |> 
  mutate(participated = as.integer(participated),
         starter = as.integer(starter),
         minutes = as.character(minutes),
         field_goals_made = as.numeric(field_goals_made)) |> 
  select(
    match_id = id, season, team_name=name, home_away, opp_name, opp_short_name, player_id = id1,
    first_name, family_name=last_name, playing_position, starter, shirt_number=jersey_number, minutes, points, field_goals_made, field_goals_attempted, field_goals_percentage,
    three_pointers_made=three_points_made, three_pointers_attempted=three_points_attempted, three_pointers_percentage=three_points_percentage,
    two_pointers_made=two_points_made, two_pointers_attempted=two_points_attempted, two_pointers_percentage=two_points_percentage,
    free_throws_made, free_throws_attempted, free_throws_percentage, rebounds_defensive=defensive_rebounds, rebounds_offensive=offensive_rebounds, rebounds_total=rebounds,
    assists, turnovers, steals, blocks, fouls_personal=fouls_total, plus_minus, efficiency, active=participated, photo_t = external_player_image, photo_s=external_player_image,
    
  )





player_box_out <- bind_rows(
  existing_player_box |> mutate(match_id = as.character(match_id)) |> filter(season != current_season),
  player_box_updated
)




save_nblr(df=player_box_out, file_name = "box_player", release_tag = "box_player")






