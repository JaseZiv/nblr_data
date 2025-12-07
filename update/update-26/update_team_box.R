
current_season <- "2025-2026"

all_season <- readRDS("matches_df.rds")

team_box <- all_season |> 
  # head(1) |> 
  select(id, team_match_statistics, home_team, away_team) |> 
  mutate(missing = mapply(length, team_match_statistics)) |> 
  filter(missing > 0) |> select(-missing) |>  
  mutate(team_match_statistics = map(team_match_statistics, ~ select(.x, -match))) |>
  unnest(team_match_statistics, names_sep = "_") |> 
  unnest()


# $ opp_name                  <chr> "New Zealand Breaker…
# $ opp_short_name            <chr> "Breakers", "36ers",…
# $ opp_score                 <int> 71, 90, 74, 79, 99, …
# $ opp_full_score            <int> 71, 90, 74, 79, 99, …


team_meta <- all_season |> 
  filter(id %in% team_box$id) |> 
  select(id, home_team, away_team, home_team_score=home_score, away_team_score=away_score, team_match_statistics) |> 
  mutate(team_match_statistics = map(team_match_statistics, ~ select(.x, -match))) |>
  unnest(c(home_team, away_team, team_match_statistics), names_sep = "_") |> 
  filter(team_match_statistics_period == "0") |> 
  distinct(id, home_team_id, away_team_id, .keep_all = T)

team_meta <- team_meta |> 
  select(id, team_id=home_team_id, opp_name=away_team_name, opp_short_name=away_team_team_code, opp_score=away_team_score, opp_full_score=away_team_score) |> 
  mutate(home_away = "home") |> 
  bind_rows(
    team_meta |> 
      select(id, team_id=away_team_id, opp_name=home_team_name, opp_short_name=home_team_team_code, opp_score=home_team_score, opp_full_score=home_team_score) |> 
      mutate(home_away = "away")
  ) |> 
  mutate(opp_score = as.numeric(opp_score),
         opp_full_score = as.numeric(opp_full_score))





team_box <- team_box |> 
  select(-team_match_statistics_id, -team_match_statistics_external_id)

names(team_box) <- gsub("team_match_statistics_", "", names(team_box))  


# Rows: 222
# Columns: 47
# $ id                      <chr> "2a64a331-a073-4374-b7…
# $ sort                    <lgl> NA, NA, NA, NA, NA, NA…
# $ user_created            <chr> "7ebce12d-5b4a-4b07-9b…
# $ date_created            <chr> "2025-08-21T08:55:44.0…
# $ user_updated            <chr> "7ebce12d-5b4a-4b07-9b…
# $ date_updated            <chr> "2025-08-21T23:59:44.0…
# $ external_id             <chr> "2276ce0d7e3965130ab34…
# $ match                   <chr> "2a64a331-a073-4374-b7…
# $ period                  <chr> "2", "1", "0", "3", "4…
# $ points                  <int> 26, 41, 116, 27, 22, 1…
# $ turnovers               <int> 1, 1, 7, 4, 1, 2, 5, 1…
# $ rebounds                <int> 11, 12, 42, 8, 11, 10,…
# $ assists                 <int> 5, 8, 26, 8, 5, 4, 7, …
# $ steals                  <int> 1, 1, 7, 3, 2, 1, 2, 4…
# $ blocks                  <int> 1, 1, 3, 0, 1, 0, 0, 6…
# $ offensive_rebounds      <int> 4, 4, 17, 6, 3, 9, 2, …
# $ defensive_rebounds      <int> 7, 8, 25, 2, 8, 1, 5, …
# $ free_throws_attempted   <int> 5, 8, 27, 8, 6, 4, 7, …
# $ free_throws_made        <int> 5, 7, 21, 5, 4, 3, 7, …
# $ minutes                 <int> 50, 50, 200, 50, 50, 5…
# $ two_points_attempted    <int> 11, 7, 38, 11, 9, 14, …
# $ two_points_made         <int> 6, 5, 25, 8, 6, 3, 7, …
# $ two_points_percentage   <dbl> 0.545455, 0.714286, 0.…
# $ three_points_attempted  <int> 10, 11, 38, 8, 9, 8, 4…
# $ three_points_made       <int> 3, 8, 15, 2, 2, 2, 3, …
# $ three_points_percentage <dbl> 0.300000, 0.727273, 0.…
# $ fouls                   <int> 7, 2, 17, 4, 4, 5, 5, …
# $ id1                     <chr> "41c8f340-4d0a-4d68-a3…
# $ name                    <chr> "Melbourne United", "M…
# $ team_logo               <chr> "https://cdn.nbl.com.a…
# $ external_team_logo      <chr> "https://images.dc.pro…
# $ color_primary           <chr> "#000526", "#000526", …
# $ team_code               <chr> "MEL", "MEL", "MEL", "…
# $ team_nickname           <chr> "United", "United", "U…
# $ ticket_url              <chr> "https://premier.ticke…
# $ external_id             <chr> "bf6cf4f0-410c-11f0-81…
# $ division                <lgl> NA, NA, NA, NA, NA, NA…
# $ conference              <lgl> NA, NA, NA, NA, NA, NA…
# $ score                   <int> 116, 116, 116, 116, 11…
# $ season                  <lgl> NA, NA, NA, NA, NA, NA…
# $ free_throws_percentage  <dbl> 1.000000, 0.875000, 0.…
# $ period_type             <chr> "regular", "regular", …
# $ personal_fouls          <int> 7, 2, 17, 4, 4, 5, 5, …
# $ technical_fouls         <int> 0, 0, 0, 0, 0, 0, 0, 0…
# $ field_goals_made        <int> 9, 13, 40, 10, 8, 5, 1…
# $ field_goals_attempted   <int> 21, 18, 76, 19, 18, 22…
# $ field_goals_percentage  <dbl> 0.42857, 0.72222, 0.52…


qtr_points <- team_box |> 
  # select(-external_id) |> 
  filter(period != "0") |> 
  mutate(period_for_score = case_when(
    as.numeric(period) <= 4 ~ paste0("p", period, "_score"),
    as.numeric(period) > 4 ~ "ot_score"
  )) |> 
  group_by(id, team, period_for_score) |> 
  summarise(points = sum(points, na.rm = T), .groups = "drop") |> 
  select(id, team, points, period_for_score) |>
  pivot_wider(names_from = period_for_score, values_from = points)

if(!any(grepl("ot_score", names(qtr_points)))) {
  qtr_points$ot_score <- NA_real_
}





team_box_fixed <- team_box |> filter(team == id1) |> 
  bind_rows(
    team_box |> filter(team == id2) |> 
      mutate(id1=id2, name=name1, team_nickname=team_nickname1, team_code=team_code1)
  )



team_box_updated <- team_box_fixed |> 
  mutate(season = current_season) |> 
  select(-external_id) |> 
  left_join(
    team_meta, by = c("id", "team" = "team_id")
  ) |> 
  left_join(
    qtr_points, by = c("id", "team")
  )

team_box_updated <- team_box_updated |> 
  filter(period == "0") |> 
  mutate(fouls_total=personal_fouls+technical_fouls) |> 
  select(
    match_id = id, season, home_away, name, short_name=team_nickname, code=team_code, score=points, full_score=points, opp_name, opp_short_name,
    opp_score, opp_full_score, p1_score, p2_score, p3_score, p4_score, fouls, minutes, field_goals_made, field_goals_attempted, field_goals_percentage,
    three_pointers_made=three_points_made, three_pointers_attempted=three_points_attempted, three_pointers_percentage=three_points_percentage,
    two_pointers_made=two_points_made, two_pointers_attempted=two_points_attempted, two_pointers_percentage=two_points_percentage,
    free_throws_made, free_throws_attempted, free_throws_percentage, rebounds_defensive=defensive_rebounds, rebounds_offensive=offensive_rebounds, rebounds_total=rebounds,
    assists, turnovers, steals, blocks, fouls_personal=personal_fouls, points, ot_score, fouls_total
  )


existing_team_box <- nblR::nbl_box_team()


final_team_box_out <- bind_rows(
  existing_team_box |> filter(season != current_season) |> mutate(match_id = as.character(match_id)),
  team_box_updated |> filter(season == current_season) |> mutate(minutes = as.character(minutes))
) 



nblscrapeR::save_nblr(df=final_team_box_out, file_name = "box_team", release_tag = "box_team")










