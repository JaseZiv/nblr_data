
all_season <- readRDS("matches_df.rds")
current_season <- "2025-2026"

# there is a game that the json structure is different for, and it's causing all sorts of headaches...
# for now I'll remove it, knowing that it'll need to be fixed somehow:
bad_pbp <- c("b1b1def4-4bef-11f0-8dce-5f21da19a0fe", "b1c59937-4bef-11f0-9850-69185882ba54")

team_pbp <- all_season |> 
  filter(!id %in% bad_pbp) |> 
  # head(1) |> 
  select(id, play_by_play) |> 
  mutate(missing = mapply(length, play_by_play)) |> 
  filter(missing > 0) |> select(-missing) |>  
  unnest(play_by_play, names_sep = "_") |> 
  unnest()


# $ opp_name                  <chr> "New Zealand Breaker…
# $ opp_short_name            <chr> "Breakers", "36ers",…
# $ opp_score                 <int> 71, 90, 74, 79, 99, …
# $ opp_full_score            <int> 71, 90, 74, 79, 99, …


team_meta <- all_season |> 
  filter(id %in% team_pbp$id) |> 
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
  ) 


pbp_start <- all_season |> 
  filter(!id %in% bad_pbp) |> 
  select(id, play_by_play) |> 
  mutate(missing = mapply(length, play_by_play)) |> 
  filter(missing > 0) |> select(-missing) |>  
  unnest(play_by_play, names_sep = "_") |> 
  unnest() |> 
  unnest(cols = team, names_sep = "_")

names(pbp_start) <- gsub("play_by_play_", "", names(pbp_start))

existing_pbp <- nblR::nbl_pbp()

names(pbp_start) |> paste0(collapse = ", ")
"id, match, action_id, period, period_type, score_1, score_2, action_type, sub_type, x, y, clock, shot_clock, timestamp, 
readable_action_type, system_action_type, id1, external_id, jersey_number, first_name, last_name, image, team_id, 
team_external_id, team_name, team_team_nickname, team_league, team_gender, team_team_code, team_team_logo, personId, success"


names(existing_pbp) |> paste0(collapse = ", ")
"match_id, season, team_name, team_short_name, home_away, opp_name, opp_short_name, period_type, period, gt, s1, s2, lead, pno, 
action_type, sub_type, success, scoring, first_name, family_name, shirt_number, scoreboard_name, qualifier, action_number, 
previous_action"

player_box <- all_season |> 
  # head(1) |> 
  select(id, player_match_statistics) |> 
  mutate(missing = mapply(length, player_match_statistics)) |> 
  filter(missing > 0) |> select(-missing) |> 
  mutate(player_match_statistics = map(player_match_statistics, ~ select(.x, -match))) |>
  mutate(player_match_statistics = map(player_match_statistics, ~ mutate(.x, field_goals_made = as.integer(field_goals_made)))) |> 
  unnest(player_match_statistics, names_sep = "_") |> 
  unnest()

team_meta <- all_season |> 
  filter(id %in% player_box$id) |>
  select(id, home_team, away_team, home_team_score=home_score, away_team_score=away_score, player_match_statistics) |> 
  mutate(player_match_statistics = map(player_match_statistics, ~ select(.x, -match))) |>
  mutate(player_match_statistics = map(player_match_statistics, ~ mutate(.x, field_goals_made = as.integer(field_goals_made)))) |> 
  unnest(c(home_team, away_team, player_match_statistics), names_sep = "_") |> 
  # filter(player_match_statistics == "0") |> 
  distinct(id, home_team_id, away_team_id, .keep_all = T)


team_meta <- team_meta |> 
  select(id, team_id=home_team_id, team_nickname=home_team_team_nickname, opp_name=away_team_name, opp_short_name=away_team_team_code, opp_team_nickname=away_team_team_nickname, opp_score=away_team_score, opp_full_score=away_team_score) |> 
  mutate(home_away = "home") |> 
  bind_rows(
    team_meta |> 
      select(id, team_id=away_team_id, team_nickname=away_team_team_nickname, opp_name=home_team_name, opp_short_name=home_team_team_code, opp_team_nickname=home_team_team_nickname, opp_score=home_team_score, opp_full_score=home_team_score) |> 
      mutate(home_away = "away")
  ) 





pbp_start <- pbp_start |> 
  # select(-external_id, -date_created, -date_updated, -sort, -user_created, -user_updated) |>
  mutate(season = current_season) |>
  left_join(
    team_meta, by = c("id", "team_id")
  )


final_out <- pbp_start |> 
  mutate(period_type = toupper(period_type),
         lead = score_1 - score_2) |> 
  select(match_id = id, season, team_name, team_short_name=team_team_nickname, home_away, opp_name, opp_short_name=opp_team_nickname,
         period_type, period, gt=clock, s1=score_1, s2=score_2, lead, action_type, sub_type, success, first_name, family_name=last_name, shirt_number=jersey_number,
         action_number = action_id, sub_type, x, y, shot_clock, timestamp, readable_action_type, system_action_type, personId, team_id)



final_final_out <- bind_rows(
  existing_pbp |> mutate(match_id = as.character(match_id)) |> filter(season != current_season), 
  final_out |> mutate(home_away = as.character(home_away)) |> filter(season == current_season)
)


save_nblr(df=final_final_out, file_name = "pbp", release_tag = "pbp")



existing_shots <- nblR::nbl_shots()


new_shots <- final_out |> 
  filter(action_type %in% c("2pt", "3pt", "freeThrow"))



shots_out <- bind_rows(
  existing_shots |> mutate(match_id = as.character(match_id)),
  new_shots |> mutate(home_away = as.character(home_away))
)


shots <- new_shots


library(ggplot2)
library(dplyr)

# ---- Example shot data ----
set.seed(123)
shots <- data.frame(
  x = runif(100, 3.4, 97.32),
  y = runif(100, 1.9, 98.46)
)

# ---- Rescale to NBL dimensions (28m x 15m) ----
rescale_to_nbl <- function(x, old_min, old_max, new_min, new_max) {
  (x - old_min) / (old_max - old_min) * (new_max - new_min) + new_min
}

shots <- shots %>%
  mutate(
    x_m = rescale_to_nbl(x, min(x), max(x), 0, 28),
    y_m = rescale_to_nbl(y, min(y), max(y), 0, 15)
  )

# Flip to single half-court (basket at x=0, y=7.5)
shots <- shots %>%
  mutate(
    x_half = ifelse(x_m > 14, 28 - x_m, x_m),
    y_half = ifelse(x_m > 14, 15 - y_m, y_m)
  )

# ---- Court drawing with correct FIBA/NBL specs ----
nbl_halfcourt <- function() {
  list(
    # Half court boundary
    geom_rect(aes(xmin = 0, xmax = 14, ymin = 0, ymax = 15),
              fill = "white", color = "black"),
    
    # Hoop at (0, 7.5)
    annotate("point", x = 0, y = 7.5, size = 4, shape = 21, fill = "orange"),
    
    # Backboard (1.575m wide, centered, 1.2m from baseline)
    geom_segment(aes(x = 1.2, y = 7.5 - 0.7875, 
                     xend = 1.2, yend = 7.5 + 0.7875), size = 1),
    
    # Paint (4.9m wide, 5.8m deep)
    geom_rect(aes(xmin = 0, xmax = 5.8, ymin = (15-4.9)/2, ymax = (15+4.9)/2),
              fill = NA, color = "black"),
    
    # Free throw circle (r = 1.8m, center at (5.8, 7.5))
    annotate("path",
             x = 5.8 + 1.8 * cos(seq(0, 2*pi, length.out = 200)),
             y = 7.5 + 1.8 * sin(seq(0, 2*pi, length.out = 200))),
    
    # Restricted area arc (r = 1.25m, center at hoop (0, 7.5))
    annotate("path",
             x = 0 + 1.25 * cos(seq(-pi/2, pi/2, length.out = 200)),
             y = 7.5 + 1.25 * sin(seq(-pi/2, pi/2, length.out = 200))),
    
    # 3-point arc (r = 6.75m, centered at hoop (0, 7.5))
    annotate("path",
             x = 0 + 6.75 * cos(seq(-pi/2, pi/2, length.out = 300)),
             y = 7.5 + 6.75 * sin(seq(-pi/2, pi/2, length.out = 300))),
    
    # 3-point straight lines (6.6m from baseline, min 0.9m from sideline)
    geom_segment(aes(x = 6.6, y = 0.9, xend = 6.6, yend = 15 - 0.9))
  )
}

# ---- Plot ----
sportyR::geom_basketball(league = "fiba") + 
  # ggplot(shots, aes(x = x_half, y = y_half)) +
  geom_point(data = shots, aes(x = x/5, y = y/10), color = "red", alpha = 0.6, size = 2) +
  # coord_fixed(xlim = c(0, 14), ylim = c(0, 15)) +
  labs(x = "Court Length (m)", y = "Court Width (m)",
       title = "NBL Half-Court Shot Chart (Scaled to Real Dimensions)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))



shots |> 
  ggplot() + 
  geom_point(data = shots, aes(x = x/5, y = y/10), color = "red", alpha = 0.6, size = 2) +
  facet_wrap(~ team_name)



shots |> 
  ggplot() + 
  geom_point(aes(x = x, y = y), color = "red", alpha = 0.6, size = 2) +
  facet_wrap(~ team_name)



# ---- Plot ----
shots |> 
  # ggplot(shots, aes(x = x_half, y = y_half)) +
  geom_point(aes(x = x/5, y = y/10), color = "red", alpha = 0.6, size = 2) +
  # coord_fixed(xlim = c(0, 14), ylim = c(0, 15)) +
  labs(x = "Court Length (m)", y = "Court Width (m)",
       title = "NBL Half-Court Shot Chart (Scaled to Real Dimensions)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))








