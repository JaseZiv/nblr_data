
all_season <- readRDS("matches_df.rds")
current_season <- "2025-2026"

# there is a game that the json structure is different for, and it's causing all sorts of headaches...
# for now I'll remove it, knowing that it'll need to be fixed somehow:
bad_pbp <- c("b1b1def4-4bef-11f0-8dce-5f21da19a0fe", "b1c59937-4bef-11f0-9850-69185882ba54", "b20bc78d-4bef-11f0-9d84-e3ab37cba7df", 
             "b22c940a-4bef-11f0-b917-b31681696f40", "b204326e-4bef-11f0-a4c0-7d8c00260a69", "b2317162-4bef-11f0-b94a-2b5bbafc199b",
             "b21aeb47-4bef-11f0-be7c-3fd9cdc8f1b9", "b217a699-4bef-11f0-bc08-933162362355", "b225ec91-4bef-11f0-ab26-f7367f0af8fe",
             "b245e256-4bef-11f0-a465-3bb81d630de9", "b25032fc-4bef-11f0-9f55-d922ae3738da", "b2521ec7-4bef-11f0-868d-35d49e4d3f0e")

player_meta <- all_season |> 
  # head(1) |> 
  select(id, player_match_statistics) |> 
  mutate(missing = mapply(length, player_match_statistics)) |> 
  filter(missing > 0) |> select(-missing) |> 
  mutate(player_match_statistics = map(player_match_statistics, ~ select(.x, -match))) |>
  mutate(player_match_statistics = map(player_match_statistics, ~ mutate(.x, field_goals_made = as.integer(field_goals_made)))) |> 
  unnest(player_match_statistics, names_sep = "_") |> 
  unnest() |> 
  distinct(id, personId=id1, first_name, family_name=last_name, shirt_number=jersey_number, team_id=id2)


team_meta <- all_season |> 
  # filter(id %in% player_box$id) |>
  filter(match_status == "complete") |> 
  select(id, home_team, away_team, home_team_score=home_score, away_team_score=away_score, player_match_statistics) |> 
  mutate(player_match_statistics = map(player_match_statistics, ~ select(.x, -match))) |>
  mutate(player_match_statistics = map(player_match_statistics, ~ mutate(.x, field_goals_made = as.integer(field_goals_made)))) |> 
  unnest(c(home_team, away_team, player_match_statistics), names_sep = "_") |> 
  # filter(player_match_statistics == "0") |> 
  distinct(id, home_team_id, away_team_id, .keep_all = T)


team_meta <- team_meta |> 
  select(id, team_id=home_team_id, team_name=home_team_name, team_nickname=home_team_team_nickname, opp_name=away_team_name, opp_short_name=away_team_team_code, opp_team_nickname=away_team_team_nickname, opp_score=away_team_score, opp_full_score=away_team_score) |> 
  mutate(home_away = "home") |> 
  bind_rows(
    team_meta |> 
      select(id, team_id=away_team_id, team_name=away_team_name, team_nickname=away_team_team_nickname, opp_name=home_team_name, opp_short_name=home_team_team_code, opp_team_nickname=home_team_team_nickname, opp_score=home_team_score, opp_full_score=home_team_score) |> 
      mutate(home_away = "away")
  ) 


pbp_start <- all_season |> 
  filter(!id %in% bad_pbp) |> 
  select(id, play_by_play) |> 
  mutate(missing = mapply(length, play_by_play)) |> 
  filter(missing > 0) |> select(-missing) |> 
  mutate(play_by_play = map(play_by_play, ~ mutate(.x, period = as.integer(period)))) |> 
  unnest(play_by_play, names_sep = "_") |> 
  unnest() |> 
  unnest(cols = team, names_sep = "_") |> 
  select(-jersey_number, -first_name, -last_name, -image)

names(pbp_start) <- gsub("play_by_play_", "", names(pbp_start))

pbp_start <- pbp_start |> 
  select(id, match, action_id, period, period_type, score_1, score_2, action_type, sub_type, x, y, clock, shot_clock, timestamp,
         readable_action_type, system_action_type, id1, external_id, personId, success, fixtureId)




pbp_start_dirty <- all_season |> 
  filter(id %in% bad_pbp) |>
  # filter(id %in% "b2317162-4bef-11f0-b94a-2b5bbafc199b") |> 
  select(id, play_by_play) |> 
  mutate(missing = mapply(length, play_by_play)) |> 
  filter(missing > 0) |> select(-missing) |> 
  mutate(play_by_play = map(play_by_play, ~ mutate(.x, period = as.integer(period)))) |> 
  unnest(play_by_play, names_sep = "_") |> 
  mutate(play_by_play_player = map(play_by_play_player, ~ if (is.null(.x)) list() else .x)) |> 
  unnest_wider(play_by_play_player, names_sep = "_") |> 
  unnest_wider(play_by_play_player_team, names_sep = "_") |> 
  unnest() |> 
  rename(id1=play_by_play_player_id)

names(pbp_start_dirty) <- gsub("play_by_play_", "", names(pbp_start_dirty))
names(pbp_start_dirty) <- gsub("player_", "", names(pbp_start_dirty))


pbp_start_dirty <- pbp_start_dirty |> 
  select(id, match, action_id, period, period_type, score_1, score_2, action_type, sub_type, x, y, clock, shot_clock, timestamp,
         readable_action_type, system_action_type, id1, external_id, personId, success, fixtureId)



pbp_start <- pbp_start |> 
  bind_rows(pbp_start_dirty)


pbp_start <- pbp_start |> 
  mutate(season = current_season) |> 
  left_join(player_meta, by = c("id", "id1" = "personId")) |> 
  left_join(team_meta, by = c("id", "team_id"))





existing_pbp <- nblR::nbl_pbp()



final_out <- pbp_start |> 
  mutate(period_type = toupper(period_type),
         lead = score_1 - score_2) |> 
  select(match_id = id, season, team_name, team_short_name=team_nickname, home_away, opp_name, opp_short_name=opp_team_nickname,
         period_type, period, gt=clock, s1=score_1, s2=score_2, lead, action_type, sub_type, success, first_name, family_name, shirt_number,
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


shots |> 
  ggplot() + 
  geom_point(aes(x = x_m, y = y_m), color = "red", alpha = 0.6, size = 2) +
  facet_wrap(~ team_name)




library(sportyR)


shot_data_scaled <- shots %>%
  # Remove the NAs as they cannot be plotted (you had 3 in each column)
  filter(!is.na(x)) %>%
  
  # Apply the scaling transformation
  mutate(
    x_coord_mirrored = if_else(
      x > 50,
      100 - x, # Reflect a value like 70 to 30
      x        # Keep a value like 30 as 30
    )
  ) %>%
  
  # --- SCALING ---
  # Scale the mirrored 0-50 range to the standard 0-15 meter width.
  # X-scale: 15 meters / 50 mirrored units = 0.3
  # Y-scale: 14 meters / 100 units (full court length) = 0.14
  mutate(
    x_coord_meter = x_coord_mirrored * (15 / 100), # Max 50 becomes 15
    y_coord_meter = y * (14 / 50) # Max 100 becomes 14
  )




shot_data_scaled |> 
  ggplot(aes(x=x_coord_meter, y=y_coord_meter)) +
  geom_point()




# --- 4. Plot the Shot Locations on the FIBA Half-Court ---

# geom_basketball(league = "fiba", display_range = "offense") is the key to drawing the court.
# "fiba" is the standard for NBL.
# "offense" draws a single half-court.

court_plot <- ggplot(data = shot_data_scaled) +
  
  # Draw the FIBA court (NBL) half-court background
  # The default sportyR coordinates for a half-court place the basket at the bottom-center.
  geom_basketball(
    league = "FIBA"
    # display_range = "offense",
    # court_units = "m" # Ensure the court is drawn in meters
  ) +
  
  # Add the shot locations using the scaled coordinates
  geom_point(
    aes(
      x = x_coord_meter, 
      y = y_coord_meter, 
      color = made # Example of coloring points by a variable
    ),
    alpha = 0.6, # Make points slightly transparent
    size = 2
  ) +
  
  # Set the aspect ratio to 1:1 so the court lines look correct
  coord_fixed() + 
  
  # Add labels and a title
  labs(
    title = "NBL Shot Locations",
    subtitle = "Scaled from 0-100 to FIBA Half-Court (Meters)",
    x = "Court Width (m)",
    y = "Court Length (m)",
    color = "Shot Made"
  ) +
  
  # Apply a clean theme
  theme_minimal() +
  
  # Optional: Customize the plot's appearance
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Print the plot object
print(court_plot)








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








