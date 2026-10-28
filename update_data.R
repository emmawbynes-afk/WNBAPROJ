# ==============================================================
# Women's college basketball: AP Top 25 data updater
# --------------------------------------------------------------
# What this does each time you run it:
#   1. Gets the current AP Top 25 from ESPN (via wehoop)
#   2. Adds it to a running history of every team ranked this season
#   3. Pulls team box scores for the season and keeps games for
#      any team that has been in the Top 25
#   4. Calculates "story" stats (pace, efficiency, how teams score)
#   5. Saves three CSV files in a folder called "output"
#
# NOTE: This was written without access to live data, so the first
# run may need small fixes. If you get an error, copy the red text
# and send it back.
# ==============================================================

library(wehoop)
library(tidyverse)

SEASON  <- 2026          # wehoop names a season by the year it ends
OUT_DIR <- "output"
dir.create(OUT_DIR, showWarnings = FALSE)

# --------------------------------------------------------------
# STEP 1: Current AP Top 25
# --------------------------------------------------------------
message("Getting AP poll...")
rankings <- espn_wbb_rankings()

# The AP poll has type == "ap". "current" is the rank number.
# Rank 0 rows are teams receiving votes but not ranked, so we drop them.
ap_now <- rankings %>%
  filter(type == "ap", current >= 1, current <= 25) %>%
  transmute(
    poll_date    = Sys.Date(),
    poll_label   = season_display_name,
    rank         = current,
    previous     = previous,
    points       = points,
    team_id      = as.character(team_id),
    team_name    = team_location,
    team_nick    = team_name,
    record       = record_summary
  ) %>%
  arrange(rank)

message("Teams in current AP Top 25: ", nrow(ap_now))

# --------------------------------------------------------------
# STEP 2: Keep a running history of the poll
# --------------------------------------------------------------
history_path <- file.path(OUT_DIR, "ap_poll_history.csv")

if (file.exists(history_path)) {
  old_history <- read_csv(history_path, show_col_types = FALSE) %>%
    mutate(team_id = as.character(team_id))
  poll_history <- bind_rows(old_history, ap_now %>% mutate(poll_date = as.Date(poll_date)))
} else {
  poll_history <- ap_now
}

# If a poll was pulled more than once the same day, keep only one copy
poll_history <- poll_history %>%
  distinct(poll_date, poll_label, rank, team_id, .keep_all = TRUE) %>%
  arrange(poll_date, rank)

write_csv(poll_history, history_path)

# Every team that has been ranked at any point this season
ranked_ids <- unique(poll_history$team_id)
current_rank <- ap_now %>% select(team_id, current_ap_rank = rank)

# --------------------------------------------------------------
# STEP 3: Pull the season's team box scores
# --------------------------------------------------------------
message("Downloading box scores for season ", SEASON, "...")
team_box <- tryCatch(
  load_wbb_team_box(seasons = SEASON),
  error = function(e) {
    message("Could not load box scores: ", conditionMessage(e))
    NULL
  }
)

if (is.null(team_box) || nrow(team_box) == 0) {
  # Remove leftover files from an earlier season or test run
  unlink(file.path(OUT_DIR, c("games_top25.csv", "team_summary.csv")))
  # Note: we use stop() here instead of quit(). quit() closes RStudio's
  # R session and shows "R encountered a fatal error".
  stop("No games found for season ", SEASON,
       " yet. This is expected before the season starts. ",
       "The poll history was still saved to the output folder. ",
       "To test the game stats now, change SEASON to 2026 at the top.",
       call. = FALSE)
}

team_box <- team_box %>%
  mutate(
    team_id          = as.character(team_id),
    opponent_team_id = as.character(opponent_team_id)
  ) %>%
  filter(!is.na(team_score), !is.na(opponent_team_score)) %>%
  distinct(game_id, team_id, .keep_all = TRUE)

# ESPN sometimes stores stats as text (e.g. "12" instead of 12), which
# breaks the math below. Convert every stat column to a real number.
stat_cols <- c(
  "team_score", "opponent_team_score",
  "field_goals_made", "field_goals_attempted",
  "three_point_field_goals_made", "three_point_field_goals_attempted",
  "free_throws_made", "free_throws_attempted",
  "offensive_rebounds", "defensive_rebounds", "total_rebounds",
  "total_turnovers", "assists", "steals", "blocks",
  "points_in_paint", "fast_break_points", "turnover_points",
  "largest_lead", "lead_changes"
)
team_box <- team_box %>%
  mutate(across(all_of(intersect(stat_cols, names(team_box))),
                ~ suppressWarnings(as.numeric(.x))))

# --------------------------------------------------------------
# STEP 4: Build the game log with opponent stats attached
# --------------------------------------------------------------
# Each row of team_box is one team in one game. To get defensive
# numbers we attach the opponent's row from the same game.
opp <- team_box %>%
  transmute(
    game_id,
    opponent_team_id = team_id,
    opp_fga  = field_goals_attempted,
    opp_fgm  = field_goals_made,
    opp_3pm  = three_point_field_goals_made,
    opp_3pa  = three_point_field_goals_attempted,
    opp_fta  = free_throws_attempted,
    opp_oreb = offensive_rebounds,
    opp_dreb = defensive_rebounds,
    opp_tov  = total_turnovers
  )

games <- team_box %>%
  filter(team_id %in% ranked_ids) %>%
  left_join(opp, by = c("game_id", "opponent_team_id")) %>%
  mutate(
    # Estimated possessions (standard formula)
    poss     = field_goals_attempted - offensive_rebounds + total_turnovers +
               0.475 * free_throws_attempted,
    opp_poss = opp_fga - opp_oreb + opp_tov + 0.475 * opp_fta,
    game_poss = (poss + opp_poss) / 2,

    # Efficiency per 100 possessions
    off_rating = 100 * team_score / game_poss,
    def_rating = 100 * opponent_team_score / game_poss,
    net_rating = off_rating - def_rating,

    # Shooting
    efg_pct     = (field_goals_made + 0.5 * three_point_field_goals_made) /
                  field_goals_attempted,
    opp_efg_pct = (opp_fgm + 0.5 * opp_3pm) / opp_fga,
    ts_pct      = team_score / (2 * (field_goals_attempted +
                                     0.44 * free_throws_attempted)),
    three_rate  = three_point_field_goals_attempted / field_goals_attempted,
    ft_rate     = free_throws_attempted / field_goals_attempted,

    # Ball security and rebounding
    tov_pct      = total_turnovers / poss,
    opp_tov_pct  = opp_tov / opp_poss,
    oreb_pct     = offensive_rebounds / (offensive_rebounds + opp_dreb),

    # How the team scores (share of total points)
    paint_share      = points_in_paint / team_score,
    fastbreak_share  = fast_break_points / team_score,
    off_tov_share    = turnover_points / team_score,

    margin = team_score - opponent_team_score,

    # Is the opponent currently ranked in the AP Top 25?
    opp_in_top25_now = opponent_team_id %in% ap_now$team_id
  ) %>%
  select(
    game_date, game_id, season_type,
    team_id, team_display_name, team_home_away,
    opponent_team_id, opponent_team_display_name, opp_in_top25_now,
    team_score, opponent_team_score, margin, team_winner,
    game_poss, off_rating, def_rating, net_rating,
    efg_pct, opp_efg_pct, ts_pct, three_rate, ft_rate,
    tov_pct, opp_tov_pct, oreb_pct,
    paint_share, fastbreak_share, off_tov_share,
    largest_lead, lead_changes,
    field_goals_made, field_goals_attempted,
    three_point_field_goals_made, three_point_field_goals_attempted,
    free_throws_made, free_throws_attempted,
    assists, steals, blocks, total_rebounds, total_turnovers
  ) %>%
  arrange(desc(game_date), team_display_name)

write_csv(games, file.path(OUT_DIR, "games_top25.csv"))

# --------------------------------------------------------------
# STEP 5: Season summary, one row per team
# --------------------------------------------------------------
team_summary <- games %>%
  group_by(team_id, team_display_name) %>%
  summarise(
    games          = n(),
    wins           = sum(team_winner, na.rm = TRUE),
    avg_margin     = mean(margin, na.rm = TRUE),
    pace           = mean(game_poss, na.rm = TRUE),
    off_rating     = mean(off_rating, na.rm = TRUE),
    def_rating     = mean(def_rating, na.rm = TRUE),
    net_rating     = mean(net_rating, na.rm = TRUE),
    efg_pct        = mean(efg_pct, na.rm = TRUE),
    opp_efg_pct    = mean(opp_efg_pct, na.rm = TRUE),
    three_rate     = mean(three_rate, na.rm = TRUE),
    ft_rate        = mean(ft_rate, na.rm = TRUE),
    tov_pct        = mean(tov_pct, na.rm = TRUE),
    opp_tov_pct    = mean(opp_tov_pct, na.rm = TRUE),
    oreb_pct       = mean(oreb_pct, na.rm = TRUE),
    paint_share    = mean(paint_share, na.rm = TRUE),
    fastbreak_share = mean(fastbreak_share, na.rm = TRUE),
    off_tov_share  = mean(off_tov_share, na.rm = TRUE),
    avg_lead_changes = mean(lead_changes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(current_rank, by = "team_id") %>%
  arrange(is.na(current_ap_rank), current_ap_rank, desc(net_rating)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(team_summary, file.path(OUT_DIR, "team_summary.csv"))

message("Done! Files saved in the '", OUT_DIR, "' folder:")
message("  - ap_poll_history.csv")
message("  - games_top25.csv  (", nrow(games), " rows)")
message("  - team_summary.csv (", nrow(team_summary), " teams)")
