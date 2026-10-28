# ==============================================================
# Women's college basketball: player stats + story finder
# --------------------------------------------------------------
# RUN update_data.R FIRST. This script reads the files it creates
# (ap_poll_history.csv and team_summary.csv) from the "output" folder.
#
# What this does each time you run it:
#   1. Reads which teams have been in the AP Top 25
#   2. Pulls player box scores and keeps players on those teams
#   3. Calculates per-game and per-40 stats, efficiency, and team shares
#   4. Flags possible social media stories in story_candidates.csv
#
# Files saved in "output":
#   player_games_top25.csv   one row per player per game
#   player_summary.csv       season averages, one row per player
#   story_candidates.csv     Best/Worst lists for every category (one big table)
#   story_candidates.xlsx    the same lists, one sheet per category
#
# NOTE: Written without access to live data, so the first run may
# need small fixes. If you get an error, copy the red text.
# ==============================================================

library(wehoop)
library(tidyverse)

SEASON  <- 2027   # change to 2026 to test with last season's games
OUT_DIR <- "output"

# Sample-size guards so early-season noise doesn't become a "story"
MIN_GAMES       <- 5    # games needed to appear in story lists
MIN_MPG         <- 10   # minutes per game needed to appear in story lists
TREND_MIN_GAMES <- 8    # games needed before we call someone hot or cold

# How long each Best / Worst list is
LIST_SIZE      <- 25    # players
TEAM_LIST_SIZE <- 10    # teams (only ~25-40 teams are tracked, so 25 would overlap)

# Extra minimums so percentages aren't won by someone who took 3 shots
MIN_FGA_FG   <- 5       # field goal attempts per game, for FG%
MIN_3PA      <- 2.5     # three-point attempts per game, for 3P%
MIN_FTA      <- 2       # free throw attempts per game, for FT%
MIN_FGA_TS   <- 8       # field goal attempts per game, for true shooting %
MIN_MPG_TOV  <- 20      # minutes per game, for turnovers per game
MIN_MPG_P40  <- 15      # minutes per game, for per-40-minute stats
MIN_MIN_GAME <- 15      # minutes in a single game, for standout-game lists

safe_div <- function(a, b) ifelse(is.na(b) | b == 0, NA_real_, a / b)

# --------------------------------------------------------------
# STEP 1: Which teams count?
# --------------------------------------------------------------
history_path <- file.path(OUT_DIR, "ap_poll_history.csv")
if (!file.exists(history_path)) {
  stop("Can't find output/ap_poll_history.csv. Run update_data.R first, ",
       "and make sure RStudio's working directory is the same folder.",
       call. = FALSE)
}

poll_history <- read_csv(history_path, show_col_types = FALSE) %>%
  mutate(team_id = as.character(team_id))

ranked_ids <- unique(poll_history$team_id)

latest_poll_date <- max(poll_history$poll_date)
current_rank <- poll_history %>%
  filter(poll_date == latest_poll_date) %>%
  transmute(team_id, current_ap_rank = rank)

# --------------------------------------------------------------
# STEP 2: Download player box scores
# --------------------------------------------------------------
message("Downloading player box scores for season ", SEASON, "...")
player_box <- tryCatch(
  load_wbb_player_box(seasons = SEASON),
  error = function(e) {
    message("Could not load player box scores: ", conditionMessage(e))
    NULL
  }
)

if (is.null(player_box) || nrow(player_box) == 0) {
  stop("No player data found for season ", SEASON, " yet. ",
       "This is expected before the season starts. ",
       "To test now, change SEASON to 2026 at the top.",
       call. = FALSE)
}

# ESPN sometimes stores stats as text. Convert to real numbers.
num_cols <- c(
  "minutes", "field_goals_made", "field_goals_attempted",
  "three_point_field_goals_made", "three_point_field_goals_attempted",
  "free_throws_made", "free_throws_attempted",
  "offensive_rebounds", "defensive_rebounds", "rebounds",
  "assists", "steals", "blocks", "turnovers", "fouls", "points",
  "team_score", "opponent_team_score"
)

player_box <- player_box %>%
  mutate(
    athlete_id       = as.character(athlete_id),
    team_id          = as.character(team_id),
    opponent_team_id = as.character(opponent_team_id),
    across(all_of(intersect(num_cols, names(player_box))),
           ~ suppressWarnings(as.numeric(.x))),
    starter = starter %in% TRUE
  ) %>%
  filter(
    team_id %in% ranked_ids,
    !(did_not_play %in% TRUE),
    !is.na(minutes), minutes > 0
  ) %>%
  distinct(game_id, athlete_id, .keep_all = TRUE)

message("Player-game rows for ranked teams: ", nrow(player_box))

# --------------------------------------------------------------
# STEP 3: Player game log
# --------------------------------------------------------------
# Team totals for each game, so we can calculate each player's share
team_tot <- player_box %>%
  group_by(game_id, team_id) %>%
  summarise(
    tm_pts   = sum(points, na.rm = TRUE),
    tm_reb   = sum(rebounds, na.rm = TRUE),
    tm_ast   = sum(assists, na.rm = TRUE),
    tm_plays = sum(field_goals_attempted + 0.44 * free_throws_attempted +
                   turnovers, na.rm = TRUE),
    .groups = "drop"
  )

player_games <- player_box %>%
  left_join(team_tot, by = c("game_id", "team_id")) %>%
  mutate(
    ts_pct       = safe_div(points, 2 * (field_goals_attempted +
                                         0.44 * free_throws_attempted)),
    pts_per40    = safe_div(points, minutes) * 40,
    reb_per40    = safe_div(rebounds, minutes) * 40,
    ast_per40    = safe_div(assists, minutes) * 40,

    # Share of the team's totals while she was playing
    point_share  = safe_div(points, tm_pts),
    reb_share    = safe_div(rebounds, tm_reb),
    ast_share    = safe_div(assists, tm_ast),
    play_share   = safe_div(field_goals_attempted +
                            0.44 * free_throws_attempted + turnovers,
                            tm_plays),

    # Hollinger Game Score: a one-number summary of a single game
    game_score = points + 0.4 * field_goals_made - 0.7 * field_goals_attempted -
                 0.4 * (free_throws_attempted - free_throws_made) +
                 0.7 * offensive_rebounds + 0.3 * (rebounds - offensive_rebounds) +
                 steals + 0.7 * assists + 0.7 * blocks - 0.4 * fouls - turnovers
  ) %>%
  select(
    game_date, game_id, season_type,
    athlete_id, athlete_display_name, athlete_position_abbreviation,
    team_id, team_display_name, home_away, starter,
    opponent_team_id, opponent_team_display_name,
    team_score, opponent_team_score, team_winner,
    minutes, points, rebounds, assists, steals, blocks, turnovers, fouls,
    field_goals_made, field_goals_attempted,
    three_point_field_goals_made, three_point_field_goals_attempted,
    free_throws_made, free_throws_attempted,
    ts_pct, pts_per40, reb_per40, ast_per40,
    point_share, reb_share, ast_share, play_share, game_score
  ) %>%
  arrange(desc(game_date), team_display_name, desc(points))

write_csv(player_games, file.path(OUT_DIR, "player_games_top25.csv"))

# --------------------------------------------------------------
# STEP 4: Season summary, one row per player
# --------------------------------------------------------------
# Games are sorted newest first, so head(x, 5) means "last 5 games"
player_summary <- player_games %>%
  arrange(desc(game_date)) %>%
  group_by(athlete_id, athlete_display_name, team_id, team_display_name) %>%
  summarise(
    position   = first(athlete_position_abbreviation),
    games      = n(),
    starts     = sum(starter),
    mpg        = mean(minutes),
    ppg        = mean(points),
    rpg        = mean(rebounds),
    apg        = mean(assists),
    spg        = mean(steals),
    bpg        = mean(blocks),
    tpg        = mean(turnovers),
    fga_pg     = mean(field_goals_attempted),
    three_pa_pg = mean(three_point_field_goals_attempted),
    fta_pg     = mean(free_throws_attempted),
    fg_pct     = safe_div(sum(field_goals_made), sum(field_goals_attempted)),
    three_pct  = safe_div(sum(three_point_field_goals_made),
                          sum(three_point_field_goals_attempted)),
    ft_pct     = safe_div(sum(free_throws_made), sum(free_throws_attempted)),
    ts_pct     = safe_div(sum(points), 2 * (sum(field_goals_attempted) +
                                            0.44 * sum(free_throws_attempted))),
    pts_per40  = safe_div(sum(points), sum(minutes)) * 40,
    point_share = mean(point_share, na.rm = TRUE),
    play_share  = mean(play_share, na.rm = TRUE),
    avg_game_score = mean(game_score, na.rm = TRUE),
    last5_ppg  = mean(head(points, 5)),
    last5_game_score = mean(head(game_score, 5), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    trend_ppg = last5_ppg - ppg,
    qualifies = games >= MIN_GAMES & mpg >= MIN_MPG
  ) %>%
  left_join(current_rank, by = "team_id") %>%
  arrange(desc(ppg)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(player_summary, file.path(OUT_DIR, "player_summary.csv"))

# --------------------------------------------------------------
# STEP 5: Best and worst lists for every category
# --------------------------------------------------------------
message("Building best/worst lists...")

# Builds a "best" list and a "worst" list for one category.
#   df       the table to rank
#   col      the column to rank by (written in quotes)
#   category name that will appear in the output
#   best_is  "high" if a bigger number is better, "low" if smaller is better
#   labels   names for the two lists (first = the "best" end)
#   scale    multiply the value for display (100 turns 0.512 into 51.2)
rank_lists <- function(df, col, category, best_is = "high",
                       labels = c("Best", "Worst"), n = LIST_SIZE,
                       scale = 1, subject_col = "athlete_display_name") {
  df <- df[!is.na(df[[col]]) & is.finite(df[[col]]), ]
  n  <- min(n, floor(nrow(df) / 2))   # avoid the same name on both lists
  if (n < 1) return(tibble())
  df$.metric <- df[[col]]
  top    <- df %>% slice_max(.metric, n = n, with_ties = FALSE)
  bottom <- df %>% slice_min(.metric, n = n, with_ties = FALSE)
  if (best_is == "high") { first <- top; second <- bottom
  } else                 { first <- bottom; second <- top }
  bind_rows(
    first  %>% mutate(list = labels[1], place = row_number()),
    second %>% mutate(list = labels[2], place = row_number())
  ) %>%
    transmute(
      category = category,
      list     = list,
      place    = place,
      subject  = .data[[subject_col]],
      team     = team_display_name,
      value    = round(.metric * scale, 2),
      detail   = context
    )
}

# ---- Player categories --------------------------------------
players <- player_summary %>%
  mutate(context = paste0(games, " GP, ", sprintf("%.1f", mpg), " mpg, ",
                          sprintf("%.1f", ppg), " ppg"))
q <- players %>% filter(qualifies)

player_lists <- bind_rows(
  rank_lists(q, "ppg", "Points per game"),
  rank_lists(q, "rpg", "Rebounds per game"),
  rank_lists(q, "apg", "Assists per game"),
  rank_lists(q, "spg", "Steals per game"),
  rank_lists(q, "bpg", "Blocks per game"),
  rank_lists(filter(q, mpg >= MIN_MPG_TOV), "tpg",
             paste0("Turnovers per game (", MIN_MPG_TOV, "+ mpg)"),
             best_is = "low"),
  rank_lists(filter(q, fga_pg >= MIN_FGA_FG), "fg_pct",
             paste0("Field goal pct (", MIN_FGA_FG, "+ FGA per game)"),
             scale = 100),
  rank_lists(filter(q, three_pa_pg >= MIN_3PA), "three_pct",
             paste0("Three-point pct (", MIN_3PA, "+ 3PA per game)"),
             scale = 100),
  rank_lists(filter(q, fta_pg >= MIN_FTA), "ft_pct",
             paste0("Free throw pct (", MIN_FTA, "+ FTA per game)"),
             scale = 100),
  rank_lists(filter(q, fga_pg >= MIN_FGA_TS), "ts_pct",
             paste0("True shooting pct (", MIN_FGA_TS, "+ FGA per game)"),
             scale = 100),
  rank_lists(filter(q, mpg >= MIN_MPG_P40), "pts_per40",
             paste0("Points per 40 minutes (", MIN_MPG_P40, "+ mpg)")),
  rank_lists(q, "point_share", "Share of team points", scale = 100),
  rank_lists(q, "play_share", "Usage: share of team plays", scale = 100),
  rank_lists(q, "avg_game_score", "Average Game Score"),
  rank_lists(filter(q, games >= TREND_MIN_GAMES), "trend_ppg",
             "Last 5 games vs season ppg",
             labels = c("Heating up", "Cooling off")),
  rank_lists(filter(q, starts / games <= 0.2), "pts_per40",
             "Bench players: points per 40 minutes")
)

# ---- Standout single games from the most recent week --------
latest_game <- max(player_games$game_date)
recent <- player_games %>%
  filter(game_date >= latest_game - 7, minutes >= MIN_MIN_GAME) %>%
  mutate(context = sprintf("%.0f pts, %.0f reb, %.0f ast on %.0f-%.0f FG vs %s (%s)",
                           points, rebounds, assists,
                           field_goals_made, field_goals_attempted,
                           opponent_team_display_name, as.character(game_date)))

game_lists <- rank_lists(recent, "game_score",
                         "Single-game Game Score, last 7 days",
                         labels = c("Best games", "Worst games"))

# ---- Team categories (needs team_summary.csv) ---------------
team_lists <- tibble()
team_path  <- file.path(OUT_DIR, "team_summary.csv")

if (file.exists(team_path)) {
  tm <- read_csv(team_path, show_col_types = FALSE) %>%
    mutate(context = paste0(games, " GP",
                            ifelse(is.na(current_ap_rank), "",
                                   paste0(", AP #", current_ap_rank))))

  tl <- function(df, col, category, ...) {
    rank_lists(df, col, category, n = TEAM_LIST_SIZE,
               subject_col = "team_display_name", ...)
  }
  HL <- c("Highest", "Lowest")   # for style stats where high isn't "better"

  # Poll rank vs. net rating, among teams in the current Top 25
  ranked_now <- tm %>%
    filter(!is.na(current_ap_rank)) %>%
    mutate(
      net_rank = rank(-net_rating, ties.method = "min"),
      poll_gap = current_ap_rank - net_rank,
      context  = paste0("AP #", current_ap_rank, ", but #", net_rank,
                        " in net rating among ranked teams")
    )

  team_lists <- bind_rows(
    tl(tm, "net_rating",      "Team net rating"),
    tl(tm, "off_rating",      "Team offensive rating"),
    tl(tm, "def_rating",      "Team defensive rating", best_is = "low"),
    tl(tm, "avg_margin",      "Team scoring margin"),
    tl(tm, "efg_pct",         "Team effective FG pct", scale = 100),
    tl(tm, "opp_efg_pct",     "Opponent effective FG pct", best_is = "low", scale = 100),
    tl(tm, "tov_pct",         "Team turnover rate", best_is = "low", scale = 100),
    tl(tm, "opp_tov_pct",     "Forced turnover rate", scale = 100),
    tl(tm, "oreb_pct",        "Offensive rebound rate", scale = 100),
    tl(tm, "pace",            "Pace (possessions per game)", labels = HL),
    tl(tm, "three_rate",      "Three-point attempt rate", labels = HL, scale = 100),
    tl(tm, "paint_share",     "Share of points in the paint", labels = HL, scale = 100),
    tl(tm, "fastbreak_share", "Share of points on fast breaks", labels = HL, scale = 100),
    tl(tm, "off_tov_share",   "Share of points off turnovers", labels = HL, scale = 100),
    tl(ranked_now, "poll_gap", "Poll rank vs net rating",
       labels = c("Ratings like them more than the poll",
                  "Ratings like them less than the poll"))
  )
}

# ---- Save ---------------------------------------------------
stories <- bind_rows(player_lists, game_lists, team_lists) %>%
  mutate(generated_on = Sys.Date())

write_csv(stories, file.path(OUT_DIR, "story_candidates.csv"))

# One sheet per category in a single Excel file (easier to scroll)
if (nrow(stories) > 0 && requireNamespace("writexl", quietly = TRUE)) {
  sheets <- split(stories, factor(stories$category, levels = unique(stories$category)))
  names(sheets) <- make.unique(substr(gsub("[^A-Za-z0-9 %-]", "", names(sheets)), 1, 31))
  writexl::write_xlsx(sheets, file.path(OUT_DIR, "story_candidates.xlsx"))
}

message("Done! Files saved in the '", OUT_DIR, "' folder:")
message("  - player_games_top25.csv (", nrow(player_games), " rows)")
message("  - player_summary.csv     (", nrow(player_summary), " players)")
message("  - story_candidates.csv / .xlsx (",
        n_distinct(stories$category), " categories)")
message("Reminder: check sample size and strength of schedule before posting.")
