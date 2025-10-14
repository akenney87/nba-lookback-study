# =====================================================
# 1) Setup
# =====================================================
library(dplyr)
library(readr)
library(stringr)

# Set your path if needed
csv_path <- "Per36Minutes.csv"

raw <- read_csv(csv_path,
                show_col_types = FALSE)

# Keep column names consistent just in case
names(raw) <- names(raw) |> tolower()

# Quick sanity: expect these columns to exist
required_cols <- c(
  "season","lg","player","player_id","age","team","pos","g","gs","mp",
  "fg_per_36_min","fga_per_36_min","fg_percent",
  "x3p_per_36_min","x3pa_per_36_min","x3p_percent",
  "x2p_per_36_min","x2pa_per_36_min","x2p_percent",
  "e_fg_percent",
  "ft_per_36_min","fta_per_36_min","ft_percent",
  "orb_per_36_min","drb_per_36_min","trb_per_36_min",
  "ast_per_36_min","stl_per_36_min","blk_per_36_min",
  "tov_per_36_min","pf_per_36_min","pts_per_36_min"
)

missing <- setdiff(required_cols, names(raw))
if (length(missing) > 0) {
  stop("Missing expected columns: ", paste(missing, collapse = ", "))
}

# =====================================================
# Rebuild from source CSV (robust season collapse)
# - Handles 2TM/TOT combined rows correctly
# - Minutes-weighted per-36 across team stints (excludes combined row)
# - Percentages recomputed from totals (fgm/fga, ftm/fta, etc.)
# - Then filter to >=2015 and mp>=1000 -> df_clean
# =====================================================

library(readr)
library(dplyr)

# 0) Read raw each time to avoid stale state
raw <- readr::read_csv("Per36Minutes.csv", show_col_types = FALSE)

# --- Rebuild by_player_season (adds fga_per_36_min, fta_per_36_min) ---

wavg <- function(x, w) ifelse(sum(w, na.rm = TRUE) > 0,
                              sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
                              NA_real_)

tot_tags <- c("TOT","2TM","3TM","4TM","5TM","6TM","7TM")
team_col <- dplyr::first(intersect(c("team","tm"), names(raw)))  # NULL if absent

by_player_season <- raw %>%
  dplyr::group_by(player_id, season) %>%
  dplyr::summarise({
    df <- dplyr::cur_data_all()
    
    # detect combined row
    if (!is.null(team_col)) {
      is_combined <- df[[team_col]] %in% tot_tags
    } else {
      g_sum <- sum(df$g, na.rm = TRUE)
      is_combined <- abs(df$g - (g_sum - df$g)) < 1e-9
    }
    team_rows <- !is_combined
    df_team <- if (any(team_rows, na.rm = TRUE)) df[team_rows, , drop = FALSE] else df
    
    # minutes & games
    mp_sum <- sum(df_team$mp, na.rm = TRUE)
    if (any(is_combined, na.rm = TRUE)) {
      g_val <- suppressWarnings(max(df$g[is_combined], na.rm = TRUE))
      mp_combined <- suppressWarnings(max(df$mp[is_combined], na.rm = TRUE))
      mp_val <- if (!is.na(mp_combined) && mp_combined >= 0.95 * mp_sum) mp_combined else mp_sum
    } else {
      g_val  <- suppressWarnings(max(df$g, na.rm = TRUE))
      mp_val <- mp_sum
    }
    
    # totals from per-36 × minutes/36 (team rows only)
    fgm_tot <- sum(df_team$fg_per_36_min  * df_team$mp / 36, na.rm = TRUE)
    fga_tot <- sum(df_team$fga_per_36_min * df_team$mp / 36, na.rm = TRUE)
    ftm_tot <- sum(df_team$ft_per_36_min  * df_team$mp / 36, na.rm = TRUE)
    fta_tot <- sum(df_team$fta_per_36_min * df_team$mp / 36, na.rm = TRUE)
    x3m_tot <- sum(df_team$x3p_per_36_min * df_team$mp / 36, na.rm = TRUE)
    x3a_tot <- sum(df_team$x3pa_per_36_min * df_team$mp / 36, na.rm = TRUE)
    x2m_tot <- sum(df_team$x2p_per_36_min * df_team$mp / 36, na.rm = TRUE)
    x2a_tot <- sum(df_team$x2pa_per_36_min * df_team$mp / 36, na.rm = TRUE)
    
    # minutes-weighted per-36 across team stints (include attempts!)
    pts36 <- wavg(df_team$pts_per_36_min, df_team$mp)
    trb36 <- wavg(df_team$trb_per_36_min, df_team$mp)
    ast36 <- wavg(df_team$ast_per_36_min, df_team$mp)
    stl36 <- wavg(df_team$stl_per_36_min, df_team$mp)
    blk36 <- wavg(df_team$blk_per_36_min, df_team$mp)
    x3p36 <- wavg(df_team$x3p_per_36_min, df_team$mp)
    tov36 <- wavg(df_team$tov_per_36_min, df_team$mp)
    fga36 <- wavg(df_team$fga_per_36_min, df_team$mp)
    fta36 <- wavg(df_team$fta_per_36_min, df_team$mp)
    
    # percentages recomputed from totals
    fg_pct   <- ifelse(fga_tot > 0, fgm_tot / fga_tot, NA_real_)
    ft_pct   <- ifelse(fta_tot > 0, ftm_tot / fta_tot, NA_real_)
    x3p_pct  <- ifelse(x3a_tot > 0, x3m_tot / x3a_tot, NA_real_)
    x2p_pct  <- ifelse(x2a_tot > 0, x2m_tot / x2a_tot, NA_real_)
    efg_pct  <- ifelse(fga_tot > 0, (fgm_tot + 0.5 * x3m_tot) / fga_tot, NA_real_)
    
    # labels from max-minute row
    idx_mm <- ifelse(all(is.na(df$mp)), 1L, which.max(df$mp))
    player_lab <- df$player[idx_mm]
    pos_lab    <- df$pos[idx_mm]
    lg_lab     <- df$lg[idx_mm]
    age_lab    <- df$age[idx_mm]
    
    tibble::tibble(
      player = player_lab, age = age_lab, pos = pos_lab, lg = lg_lab,
      g = g_val, mp = mp_val, mpg = ifelse(g_val > 0, mp_val / g_val, NA_real_),
      # percents
      fg_percent = fg_pct, ft_percent = ft_pct, x3p_percent = x3p_pct,
      x2p_percent = x2p_pct, e_fg_percent = efg_pct,
      # per-36 outputs we need downstream
      pts_per_36_min = pts36, trb_per_36_min = trb36, ast_per_36_min = ast36,
      stl_per_36_min = stl36, blk_per_36_min = blk36, x3p_per_36_min = x3p36,
      tov_per_36_min = tov36,
      fga_per_36_min = fga36, fta_per_36_min = fta36
    )
  }, .groups = "drop")


df_clean <- by_player_season %>%
  dplyr::filter(season >= 2015, mp >= 1000) %>%
  dplyr::select(
    player_id, player, pos, season, mp,
    fg_percent, ft_percent,
    pts_per_36_min, trb_per_36_min, ast_per_36_min,
    stl_per_36_min, blk_per_36_min, x3p_per_36_min, tov_per_36_min,
    fga_per_36_min, fta_per_36_min
  )


# Sanity: Luka 2025 should now be G=50 if 2TM present
by_player_season %>%
  dplyr::filter(player_id == "doncilu01", season == 2025) %>%
  dplyr::select(player_id, season, g, mp, mpg) %>%
  print(n = Inf)


# =====================================================
# 3) Filter: seasons >= 2015 and minutes >= 1000
# =====================================================

# 2) Filter to your modeling window (>=2015 and mp >= 1000)
df_clean <- by_player_season %>%
  dplyr::filter(season >= 2015, mp >= 1000) %>%
  dplyr::select(
    player_id, player, pos, season, mp,
    fg_percent, ft_percent,
    pts_per_36_min, trb_per_36_min, ast_per_36_min,
    stl_per_36_min, blk_per_36_min, x3p_per_36_min, tov_per_36_min
  )

# Peek
message("# Rows after merge & filter: ", nrow(df_clean))
print(df_clean %>% select(player, player_id, season, mp, pts_per_36_min, trb_per_36_min, ast_per_36_min) %>% head(10))

# (Optional) Save an intermediate file for reproducibility
write_csv(df_clean, "Per36_clean_2015plus_1000mp.csv")

# =====================================================
# 4) Build lookback windows (1–5 seasons), even averages
#     - Uses only df_clean (season >= 2015, mp >= 1000)
#     - Skips any target season lacking ALL of the required
#       previous k seasons (because those rows won't exist in df_clean)
# =====================================================

library(dplyr)
library(purrr)
library(tidyr)

# Stats to include in lookbacks
stats_vec <- c(
  "pts_per_36_min",
  "trb_per_36_min",
  "ast_per_36_min",
  "stl_per_36_min",
  "blk_per_36_min",
  "x3p_per_36_min",
  "fg_percent",
  "ft_percent",
  "tov_per_36_min"
)

# Base table: only columns we need for joins/averages
base <- df_clean %>%
  select(player_id, season, all_of(stats_vec))

# Create lag tables for seasons t-1 ... t-5
lagged_list <- lapply(1:5, function(i) {
  base %>%
    rename_with(~ paste0(.x, "_L", i), all_of(stats_vec)) %>%
    mutate(season = season + i)  # shift forward so join on (player_id, season) hits t-i rows
})

# Join all lag tables onto the base target season
joined <- reduce(lagged_list, ~ left_join(.x, .y, by = c("player_id", "season")), .init = base)

# Helper to compute evenly averaged lookbacks for a given k
make_lb_means <- function(df, k) {
  # for each stat, average across the k lag columns (L1..Lk)
  out <- df
  for (stat in stats_vec) {
    lag_cols <- paste0(stat, "_L", 1:k)
    new_col  <- paste0(stat, "_lb", k)
    out[[new_col]] <- if (k == 1) out[[lag_cols]] else rowMeans(out[, lag_cols], na.rm = FALSE)
  }
  # keep only rows where ALL needed lags exist (no NA among required lag cols)
  needed <- unlist(lapply(stats_vec, function(s) paste0(s, "_L", 1:k)))
  out %>%
    filter(if_all(all_of(needed), ~ !is.na(.x))) %>%
    select(player_id, season, ends_with(paste0("_lb", k)))
}

# Build lb1..lb5 tables and join them back (wide)
lb_tables <- lapply(1:5, function(k) make_lb_means(joined, k))
lookbacks_wide <- reduce(lb_tables, ~ full_join(.x, .y, by = c("player_id","season")))

# Final lookback feature table: one row per (player_id, season) with lb1..lb5 columns
lookbacks <- lookbacks_wide %>%
  arrange(player_id, season)

# Quick sanity: how many target seasons have a valid lookback of each depth?
lb_counts <- tibble(
  k = 1:5,
  n_rows = sapply(1:5, function(k) {
    cols_k <- grep(paste0("_lb", k, "$"), names(lookbacks), value = TRUE)
    lookbacks %>%
      filter(if_all(all_of(cols_k), ~ !is.na(.x))) %>%
      nrow()
  })
)

print(lb_counts)
message("Built lookback features: columns end with _lb1, _lb2, _lb3, _lb4, _lb5")

# Example peek: show one player's row with all lookbacks
# (Change the player_id below to someone you know is present)
# lookbacks %>% filter(player_id == "jamesle01") %>% head()

# =====================================================
# 5) Join lookbacks (t−k averages) to actual season t stats
#     -> evaluation-ready data (wide and long)
# =====================================================

library(dplyr)
library(tidyr)
library(stringr)

# Targets: actual per-36/rate stats for season t
stats_vec <- c(
  "pts_per_36_min","trb_per_36_min","ast_per_36_min",
  "stl_per_36_min","blk_per_36_min","x3p_per_36_min",
  "fg_percent","ft_percent","tov_per_36_min"
)

targets <- df_clean %>%
  select(player_id, season, all_of(stats_vec))

# Wide: one row per (player_id, season) containing:
#   - actual stats for season t
#   - lookback means *_lb1..*_lb5 (when available)
eval_wide <- targets %>%
  inner_join(lookbacks, by = c("player_id", "season")) %>%
  arrange(player_id, season)

message("Rows in eval_wide: ", nrow(eval_wide))
# peek a few columns
eval_wide %>%
  select(player_id, season, pts_per_36_min, starts_with("pts_per_36_min_lb")) %>%
  head() %>%
  print()

# ----- Optional: long format for easy per-k evaluation later -----
# This produces rows like:
#   player_id, season, k, stat, lb_value, actual_value
eval_long <- eval_wide %>%
  # gather actuals
  pivot_longer(
    cols = all_of(stats_vec),
    names_to = "stat",
    values_to = "actual_value"
  ) %>%
  # gather lookbacks
  pivot_longer(
    cols = matches("_lb[1-5]$"),
    names_to = "lb_metric",
    values_to = "lb_value"
  ) %>%
  # keep matching stat only (e.g., pts_per_36_min with pts_per_36_min_lbK)
  filter(str_remove(lb_metric, "_lb[1-5]$") == stat) %>%
  mutate(
    k = as.integer(str_remove(str_extract(lb_metric, "lb[1-5]$"), "lb"))
  ) %>%
  select(player_id, season, k, stat, lb_value, actual_value) %>%
  arrange(player_id, season, k, stat)

# Sanity: panel sizes per k (how many target seasons have a valid k lookback)
panel_sizes <- eval_long %>%
  group_by(k) %>%
  summarise(n_seasons = n_distinct(paste(player_id, season)), .groups = "drop")

print(panel_sizes)

# Example: LeBron check — see his target seasons and which k are available
eval_long %>%
  filter(player_id == "jamesle01") %>%
  distinct(season, k) %>%
  arrange(season, k) %>%
  print(n = Inf)

# =====================================================
# 6) Evaluate lookback performance by k and stat
#     Metrics: RMSE, MAE, Bias, R²
# =====================================================

library(dplyr)
library(tidyr)

# Safety: drop any rows with missing values
eval_ok <- eval_long %>%
  filter(!is.na(lb_value), !is.na(actual_value))

metrics_by_k_stat <- eval_ok %>%
  group_by(stat, k) %>%
  summarise(
    n = n(),
    rmse = sqrt(mean((lb_value - actual_value)^2)),
    mae  = mean(abs(lb_value - actual_value)),
    bias = mean(lb_value - actual_value),  # positive => overpredicting
    sse  = sum((actual_value - lb_value)^2),
    sst  = sum((actual_value - mean(actual_value))^2),
    r2   = ifelse(sst > 0, 1 - sse/sst, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(stat, k)

# View the full metrics table
print(metrics_by_k_stat)

# Wide RMSE table (stats as rows, k as columns) for quick scanning
rmse_wide <- metrics_by_k_stat %>%
  select(stat, k, rmse) %>%
  pivot_wider(names_from = k, values_from = rmse, names_prefix = "lb")
print(rmse_wide)

# Best k per stat (primary = RMSE, tiebreaker = MAE)
best_k_by_stat <- metrics_by_k_stat %>%
  group_by(stat) %>%
  arrange(rmse, mae, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(stat)

cat("\n===== Best lookback per stat (by RMSE) =====\n")
print(best_k_by_stat)

# (Optional) Overall winner across all stats by macro-averaged RMSE
overall_by_k <- metrics_by_k_stat %>%
  group_by(k) %>%
  summarise(
    stats_covered = n_distinct(stat),
    rmse_mean = mean(rmse, na.rm = TRUE),
    mae_mean  = mean(mae,  na.rm = TRUE),
    r2_mean   = mean(r2,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rmse_mean)

cat("\n===== Macro-averaged metrics across stats =====\n")
print(overall_by_k)

# =====================================================
# 7) Break down results by stat
#     Visualize and rank best lookback per stat
# =====================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# We'll reuse metrics_by_k_stat from the previous step

# ----- Table view -----
best_k_table <- metrics_by_k_stat %>%
  group_by(stat) %>%
  arrange(rmse, mae, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(stat, best_k = k, rmse, mae, r2)

cat("\n===== Best lookback per stat =====\n")
print(best_k_table)

# ----- Bar chart: RMSE by lookback window for each stat -----
ggplot(metrics_by_k_stat, aes(x = factor(k), y = rmse, group = stat, color = stat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ stat, scales = "free_y") +
  labs(
    title = "RMSE by Lookback Window (Per Stat)",
    x = "Lookback window (number of past seasons averaged)",
    y = "RMSE (lower = better)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10)
  )

# ----- Optional: Which stats are most predictable overall -----
stat_stability <- metrics_by_k_stat %>%
  group_by(stat) %>%
  summarise(
    mean_rmse = mean(rmse, na.rm = TRUE),
    mean_r2   = mean(r2, na.rm = TRUE),
    best_k    = best_k_table$best_k[match(stat, best_k_table$stat)],
    .groups = "drop"
  ) %>%
  arrange(mean_rmse)

cat("\n===== Average Predictability per Stat =====\n")
print(stat_stability)

# =====================================================
# 8) Normalize RMSE by stat mean (relative error)
# =====================================================

rel_error <- eval_ok %>%
  group_by(stat) %>%
  summarise(mean_actual = mean(actual_value, na.rm = TRUE), .groups = "drop")

metrics_scaled <- metrics_by_k_stat %>%
  left_join(rel_error, by = "stat") %>%
  mutate(
    rmse_rel = rmse / mean_actual,
    mae_rel  = mae  / mean_actual
  ) %>%
  arrange(stat, k)

# Summarize relative RMSE by stat
rel_summary <- metrics_scaled %>%
  group_by(stat) %>%
  summarise(
    best_k = k[which.min(rmse_rel)],
    rel_rmse_mean = mean(rmse_rel, na.rm = TRUE),
    rel_r2_mean   = mean(r2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rel_rmse_mean)

print(rel_summary)

# =====================================================
# 9) Per-stat linear regressions (self-contained)
# =====================================================

library(dplyr)
library(purrr)
library(broom)

# --- Define the stats vector again (in case not in memory) ---
stats_vec <- c(
  "pts_per_36_min","trb_per_36_min","ast_per_36_min",
  "stl_per_36_min","blk_per_36_min","x3p_per_36_min",
  "fg_percent","ft_percent","tov_per_36_min"
)

# --- Rebuild joined dataset with lag columns (L1..L5) ---
base <- df_clean %>%
  select(player_id, season, all_of(stats_vec))

lagged_list <- lapply(1:5, function(i) {
  base %>%
    rename_with(~ paste0(.x, "_L", i), all_of(stats_vec)) %>%
    mutate(season = season + i)
})

joined <- reduce(lagged_list, ~ left_join(.x, .y, by = c("player_id", "season")), .init = base)

# --- Function: fit regressions for each stat ---
fit_models_for_stat <- function(stat) {
  df <- joined %>%
    select(player_id, season,
           all_of(stat),
           matches(paste0("^", stat, "_L[1-5]$"))) %>%
    drop_na()
  
  map_dfr(1:5, function(k) {
    lag_cols <- paste0(stat, "_L", 1:k)
    formula <- as.formula(paste(stat, "~", paste(lag_cols, collapse = " + ")))
    fit <- lm(formula, data = df)
    glance(fit) %>%
      select(r.squared, adj.r.squared, sigma) %>%
      mutate(k = k)
  }, .id = NULL) %>%
    mutate(stat = stat)
}

# --- Run across all stats ---
lm_metrics <- map_dfr(stats_vec, fit_models_for_stat)

# --- Identify best k per stat ---
best_k_lm <- lm_metrics %>%
  group_by(stat) %>%
  slice_max(adj.r.squared, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(adj.r.squared))

cat("\n===== Best lookback depth (linear regression) per stat =====\n")
print(best_k_lm)

# --- Optional: inspect coefficients for a specific stat (e.g., pts_per_36_min) ---
get_coefs_for_stat <- function(stat) {
  df <- joined %>%
    select(player_id, season,
           all_of(stat),
           matches(paste0("^", stat, "_L[1-5]$"))) %>%
    drop_na()
  
  map_dfr(1:5, function(k) {
    lag_cols <- paste0(stat, "_L", 1:k)
    formula <- as.formula(paste(stat, "~", paste(lag_cols, collapse = " + ")))
    fit <- lm(formula, data = df)
    tidy(fit) %>% mutate(k = k, stat = stat)
  })
}

# Example: get coefficients for Points per 36
pts_coefs <- get_coefs_for_stat("pts_per_36_min")
cat("\n===== Example coefficients for PTS/36 =====\n")
print(pts_coefs %>% select(stat, k, term, estimate))

# =====================================================
# 10) Visualize lookback coefficients (memory decay)
# =====================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(broom)

# If lm_coefs_all not already created, generate it
if (!exists("lm_coefs_all")) {
  get_coefs_for_stat <- function(stat) {
    df <- joined %>%
      select(player_id, season,
             all_of(stat),
             matches(paste0("^", stat, "_L[1-5]$"))) %>%
      drop_na()
    map_dfr(1:5, function(k) {
      lag_cols <- paste0(stat, "_L", 1:k)
      formula <- as.formula(paste(stat, "~", paste(lag_cols, collapse = " + ")))
      fit <- lm(formula, data = df)
      tidy(fit) %>% mutate(k = k, stat = stat)
    })
  }
  lm_coefs_all <- map_dfr(stats_vec, get_coefs_for_stat)
}

# Filter to slope terms only (exclude intercepts)
lm_coefs_clean <- lm_coefs_all %>%
  filter(str_detect(term, "_L\\d$")) %>%
  mutate(
    lag = as.integer(str_extract(term, "\\d$")),
    stat = factor(stat, levels = stats_vec)
  )

# ---- Plot ----
ggplot(lm_coefs_clean, aes(x = lag, y = estimate, group = stat, color = stat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ stat, scales = "free_y") +
  labs(
    title = "Regression Coefficients by Lag (Memory Decay Across Seasons)",
    subtitle = "β estimates from per-stat linear models, showing how much each past season contributes",
    x = "Lag (years back)",
    y = "Coefficient (weight on past season)",
    color = "Stat"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10)
  )
# =====================================================
# 11) Three predictors for next-season stats:
#     - Equal average (k years)
#     - Regression-weighted (fit on history only)
#     - Exponential-weighted (alpha decay)
#     + Rolling (no leakage) evaluation helper
# =====================================================

library(dplyr)
library(purrr)
library(broom)
library(stringr)
library(tidyr)

# ---- Inputs you already have ----
# df_clean: merged, filtered (season >= 2015, mp >= 1000)
stats_vec <- c(
  "pts_per_36_min","trb_per_36_min","ast_per_36_min",
  "stl_per_36_min","blk_per_36_min","x3p_per_36_min",
  "fg_percent","ft_percent","tov_per_36_min"
)

# Rebuild lag table with L1..L5 (safe to reuse)
base <- df_clean %>% select(player_id, season, all_of(stats_vec))
lagged_list <- lapply(1:5, function(i) {
  base %>%
    rename_with(~ paste0(.x, "_L", i), all_of(stats_vec)) %>%
    mutate(season = season + i)
})
joined <- reduce(lagged_list, ~ left_join(.x, .y, by = c("player_id","season")), .init = base)

# -----------------------------------------------------
# Equal-average predictor: mean of t-1..t-k (no fitting)
# -----------------------------------------------------
predict_equal_k <- function(df, stat, k) {
  lag_cols <- paste0(stat, "_L", 1:k)
  df %>%
    filter(if_all(all_of(lag_cols), ~ !is.na(.x))) %>%
    mutate(pred_equal = rowMeans(across(all_of(lag_cols)), na.rm = FALSE)) %>%
    select(player_id, season, actual = !!stat, pred_equal)
}

# -----------------------------------------------------
# Exponential-weighted predictor: alpha decay (no fitting)
# weights: w_i = alpha * (1-alpha)^(i-1), i = 1..k (L1 newest)
# -----------------------------------------------------
predict_exp_k <- function(df, stat, k, alpha = 0.5) {
  lag_cols <- paste0(stat, "_L", 1:k)
  w <- alpha * (1 - alpha)^(0:(k-1))
  w <- w / sum(w)
  df %>%
    filter(if_all(all_of(lag_cols), ~ !is.na(.x))) %>%
    mutate(pred_exp = as.numeric(as.matrix(select(., all_of(lag_cols))) %*% w)) %>%
    select(player_id, season, actual = !!stat, pred_exp)
}

# -----------------------------------------------------
# Regression-weighted predictor:
#   Fit on TRAIN seasons only, then predict on TEST seasons
#   (avoids leakage; fits per-stat and per-k)
# -----------------------------------------------------
fit_and_predict_reg_k <- function(df, stat, k, train_end_season) {
  lag_cols <- paste0(stat, "_L", 1:k)
  d <- df %>%
    select(player_id, season, all_of(stat), all_of(lag_cols)) %>%
    drop_na()
  
  train <- d %>% filter(season <= train_end_season)
  test  <- d %>% filter(season == train_end_season + 1)  # <-- only next season
  
  if (nrow(train) == 0 || nrow(test) == 0) return(tibble())
  
  form <- as.formula(paste(stat, "~", paste(lag_cols, collapse = " + ")))
  fit  <- lm(form, data = train)
  
  test %>%
    mutate(pred_lm = predict(fit, newdata = test)) %>%
    select(player_id, season, actual = !!stat, pred_lm)
}


# -----------------------------------------------------
# Rolling-origin evaluation (no leakage) for any method
# Usage:
#   method = "equal" | "exp" | "lm"
#   For "exp", pass alpha; for "lm", pass nothing; supply k
# -----------------------------------------------------
evaluate_rolling <- function(df_joined, stat, k, method = c("equal","exp","lm"),
                             alpha = 0.5,
                             first_train_end = 2018,
                             last_test_season = max(df_joined$season)) {
  method <- match.arg(method)
  
  seasons <- sort(unique(df_joined$season))
  stops <- seasons[seasons >= first_train_end & seasons < last_test_season]
  out <- list()
  
  for (te in stops) {
    if (method == "equal") {
      preds <- predict_equal_k(df_joined %>% filter(season <= te + 1), stat, k) %>% filter(season > te)
    } else if (method == "exp") {
      preds <- predict_exp_k(df_joined %>% filter(season <= te + 1), stat, k, alpha) %>% filter(season > te)
    } else {
      preds <- fit_and_predict_reg_k(df_joined, stat, k, train_end_season = te)
    }
    if (nrow(preds)) out[[length(out)+1]] <- preds %>% mutate(train_end = te)
  }
  
  bind_rows(out)
}

# -----------------------------------------------------
# Helper to compute metrics
# -----------------------------------------------------
compute_metrics <- function(df, pred_col = "pred") {
  df %>%
    mutate(err = .data[[pred_col]] - actual) %>%
    summarise(
      n = n(),
      rmse = sqrt(mean(err^2)),
      mae  = mean(abs(err)),
      bias = mean(err),
      sse  = sum((actual - .data[[pred_col]])^2),
      sst  = sum((actual - mean(actual))^2),
      r2   = ifelse(sst > 0, 1 - sse/sst, NA_real_)
    )
}

# ============================
# EXAMPLE: compare methods
# ============================
# Pick a stat and k:
stat <- "pts_per_36_min"; k <- 3

eq_res  <- evaluate_rolling(joined, stat, k, method = "equal", first_train_end = 2018)
exp_res <- evaluate_rolling(joined, stat, k, method = "exp",   alpha = 0.5, first_train_end = 2018)
lm_res  <- evaluate_rolling(joined, stat, k, method = "lm",    first_train_end = 2018)

cat("\nPTS/36, k=3\n")
cat("Equal avg   -> "); print(compute_metrics(eq_res,  "pred_equal"))
cat("Exp (a=0.5) -> "); print(compute_metrics(exp_res, "pred_exp"))
cat("Linear Reg  -> "); print(compute_metrics(lm_res,  "pred_lm"))

# (Optional) grid search alpha for exponential:
grid_alpha <- c(0.3, 0.4, 0.5, 0.6, 0.7)
exp_grid <- map_dfr(grid_alpha, function(a) {
  r <- evaluate_rolling(joined, stat, k, method = "exp", alpha = a, first_train_end = 2018)
  m <- compute_metrics(r, "pred_exp") %>% mutate(alpha = a)
  m
})
exp_grid %>% arrange(rmse)

# =====================================================
# 12) Leaderboard: best method per stat (Equal vs Exp vs Linear Reg)
# =====================================================

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# ---- helpers already defined earlier ----
# predict_equal_k, predict_exp_k, fit_and_predict_reg_k
# evaluate_rolling, compute_metrics
# joined (with _L1.._L5), stats_vec

alpha_grid <- c(0.3, 0.4, 0.5, 0.6, 0.7)
k_grid <- 1:5
first_train_end <- 2018

evaluate_equal <- function(stat, k) {
  res <- evaluate_rolling(joined, stat, k, method = "equal", first_train_end = first_train_end)
  if (nrow(res) == 0) return(NULL)
  compute_metrics(res, "pred_equal") %>%
    mutate(method = "equal", stat = stat, k = k, alpha = NA_real_)
}

evaluate_exp <- function(stat, k, alpha) {
  res <- evaluate_rolling(joined, stat, k, method = "exp", alpha = alpha, first_train_end = first_train_end)
  if (nrow(res) == 0) return(NULL)
  compute_metrics(res, "pred_exp") %>%
    mutate(method = "exp", stat = stat, k = k, alpha = alpha)
}

evaluate_lm <- function(stat, k) {
  res <- evaluate_rolling(joined, stat, k, method = "lm", first_train_end = first_train_end)
  if (nrow(res) == 0) return(NULL)
  compute_metrics(res, "pred_lm") %>%
    mutate(method = "lm", stat = stat, k = k, alpha = NA_real_)
}

compare_methods_for_stat <- function(stat) {
  eq <- map_dfr(k_grid, ~ evaluate_equal(stat, .x))
  ex <- map_dfr(k_grid, ~ map_dfr(alpha_grid, function(a) evaluate_exp(stat, .x, a)))
  lm <- map_dfr(k_grid, ~ evaluate_lm(stat, .x))
  bind_rows(eq, ex, lm) %>%
    arrange(rmse) %>%
    mutate(rank = row_number())
}

# Run across all stats
all_results <- map_dfr(stats_vec, compare_methods_for_stat)

# Leaderboard: pick best per stat, plus margin vs 2nd best
winners <- all_results %>%
  group_by(stat) %>%
  arrange(rmse, .by_group = TRUE) %>%
  mutate(rmse_next_best = lead(rmse)) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    stat,
    best_method = method,
    best_k = k,
    best_alpha = alpha,
    rmse,
    mae,
    r2,
    margin_vs_next = rmse_next_best - rmse
  ) %>%
  arrange(stat)

cat("\n===== Best method per stat =====\n")
print(winners, n = Inf)

# Optional: quick summary counts
cat("\nWins by method:\n")
print(winners %>% count(best_method))

cat("\nAvg RMSE by method (winners only):\n")
print(winners %>% group_by(best_method) %>% summarise(avg_rmse = mean(rmse), .groups = "drop"))

# =====================================================
# 17) 2026 predictions with BINNED YoY adjustments
#     - Uses winners (method/k/alpha) per stat
#     - Requires players to have season 2025 in df_clean (>=1000 mp)
#     - Graceful k fallback: k -> k-1 -> ... -> 1
#     - Binned YoY adjustment added (2–3, 4–6, 7–10, 11–15, 16+)
#     - Guarantees 9 stats per eligible player
#     - Player names populated; no duplicates
# =====================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(broom)

T_target <- 2026

# ---------- Helper: build feature frame for T using T-1..T-5 ----------
make_features_for_target <- function(df_base, T, k_max = 5, stats = stats_vec) {
  feat <- df_base %>%
    filter(season == T - 1) %>%           # ensures eligibility via presence in 2025
    distinct(player_id) %>%
    mutate(season = T)
  for (i in 1:k_max) {
    lag_i <- df_base %>%
      filter(season == T - i) %>%
      select(player_id, all_of(stats))
    names(lag_i)[-1] <- paste0(names(lag_i)[-1], "_L", i)
    feat <- feat %>% left_join(lag_i, by = "player_id")
  }
  feat
}

# ---------- Simple predictors on a pre-built feature frame ----------
predict_equal_k_on_features <- function(feat, stat, k) {
  lag_cols <- paste0(stat, "_L", 1:k)
  feat %>%
    filter(if_all(all_of(lag_cols), ~ !is.na(.))) %>%
    mutate(pred = rowMeans(across(all_of(lag_cols)), na.rm = FALSE)) %>%
    transmute(player_id, season, stat = stat, pred, method = "equal", k_used = k, alpha = NA_real_)
}

predict_exp_k_on_features <- function(feat, stat, k, alpha = 0.5) {
  lag_cols <- paste0(stat, "_L", 1:k)
  w <- alpha * (1 - alpha)^(0:(k-1)); w <- w / sum(w)
  feat %>%
    filter(if_all(all_of(lag_cols), ~ !is.na(.))) %>%
    mutate(pred = as.numeric(as.matrix(select(., all_of(lag_cols))) %*% w)) %>%
    transmute(player_id, season, stat = stat, pred, method = "exp", k_used = k, alpha = alpha)
}

# ---------- LM predictor for FUTURE (train ≤ T-1, predict T) ----------
fit_predict_lm_k_for_future <- function(df_hist, featT, stat, k, T) {
  base_hist <- df_hist %>% select(player_id, season, all_of(stats_vec))
  lagged_list <- lapply(1:5, function(i) {
    base_hist %>%
      rename_with(~ paste0(.x, "_L", i), all_of(stats_vec)) %>%
      mutate(season = season + i)
  })
  joined_hist <- reduce(lagged_list, ~ left_join(.x, .y, by = c("player_id","season")), .init = base_hist)
  
  lag_cols <- paste0(stat, "_L", 1:k)
  train <- joined_hist %>%
    select(player_id, season, all_of(stat), all_of(lag_cols)) %>%
    filter(season <= T - 1) %>%
    drop_na()
  if (nrow(train) == 0) {
    return(tibble(player_id = character(), season = integer(), stat = character(),
                  pred = double(), method = character(), k_used = integer(), alpha = double()))
  }
  
  form <- as.formula(paste(stat, "~", paste(lag_cols, collapse = " + ")))
  fit  <- lm(form, data = train)
  
  featT %>%
    filter(if_all(all_of(lag_cols), ~ !is.na(.))) %>%
    mutate(pred = predict(fit, newdata = .)) %>%
    transmute(player_id, season, stat = stat, pred, method = "lm", k_used = k, alpha = NA_real_)
}

# ---------- Graceful-fallback predictor for ALL stats (base preds) ----------
predict_target_season_all_fallback <- function(T = 2026) {
  featT <- make_features_for_target(df_clean %>% select(player_id, season, all_of(stats_vec)), T, k_max = 5, stats = stats_vec)
  
  out_list <- vector("list", nrow(winners))
  for (i in seq_len(nrow(winners))) {
    row   <- winners[i, ]
    stat  <- row$stat
    meth  <- row$best_method
    k_top <- as.integer(row$best_k)
    alpha <- row$best_alpha
    
    ks <- k_top:1
    preds_k <- map_dfr(ks, function(kk) {
      if (meth == "equal") {
        predict_equal_k_on_features(featT, stat, kk)
      } else if (meth == "exp") {
        predict_exp_k_on_features(featT, stat, kk, ifelse(is.na(alpha), 0.5, alpha))
      } else { # "lm"
        fit_predict_lm_k_for_future(df_clean, featT, stat, kk, T)
      }
    })
    
    # For each player, keep the highest k_used available
    preds_best <- preds_k %>%
      arrange(player_id, desc(k_used)) %>%
      group_by(player_id) %>%
      slice_head(n = 1) %>%
      ungroup()
    
    out_list[[i]] <- preds_best
  }
  
  # Bind and enforce one row per (player_id, season, stat)
  bind_rows(out_list) %>%
    group_by(player_id, season, stat) %>%
    summarise(
      pred   = first(pred),
      method = first(method),
      k_used = first(k_used),
      alpha  = first(alpha),
      .groups = "drop"
    )
}

# ---------- Build binned YoY adjustments (2–3, 4–6, 7–10, 11–15, 16+) ----------
# Recreate YoY deltas if not in memory
if (!exists("career")) {
  career <- read_csv("PlayerCareerInfo.csv", show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    select(player_id, player_career = player, from) %>%
    mutate(from = as.integer(from))
}

# Rebuild YoY (self-contained)
df_exp <- df_clean %>%
  select(player_id, season, mp, all_of(stats_vec)) %>%
  left_join(career, by = "player_id") %>%
  mutate(exp_year_t = ifelse(!is.na(from), season - from + 1L, NA_integer_))

lag_tbl <- df_exp %>%
  select(player_id, season, all_of(stats_vec)) %>%
  mutate(season = season + 1L) %>%
  rename_with(~ paste0(.x, "_lag1"), all_of(stats_vec))

yoy <- df_exp %>%
  inner_join(lag_tbl, by = c("player_id","season")) %>%
  mutate(across(all_of(stats_vec),
                ~ .x - get(paste0(cur_column(), "_lag1")),
                .names = "{.col}_delta"))

yoy_long <- yoy %>%
  select(player_id, season, exp_year_t, ends_with("_delta")) %>%
  pivot_longer(cols = ends_with("_delta"),
               names_to = "stat", values_to = "delta") %>%
  mutate(
    stat = str_remove(stat, "_delta$"),
    exp_year = exp_year_t
  ) %>%
  filter(!is.na(delta), !is.na(exp_year), exp_year >= 2)

# Bins
exp_breaks <- c(2, 4, 7, 11, 16, Inf)
exp_labels <- c("2–3", "4–6", "7–10", "11–15", "16+")
yoy_binned <- yoy_long %>%
  mutate(exp_bin = cut(exp_year, breaks = exp_breaks, labels = exp_labels, right = FALSE)) %>%
  group_by(stat, exp_bin) %>%
  summarise(
    n_pairs    = n(),
    mean_delta = mean(delta, na.rm = TRUE),
    .groups = "drop"
  )

# Make a lookup (stat, exp_bin) -> mean_delta
delta_lookup <- yoy_binned %>%
  select(stat, exp_bin, mean_delta)

# Helper to map EXP → bin label
bin_exp <- function(exp_year) {
  cut(exp_year, breaks = exp_breaks, labels = exp_labels, right = FALSE)
}

# ---------- Run base predictions with fallback ----------
preds_2026_base <- predict_target_season_all_fallback(T_target)

# ---------- Ensure ALL 9 stats per eligible player (robust L1..L5 fallback) ----------
features_2026 <- make_features_for_target(
  df_clean %>% select(player_id, season, all_of(stats_vec)),
  T = T_target, k_max = 5, stats = stats_vec
)
eligible_ids <- features_2026 %>% distinct(player_id)

grid_all <- eligible_ids %>%
  tidyr::crossing(stat = stats_vec) %>%
  mutate(season = T_target)

preds_joined <- grid_all %>%
  left_join(preds_2026_base, by = c("player_id","season","stat"))

missing_rows <- preds_joined %>%
  filter(is.na(pred)) %>%
  select(player_id, season, stat)

if (nrow(missing_rows) > 0) {
  cand <- missing_rows %>%
    left_join(features_2026, by = c("player_id","season"))
  
  latest_non_na <- function(df, cols) {
    out <- rep(NA_real_, nrow(df))
    for (i in seq_along(cols)) {
      v <- df[[cols[i]]]
      idx <- is.na(out) & !is.na(v)
      out[idx] <- v[idx]
      if (all(!is.na(out))) break
    }
    out
  }
  which_lag_used <- function(df, cols) {
    out <- rep(NA_integer_, nrow(df))
    for (i in seq_along(cols)) {
      v <- df[[cols[i]]]
      idx <- is.na(out) & !is.na(v)
      out[idx] <- i
      if (all(!is.na(out))) break
    }
    out
  }
  
  fallback_list <- lapply(stats_vec, function(s) {
    sub <- cand %>% filter(stat == s)
    if (nrow(sub) == 0) return(NULL)
    lag_cols <- paste0(s, "_L", 1:5)
    # Guard: keep only lag columns that actually exist
    lag_cols <- lag_cols[lag_cols %in% names(sub)]
    pred_val <- latest_non_na(sub, lag_cols)
    k_used   <- which_lag_used(sub, lag_cols)
    tibble(
      player_id = sub$player_id,
      season    = sub$season,
      stat      = s,
      pred_fb   = pred_val,
      method_fb = "equal",
      k_used_fb = k_used,
      alpha_fb  = NA_real_
    )
  })
  
  fallback_any <- bind_rows(fallback_list)
  
  preds_2026_base <- preds_joined %>%
    left_join(fallback_any, by = c("player_id","season","stat")) %>%
    transmute(
      player_id, season, stat,
      pred   = dplyr::coalesce(pred, pred_fb),
      method = dplyr::coalesce(method, method_fb),
      k_used = dplyr::coalesce(k_used, k_used_fb),
      alpha  = dplyr::coalesce(alpha, alpha_fb)
    ) %>%
    group_by(player_id, season, stat) %>%
    summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")
}

# Sanity: everyone should now have 9 stats
stopifnot(all(
  preds_2026_base %>% count(player_id, season, name = "n_stats") %>% pull(n_stats) ==
    length(stats_vec)
))

# ---------- Compute 2026 experience and add BINNED adjustments ----------
# Load career info (already in memory) and compute EXP in 2026
exp_2026 <- eligible_ids %>%
  mutate(season = T_target) %>%
  left_join(career %>% select(player_id, from), by = "player_id") %>%
  mutate(exp_year_2026 = ifelse(!is.na(from), season - from + 1L, NA_integer_),
         exp_bin = bin_exp(exp_year_2026))

# Join delta per stat/bin; if no delta for a bin (rare), default to 0
preds_2026_adj <- preds_2026_base %>%
  left_join(exp_2026 %>% select(player_id, season, exp_year_2026, exp_bin), by = c("player_id","season")) %>%
  left_join(delta_lookup, by = c("stat","exp_bin")) %>%
  mutate(delta_exp = ifelse(is.na(mean_delta), 0, mean_delta)) %>%
  mutate(pred_adj_raw = pred + delta_exp)

# Clip % stats to [0,1]
is_pct_stat <- function(s) s %in% c("fg_percent","ft_percent")
preds_2026_adj <- preds_2026_adj %>%
  mutate(pred_adj = ifelse(is_pct_stat(stat),
                           pmax(0, pmin(1, pred_adj_raw)),
                           pred_adj_raw))

# ---------- Label with player names/positions for 2026 (from 2025 or latest ≤2025) ----------
get_roster_for_target <- function(T) {
  df_clean %>%
    filter(season <= T - 1) %>%
    arrange(player_id, desc(season), desc(mp)) %>%
    group_by(player_id) %>%
    summarise(
      player = dplyr::first(player),
      pos    = dplyr::first(pos),
      .groups = "drop"
    ) %>%
    mutate(season = T) %>%
    select(player_id, season, player, pos)
}
roster_2026 <- get_roster_for_target(T_target)

preds_2026_final <- preds_2026_adj %>%
  left_join(roster_2026, by = c("player_id","season")) %>%
  select(player_id, player, pos, season, stat,
         pred_base = pred, delta_exp, pred_adj,
         method, k_used, alpha, exp_year_2026, exp_bin) %>%
  arrange(player, stat)

# ---------- De-dup and final checks ----------
preds_2026_final <- preds_2026_final %>%
  group_by(player_id, season, stat) %>%
  summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")

dup_check <- preds_2026_final %>%
  count(player_id, season, stat) %>%
  filter(n > 1)
cat("Remaining duplicates:", nrow(dup_check), "\n")

cov_summary <- preds_2026_final %>%
  count(player_id, season, name = "n_stats")
cat("Players with full 9 stats:", sum(cov_summary$n_stats == length(stats_vec)), " / ", nrow(cov_summary), "\n")

# ---------- (Optional) Save ----------
# readr::write_csv(preds_2026_final, "predictions_2026_binned_adjusted.csv")
# preds_2026_wide <- preds_2026_final %>%
#   select(player_id, player, pos, season, stat, pred_adj) %>%
#   pivot_wider(names_from = stat, values_from = pred_adj)
# readr::write_csv(preds_2026_wide, "predictions_2026_binned_adjusted_wide.csv")

# Quick peek
preds_2026_final %>% group_by(stat) %>% slice_head(n = 5) %>% ungroup() %>% print(n = 45)

# =====================================================
# 19) Wide table: one row per player, columns = stats
#     (values = pred_adj)
# =====================================================

library(dplyr)
library(tidyr)

preds_2026_wide <- preds_2026_final %>%
  select(
    player_id, player, pos, season, exp_year_2026, exp_bin,
    stat, pred_adj
  ) %>%
  tidyr::pivot_wider(
    names_from  = stat,
    values_from = pred_adj
  ) %>%
  arrange(player)

# quick peek
preds_2026_wide %>% slice_head(n = 10)

# optional: save to disk
readr::write_csv(preds_2026_wide, "predictions_2026_binned_adjusted_wide.csv")

