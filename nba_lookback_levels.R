# =====================================================
# NBA Per-36 Projection Pipeline (Cleaned)
# =====================================================
# What this script does (high level):
# 1) Load Per36Minutes.csv and robustly collapse per-team rows into per-season rows.
#    - Correctly handles combined "TOT/2TM/..." rows.
#    - Recomputes FG%/FT% from totals; minutes-weighted per-36 for team splits.
#    - Filters to seasons >= 2015 and mp >= 1000 -> df_clean.
# 2) Build lookback features (L1..L5), evaluate equal/exp/LM methods, pick best per stat.
# 3) Compute experience-binned YoY deltas (2–3, 4–6, 7–10, 11–15, 16+).
# 4) Predict 2026 per-36 stats with graceful fallback + add YoY deltas; clip percentages.

#
# Notes:
# - Keep object names stable across steps to make downstream joins simple.
# - This is a self-contained, runnable script; adjust file paths as needed.
# =====================================================

# ------------------ 1) Setup ------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(broom)
})

# File paths (adjust if needed)
path_per36 <- "Per36Minutes.csv"
path_career <- "PlayerCareerInfo.csv"


# Target season
T_target   <- 2026

# ------------------ 2) Load & robust season collapse ------------------
raw <- readr::read_csv(path_per36, show_col_types = FALSE)
names(raw) <- tolower(names(raw))

# helper: weighted average
wavg <- function(x, w) ifelse(sum(w, na.rm = TRUE) > 0,
                              sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
                              NA_real_)

tot_tags <- c("TOT","2TM","3TM","4TM","5TM","6TM","7TM")
team_col <- dplyr::first(intersect(c("team","tm"), names(raw)))

by_player_season <- raw %>%
  group_by(player_id, season) %>%
  summarise({
    df <- cur_data_all()

    # detect combined row (TOT/2TM) if team column exists; else G equality fallback
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

    # minutes-weighted per-36 across team stints
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

    # labels from max-minute row
    idx_mm <- ifelse(all(is.na(df$mp)), 1L, which.max(df$mp))
    tibble::tibble(
      player = df$player[idx_mm], pos = df$pos[idx_mm], lg = df$lg[idx_mm], age = df$age[idx_mm],
      g = g_val, mp = mp_val, mpg = ifelse(g_val > 0, mp_val / g_val, NA_real_),
      fg_percent = fg_pct, ft_percent = ft_pct,
      pts_per_36_min = pts36, trb_per_36_min = trb36, ast_per_36_min = ast36,
      stl_per_36_min = stl36, blk_per_36_min = blk36, x3p_per_36_min = x3p36, tov_per_36_min = tov36,
      fga_per_36_min = fga36, fta_per_36_min = fta36
    )
  }, .groups = "drop")

# Filter window -> df_clean
df_clean <- by_player_season %>%
  filter(season >= 2015, mp >= 1000) %>%
  select(
    player_id, player, pos, season, mp,
    fg_percent, ft_percent,
    pts_per_36_min, trb_per_36_min, ast_per_36_min,
    stl_per_36_min, blk_per_36_min, x3p_per_36_min, tov_per_36_min,
    fga_per_36_min, fta_per_36_min
  )

# ------------------ 3) Lookbacks & model selection ------------------
stats_vec <- c(
  "pts_per_36_min","trb_per_36_min","ast_per_36_min",
  "stl_per_36_min","blk_per_36_min","x3p_per_36_min",
  "fg_percent","ft_percent","tov_per_36_min"
)

# build lag table L1..L5
base <- df_clean %>% select(player_id, season, all_of(stats_vec))
lagged_list <- lapply(1:5, function(i) {
  base %>%
    rename_with(~ paste0(.x, "_L", i), all_of(stats_vec)) %>%
    mutate(season = season + i)
})
joined <- Reduce(function(x, y) left_join(x, y, by = c("player_id","season")), lagged_list, init = base)

# predictors
predict_equal_k <- function(df, stat, k) {
  lag_cols <- paste0(stat, "_L", 1:k)
  df %>%
    filter(if_all(all_of(lag_cols), ~ !is.na(.x))) %>%
    mutate(pred_equal = rowMeans(across(all_of(lag_cols)), na.rm = FALSE)) %>%
    select(player_id, season, actual = !!stat, pred_equal)
}
predict_exp_k <- function(df, stat, k, alpha = 0.5) {
  lag_cols <- paste0(stat, "_L", 1:k)
  w <- alpha * (1 - alpha)^(0:(k-1)); w <- w / sum(w)
  df %>%
    filter(if_all(all_of(lag_cols), ~ !is.na(.x))) %>%
    mutate(pred_exp = as.numeric(as.matrix(select(., all_of(lag_cols))) %*% w)) %>%
    select(player_id, season, actual = !!stat, pred_exp)
}
fit_and_predict_reg_k <- function(df, stat, k, train_end_season) {
  lag_cols <- paste0(stat, "_L", 1:k)
  d <- df %>% select(player_id, season, all_of(stat), all_of(lag_cols)) %>% tidyr::drop_na()
  train <- d %>% filter(season <= train_end_season)
  test  <- d %>% filter(season == train_end_season + 1)
  if (nrow(train) == 0 || nrow(test) == 0) return(tibble())
  form <- as.formula(paste(stat, "~", paste(lag_cols, collapse = " + ")))
  fit  <- lm(form, data = train)
  test %>% mutate(pred_lm = predict(fit, newdata = test)) %>%
    select(player_id, season, actual = !!stat, pred_lm)
}
evaluate_rolling <- function(df_joined, stat, k, method = c("equal","exp","lm"),
                             alpha = 0.5, first_train_end = 2018,
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

# leaderboard search
alpha_grid <- c(0.3, 0.4, 0.5, 0.6, 0.7)
k_grid <- 1:5
first_train_end <- 2018
evaluate_equal <- function(stat, k) {
  res <- evaluate_rolling(joined, stat, k, method = "equal", first_train_end = first_train_end)
  if (nrow(res) == 0) return(NULL)
  compute_metrics(res, "pred_equal") %>% mutate(method = "equal", stat = stat, k = k, alpha = NA_real_)
}
evaluate_exp <- function(stat, k, alpha) {
  res <- evaluate_rolling(joined, stat, k, method = "exp", alpha = alpha, first_train_end = first_train_end)
  if (nrow(res) == 0) return(NULL)
  compute_metrics(res, "pred_exp") %>% mutate(method = "exp", stat = stat, k = k, alpha = alpha)
}
evaluate_lm <- function(stat, k) {
  res <- evaluate_rolling(joined, stat, k, method = "lm", first_train_end = first_train_end)
  if (nrow(res) == 0) return(NULL)
  compute_metrics(res, "pred_lm") %>% mutate(method = "lm", stat = stat, k = k, alpha = NA_real_)
}
compare_methods_for_stat <- function(stat) {
  eq <- purrr::map_dfr(k_grid, ~ evaluate_equal(stat, .x))
  ex <- purrr::map_dfr(k_grid, ~ purrr::map_dfr(alpha_grid, function(a) evaluate_exp(stat, .x, a)))
  lm <- purrr::map_dfr(k_grid, ~ evaluate_lm(stat, .x))
  dplyr::bind_rows(eq, ex, lm) %>% arrange(rmse) %>% mutate(rank = dplyr::row_number())
}
all_results <- purrr::map_dfr(stats_vec, compare_methods_for_stat)
winners <- all_results %>%
  group_by(stat) %>% arrange(rmse, .by_group = TRUE) %>%
  mutate(rmse_next_best = lead(rmse)) %>% slice(1) %>% ungroup() %>%
  transmute(stat, best_method = method, best_k = k, best_alpha = alpha, rmse, mae, r2,
            margin_vs_next = rmse_next_best - rmse)

# ------------------ 4) Experience-binned YoY adjustments ------------------
career <- readr::read_csv(path_career, show_col_types = FALSE) %>%
  janitor::clean_names() %>% select(player_id, player_career = player, from) %>%
  mutate(from = as.integer(from))

df_exp <- df_clean %>%
  select(player_id, season, mp, all_of(stats_vec)) %>%
  left_join(career, by = "player_id") %>%
  mutate(exp_year_t = ifelse(!is.na(from), season - from + 1L, NA_integer_))

lag_tbl <- df_exp %>% select(player_id, season, all_of(stats_vec)) %>%
  mutate(season = season + 1L) %>%
  rename_with(~ paste0(.x, "_lag1"), all_of(stats_vec))

yoy <- df_exp %>%
  inner_join(lag_tbl, by = c("player_id","season")) %>%
  mutate(across(all_of(stats_vec),
                ~ .x - get(paste0(cur_column(), "_lag1")),
                .names = "{.col}_delta"))

yoy_long <- yoy %>% select(player_id, season, exp_year_t, ends_with("_delta")) %>%
  pivot_longer(cols = ends_with("_delta"), names_to = "stat", values_to = "delta") %>%
  mutate(stat = str_remove(stat, "_delta$"), exp_year = exp_year_t) %>%
  filter(!is.na(delta), !is.na(exp_year), exp_year >= 2)

exp_breaks <- c(2, 4, 7, 11, 16, Inf); exp_labels <- c("2–3", "4–6", "7–10", "11–15", "16+")
yoy_binned <- yoy_long %>%
  mutate(exp_bin = cut(exp_year, breaks = exp_breaks, labels = exp_labels, right = FALSE)) %>%
  group_by(stat, exp_bin) %>%
  summarise(n_pairs = n(), mean_delta = mean(delta, na.rm = TRUE), .groups = "drop")
delta_lookup <- yoy_binned %>% select(stat, exp_bin, mean_delta)
bin_exp <- function(exp_year) cut(exp_year, breaks = exp_breaks, labels = exp_labels, right = FALSE)

# ------------------ 5) Predict 2026 per-36 with fallback + YoY ------------------
make_features_for_target <- function(df_base, T, k_max = 5, stats = stats_vec) {
  feat <- df_base %>% filter(season == T - 1) %>% distinct(player_id) %>% mutate(season = T)
  for (i in 1:k_max) {
    lag_i <- df_base %>% filter(season == T - i) %>% select(player_id, all_of(stats))
    names(lag_i)[-1] <- paste0(names(lag_i)[-1], "_L", i)
    feat <- feat %>% left_join(lag_i, by = "player_id")
  }
  feat
}
predict_equal_k_on_features <- function(feat, stat, k) {
  lag_cols <- paste0(stat, "_L", 1:k)
  feat %>% filter(if_all(all_of(lag_cols), ~ !is.na(.))) %>%
    mutate(pred = rowMeans(across(all_of(lag_cols)), na.rm = FALSE)) %>%
    transmute(player_id, season, stat = stat, pred, method = "equal", k_used = k, alpha = NA_real_)
}
predict_exp_k_on_features <- function(feat, stat, k, alpha = 0.7) {
  lag_cols <- paste0(stat, "_L", 1:k)
  w <- alpha * (1 - alpha)^(0:(k-1)); w <- w / sum(w)
  feat %>% filter(if_all(all_of(lag_cols), ~ !is.na(.))) %>%
    mutate(pred = as.numeric(as.matrix(select(., all_of(lag_cols))) %*% w)) %>%
    transmute(player_id, season, stat = stat, pred, method = "exp", k_used = k, alpha = alpha)
}
fit_predict_lm_k_for_future <- function(df_hist, featT, stat, k, T) {
  base_hist <- df_hist %>% select(player_id, season, all_of(stats_vec))
  lagged_list <- lapply(1:5, function(i) {
    base_hist %>% rename_with(~ paste0(.x, "_L", i), all_of(stats_vec)) %>% mutate(season = season + i)
  })
  joined_hist <- Reduce(function(x, y) left_join(x, y, by = c("player_id","season")), lagged_list, init = base_hist)
  lag_cols <- paste0(stat, "_L", 1:k)
  train <- joined_hist %>% select(player_id, season, all_of(stat), all_of(lag_cols)) %>%
    filter(season <= T - 1) %>% tidyr::drop_na()
  if (nrow(train) == 0) return(tibble(player_id=character(),season=integer(),stat=character(),pred=double(),method=character(),k_used=integer(),alpha=double()))
  form <- as.formula(paste(stat, "~", paste(lag_cols, collapse = " + ")))
  fit  <- lm(form, data = train)
  featT %>% filter(if_all(all_of(lag_cols), ~ !is.na(.))) %>%
    mutate(pred = predict(fit, newdata = .)) %>%
    transmute(player_id, season, stat = stat, pred, method = "lm", k_used = k, alpha = NA_real_)
}
predict_target_season_all_fallback <- function(T = 2026) {
  featT <- make_features_for_target(df_clean %>% select(player_id, season, all_of(stats_vec)), T, k_max = 5, stats = stats_vec)
  out_list <- vector("list", nrow(winners))
  for (i in seq_len(nrow(winners))) {
    row   <- winners[i, ]
    stat  <- row$stat; meth <- row$best_method; k_top <- as.integer(row$best_k); alpha <- row$best_alpha
    ks <- k_top:1
    preds_k <- purrr::map_dfr(ks, function(kk) {
      if (meth == "equal") {
        predict_equal_k_on_features(featT, stat, kk)
      } else if (meth == "exp") {
        predict_exp_k_on_features(featT, stat, kk, ifelse(is.na(alpha), 0.7, alpha))
      } else {
        fit_predict_lm_k_for_future(df_clean, featT, stat, kk, T)
      }
    })
    preds_best <- preds_k %>% arrange(player_id, dplyr::desc(k_used)) %>% group_by(player_id) %>% slice_head(n = 1) %>% ungroup()
    out_list[[i]] <- preds_best
  }
  dplyr::bind_rows(out_list) %>% group_by(player_id, season, stat) %>%
    summarise(pred = first(pred), method = first(method), k_used = first(k_used), alpha = first(alpha), .groups = "drop")
}
preds_2026_base <- predict_target_season_all_fallback(T_target)

# ensure 9 stats/player with L1..L5 fallback
features_2026 <- make_features_for_target(df_clean %>% select(player_id, season, all_of(stats_vec)), T = T_target, k_max = 5, stats = stats_vec)
eligible_ids <- features_2026 %>% distinct(player_id)
grid_all <- eligible_ids %>% tidyr::crossing(stat = stats_vec) %>% mutate(season = T_target)
preds_joined <- grid_all %>% left_join(preds_2026_base, by = c("player_id","season","stat"))
if (any(is.na(preds_joined$pred))) {
  cand <- preds_joined %>% filter(is.na(pred)) %>% left_join(features_2026, by = c("player_id","season"))
  latest_non_na <- function(df, cols) { out <- rep(NA_real_, nrow(df)); for (i in seq_along(cols)) { v <- df[[cols[i]]]; idx <- is.na(out) & !is.na(v); out[idx] <- v[idx]; if (all(!is.na(out))) break }; out }
  which_lag_used <- function(df, cols) { out <- rep(NA_integer_, nrow(df)); for (i in seq_along(cols)) { v <- df[[cols[i]]]; idx <- is.na(out) & !is.na(v); out[idx] <- i; if (all(!is.na(out))) break }; out }
  fb_list <- lapply(stats_vec, function(s) {
    sub <- cand %>% filter(stat == s); if (nrow(sub) == 0) return(NULL)
    lag_cols <- paste0(s, "_L", 1:5); lag_cols <- lag_cols[lag_cols %in% names(sub)]
    tibble(player_id = sub$player_id, season = sub$season, stat = s,
           pred = latest_non_na(sub, lag_cols), method = "equal", k_used = which_lag_used(sub, lag_cols), alpha = NA_real_) %>%
      filter(!is.na(pred))
  })
  fb_any <- bind_rows(fb_list)
  preds_2026_base <- preds_joined %>% select(player_id, season, stat, pred, method, k_used, alpha) %>%
    bind_rows(fb_any) %>% arrange(player_id, stat, desc(k_used)) %>% group_by(player_id, season, stat) %>% slice_head(n = 1) %>% ungroup()
}

# add YoY bin delta; clip %
career_min <- readr::read_csv(path_career, show_col_types = FALSE) %>% janitor::clean_names() %>% select(player_id, from)
exp_2026 <- eligible_ids %>% mutate(season = T_target) %>% left_join(career_min, by = "player_id") %>%
  mutate(exp_year_2026 = ifelse(!is.na(from), season - from + 1L, NA_integer_), exp_bin = bin_exp(exp_year_2026))
is_pct_stat <- function(s) s %in% c("fg_percent","ft_percent")
preds_2026_final <- preds_2026_base %>%
  left_join(exp_2026 %>% select(player_id, season, exp_year_2026, exp_bin), by = c("player_id","season")) %>%
  left_join(delta_lookup, by = c("stat","exp_bin")) %>%
  mutate(delta_exp = ifelse(is.na(mean_delta), 0, mean_delta),
         pred_adj_raw = pred + delta_exp,
         pred_adj = ifelse(is_pct_stat(stat), pmax(0, pmin(1, pred_adj_raw)), pred_adj_raw)) %>%
  left_join(df_clean %>% filter(season == T_target - 1) %>% select(player_id, player, pos), by = "player_id") %>%
  select(player_id, player, pos, season, stat, pred_base = pred, delta_exp, pred_adj, method, k_used, alpha, exp_year_2026, exp_bin) %>%
  group_by(player_id, season, stat) %>% summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")

preds_2026_wide <- preds_2026_final %>%
  select(player_id, player, pos, season, stat, pred_adj) %>%
  tidyr::pivot_wider(names_from = stat, values_from = pred_adj)
