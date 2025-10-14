NBA Per-36 Projection & Lookback Study

Goal: Determine the most predictive lookback window for NBA player stats and build a simple, reproducible pipeline to forecast next-season per-36 performance.

TL;DR: Using walk-forward evaluation on seasons ≥ 2015 with ≥ 1000 minutes, the linear model with k≈4–5 lookback years wins for most stats; exponential weighting (α≈0.7, k=3) performs best for PTS/36. We also adjust predictions with experience-binned year-over-year deltas.

🔍 What’s in this project?

Lookback windows (1–5 years) compared across three methods:

Equal average (simple mean of past k)

Exponential average (recent seasons weighted more; tune α)

Linear regression (past k seasons as features)

Walk-forward evaluation to avoid horizon leakage

2015+ filter & minutes ≥ 1000 for stability

Team-split handling (2TM/TOT) with minutes-weighted merges

Experience-binned YoY deltas to nudge predictions by career stage

R Markdown report for reproducibility



🗂️ Repository structure
.
├─ README.md
├─ .gitignore
├─ nba_per36_pipeline.Rmd        # R Markdown (Steps 1–4)
├─ R/                            # (optional) helper functions
├─ data/                         # CSVs (or samples) go here
│  ├─ Per36Minutes.csv
│  └─ PlayerCareerInfo.csv
├─ output/                       # knitted HTML / small CSV outputs
└─ renv.lock                     # reproducible package snapshot

📊 Data

Per36Minutes.csv — season-level per-36 and rate stats (historical)

PlayerCareerInfo.csv — career metadata (from, to, etc.)

Columns used (subset):

Identifiers: season, player, player_id, team|tm, pos, mp, g

Per-36: pts_per_36_min, trb_per_36_min, ast_per_36_min, stl_per_36_min,
blk_per_36_min, x3p_per_36_min, tov_per_36_min, fga_per_36_min, fta_per_36_min

Percentages: fg_percent, ft_percent

Filters:

Seasons ≥ 2015

Season minutes ≥ 1000

If multiple team rows in a season, merge minutes-weighted, prefer combined row when valid.

🧪 Methods (high-level)

Collapse per-team rows to per-player-season:

Minutes-weighted per-36

Percentages recomputed from totals (fg/ft makes & attempts from per-36 × minutes/36)

Feature engineering: build L1..L5 lags per stat.

Compare lookbacks (k=1..5) with:

Equal mean, exponential mean (grid α∈{0.3..0.7}), linear regression

Walk-forward evaluation (train through year t, test on t+1)

Predict 2026 per-36, then adjust with experience-binned YoY deltas (bins: 2–3, 4–6, 7–10, 11–15, 16+). Clip FG%/FT% to [0,1].

Preliminary findings

Linear (k≈4) wins for 8/9 stats (by RMSE);

Exponential (α≈0.7, k=3) wins for PTS/36.

Adding YoY deltas by experience provides small, directional improvements.

▶️ How to run
0) Clone & open
git clone https://github.com/<you>/nba-lookback-study.git
cd nba-lookback-study


Open the project in RStudio (or your IDE).

1) Reproducible environment (recommended)
install.packages("renv")
renv::init()        # first time; or renv::restore() if lockfile exists

2) Place data

Put CSVs in data/:

data/Per36Minutes.csv

data/PlayerCareerInfo.csv

(If you can’t commit the full data, include a small sample CSV and explain how to obtain the full files.)

3) Knit the report
rmarkdown::render("nba_per36_pipeline.Rmd")


The HTML will land in output/ (or alongside the Rmd, depending on your YAML).

📄 Results artifact (shareable)

HTML report: output/nba_per36_pipeline.html
Publish via GitHub Pages (Settings → Pages → /docs).
Optional: copy the HTML to docs/index.html and turn on Pages for a live preview.

🧱 Design decisions & guardrails

Walk-forward evaluation to reflect real forecasting.

Graceful fallbacks: if insufficient history for k, back off to shorter k; if still missing, use last non-missing season.

Minutes merge for team splits (2TM/TOT).

Clipping: FG%/FT% to [0,1].

Experience-binned adjustments rather than raw career curves (focus on YoY deltas).

🚀 Roadmap (future work)

Project minutes & games using similar exp-weighted approach (robust to combined rows).

Attempt-weighted FG/FT contributions for z-scores.

Top-200 MPG cohort filter for category z-scores.

3rd-party projections (rookies/role changes) with 50/50 blend.

Auction values with a curved (nonlinear) budget allocation.

📎 Reproduce/extend

Switch parameters in the Rmd header:

params:
  per36_csv: "data/Per36Minutes.csv"
  career_csv: "data/PlayerCareerInfo.csv"
  target_season: 2026


Edit bins (Rmd Step 4) if you want a different experience segmentation.

📢 Credits & disclaimers

Data files provided by the project owner; not redistributed here if proprietary or large.

This repo shows methods & reproducibility, not an official NBA product.

🧑‍💻 Author & links

Author: Alexander Kenney

GitHub: https://github.com/akenney87

LinkedIn: https://www.linkedin.com/in/alexanderkenney


📝 License

MIT — see LICENSE.

Badges
![R](https://img.shields.io/badge/R-4.x-blue)
[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)