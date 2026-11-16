# 1pt

R programs for "1 point = 1 point?", a paper on the rubber band effect in NBA games. The programs reproduce the tables and figures in the paper and its Supporting Information (SI).

## Data

The game data come from BigDataBall (bigdataball.com) and are not included here, apart from a sample. The programs expect one folder per season, named like `2020-2021`, inside a data folder. Each season folder holds:

- `combined-stats.csv`: play-by-play data.
- `Team-Stats.xlsx`: game summary data, including the closing spread (`CLOSING SPREAD`), possessions (`POSS`) and rest days (`REST`).

`combined stats 4 games.csv` is a sample of the play-by-play data for four games. It shows the file format, but it is too small to reproduce the results.

## Setting paths

Each program sets its paths near the top, in a variable named `base_path`, `datadir` or `outdir`. Change these to the data folder and an output folder on your computer before running a program. The programs need the R packages `data.table`, `readxl`, `readr`, `ggplot2` and `tseries`.

## Programs and results

| Program | Results |
|---|---|
| `PbPdata import score ratio test corrected.R` | Tables 1 and 2, and Tables S1 to S5 (breaks across games, controls, playoffs, referee fixed effects) |
| `GameSimAMLM.R` | Simulated AMLM statistics under the null and alternatives (Figure 2) |
| `AMLM power trend.R` | Power of the AMLM test with and without an estimated trend (Table 3 and Section 6) |
| `Margin plots corrected.R` | Figure 1, Table 4 and Figure 3 (example games, with *p*-values) |
| `AMLM tables corrected.R` | Scoring probabilities and critical values (Tables S6 and S7), and AMLM rejection rates with no trend, the closing spread trend and the estimated trend (Tables S8 and S9, Section 6) |
| `Game Sim Plot.R` | Simulated margins (Figure S1) |
| `GameSimDF.R` | Augmented Dickey-Fuller statistics for simulated margins (Figure S2) |
| `GameSimTrapani.R`, `TR test NBA data.R`, `Histograms TR.R` | Tests under the null of stationarity (S7 and Figure S3) |
| `shooting % v margin ranges FE corrected.R` | Builds the possession level data |
| `shooting % v margin tables.R`, `shooting % v margin all seasons.R` | Possession level estimates for scoring and turnovers (Table 5, Tables S10 and S11) |
| `shooting % v margin FE checks.R` | Lineup fixed effects and placebo tests (Table S12) |
| `shooting % v margin threshold checks.R` | Sensitivity to the threshold for high use lineups (Table S13) |

## Earlier versions

The remaining programs are earlier versions kept for reference, including `combined programs.R`, `AMLM test NBA data.R`, the `AMLMdetvar` and `TR test detvar` programs, and the `PbPdata import` programs without `corrected` in the name. The programs marked `corrected` replace them and produce the results in the current paper.
