# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: Ammar | Reviewed by: N/A

# ── Libraries ──────────────────────────────────────────────────────────────────

library(readxl)
library(tidyverse)
library(scales)

# ── File Path ──────────────────────────────────────────────────────────────────

# replace with local path or relative path if cloning via GitHub
# recommended: "data/PortfoliobyLoanStatus.xls"

portfolio_fp <- "C:\\Users\\ammar\\OneDrive\\Desktop\\personal\\spring 2026\\STAT 184\\project_datasets\\PortfoliobyLoanStatus.xls"

# ── Data Import (PCIP) ────────────────────────────────────────────────────────

# Plan: compute the quarter-over-quarter change in cumulative default to isolate
#       the FY2026 Q1 surge as a single clearly visible data point
#       The QoQ change is more informative than the raw cumulative level for
#       showing the magnitude of the post-pause default wave

# Needs: the readxl and tidyverse packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. read the Direct Loan sheet, selecting only cumulative_default
# 2. apply the standard fill, strip, filter, convert pipeline
# 3. compute qoq_default_change as the first difference of cumulative_default
# 4. flag quarters by policy era for color-coded bar chart

raw_surge <- read_excel(
  path       = portfolio_fp,
  sheet      = "Direct Loan",
  skip       = 6,
  col_names  = FALSE,
  .name_repair = "unique"
) |>
  select(
    federal_fiscal_year = 1,
    quarter             = 2,
    cumulative_default  = 13
  ) |>
  fill(federal_fiscal_year) |>
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    federal_fiscal_year = as.numeric(federal_fiscal_year),
    cumulative_default  = as.numeric(cumulative_default),
    time                = paste(federal_fiscal_year, quarter_clean, sep = " "),
    time_index          = row_number(),
    # quarter-over-quarter change: negative during CARES, large positive in FY2026 Q1
    qoq_default_change  = cumulative_default - lag(cumulative_default),
    # flag post-pause era: payment pause expired Sept 2023 (FY2023 Q4)
    post_pause          = federal_fiscal_year >= 2024 |
      (federal_fiscal_year == 2023 & quarter_clean == "Q4")
  ) |>
  filter(!is.na(cumulative_default))

# Check: FY2026 Q1 qoq_default_change should be approximately 58.6 billion
# CARES-era rows should have negative qoq_default_change values
glimpse(raw_surge)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# ── Figure: Quarter-over-Quarter Change in Cumulative Default ─────────────────

# Plan: bar chart of QoQ change colored by policy era
#       the FY2026 Q1 bar should visually dwarf every other bar in the chart
#       making the post-pause default wave immediately legible

raw_surge |>
  filter(!is.na(qoq_default_change)) |>
  mutate(
    era = case_when(
      post_pause                  ~ "Post-Pause Era",
      federal_fiscal_year >= 2020 ~ "CARES Era",
      .default = "Pre-Pandemic"
    )
  ) |>
  ggplot(aes(x = time_index, y = qoq_default_change, fill = era)) +
  geom_col(width = 0.85, alpha = 0.88) +
  geom_hline(yintercept = 0, color = "#444444", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Pre-Pandemic"   = "#2980B9",
    "CARES Era"      = "#27AE60",
    "Post-Pause Era" = "#C0392B"
  )) +
  scale_x_continuous(
    breaks = seq(1, nrow(filter(raw_surge, !is.na(qoq_default_change))), by = 4),
    labels = (raw_surge |> filter(!is.na(qoq_default_change)))$time[
      seq(1, nrow(filter(raw_surge, !is.na(qoq_default_change))), by = 4)
    ]
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 1)) +
  labs(
    title   = "Quarter-over-Quarter Change in Cumulative Default, FY2013 to FY2026",
    subtitle = "FY2026 Q1 shows the largest single-quarter increase in the dataset",
    x       = "Fiscal Quarter",
    y       = "QoQ Change in Cumulative Default ($ billions)",
    fill    = "Policy Era",
    caption = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status."
  ) +
  stat184_theme +
  theme(legend.position = "bottom")
