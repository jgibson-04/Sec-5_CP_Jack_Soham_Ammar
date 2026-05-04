# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: Ammar | Reviewed by: N/A

# ── Libraries ──────────────────────────────────────────────────────────────────

# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")

library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

# ── File Path ──────────────────────────────────────────────────────────────────

# FP = File Path, p = portfolio
# replace path with local path on your device or a relative path if cloning via GitHub
# recommended: place data files in a data/ folder at the repo root and use
# direct_loan_p_FP <- "data/PortfoliobyLoanStatus.xls"

direct_loan_p_FP <- "C:\\Users\\ammar\\OneDrive\\Desktop\\personal\\spring 2026\\STAT 184\\project_datasets\\PortfoliobyLoanStatus.xls"

# ── Data Import ────────────────────────────────────────────────────────────────

# reading Portfolio by Loan Status - Direct Loans sheet
# intent: find the trend of defaults in direct federal loans across fiscal quarters
# coverage: FY2013 Q3 through FY2026 Q1

direct_loan_portfolio <- read_excel(
  path      = direct_loan_p_FP,
  sheet     = "Direct Loan",
  col_names = TRUE
)

# Check: verify raw dimensions immediately after import
# the raw sheet has metadata rows at the top so this will not be a clean frame yet
glimpse(direct_loan_portfolio)

# ── Data Cleaning (PCIP) ───────────────────────────────────────────────────────

# Plan: create a clean dataset with only the relevant loan status columns,
#       proper header names, and no metadata rows or footnote rows

# Needs: the tidyverse package and the raw data file

# Steps:
# 1. subset rows 4 to 56 to skip the metadata header block at the top of the sheet
# 2. drop every other column (recipient count columns) keeping only dollar columns
# 3. overwrite column names with meaningful snake_case names
# 4. remove the first two rows which are the original header rows now shifted down
# 5. strip CARES-era asterisks from quarter labels and filter to real Q1-Q4 rows
# 6. convert numeric columns from character to numeric
# 7. build a time label and a sequential index for plotting

cleaned_dlp <- direct_loan_portfolio[4:56, -c(4, 6, 8, 10, 12, 14, 16)]

# 1. The Attribute Fix: overwrite the current headers with meaningful names
# R assigns the vector from c() to columns in order
names(cleaned_dlp) <- c(
  "fiscal_year", "quarter", "in_School", "grace",
  "repayment", "deferment", "forbearance",
  "cumulative_in_default", "other"
)

cleaned_dlp <- cleaned_dlp[-c(1, 2), ] |>
  # strip asterisks that the FSA appends to CARES-era quarter labels e.g. "Q3*"
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    fiscal_year = as.numeric(fiscal_year),
    across(
      c(in_School, repayment, deferment, forbearance, cumulative_in_default),
      as.numeric
    ),
    # time label used as x-axis in plots
    time       = paste(fiscal_year, quarter_clean, sep = " "),
    # sequential integer index avoids the crowded x-axis problem with raw time strings
    time_index = row_number()
  ) |>
  filter(!is.na(in_School))

# Check: confirm expected shape and column types after cleaning
glimpse(cleaned_dlp)

# ── Shared Theme ───────────────────────────────────────────────────────────────

# consistent visual style applied to all plots in this file
# matches the shared theme used in the QMD

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# ── Figure 1: Loan Status Trends Over Time ────────────────────────────────────

# Plan: show the full time series of all five loan statuses to capture both the
#       steady pre-pandemic growth and the dramatic CARES Act structural break

# Improve: replaced recode() with case_when() as recode() is superseded in dplyr
# Improve: switched from raw time string x-axis to time_index with labeled breaks
#          to prevent the crowded overlapping labels seen in the original version

cares_idx <- which(cleaned_dlp$time == "2020 Q3")

cleaned_dlp |>
  pivot_longer(
    cols      = c(in_School, repayment, deferment, forbearance, cumulative_in_default),
    names_to  = "category",
    values_to = "value"
  ) |>
  mutate(
    category = case_when(
      category == "in_School"             ~ "In School",
      category == "repayment"             ~ "Repayment",
      category == "deferment"             ~ "Deferment",
      category == "forbearance"           ~ "Forbearance",
      category == "cumulative_in_default" ~ "Cumulative in Default",
      .default = category
    )
  ) |>
  ggplot(aes(x = time_index, y = value, group = category, color = category)) +
  geom_vline(
    xintercept = cares_idx,
    linetype   = "dashed",
    color      = "#666666",
    linewidth  = 0.7
  ) +
  annotate(
    "text",
    x     = cares_idx + 0.6,
    y     = max(cleaned_dlp$forbearance, na.rm = TRUE) * 0.88,
    label = "CARES Act\n(Mar 2020)",
    size  = 2.4,
    color = "#555555",
    hjust = 0
  ) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(
    breaks = seq(1, nrow(cleaned_dlp), by = 4),
    labels = cleaned_dlp$time[seq(1, nrow(cleaned_dlp), by = 4)]
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 1)) +
  scale_color_manual(values = c(
    "Cumulative in Default" = "#C0392B",
    "Deferment"             = "#E67E22",
    "Forbearance"           = "#27AE60",
    "In School"             = "#2980B9",
    "Repayment"             = "#8E44AD"
  )) +
  labs(
    title   = "Direct Loan Portfolio by Status, FY2013 Q3 to FY2026 Q1",
    x       = "Fiscal Quarter",
    y       = "Dollars Outstanding ($ billions)",
    color   = "Loan Status",
    caption = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status."
  ) +
  stat184_theme +
  theme(legend.position = "right", legend.text = element_text(size = 7))

# ── Figure 3: Deferment vs. Forbearance Rate of Change ───────────────────────

# Plan: show the year-over-year average rate of change for deferment and
#       forbearance to clarify why deferment goes to near-zero in 2020
#       even as forbearance spikes — both populations were moved into
#       administrative forbearance under the CARES Act

# Improve: replaced superseded recode() with case_when()
# Improve: replaced geom_bar(stat="identity") with geom_col() which is the
#          tidyverse idiomatic equivalent for pre-summarized data

cleaned_dlp |>
  arrange(fiscal_year, quarter_clean) |>
  mutate(
    deferment_change   = deferment   - lag(deferment),
    forbearance_change = forbearance - lag(forbearance)
  ) |>
  group_by(fiscal_year) |>
  summarize(
    avg_deferment_change   = mean(deferment_change,   na.rm = TRUE),
    avg_forbearance_change = mean(forbearance_change, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols      = c(avg_deferment_change, avg_forbearance_change),
    names_to  = "category",
    values_to = "avg_change"
  ) |>
  mutate(
    category = case_when(
      category == "avg_deferment_change"   ~ "Avg. Deferment Change",
      category == "avg_forbearance_change" ~ "Avg. Forbearance Change",
      .default = category
    )
  ) |>
  ggplot(aes(x = factor(fiscal_year), y = avg_change, fill = category)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.88) +
  geom_hline(yintercept = 0, color = "#666666", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Avg. Deferment Change"   = "#E67E22",
    "Avg. Forbearance Change" = "#27AE60"
  )) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 1)) +
  labs(
    title   = "Average Yearly Rate of Change for Deferment and Forbearance (Direct Loans)",
    x       = "Fiscal Year",
    y       = "Average Year-over-Year Change ($ billions)",
    fill    = NULL,
    caption = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status."
  ) +
  stat184_theme +
  theme(legend.position = "bottom")
