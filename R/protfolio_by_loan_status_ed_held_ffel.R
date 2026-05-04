# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: Ammar | Reviewed by: N/A

# ── Libraries ──────────────────────────────────────────────────────────────────

library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

# ── File Path ──────────────────────────────────────────────────────────────────

# replace with local path or relative path if cloning via GitHub
# recommended: "data/PortfoliobyLoanStatus.xls"

ed_held_ffel_p_FP <- "C:\\Users\\ammar\\OneDrive\\Desktop\\personal\\spring 2026\\STAT 184\\project_datasets\\PortfoliobyLoanStatus.xls"

# ── Data Import (PCIP) ────────────────────────────────────────────────────────

# Plan: read the ED-Held FFEL sheet and replicate the same loan status trend
#       analysis done for Direct Loans, to check whether the CARES Act pattern
#       holds for this portfolio type as well

# Needs: the readxl and tidyverse packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. read the ED-Held FFEL sheet, applying the same row/column subsetting used
#    for Direct Loans since the sheet structure is identical
# 2. rename columns to match the Direct Loan naming convention for consistency
# 3. strip asterisks and filter to real Q1-Q4 rows
# 4. convert all value columns to numeric
# 5. filter out any rows where key columns are NA

ed_held_ffel_portfolio <- read_excel(
  path      = ed_held_ffel_p_FP,
  sheet     = "ED-Held FFEL",
  col_names = TRUE
)

# Check: raw shape before subsetting
glimpse(ed_held_ffel_portfolio)

cleaned_ed_held_ffel <- ed_held_ffel_portfolio[4:56, -c(4, 6, 8, 10, 12, 14, 16)]

# Attribute Fix: overwrite headers with meaningful snake_case names
# same convention as the Direct Loan file for consistency across scripts
names(cleaned_ed_held_ffel) <- c(
  "fiscal_year",
  "quarter",
  "in_School",
  "grace",
  "repayment",
  "deferment",
  "forbearance",
  "cumulative_in_default",
  "other"
)

cleaned_ed_held_ffel <- cleaned_ed_held_ffel[-c(1, 2), ] |>
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    fiscal_year           = as.numeric(fiscal_year),
    across(
      c(in_School, repayment, deferment, forbearance, cumulative_in_default),
      as.numeric
    ),
    time       = paste(fiscal_year, quarter_clean, sep = " "),
    time_index = row_number()
  ) |>
  filter(
    !is.na(fiscal_year),
    !is.na(in_School),
    !is.na(repayment),
    !is.na(deferment),
    !is.na(forbearance),
    !is.na(cumulative_in_default)
  )

# Check: confirm expected rows and that numeric columns are not character
glimpse(cleaned_ed_held_ffel)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# ── Figure: ED-Held FFEL Loan Status Trends Over Time ────────────────────────

# Plan: show whether the ED-Held FFEL portfolio mirrors the direct loan
#       CARES Act pattern or behaves differently — this comparison is the
#       main insight the file contributes to the cross-loan section

# Improve: replaced superseded recode() with case_when()
# Improve: switched to time_index x-axis to fix crowded label overlap

cares_idx <- which(cleaned_ed_held_ffel$time == "2020 Q3")

cleaned_ed_held_ffel |>
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
    x     = cares_idx + 0.5,
    y     = max(cleaned_ed_held_ffel$forbearance, na.rm = TRUE) * 0.85,
    label = "CARES Act\n(Mar 2020)",
    size  = 2.4,
    color = "#555555",
    hjust = 0
  ) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(
    breaks = seq(1, nrow(cleaned_ed_held_ffel), by = 4),
    labels = cleaned_ed_held_ffel$time[seq(1, nrow(cleaned_ed_held_ffel), by = 4)]
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
    title   = "ED-Held FFEL Loan Portfolio by Status Over Time",
    x       = "Fiscal Quarter",
    y       = "Dollars Outstanding ($ billions)",
    color   = "Loan Status",
    caption = "Source: FSA Data Center / NSLDS — ED-Held FFEL Portfolio by Loan Status."
  ) +
  stat184_theme +
  theme(legend.position = "right", legend.text = element_text(size = 7))

# ── Figure: ED-Held FFEL Deferment vs Forbearance Rate of Change ─────────────

# Plan: mirror the same deferment/forbearance rate of change analysis from the
#       Direct Loan file to check whether the same CARES Act consolidation
#       pattern appears in the ED-Held FFEL portfolio

cleaned_ed_held_ffel |>
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
    title   = "Average Yearly Rate of Change for Deferment and Forbearance (ED-Held FFEL)",
    x       = "Fiscal Year",
    y       = "Average Year-over-Year Change ($ billions)",
    fill    = NULL,
    caption = "Source: FSA Data Center / NSLDS — ED-Held FFEL Portfolio by Loan Status."
  ) +
  stat184_theme +
  theme(legend.position = "bottom")
