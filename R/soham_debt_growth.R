# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: Soham | Reviewed by: Ammar

# ── Libraries ──────────────────────────────────────────────────────────────────

library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

# ── File Path ──────────────────────────────────────────────────────────────────

# replace with local path or relative path if cloning via GitHub
# recommended: "data/PortfoliobyLoanStatus.xls"

portfolio_fp <- "data/PortfoliobyLoanStatus.xls"

# ── Data Import (PCIP) ────────────────────────────────────────────────────────

# Plan: read all seven dollar columns from the Direct Loan sheet and build
#       a tidy frame for total portfolio growth analysis
#       Goal is to show how the overall portfolio has grown and how the
#       composition across loan statuses has shifted from FY2013 to FY2026

# Needs: the readxl and tidyverse packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. skip the first 6 rows which are metadata, not data
# 2. select only the dollar columns (odd-numbered positions after col 2)
#    and drop the recipient count columns
# 3. fill fiscal year downward since the workbook uses merged cells
# 4. strip CARES-era asterisks from quarter labels and filter to real Q1-Q4 rows
# 5. convert all dollar columns to numeric
# 6. compute total_portfolio as the row sum of all seven status columns
# 7. build time label and sequential index for plotting

# Code: rowSums with across() is more robust than manual addition because it
#       handles NAs gracefully with na.rm = TRUE

# Improve: added grace and other to the across() conversion call — an earlier
#          version only converted the five main status columns, which left
#          grace and other as character and caused rowSums to return NA

# Polish: confirmed column positions by checking the raw sheet manually:
#         col 1 = fiscal year, col 2 = quarter, odd cols 3-15 = dollar values
#         even cols 4-16 = recipient counts (dropped)

soham_data <- read_excel(
  path       = portfolio_fp,
  sheet      = "Direct Loan",
  skip       = 6,
  col_names  = FALSE,
  .name_repair = "unique"
) |>
  select(
    fiscal_year           = 1,
    quarter               = 2,
    in_school             = 3,
    grace                 = 5,
    repayment             = 7,
    deferment             = 9,
    forbearance           = 11,
    cumulative_in_default = 13,
    other                 = 15
  ) |>
  fill(fiscal_year) |>
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    across(
      c(in_school, grace, repayment, deferment,
        forbearance, cumulative_in_default, other),
      as.numeric
    ),
    fiscal_year     = as.numeric(fiscal_year),
    total_portfolio = rowSums(
      across(c(in_school, grace, repayment, deferment,
               forbearance, cumulative_in_default, other)),
      na.rm = TRUE
    ),
    time       = paste(fiscal_year, quarter_clean, sep = " "),
    time_index = row_number()
  ) |>
  filter(!is.na(in_school))

# Check: total_portfolio should range from ~570B (FY2013 Q3) to ~1600B (FY2026 Q1)
# all seven status columns should be numeric
# expected: 51 rows, 12 columns
glimpse(soham_data)

# ── Annual Summary (PCIP) ──────────────────────────────────────────────────────

# Plan: compute average total portfolio per fiscal year and the year-over-year
#       growth rate for the bar chart
# Code: mean() across all quarters per year so partial years (FY2026 has only
#       Q1) are still represented without distorting the series
# Improve: using mean() per year rather than Q4 snapshot avoids the CARES-era
#          distortion where Q4 values swung wildly between forbearance peaks

soham_yearly <- soham_data |>
  group_by(fiscal_year) |>
  summarize(
    avg_total = mean(total_portfolio, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    growth_pct = (avg_total - lag(avg_total)) / lag(avg_total) * 100
  ) |>
  filter(!is.na(growth_pct))

# Check: growth_pct should be positive for most years
# FY2021 will show a negative or near-zero value due to the CARES reclassification
glimpse(soham_yearly)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# CARES Act index for annotation line — FY2020 Q3 is where repayment collapses
cares_idx <- which(soham_data$time == "2020 Q3")

# ── Figure soham-1: Stacked Area — Portfolio Composition Over Time ─────────────

# Plan: stacked area chart showing all six status categories so the reader
#       can see both total portfolio growth and how the composition shifted
#       The CARES Act annotation makes the structural break visible

# Improve: factor() sets a meaningful stacking order — without it ggplot
#          stacks alphabetically which buries repayment under smaller categories

soham_data |>
  pivot_longer(
    cols      = c(in_school, grace, repayment, deferment,
                  forbearance, cumulative_in_default),
    names_to  = "status",
    values_to = "balance"
  ) |>
  mutate(
    status = case_when(
      status == "in_school"             ~ "In School",
      status == "grace"                 ~ "Grace",
      status == "repayment"             ~ "Repayment",
      status == "deferment"             ~ "Deferment",
      status == "forbearance"           ~ "Forbearance",
      status == "cumulative_in_default" ~ "Cumulative in Default",
      .default = status
    ),
    status = factor(status, levels = c(
      "Cumulative in Default", "Forbearance", "Deferment",
      "Grace", "In School", "Repayment"
    ))
  ) |>
  ggplot(aes(x = time_index, y = balance, fill = status)) +
  geom_area(alpha = 0.85) +
  geom_vline(
    xintercept = cares_idx,
    linetype   = "dashed",
    color      = "#666666",
    linewidth  = 0.7
  ) +
  annotate(
    "text",
    x     = cares_idx + 0.6,
    y     = max(soham_data$total_portfolio, na.rm = TRUE) * 0.88,
    label = "CARES Act\n(Mar 2020)",
    size  = 2.4,
    color = "#555555",
    hjust = 0
  ) +
  scale_x_continuous(
    breaks = seq(1, nrow(soham_data), by = 4),
    labels = soham_data$time[seq(1, nrow(soham_data), by = 4)]
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 1)) +
  scale_fill_manual(values = c(
    "Repayment"             = "#8E44AD",
    "In School"             = "#2980B9",
    "Grace"                 = "#AED6F1",
    "Deferment"             = "#E67E22",
    "Forbearance"           = "#27AE60",
    "Cumulative in Default" = "#C0392B"
  )) +
  labs(
    title   = "Total Direct Loan Portfolio by Status, FY2013 Q3 to FY2026 Q1",
    x       = "Fiscal Quarter",
    y       = "Outstanding Balance ($ billions)",
    fill    = "Loan Status",
    caption = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status."
  ) +
  stat184_theme +
  theme(legend.position = "right", legend.text = element_text(size = 7))

# ── Figure soham-2: Bar Chart — Year-over-Year Portfolio Growth Rate ───────────

# Plan: bar chart of annual year-over-year growth so the reader can see
#       which years had the fastest expansion and where the CARES dip falls
#       Bar color distinguishes positive from negative growth years

# Improve: if_else() used instead of ifelse() per tidyverse style guide
# Improve: geom_text labels placed above or below bar depending on sign
#          so labels never overlap the zero line

soham_yearly |>
  mutate(bar_color = if_else(growth_pct >= 0, "Positive", "Negative")) |>
  ggplot(aes(x = factor(fiscal_year), y = growth_pct, fill = bar_color)) +
  geom_col(width = 0.7, alpha = 0.88) +
  geom_hline(yintercept = 0, color = "#444444", linewidth = 0.5) +
  geom_text(
    aes(
      label = paste0(round(growth_pct, 1), "%"),
      vjust = if_else(growth_pct >= 0, -0.4, 1.2)
    ),
    size = 2.6
  ) +
  scale_fill_manual(values = c("Positive" = "#2980B9", "Negative" = "#C0392B")) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(
    title   = "Year-over-Year Growth in Average Total Direct Loan Portfolio",
    x       = "Fiscal Year",
    y       = "Year-over-Year Growth (%)",
    caption = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status.\nGrowth computed from annual mean of quarterly totals."
  ) +
  stat184_theme +
  theme(legend.position = "none")
