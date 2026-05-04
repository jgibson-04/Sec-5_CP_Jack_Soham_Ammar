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

# Plan: show the long-term runoff trajectory of the FFEL portfolio against the
#       growth of the Direct Loan portfolio, to explain why the two portfolio
#       types behave so differently in the cumulative default cross-loan chart
#       FFEL loans were discontinued in 2010 so the FFEL book is a closed,
#       aging pool that will eventually reach near-zero

# Needs: the readxl and tidyverse packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. write a helper function to read total outstanding for any sheet
#    total outstanding = sum of all seven dollar status columns
# 2. read both FFEL and Direct Loan using the helper
# 3. bind into one frame for comparison plotting

# helper: reads all seven dollar columns from any sheet and sums them
# into a single total_outstanding column
read_total_portfolio <- function(sheet_name, label) {

  read_excel(
    path       = portfolio_fp,
    sheet      = sheet_name,
    skip       = 6,
    col_names  = FALSE,
    .name_repair = "unique"
  ) |>
    select(
      federal_fiscal_year = 1,
      quarter             = 2,
      in_school           = 3,
      grace               = 5,
      repayment           = 7,
      deferment           = 9,
      forbearance         = 11,
      cumulative_default  = 13,
      other               = 15
    ) |>
    fill(federal_fiscal_year) |>
    mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
    filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
    mutate(
      across(
        c(in_school, grace, repayment, deferment, forbearance, cumulative_default, other),
        as.numeric
      ),
      federal_fiscal_year = as.numeric(federal_fiscal_year),
      total_outstanding   = rowSums(
        across(c(in_school, grace, repayment, deferment, forbearance, cumulative_default, other)),
        na.rm = TRUE
      ),
      loan_type  = label,
      time       = paste(federal_fiscal_year, quarter_clean, sep = " ")
    ) |>
    filter(!is.na(total_outstanding), total_outstanding > 0) |>
    select(federal_fiscal_year, quarter_clean, time, loan_type, total_outstanding)
}

# read total portfolio for FFEL and Direct Loan
ffel_total  <- read_total_portfolio(sheet_name = "FFEL",        label = "FFEL (All)")
direct_loan <- read_total_portfolio(sheet_name = "Direct Loan", label = "Direct Loan")

# combine into one frame for comparison
combined_portfolios <- bind_rows(ffel_total, direct_loan) |>
  group_by(loan_type) |>
  mutate(time_index = row_number()) |>
  ungroup()

# Check: FFEL total_outstanding should be declining throughout the period
# Direct Loan total_outstanding should be growing, with a CARES dip around 2020
glimpse(combined_portfolios)

# annual summary for FFEL to compute year-over-year change rates
ffel_annual <- ffel_total |>
  group_by(federal_fiscal_year) |>
  summarize(
    avg_total = mean(total_outstanding, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    yoy_change_pct = round((avg_total / lag(avg_total) - 1) * 100, 1)
  )

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# ── Figure: FFEL Portfolio Runoff vs Direct Loan Growth ──────────────────────

# Plan: overlay FFEL and Direct Loan total portfolio on the same chart
#       the contrasting trajectories explain the different cumulative default
#       behavior seen in the cross-loan comparison chart

combined_portfolios |>
  ggplot(aes(
    x     = time_index,
    y     = total_outstanding,
    color = loan_type,
    group = loan_type
  )) +
  geom_line(linewidth = 1.5) +
  scale_x_continuous(
    breaks = seq(1, max(combined_portfolios$time_index), by = 4),
    labels = direct_loan$time[seq(1, nrow(direct_loan), by = 4)]
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 1)) +
  scale_color_manual(values = c(
    "FFEL (All)"  = "#8E44AD",
    "Direct Loan" = "#2980B9"
  )) +
  labs(
    title   = "FFEL Portfolio Runoff vs. Direct Loan Growth",
    subtitle = "FFEL was discontinued in 2010 and has been declining ever since",
    x       = "Fiscal Quarter",
    y       = "Total Portfolio Outstanding ($ billions)",
    color   = "Loan Type",
    caption = "Source: FSA Data Center / NSLDS — Portfolio by Loan Status workbook."
  ) +
  stat184_theme +
  theme(legend.position = "right")

# ── Figure: FFEL Annual Average with Year-over-Year Change ───────────────────

# Plan: bar chart of annual average FFEL outstanding with year-over-year
#       percentage labels to show the pace of portfolio runoff

ffel_annual |>
  ggplot(aes(x = factor(federal_fiscal_year), y = avg_total)) +
  geom_col(fill = "#8E44AD", alpha = 0.75, width = 0.7) +
  geom_text(
    aes(
      label = ifelse(!is.na(yoy_change_pct), paste0(yoy_change_pct, "%"), ""),
      y     = avg_total + 2
    ),
    size  = 2.8,
    color = "#555555"
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 1)) +
  labs(
    title   = "Annual Average FFEL Portfolio Outstanding with Year-over-Year Change",
    x       = "Fiscal Year",
    y       = "Avg. FFEL Portfolio Outstanding ($ billions)",
    caption = "Source: FSA Data Center / NSLDS — FFEL sheet of Portfolio by Loan Status workbook.\nLabels show year-over-year change in annual average outstanding balance."
  ) +
  stat184_theme +
  theme(panel.grid.major.x = element_blank())
