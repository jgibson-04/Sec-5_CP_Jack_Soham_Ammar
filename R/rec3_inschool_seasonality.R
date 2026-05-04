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

# Plan: isolate the In-School column to examine its seasonal pattern
#       The federal fiscal year Q1 (Oct-Dec) and Q2 (Jan-Mar) align with fall
#       and spring semesters, while Q3 and Q4 are summer-heavy
#       This pattern is important context for interpreting any single-quarter
#       snapshot of the portfolio

# Needs: the readxl and tidyverse packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. read the Direct Loan sheet, selecting only the in_school dollar column
# 2. apply standard fill, strip, filter, convert pipeline
# 3. filter to pre-pandemic years to isolate the structural seasonal signal
#    without the CARES Act disruption contaminating the pattern
# 4. compute average in_school balance by quarter across the pre-pandemic years

raw_seasonal <- read_excel(
  path       = portfolio_fp,
  sheet      = "Direct Loan",
  skip       = 6,
  col_names  = FALSE,
  .name_repair = "unique"
) |>
  select(
    federal_fiscal_year = 1,
    quarter             = 2,
    in_school           = 3
  ) |>
  fill(federal_fiscal_year) |>
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    federal_fiscal_year = as.numeric(federal_fiscal_year),
    in_school           = as.numeric(in_school),
    time                = paste(federal_fiscal_year, quarter_clean, sep = " "),
    time_index          = row_number()
  ) |>
  filter(!is.na(in_school))

# Check: in_school should be numeric and in the range of 90 to 165 billion
glimpse(raw_seasonal)

# compute the average in-school balance by quarter across pre-pandemic years only
# pre-pandemic = before FY2020 to avoid the CARES disruption to enrollment patterns
seasonal_avg <- raw_seasonal |>
  filter(federal_fiscal_year < 2020) |>
  group_by(quarter_clean) |>
  summarize(
    avg_in_school = mean(in_school, na.rm = TRUE),
    .groups       = "drop"
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

# ── Figure: Average In-School Balance by Fiscal Quarter ──────────────────────

# Plan: bar chart of average in-school balance by Q1-Q4, averaged over
#       pre-pandemic years, to communicate the seasonal magnitude clearly

seasonal_avg |>
  ggplot(aes(x = quarter_clean, y = avg_in_school, fill = quarter_clean)) +
  geom_col(width = 0.6, alpha = 0.88) +
  geom_text(
    aes(label = dollar(avg_in_school, suffix = "B", accuracy = 1)),
    vjust    = -0.4,
    size     = 3,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    Q1 = "#1B3A5C",
    Q2 = "#2E75B6",
    Q3 = "#7FB3D9",
    Q4 = "#C6DCF0"
  )) +
  scale_y_continuous(
    labels = label_dollar(suffix = "B", accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Average In-School Balance by Fiscal Quarter (FY2013 to FY2019)",
    subtitle = "Q1 and Q2 are fall and spring semesters and carry higher in-school balances",
    x        = "Fiscal Quarter (within year)",
    y        = "Average In-School Balance ($ billions)",
    caption  = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status.\nAveraged over FY2013 to FY2019 to isolate the structural seasonal pattern."
  ) +
  stat184_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank()
  )
