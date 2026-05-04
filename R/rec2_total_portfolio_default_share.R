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

# Plan: compute total portfolio outstanding by summing all seven loan status
#       columns, then express cumulative default as a share of that total
#       Intent is to show whether the default surge in FY2026 is genuinely
#       worse relative to portfolio size, or just an artifact of a larger pool

# Needs: the readxl and tidyverse packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. select all seven dollar columns (odd-numbered after skipping metadata)
# 2. apply the standard fill, strip, filter, convert pipeline
# 3. sum all seven columns into total_portfolio using rowSums with across()
# 4. compute default_pct_of_total as cumulative_default / total_portfolio * 100

raw_dl_total <- read_excel(
  path       = portfolio_fp,
  sheet      = "Direct Loan",
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
    federal_fiscal_year  = as.numeric(federal_fiscal_year),
    # sum all status columns — this is the total outstanding portfolio each quarter
    total_portfolio      = rowSums(
      across(c(in_school, grace, repayment, deferment, forbearance, cumulative_default, other)),
      na.rm = TRUE
    ),
    # default share: what fraction of the total portfolio is in cumulative default
    default_pct_of_total = (cumulative_default / total_portfolio) * 100,
    time                 = paste(federal_fiscal_year, quarter_clean, sep = " "),
    time_index           = row_number()
  ) |>
  filter(!is.na(total_portfolio))

# Check: total_portfolio should be in the range of 500 to 1800 billion
# default_pct_of_total should be in the range of 4 to 10 percent pre-pandemic
glimpse(raw_dl_total)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# pre-pandemic peak used as a reference line
pre_pandemic_peak <- max(
  raw_dl_total$default_pct_of_total[raw_dl_total$federal_fiscal_year < 2020],
  na.rm = TRUE
)

# ── Figure: Cumulative Default as a Percentage of Total Portfolio ─────────────

# Plan: area-line chart normalized by total portfolio size
#       the dashed reference line at the pre-pandemic peak lets the reader see
#       whether the post-pause surge has crossed back above that level

raw_dl_total |>
  ggplot(aes(x = time_index, y = default_pct_of_total)) +
  geom_area(fill = "#C0392B", alpha = 0.18) +
  geom_line(color = "#C0392B", linewidth = 1.3) +
  geom_hline(
    yintercept = pre_pandemic_peak,
    linetype   = "dashed",
    color      = "#888888",
    linewidth  = 0.6
  ) +
  annotate(
    "text",
    x     = 4,
    y     = pre_pandemic_peak + 0.3,
    label = "Pre-pandemic peak",
    size  = 2.5,
    color = "#888888"
  ) +
  scale_x_continuous(
    breaks = seq(1, nrow(raw_dl_total), by = 4),
    labels = raw_dl_total$time[seq(1, nrow(raw_dl_total), by = 4)]
  ) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(
    title   = "Cumulative Default as a Percentage of Total Portfolio, FY2013 to FY2026",
    x       = "Fiscal Quarter",
    y       = "Default Share of Total Portfolio (%)",
    caption = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status.\nDashed line marks the highest pre-pandemic default share."
  ) +
  stat184_theme
