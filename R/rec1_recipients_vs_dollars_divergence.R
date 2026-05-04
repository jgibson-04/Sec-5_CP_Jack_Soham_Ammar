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

# Plan: compute the average outstanding default balance per defaulter by dividing
#       cumulative default dollars (col 13) by the recipient count (col 14)
#       Intent is to show that not only are more borrowers defaulting, but each
#       borrower in default is carrying a larger balance than before

# Needs: the readxl and tidyverse packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. select only fiscal year, quarter, default dollars, and default recipients
# 2. apply the standard fill, strip, filter, and convert pipeline
# 3. compute avg_debt_per_defaulter_k in thousands (billions / millions * 1000)

raw_dl_recip <- read_excel(
  path       = portfolio_fp,
  sheet      = "Direct Loan",
  skip       = 6,
  col_names  = FALSE,
  .name_repair = "unique"
) |>
  select(
    federal_fiscal_year = 1,
    quarter             = 2,
    default_dollars     = 13,
    default_recipients  = 14
  ) |>
  fill(federal_fiscal_year) |>
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    federal_fiscal_year      = as.numeric(federal_fiscal_year),
    default_dollars          = as.numeric(default_dollars),
    default_recipients       = as.numeric(default_recipients),
    # billions / millions * 1000 = thousands of dollars per borrower
    avg_debt_per_defaulter_k = (default_dollars / default_recipients) * 1000,
    time                     = paste(federal_fiscal_year, quarter_clean, sep = " "),
    time_index               = row_number()
  ) |>
  filter(!is.na(default_dollars), !is.na(default_recipients))

# Check: confirm avg_debt_per_defaulter_k is numeric and in a plausible range
# expect values around 14 to 24 thousand based on the data
glimpse(raw_dl_recip)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# ── Figure: Average Default Balance per Borrower ──────────────────────────────

# Plan: area-line chart of avg_debt_per_defaulter_k over time
#       the upward trend shows that the average borrower in default is carrying
#       an increasingly large balance, which makes rehabilitation harder

raw_dl_recip |>
  ggplot(aes(x = time_index, y = avg_debt_per_defaulter_k)) +
  geom_area(fill = "#8E44AD", alpha = 0.15) +
  geom_line(color = "#8E44AD", linewidth = 1.3) +
  scale_x_continuous(
    breaks = seq(1, nrow(raw_dl_recip), by = 4),
    labels = raw_dl_recip$time[seq(1, nrow(raw_dl_recip), by = 4)]
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "K", accuracy = 1)) +
  labs(
    title   = "Average Outstanding Default Balance per Borrower, FY2013 to FY2026",
    x       = "Fiscal Quarter",
    y       = "Avg. Default Balance per Borrower ($K)",
    caption = "Source: FSA Data Center / NSLDS — Direct Loan Portfolio by Loan Status.\nCalculated as cumulative default dollars (billions) / recipients (millions) x 1,000."
  ) +
  stat184_theme
