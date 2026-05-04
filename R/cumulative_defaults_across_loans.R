# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: Ammar | Reviewed by: N/A

# ── Libraries ──────────────────────────────────────────────────────────────────

library(tidyverse)
library(readxl)
library(scales)

# ── File Path ──────────────────────────────────────────────────────────────────

# replace with local path or relative path if cloning via GitHub
# recommended: "data/PortfoliobyLoanStatus.xls"

portfolio_fp <- "C:\\Users\\ammar\\OneDrive\\Desktop\\personal\\spring 2026\\STAT 184\\project_datasets\\PortfoliobyLoanStatus.xls"

# ── Data Import (PCIP) ────────────────────────────────────────────────────────

# Plan: read cumulative default from all four loan type sheets and join them
#       into a single tidy frame for cross-loan comparison
#       Goal is to see whether the CARES Act pattern in direct loans holds
#       across other portfolio types, or whether it is specific to direct loans

# Needs: the tidyverse and readxl packages and the PortfoliobyLoanStatus.xls file

# Steps:
# 1. write a helper function that reads one sheet and returns only the columns
#    needed: fiscal year, quarter, and cumulative default
# 2. map the function across all four sheet names
# 3. join the results into one wide frame using reduce(full_join)
# 4. arrange and add a time label for plotting

# Code: used map() and reduce(full_join) to avoid writing the same
#       read_excel call four separate times
# Improve: added quarter_clean to the join key after an earlier draft produced
#          misaligned rows when joining on the raw quarter string which still
#          had asterisks in some sheets

loan_sheets <- c(
  "Direct Loan",
  "FFEL",
  "ED-Held FFEL",
  "Federally Managed"
)

# helper function: reads cumulative default column from any sheet
# renames the cumulative_default column using the sheet name so that after
# the full_join each loan type has its own clearly labeled column
read_cumulative_default <- function(sheet_name) {

  loan_name <- sheet_name |>
    tolower() |>
    str_replace_all(" ", "_") |>
    str_replace_all("-", "_")

  read_excel(
    path      = portfolio_fp,
    sheet     = sheet_name,
    skip      = 6,
    col_names = FALSE
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
      cumulative_default  = as.numeric(cumulative_default)
    ) |>
    rename(!!paste0(loan_name, "_cumulative_default") := cumulative_default)
}

cumulative_across_loans <- loan_sheets |>
  map(read_cumulative_default) |>
  reduce(full_join, by = c("federal_fiscal_year", "quarter", "quarter_clean")) |>
  arrange(federal_fiscal_year, quarter_clean) |>
  mutate(
    time       = paste(federal_fiscal_year, quarter_clean, sep = " "),
    time_index = row_number()
  )

# Check: confirm all four loan type columns are present and numeric
glimpse(cumulative_across_loans)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# ── Figure 4: Cumulative Defaults Across Loan Types ───────────────────────────

# Plan: overlay cumulative default for all four loan types on a single chart
#       to show that the CARES Act pattern holds for direct and ED-held FFEL
#       but not for the closed-book FFEL and federally managed portfolios

# Improve: replaced superseded recode() with case_when()
# Improve: switched from raw time string x-axis to time_index with labeled breaks
#          to fix the crowded label issue from the original version

cares_idx <- which(cumulative_across_loans$time == "2020 Q3")

cumulative_across_loans |>
  pivot_longer(
    cols      = ends_with("_cumulative_default"),
    names_to  = "loan_type",
    values_to = "cumulative_default"
  ) |>
  mutate(
    loan_type = case_when(
      loan_type == "direct_loan_cumulative_default"       ~ "Direct Loan",
      loan_type == "ffel_cumulative_default"              ~ "FFEL",
      loan_type == "ed_held_ffel_cumulative_default"      ~ "ED-Held FFEL",
      loan_type == "federally_managed_cumulative_default" ~ "Federally Managed",
      .default = loan_type
    )
  ) |>
  ggplot(aes(
    x     = time_index,
    y     = cumulative_default,
    color = loan_type,
    group = loan_type
  )) +
  geom_vline(
    xintercept = cares_idx,
    linetype   = "dashed",
    color      = "#666666",
    linewidth  = 0.7
  ) +
  annotate(
    "text",
    x     = cares_idx + 0.5,
    y     = 155,
    label = "CARES Act",
    size  = 2.4,
    color = "#555555",
    hjust = 0
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = seq(1, nrow(cumulative_across_loans), by = 4),
    labels = cumulative_across_loans$time[seq(1, nrow(cumulative_across_loans), by = 4)]
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 1)) +
  scale_color_manual(values = c(
    "Direct Loan"        = "#C0392B",
    "FFEL"               = "#8E44AD",
    "ED-Held FFEL"       = "#27AE60",
    "Federally Managed"  = "#2980B9"
  )) +
  labs(
    title   = "Cumulative Default Across All Federal Loan Types",
    x       = "Fiscal Quarter",
    y       = "Cumulative Default ($ billions)",
    color   = "Loan Type",
    caption = "Source: FSA Data Center / NSLDS — Portfolio by Loan Status workbook."
  ) +
  stat184_theme +
  theme(legend.position = "right", legend.text = element_text(size = 7))
