# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: Ammar | Reviewed by: N/A

# ── Libraries ──────────────────────────────────────────────────────────────────

library(readxl)
library(tidyverse)
library(scales)

# ── File Path ──────────────────────────────────────────────────────────────────

# replace with local path or relative path if cloning via GitHub
# recommended: "data/DLEnteringDefaults.xls"

defaults_fp <- "C:\\Users\\ammar\\OneDrive\\Desktop\\personal\\spring 2026\\STAT 184\\project_datasets\\DLEnteringDefaults.xls"

# ── Data Import (PCIP) ────────────────────────────────────────────────────────

# Plan: compute a normalized default rate — the percentage of borrowers in
#       repayment from the prior quarter who entered default in the current
#       quarter — to correct for the growth of the overall portfolio
#       This addresses the limitation of raw default dollar totals, which grow
#       partly just because the portfolio itself is larger each year

# Needs: the readxl and tidyverse packages and the DLEnteringDefaults.xls file

# Steps:
# 1. read the full defaults workbook with all 12 columns named explicitly
# 2. fill fiscal year downward and strip CARES asterisks from quarter labels
# 3. convert the pct_borrowers_defaulted column from decimal to percentage
# 4. flag CARES-era quarters for the shading annotation on the plot
#    CARES prohibition started FY2020 Q3 so Q1 and Q2 of 2020 are pre-CARES

raw_defaults <- read_excel(
  path       = defaults_fp,
  skip       = 6,
  col_names  = FALSE,
  .name_repair = "unique"
)

colnames(raw_defaults) <- c(
  "federal_fiscal_year",
  "quarter",
  "dollars_in_repayment_prev_q",
  "recipients_in_repayment_prev_q",
  "dollars_total_entering_default",
  "unique_borrowers_entering_default",
  "pct_dollars_defaulted",
  "pct_borrowers_defaulted",
  "dollars_first",
  "borrowers_first",
  "dollars_second",
  "borrowers_second"
)

clean_default_rate <- raw_defaults |>
  fill(federal_fiscal_year) |>
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    federal_fiscal_year  = as.numeric(federal_fiscal_year),
    pct_borrowers_defaulted = as.numeric(pct_borrowers_defaulted),
    # convert decimal rate to percentage for readable y-axis labels
    default_rate_pct     = pct_borrowers_defaulted * 100,
    time                 = paste(federal_fiscal_year, quarter_clean, sep = " "),
    time_index           = row_number(),
    # flag CARES-era quarters for background shading
    # CARES took effect in March 2020 which is FY2020 Q3
    is_cares_era         = federal_fiscal_year >= 2020 &
      !(federal_fiscal_year == 2020 & quarter_clean %in% c("Q1", "Q2"))
  ) |>
  filter(!is.na(federal_fiscal_year), !is.na(pct_borrowers_defaulted))

# Check: confirm default_rate_pct is numeric and CARES flag looks correct
# rows for FY2020 Q1 and Q2 should have is_cares_era = FALSE
glimpse(clean_default_rate)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# boundaries for the CARES shading rectangle
cares_start_idx <- min(clean_default_rate$time_index[clean_default_rate$is_cares_era])
cares_end_idx   <- max(clean_default_rate$time_index)

# ── Figure 5: Quarterly Default Rate ──────────────────────────────────────────

# Plan: show the default rate as a normalized metric so the reader can compare
#       across time without the confound of a growing portfolio
#       The green shading makes the CARES prohibition period immediately visible

clean_default_rate |>
  ggplot(aes(x = time_index, y = default_rate_pct)) +
  # shade the CARES era in green to distinguish it from normal quarters
  annotate(
    "rect",
    xmin = cares_start_idx - 0.5,
    xmax = cares_end_idx   + 0.5,
    ymin = -Inf,
    ymax = Inf,
    fill  = "#2ECC71",
    alpha = 0.12
  ) +
  geom_area(fill = "#C0392B", alpha = 0.18) +
  geom_line(color = "#C0392B", linewidth = 1.3) +
  # add points only for the pre-CARES quarters where there is real variation
  geom_point(
    data  = filter(clean_default_rate, !is_cares_era),
    color = "#C0392B",
    size  = 1.4
  ) +
  annotate(
    "text",
    x     = cares_start_idx + (cares_end_idx - cares_start_idx) / 2,
    y     = max(clean_default_rate$default_rate_pct, na.rm = TRUE) * 0.85,
    label = "CARES Act / Admin. Forbearance\n(Mar 2020 - Aug 2023)",
    size  = 2.5,
    color = "#1A7A45",
    fontface = "italic"
  ) +
  scale_x_continuous(
    breaks = seq(1, nrow(clean_default_rate), by = 3),
    labels = clean_default_rate$time[seq(1, nrow(clean_default_rate), by = 3)]
  ) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(
    title   = "Quarterly Default Rate, FY2015 to FY2023",
    subtitle = "Percentage of prior-quarter repayment borrowers entering default",
    x       = "Fiscal Quarter",
    y       = "Default Rate (% of Borrowers)",
    caption = "Source: FSA Data Center / NSLDS — Direct Loans Entering Default workbook.\nNote: CARES Act prohibited new defaults from FY2020 Q3 through FY2023 Q3."
  ) +
  stat184_theme
