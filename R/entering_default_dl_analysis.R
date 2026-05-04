# Style Guide: Tidyverse Style Guide (https://style.tidyverse.org/)
# Primary Author: Ammar | Reviewed by: N/A

# ── Libraries ──────────────────────────────────────────────────────────────────

library(tidyverse)
library(readxl)
library(scales)

# ── File Path ──────────────────────────────────────────────────────────────────

# replace with local path or relative path if cloning via GitHub
# recommended: "data/DLEnteringDefaults.xls"

defaults_fp <- "C:/Users/ammar/OneDrive/Desktop/personal/spring 2026/STAT 184/project_datasets/DLEnteringDefaults.xls"

# ── Data Import (PCIP) ────────────────────────────────────────────────────────

# Plan: read the Direct Loans Entering Default workbook and produce two plots:
#       one for borrowers entering default and one for dollars entering default
#       broken down by first-time vs repeat default

# Needs: the readxl and tidyverse packages and the DLEnteringDefaults.xls file

# Steps:
# 1. skip the first 6 rows which are metadata and not data
# 2. name all 12 columns explicitly
# 3. fill the fiscal year downward since the workbook uses merged cells
# 4. strip asterisks from CARES-era quarter labels and filter to real Q1-Q4 rows
# 5. convert dollar and borrower columns to numeric
#    note: some cells contain "NA*" as text during the CARES era, so
#    suppressWarnings() is used to avoid noise when coercing those to NA

# Improve: consolidated to a single read_excel call with all 12 columns named
#          directly — an earlier draft read the file twice which was redundant

raw_entering <- read_excel(
  path       = defaults_fp,
  skip       = 6,
  col_names  = FALSE,
  .name_repair = "unique"
)

colnames(raw_entering) <- c(
  "federal_fiscal_year",
  "quarter",
  "dollars_in_repayment_prev_q",
  "recipients_in_repayment_prev_q",
  "dollars_total",
  "unique_borrowers",
  "pct_dollars_defaulted",
  "pct_borrowers_defaulted",
  "dollars_first",
  "borrowers_first",
  "dollars_second",
  "borrowers_second"
)

clean_entering <- raw_entering |>
  fill(federal_fiscal_year) |>
  mutate(quarter_clean = str_remove(as.character(quarter), "\\*+")) |>
  filter(str_detect(quarter_clean, "^Q[1-4]$")) |>
  mutate(
    federal_fiscal_year = as.numeric(federal_fiscal_year),
    across(
      c(
        dollars_total, dollars_first, dollars_second,
        unique_borrowers, borrowers_first, borrowers_second
      ),
      ~ suppressWarnings(as.numeric(.))
    ),
    time       = paste(federal_fiscal_year, quarter_clean, sep = " "),
    time_index = row_number()
  ) |>
  filter(!is.na(federal_fiscal_year))

# Check: confirm CARES-era dollar values are NA (not character "NA*")
# and that pre-pandemic rows have valid numeric dollar amounts
glimpse(clean_entering)

# ── Shared Theme ───────────────────────────────────────────────────────────────

stat184_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#1B3A5C", size = 11),
    plot.subtitle    = element_text(color = "#555555", size = 8.5),
    plot.caption     = element_text(color = "#888888", size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.minor = element_blank()
  )

# find the first quarter where new defaults drop to zero
# this is used to draw the CARES Act annotation line on the plot
zero_start <- min(
  clean_entering$time_index[
    !is.na(clean_entering$dollars_total) & clean_entering$dollars_total == 0
  ],
  na.rm = TRUE
)

# ── Figure 2: Dollars Entering Default ────────────────────────────────────────

# Plan: show that new defaults did not gradually decline — they dropped to
#       zero abruptly in FY2020 Q3, which is the key observation that
#       explains why cumulative default stopped growing during the CARES period

# Improve: replaced superseded recode() with case_when()
# Improve: added zero_start annotation line to make the CARES break visible

clean_entering |>
  select(time_index, time, dollars_total, dollars_first, dollars_second) |>
  pivot_longer(
    cols      = c(dollars_total, dollars_first, dollars_second),
    names_to  = "type",
    values_to = "value"
  ) |>
  mutate(
    type = case_when(
      type == "dollars_total"  ~ "Total Entering Default",
      type == "dollars_first"  ~ "First-Time Default",
      type == "dollars_second" ~ "Repeat Default",
      .default = type
    )
  ) |>
  ggplot(aes(x = time_index, y = value, color = type, group = type)) +
  geom_vline(
    xintercept = zero_start,
    linetype   = "dashed",
    color      = "#666666",
    linewidth  = 0.7
  ) +
  annotate(
    "text",
    x     = zero_start + 0.5,
    y     = max(clean_entering$dollars_total, na.rm = TRUE) * 0.82,
    label = "Defaults -> $0\n(CARES Act)",
    size  = 2.4,
    color = "#555555",
    hjust = 0
  ) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(
    breaks = seq(1, nrow(clean_entering), by = 3),
    labels = clean_entering$time[seq(1, nrow(clean_entering), by = 3)]
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B", accuracy = 0.1)) +
  scale_color_manual(values = c(
    "Total Entering Default" = "#C0392B",
    "First-Time Default"     = "#2980B9",
    "Repeat Default"         = "#E67E22"
  )) +
  labs(
    title   = "Direct Loans Entering Default per Quarter, FY2015 to FY2023",
    x       = "Fiscal Quarter",
    y       = "Dollars Entering Default ($ billions)",
    color   = NULL,
    caption = "Source: FSA Data Center / NSLDS — Direct Loans Entering Default workbook."
  ) +
  stat184_theme +
  theme(legend.position = "bottom")

# ── Borrowers Entering Default (supplementary) ────────────────────────────────

# Plan: the borrower count version of the same graph confirms the pattern
#       is not an artifact of rising loan sizes — the actual number of
#       borrowers entering default also dropped to zero

clean_entering |>
  select(time_index, time, unique_borrowers, borrowers_first, borrowers_second) |>
  pivot_longer(
    cols      = c(unique_borrowers, borrowers_first, borrowers_second),
    names_to  = "type",
    values_to = "value"
  ) |>
  mutate(
    type = case_when(
      type == "unique_borrowers" ~ "Total Entering Default",
      type == "borrowers_first"  ~ "First-Time Default",
      type == "borrowers_second" ~ "Repeat Default",
      .default = type
    )
  ) |>
  ggplot(aes(x = time_index, y = value, color = type, group = type)) +
  geom_vline(
    xintercept = zero_start,
    linetype   = "dashed",
    color      = "#666666",
    linewidth  = 0.7
  ) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(
    breaks = seq(1, nrow(clean_entering), by = 3),
    labels = clean_entering$time[seq(1, nrow(clean_entering), by = 3)]
  ) +
  scale_y_continuous(labels = label_comma(suffix = "K")) +
  scale_color_manual(values = c(
    "Total Entering Default" = "#C0392B",
    "First-Time Default"     = "#2980B9",
    "Repeat Default"         = "#E67E22"
  )) +
  labs(
    title   = "Borrowers Entering Default per Quarter, FY2015 to FY2023",
    x       = "Fiscal Quarter",
    y       = "Borrowers Entering Default (thousands)",
    color   = NULL,
    caption = "Source: FSA Data Center / NSLDS — Direct Loans Entering Default workbook."
  ) +
  stat184_theme +
  theme(legend.position = "bottom")
