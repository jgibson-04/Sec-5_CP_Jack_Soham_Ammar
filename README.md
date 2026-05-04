# Federal Student Loan Default Trends
## STAT 184 Course Project — Spring 2026

**Team:** Ammar, Soham, Jack
**Section:** [Your section number]
**Submitted:** May 2026

---

## Project Overview

This project is an exploratory data analysis of federal student loan default trends from fiscal year 2013 through fiscal year 2026 Q1. The analysis examines how the total portfolio of outstanding federal student loans has evolved across loan statuses over time, with particular focus on the role of the CARES Act of 2020 and subsequent executive actions in reshaping default, forbearance, and repayment patterns.

The central research question driving the work is: how have macroeconomic policy interventions shaped the trajectory of federal student loan defaults, and what do the post-pandemic trends tell us about where the borrower population stands today?

The project covers three interconnected areas. Ammar analyzes default rate trends across loan types. Soham examines total debt growth over time. Jack investigates repayment plan composition and how it has shifted since 2016.

---

## Data Sources

Both datasets are sourced from the **Federal Student Aid (FSA) Data Center**, published by the U.S. Department of Education and drawn from the National Student Loan Data System (NSLDS). They are publicly available at:

https://studentaid.gov/data-center/student/portfolio

The two files used are:

**PortfoliobyLoanStatus.xls** — Quarterly snapshots of outstanding principal and interest balances across seven loan statuses (In School, Grace, Repayment, Deferment, Forbearance, Cumulative in Default, Other) for four portfolio types: Direct Loan, FFEL, ED-Held FFEL, and Federally Managed. Coverage begins in FY2013 Q3 for Direct Loans and runs through FY2026 Q1.

**DLEnteringDefaults.xls** — Quarterly counts of Direct Loan dollars and borrowers newly entering default, with breakdowns for first-time and repeat defaults. Coverage runs from FY2015 Q1 through FY2023 Q3.

Both files should be placed in the `data/` folder of this repo before rendering. If the data files are not present in the repo due to size constraints, download them directly from the FSA Data Center link above.

---

## Repository Structure

```
federal_student_loan_analysis.qmd    Main analysis file (renders to PDF)
federal_student_loan_analysis.pdf    Rendered report submitted to Canvas
README.md                            This file
TEAM_GUIDE.md                        Collaboration guide for team members
data/
  PortfoliobyLoanStatus.xls          Portfolio by Loan Status workbook
  DLEnteringDefaults.xls             Direct Loans Entering Default workbook
R/
  Protfolio_by_loan_status_direct_loans.R
  entering_default_dl_analysis.R
  cumulative_defaults_across_loans.R
  protfolio_by_loan_status_ed_held_ffel.R
  fig5_default_rate.R
  rec1_recipients_vs_dollars_divergence.R
  rec2_total_portfolio_default_share.R
  rec3_inschool_seasonality.R
  rec4_ffel_runoff.R
  rec5_fy2026_default_surge.R
```

---

## How to Reproduce This Analysis

1. Clone the repository to your local machine.

2. Open RStudio and install the required packages if you have not already:

```r
install.packages(c("readxl", "tidyverse", "ggplot2", "scales", "knitr", "kableExtra"))
```

3. Place the two data files in the `data/` folder, or update the file paths in the setup chunk of the QMD to point to wherever the files are on your machine.

4. Open `federal_student_loan_analysis.qmd` in RStudio and click Render, or run from the terminal:

```
quarto render federal_student_loan_analysis.qmd
```

5. The rendered PDF will appear in the project root directory.

If you do not have TinyTeX installed (required for PDF rendering), install it once with:

```
quarto install tinytex
```

---

## Coding Standards

All code in this project follows the [Tidyverse Style Guide](https://style.tidyverse.org/). Key conventions used throughout:

- Variable names use `snake_case`
- The native pipe `|>` is used for all chaining
- Function arguments are named explicitly
- Lines are kept under 80 characters
- All code chunks have meaningful labels, author headers, and PCIP-cycle comments

---

## Open Science Statement

This project follows the principles of Open Science as outlined by UNESCO. The data used is publicly available from a primary government source with no access restrictions. The full analysis code is contained in the QMD file and version controlled via GitHub, making the work transparent, reproducible, and auditable by any reader. The rendered PDF report and all source files are committed to this repository.

The data does not disaggregate by demographic group (race, gender, institution type), which is a limitation acknowledged in the report. This constrains the equity dimensions of the analysis and represents an area where the underlying data collection could be strengthened.

---

## Author Contributions

| Role | Contributor(s) |
|------|----------------|
| Conceptualization | Ammar, Soham, Jack |
| Data Curation | Ammar |
| Formal Analysis | Ammar (default trends), Soham (debt growth), Jack (repayment plans) |
| Visualization | Ammar (Figures 1–5), Soham (Figures 6–7), Jack (Figures 8–9) |
| Writing – Original Draft | Ammar (Sections 1–4), Soham (Section 5.1), Jack (Section 5.2) |
| Writing – Review & Editing | All members |

---

## References

U.S. Department of Education, Federal Student Aid (FSA) Data Center. (2026). *Federal Student Loan Portfolio: Portfolio by Loan Status* [Data set]. National Student Loan Data System (NSLDS). https://studentaid.gov/data-center/student/portfolio

U.S. Department of Education, Federal Student Aid (FSA) Data Center. (2026). *Direct Loans Entering Default* [Data set]. National Student Loan Data System (NSLDS). https://studentaid.gov/data-center/student/portfolio

U.S. Congress. (2020). *Coronavirus Aid, Relief, and Economic Security (CARES) Act*, Pub. L. No. 116-136, 134 Stat. 281.
