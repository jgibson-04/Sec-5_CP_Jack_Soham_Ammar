# Team Guide — Federal Student Loan Default Analysis
## STAT 184 Spring 2026

This guide covers everything Soham and Jack need to add their sections to the QMD, as well as the GitHub workflow the whole team needs to follow for the rubric. Read through once before you start coding.

---

## What each person still needs to do

**Soham** — Debt Growth Over Time section (Section 5.1 in the QMD)
**Jack** — Repayment Plan Breakdown section (Section 5.2 in the QMD)
**Everyone** — GitHub commits, Issues, and Pull Requests (see below)

---

## How to add your section to the QMD

Open `federal_student_loan_analysis.qmd` and find your section. It has HTML comment placeholders that tell you exactly where to put things. They look like this:

```
<!-- SOHAM: Add your narrative introduction here -->
```

These comments will not appear in the rendered PDF. They are just instructions. Replace each one with your actual content.

### File paths

At the top of the setup chunk you will see:

```r
portfolio_fp  <- "C:\\Users\\ammar\\..."
defaults_fp   <- "C:\\Users\\ammar\\..."
```

You need to update these to point to wherever the data files are on your own machine, or move the files into the repo and use a relative path like `"data/PortfoliobyLoanStatus.xls"`. Using relative paths is the better option because it means anyone who clones the repo can render the document without changing anything.

### Code chunk format

Every chunk you add must follow this format:

````
```{r}
#| label: fig-soham-1
#| fig-cap: "Your figure caption here. Should describe what the figure shows."
#| fig-alt: "Alt text describing the figure for accessibility. Describe what you see, including key values."
#| fig-width: 6.5
#| fig-height: 3.8

# Primary Author: Soham | Reviewed by: [other member's name]
# Source file: your_r_script_filename.R
# Plan: what were you trying to do with this chunk
# Improve: anything you changed from an earlier version

# your ggplot code here
```
````

The `label` must be unique across the whole document. Use `fig-soham-1`, `fig-soham-2`, `fig-jack-1`, `fig-jack-2` etc.

### Writing your narrative

After each figure, write a paragraph explaining what you observe. Look at Ammar's sections as a model. Start by saying what you see in the graph, then explain what it means. A good pattern is:

> "From the graph we can observe... This is explained by... However..."

Do not use bullet points in the body text. Write in sentences.

### Descriptive statistics table

Each team member needs at least one professional table. The best approach is a `summarize()` call followed by `kable()`. Here is a template you can adapt:

```r
your_data |>
  summarize(
    Min    = round(min(your_column, na.rm = TRUE), 1),
    Median = round(median(your_column, na.rm = TRUE), 1),
    Mean   = round(mean(your_column, na.rm = TRUE), 1),
    Max    = round(max(your_column, na.rm = TRUE), 1),
    SD     = round(sd(your_column, na.rm = TRUE), 1)
  ) |>
  kable(booktabs = TRUE) |>
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 9)
```

### Code style rules (Prog.3, Prog.5, Prog.6)

The whole team is using the Tidyverse Style Guide. These are the specific things the grader checks:

- Variable names use `snake_case` (no dots, no camelCase)
- Lines should not exceed 80 characters. In RStudio you can turn on a margin guide under Tools > Global Options > Code > Display > Show margin
- Use the native pipe `|>` not `%>%`
- Always name function arguments explicitly, for example `read_excel(path = fp, sheet = "Direct Loan")` not `read_excel(fp, "Direct Loan")`
- Do not mix tidyverse functions with base R functions inside the same pipe chain
- Do not use `recode()` — it is superseded. Use `case_when()` instead
- Do not use `select_vars()`, `rename_vars()`, `tbl_df()`, `aes_string()`, or `separate_rows()` — all defunct or deprecated

### PCIP comments (CT.2)

Every data loading chunk should have comments showing the PCIP cycle. This does not need to be elaborate:

```r
# Plan: load the repayment plan data and compute share of borrowers per plan type
# Code: used pivot_longer to reshape from wide to tidy before plotting
# Improve: changed from read.csv to read_excel after realizing the source is .xls
# Polish: confirmed column types with glimpse() before proceeding
```

### Check your data after importing (DA.1)

After any `read_excel()` call, add a `glimpse()` line:

```r
raw_data <- read_excel(path = portfolio_fp, sheet = "Direct Loan", skip = 6)
glimpse(raw_data)  # Check: confirm expected rows, cols, and types
```

---

## GitHub requirements (Repro.4 — worth 10 points)

This is fully checked by the grader via the repo. Every item below is required.

### Branches

Each person needs their own development branch. Create yours from terminal or the RStudio Git panel:

```
git checkout -b dev-soham
git checkout -b dev-jack
git checkout -b dev-ammar
```

Keep `main` clean. All work goes on your dev branch first.

### Commits — at least 5 per person, with meaningful messages

Bad commit message: `update` / `changes` / `fix`

Good commit message format:
```
Add Soham debt growth visualization (fig-soham-1)
Fix fiscal year fill() in load-soham-data chunk
Add PCIP comments to load-soham-data per rubric
Add narrative text for debt growth section
Update file path to relative path for reproducibility
```

Each commit should describe what changed and why, not just that something changed. Aim for one logical change per commit rather than batching everything together.

### Pull Requests

When your section is ready to merge into main, open a Pull Request on GitHub. The person who opens the PR should not be the person who approves it. So if Soham opens the PR for `dev-soham`, either Jack or Ammar should review and approve it. Write a short description in the PR body summarizing what you added.

### Issues

Use GitHub Issues to track tasks and problems. Create an Issue for each major task before you start it, and close the Issue when it is done either by referencing it in a commit message (`closes #3`) or by closing it manually. Good examples of Issues to create:

- "Add Soham debt growth section to QMD"
- "Update file paths to relative paths"
- "Add descriptive stats table for repayment plan data — Jack"
- "Review and merge dev-soham into main"
- "Final render check before submission"

---

## README and repo structure (Repro.2, Repro.5)

The grader checks the README for a complete project description. The README.md file is already in this repo. Make sure the repo contains:

```
federal_student_loan_analysis.qmd   <- the main analysis file
federal_student_loan_analysis.pdf   <- the rendered output
data/
  PortfoliobyLoanStatus.xls
  DLEnteringDefaults.xls
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
README.md
TEAM_GUIDE.md
```

If the data files are too large to commit to GitHub (Excel files can be), add them to `.gitignore` and note in the README where the grader can download them from (studentaid.gov).

---

## Rendering the final PDF

Once all sections are merged into main, one person renders the final PDF:

```
quarto render federal_student_loan_analysis.qmd
```

or use the Render button in RStudio. The PDF output file should be committed to main and is what gets submitted to Canvas. In the Canvas submission comment, paste the direct link to the GitHub repo.

---

## Author Contribution table

Near the end of the QMD there is an Author Contribution table. Fill it in before submission using the CRediT taxonomy. Here is a template:

| Role | Contributor(s) |
|------|----------------|
| Conceptualization | Ammar, Soham, Jack |
| Data Curation | Ammar |
| Formal Analysis | Ammar (default trends), Soham (debt growth), Jack (repayment plans) |
| Visualization | Ammar (Figures 1–5), Soham (Figures 6–7), Jack (Figures 8–9) |
| Writing – Original Draft | Ammar (Sections 1–4), Soham (Section 5.1), Jack (Section 5.2) |
| Writing – Review & Editing | All members |

Update the figure numbers once you know how many each person produced.

---

## Quick checklist before final submission

- [ ] All `[Member A]` placeholders replaced with real names
- [ ] File paths changed to relative paths (not `C:\Users\ammar\...`)
- [ ] Soham's section complete with at least one table and one plot
- [ ] Jack's section complete with at least one table and one plot
- [ ] Author Contribution table filled in
- [ ] At least 5 commits per person with meaningful messages
- [ ] Each person has a dev branch
- [ ] At least one Pull Request per person merged into main
- [ ] At least 3 open-then-closed Issues in the repo
- [ ] README.md complete
- [ ] Final PDF rendered from QMD and committed to main
- [ ] Canvas submission includes link to repo in the comment field
