# Case 1 — Returns to Education: School Quality & Student Test Scores (California)

## Overview
This repository contains a case study developed for the module **Econometrics I** at **Vienna University of Economics and Business**.  
The project applies econometric methods to analyze **return of education**.

**Authors:** Tsz Lam Hung, Daniel Diederichs and Julian Hofmaninger  
**Instructor:** Univ.Prof. David Preinerstorfer, Ph.D  
**Term:** Winter Term 2025/2026  

## Research Question

What school-level characteristics predict student academic achievement? This case study investigates the relationship between district median income, student-teacher ratio, and standardised test scores across California public schools, using data from the Stock-Watson 4th Edition dataset.

## Dataset

**Source:** Stock & Watson, *Introduction to Econometrics*, 4th Edition (Princeton University Press)
**Coverage:** California public (non-charter) schools, cross-sectional
**Key variables:** `testscore` (sum of 5th grade Maths and English exam scores), `str_s` (student-teacher ratio), `med_income_z` (median district income of residents aged 15+), `te_salary_avg_d` (average teacher salary)

## Analysis

### Data Acquisition & Variable Description
- Loaded data via `readxl`, filtered to non-charter (public) schools using `charter_s == 0`
- Described variables and formed prior hypotheses on expected correlations with test scores
- Computed Pearson correlations: income and testscore (r = +0.595, confirming wealthier districts achieve higher scores); student-teacher ratio and testscore (r = −0.056, confirming smaller classes tend to correlate with better outcomes)

### Descriptive Statistics
- Created a colour-formatted histogram of testscore distribution
- Identified extreme schools: Westmorland Elementary (lowest) vs Tom Matsumoto Elementary (highest)
- Compared teacher salary, student-teacher ratio, and district income across the two extremes — the lowest-scoring school had teacher salaries ~26% lower and district incomes ~70% lower than the highest-scoring school

### Visual Analysis
- Constructed a quantile-based student-teacher ratio grouping variable (`group_strs`: −1 / 0 / 1 for bottom 20%, middle, top 20%)
- Boxplots confirmed that higher student-teacher ratios are associated with lower median test scores

### OLS Estimation
- Applied log transformation to median income (`lmed_income = log(med_income_z)`) to linearise the relationship
- Estimated simple OLS: `testscore ~ log(med_income_z)`
- Reported OLS estimates: β̂₀ ≈ −396.8, β̂₁ ≈ 112.8
- Overlaid estimated regression line (blue) on scatterplot
- Generated predictions three ways: graphical reading (~725), manual formula (731.2), and `predict()` function (731.26) — all consistent
- Calculated predicted score change for a 0.5 unit decrease in log income: score drops to ~674.9

### Residual Analysis
- Plotted residuals against `lmed_income` — spread appears broadly constant, suggesting homoscedasticity holds
- LOESS curve in residuals-vs-fitted plot showed slight non-linearity, suggesting a log-log model (`log(testscore) ~ log(med_income_z)`) as a potential improvement

## Key Finding

District median income is the strongest predictor of test scores (r = 0.595), substantially outweighing the student-teacher ratio. After log-transforming income, OLS fits well with broadly satisfied assumptions, though a slight non-linear trend in residuals suggests further transformation could improve specification.

## Files

| File | Description |
|---|---|
| `CaseStudy1.Rmd` | R Markdown source |
| `CaseStudy1.pdf` | Compiled output with all plots, estimates and interpretation |
