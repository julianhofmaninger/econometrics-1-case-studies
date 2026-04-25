# Case 2 — Determinants of Birth Weight: Smoking, Prenatal Care & Maternal Characteristics

## Overview
This repository contains the second case study developed for the module **Econometrics I** at **Vienna University of Economics and Business**.  
The project applies econometric methods to analyze the determinants of **birth weight**, an important health indicator for newborns.

**Authors:** Tsz Lam Hung, Daniel Diederichs and Julian Hofmaninger  
**Instructor:** Univ.Prof. David Preinerstorfer, Ph.D  
**Term:** Winter Term 2025/2026  

---

## Research Question

What factors influence the birth weight of newborns? This case study analyses the effects of maternal smoking, prenatal care attendance, age, education, and alcohol consumption on birth weight, replicating and extending the empirical framework of Almond, Chay & Lee (2005).

## Dataset

**Source:** Almond, D., Chay, K.Y., & Lee, D.S. (2005). *The Costs of Low Birth Weight*. The Quarterly Journal of Economics, 120(3), 1031–1083.
**Coverage:** 3,000 US newborns
**Key variables:** `birthweight` (grams), `smoker` (binary), `tripre0` (no prenatal visit, binary), `age`, `educ` (years), `drinks` (weekly)

## Analysis

### Data Acquisition & Hypotheses
- Loaded and described all six variables with scientific justification for expected correlations
- Formed directional hypotheses supported by academic literature for each variable pair
- Empirical correlations confirmed: smoking negatively correlated with birthweight (r = −0.169); no prenatal visit negatively correlated (r = −0.123); age weakly positively correlated (r = 0.080)

### Density Plot Visualisation (ggplot2)
- Overlaid density plots using `geom_density` for birthweight by smoker status and by prenatal visit status
- Non-smoker births showed a higher mean birthweight and narrower distribution; mothers with prenatal visits had substantially higher birthweight distributions with a thinner left tail

### Multiple Linear Regression

**Model+ (full model including smoker):**
`birthweight ~ age + educ + drinks + smoker + tripre0`

- Estimated using OLS; R² = 0.046 vs 0.027 for model without smoker — including smoker improves fit
- Smoking during pregnancy associated with −216.5g reduction in birthweight (p < 0.001)
- No prenatal visit (tripre0 = 1) associated with −654.6g reduction in birthweight (p < 0.001)
- Drinks per week: not statistically significant (p = 0.41)
- Age: marginally positive effect (+3.6g per year, p = 0.10)

### Error Variance & Variance-Covariance Matrix
- Manually computed unbiased estimator of σ² from residual sum of squares: σ̂² = 334,919
- Constructed full variance-covariance matrix of OLS estimator using X'X formula
- Cov(educ, tripre0) = +19.60 — positive covariance indicating that more educated mothers tend to attend more prenatal visits

### Hypothesis Testing
- Formally tested impact of drinks (H₀: β₃ = 0): t-statistic < critical value → do not reject H₀
- Formally tested impact of prenatal visits (H₀: β₅ = 0): t-statistic > critical value → reject H₀ (p < 0.001)
- Custom t-statistic (H₀: β_tripre0 = 1): tested using student-t distribution with 2,994 df → rejected (p ≈ 8.7 × 10⁻¹⁰)

### Prediction
- Demonstrated birthweight prediction for a specified maternal profile (28 years, 12 years education, 2 drinks/week, smoker, no prenatal visit) → predicted birthweight: 2,527g

## Key Finding

Smoking during pregnancy (−216g) and absence of prenatal care (−655g) are the strongest and most statistically significant negative predictors of birth weight. Alcohol consumption showed no significant effect in this sample after controlling for other variables.

## Files

| File | Description |
|---|---|
| `CaseStudy2.Rmd` | R Markdown source |
| `CaseStudy2.pdf` | Compiled output with density plots, regression results and hypothesis tests |
