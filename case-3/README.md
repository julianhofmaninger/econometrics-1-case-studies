# Case 3 — Employer Mobility in Austria: Labour Market Analysis 1986–1998

## Overview
This repository contains the third case study developed for the module **Econometrics I** at **Vienna University of Economics and Business**.  
The project applies econometric methods to analyze labor market mobility, specifically the determinants of employer changes among Austrian workers.

**Authors:** Tsz Lam Hung, Daniel Diederichs and Julian Hofmaninger  
**Instructor:** Univ.Prof. David Preinerstorfer, Ph.D  
**Term:** Winter Term 2025/2026   

## Research Question

How often do Austrian workers change employer, and what factors drive this decision? This case study models employer change frequency using a 12-year panel of 2,222 Austrian workers, examining the roles of gender, occupation, age, income stability, and wage level.

## Dataset

**Source:** Austrian labour market panel data, 1986–1998
**Coverage:** 2,222 non-self-employed Austrian workers
**Key variables:** `nchange` (employer changes 1986–1998), `gender`, `occupation` (blue/white collar), `age` (in 1986), `periodsincome` (years with positive income), `medianwage` (quintile wage category 1–5)

## Analysis

### Descriptive Statistics
- Summary statistics for numerical variables: age range 22–42 (mean 31.3), income periods 1–13 (mean 10.0), employer changes 0–9 (median 1)
- Mode analysis for categorical variables: 57.9% male, 54.2% blue collar, most common wage category: lowest quintile (21.8%)
- Distribution of employer changes: strongly right-skewed — majority of workers changed employer 0–1 times
- Boxplots by gender, wage category, income periods, and occupation — median employer changes similar across gender and occupation, but extreme values more concentrated among men and blue collar workers

### Model 1 — Linear Regression (Baseline)
`nchange ~ gender + occupation + age + periodsincome + medianwage`

- Women change employer 0.242 fewer times than men (p < 0.001)
- White collar workers change 0.211 fewer times than blue collar (p < 0.001)
- Older workers change less frequently: −0.028 per additional year (p < 0.001)
- More stable income history → fewer changes: −0.031 per additional income period (p < 0.001)
- Higher wage categories significantly associated with fewer changes; lowest two categories statistically indistinguishable (linear hypothesis test: p = 0.28); highest two categories also indistinguishable (p = 0.94)

### Model 2 — Quadratic Effect of Income Periods
`nchange ~ gender + occupation + age + periodsincome + I(periodsincome²) + medianwage`

- Quadratic term highly significant (p < 0.001), substantially improving model fit (R² from 0.07 to 0.18)
- Vertex of the parabola at periodsincome = 7.63 — within the data range (1–13), confirming a non-monotone relationship: employer changes first increase then decrease as income stability grows
- Effect of two additional income years at the sample mean: −0.536 (exact) / −0.378 (marginal approximation)

### Model 3 — Gender × Occupation Interaction
`nchange ~ gender + occupation + gender:occupation + age + periodsincome + I(periodsincome²) + medianwage`

- Interaction term significant (p = 0.029)
- Marginal effects: women change less than men regardless of occupation, but the gender gap is much larger for blue collar workers (−0.411) than white collar (−0.165)
- White collar workers change less than blue collar regardless of gender, but the occupation gap is larger for men (−0.316) than women (−0.070)
- Prediction for specified profile (blue collar woman, age 35, 11 income periods, 2nd wage category): 1.55 employer changes

### Model Comparison
- AIC favours Model 3 (7,204.4); BIC favours Model 2 (7,269.9) — ambiguous
- Adjusted R²: Model 3 marginally better (0.180 vs 0.179) → Model 3 selected, though decision is acknowledged as vague

### Residual Diagnostics
- Residuals vs Fitted: slight non-linearity in LOESS curve; heteroscedasticity visible — residual spread increases with fitted values
- Q-Q plot: clear right-skew and heavy upper tail
- Jarque-Bera test: X² = 1,291.9, p < 0.001 → normality of errors conclusively rejected
- Conclusion: OLS coefficient estimates remain approximately unbiased (zero conditional mean broadly satisfied), but standard errors and p-values are unreliable due to heteroscedasticity and non-normality — significance conclusions should be treated with caution

## Key Finding

Income stability (periodsincome) has the most interesting non-linear relationship with employer mobility, with a turning point at approximately 7.6 years of positive income. Gender and occupation interact significantly: the gender gap in employer mobility is substantially larger among blue collar workers. The model's diagnostic limitations highlight the potential value of robust standard errors or count-data models (e.g. Poisson regression) for this type of outcome variable.

## Files

| File | Description |
|---|---|
| `CaseStudy3.Rmd` | R Markdown source |
| `CaseStudy3.pdf` | Compiled output with all models, diagnostics and interpretation |
