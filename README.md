# Econometrics I

This repository contains three applied econometrics case studies completed as part of the course **Econometrics I** at the Vienna University of Economics and Business (WU Vienna), academic year 2025/26, under the supervision of Univ.Prof. David Preinerstorfer, Ph.D.

Each case study applies econometric methods to a real-world research question using published datasets, combining data acquisition, exploratory analysis, model estimation, hypothesis testing, and critical interpretation of results.

**Authors:** Tsz Lam Hung, Daniel Diederichs and Julian Hofmaninger  
**Instructor:** Univ.Prof. David Preinerstorfer, Ph.D  
**Term:** Winter Term 2025/2026  

## Repository Structure

| Folder | Topic | Key Methods |
|---|---|---|
| `case-1` | School quality & student test scores (California) | Simple OLS, log transformation, residual analysis, prediction |
| `case-2` | Determinants of birth weight (USA) | Multiple OLS, hypothesis testing, variance-covariance matrix, GLM |
| `case-3` | Employer mobility in Austria 1986–1998 | OLS with dummies, quadratic effects, interaction terms, AIC/BIC, Jarque-Bera |

## Technical Stack

- **Language:** R
- **Key packages:** `readxl`, `ggplot2`, `car`, `tseries`
- **Data sources:** Stock-Watson 4th Edition (Princeton), Almond-Chay-Lee (2005, QJE), Austrian labour market panel 1986–1998
- **Output format:** R Markdown (PDF via knitr)

## Skills Demonstrated

- OLS estimation: simple and multiple regression, interpretation of coefficients and standard errors
- Variable transformations: log, log-log, quadratic, dummy encoding
- Hypothesis testing: t-tests, F-tests, partial F-tests, linear hypothesis testing (`car::linearHypothesis`)
- Model selection: AIC, BIC, adjusted R-squared comparison
- Residual diagnostics: LOESS curves, Q-Q plots, Jarque-Bera normality test, heteroscedasticity assessment
- Prediction: graphical reading, manual calculation, `predict()` function
- Causal reasoning: confounding, omitted variable bias, endogeneity discussion

## Datasets

All datasets are from publicly available or published academic sources:

- **CA Schools:** Stock & Watson 4th Edition supplementary data (Princeton University)
- **Birthweight & Smoking:** Almond, Chay & Lee (2005), *The Quarterly Journal of Economics*, 120(3)
- **Austrian Labour Market:** Panel data on 2,222 Austrian workers, 1986–1998
