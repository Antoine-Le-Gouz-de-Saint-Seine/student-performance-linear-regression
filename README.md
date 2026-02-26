# Student Performance – Linear Regression (R)

## Project Overview

This project investigates the factors associated with student exam performance using multiple linear regression in R.

The objective is to build an interpretable and statistically sound model explaining the relationship between exam scores and various student characteristics such as study habits, attendance, sleep, parental education, and school environment.

This work was completed as part of an academic project (M1 – MIDO, 2025–2026).

---

## Dataset

The dataset contains one observation per student.

- Target variable: `y` (exam score)
- Predictors include:
  - Age
  - Gender
  - Study hours
  - Sleep duration and quality
  - School attendance
  - Parental education
  - School type
  - Internet access
  - Extracurricular activities
  - Study method

The goal is to identify statistically meaningful associations and construct a parsimonious regression model.

---

## Methodology

The analysis follows a structured statistical workflow:

1. Data cleaning and preparation  
2. Exploratory Data Analysis (EDA)  
3. Construction of multiple regression models  
4. Model comparison using statistical criteria (AIC, Adjusted R², nested F-tests)  
5. Diagnostic checks (Residual analysis, Linearity assessment, Normality, Outliers detection) 
6. Internal validation using a reproducible train/test split (`set.seed(42)`)

The final submitted model was evaluated separately by the instructor on an external held-out dataset.

All analyses are fully reproducible in R.

---

## How to Reproduce the Analysis

Open the project in RStudio and run:

```r
source("R/code_student_performance.r")
