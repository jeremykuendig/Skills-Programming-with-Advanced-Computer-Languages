**Skills Programming with Advanced Computer Languages**
Group Project (Completed Individually)

**About**
This project is a student assignment for the course Skills Programming with Advanced Computer Languages at the University of St. Gallen (HSG).
Although designed as a group project, this work was completed individually.

The objective of the project is to build a supervised machine learning pipeline to predict next-season (t+1) NBA player salaries, adjusted for inflation, using only information available at season t.
This design ensures a realistic forecasting setting and avoids look-ahead bias.

The project covers the full data science workflow, including data engineering, feature construction, model training, validation, hyperparameter tuning, and out-of-sample evaluation.

**Project Objective**
Predict inflation-adjusted NBA salaries for the following season
Use a strict temporal setup (train on past seasons, test on future seasons)
Compare the performance of three regression models:
- Linear Regression (OLS)
- Random Forest
- Tuned Random Forest

**Pre-requisites**
The project is implemented in R.
To run the program, the following packages must be installed:
- tidyverse
- readxl
- janitor
- stringr
- mlr3
- mlr3learners
- mlr3pipelines
- mlr3tuning
- paradox

You can install them using:
install.packages(c(
  "tidyverse", "readxl", "janitor", "stringr",
  "mlr3", "mlr3learners", "mlr3pipelines",
  "mlr3tuning", "paradox"
))

**Instructions**
Set the correct project root path in the script:
NBA_PROJECT_ROOT <- "your/local/path"

Ensure the required datasets are located in the /data folder:
- NBA Player Stats (1950–2022)
- NBA Salaries (1990–2023)
- Salary cap data
- Team records

Run the main R script from start to end.

The script will:
- Build a cleaned and feature-engineered dataset
- Train and validate three models using rolling time-series cross-validation
- Evaluate final performance on the 2020 season

**Methodology**
Data Engineering (Part A):
Player-season statistics are merged with salary data, salary cap information, and team performance records.
Team names are standardized across datasets.
Multi-team seasons are handled carefully to avoid double counting.
Experience and team context variables are computed using only past information.
A proxy for contract renewal is constructed based on salary changes.

Machine Learning Pipeline (Part B):
Target variable: inflation-adjusted salary at season t+1
Training period: 2015–2019
Test period: 2020
Validation method: rolling forward time-series cross-validation

Models Used:
- Linear Regression (OLS): Baseline interpretable model.
- Random Forest: Non-linear model capturing interactions and complex patterns.
- Tuned Random Forest: Random Forest optimized via random search over key hyperparameters.

Evaluation Metrics:
- R²
- RMSE
- MSE

**Sources**
- Basketball-Reference (player statistics and team data): https://www.basketball-reference.com/
- NBA salary datasets: https://www.hoopshype.com/salaries/players/
