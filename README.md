# Functional Data Analysis: 24-Hour Heart Rate Profiles

## About the Project
This repository contains the code for a University project focusing on the application of **Functional Data Analysis (FDA)** to physiological data.
**Goal:** Analyze 24-hour heart rate (HR) profiles collected via an Apple Watch. The project aims to represent discrete heart rate measurements as continuous functional curves, identify anomalous daily patterns, and construct valid **Conformal Prediction Bands** to classify and bound different types of daily routines (e.g., resting days vs. training days).

## Dataset & Preprocessing
The dataset consists of high-frequency heart rate measurements recorded throughout the day.
* **Data Cleaning:** Days with sparse observations ($n < 240$) or unphysiological values (outside the 30-200 BPM range) were filtered out to ensure robust functional representation.
* **Exploratory Data Analysis (EDA):** Initial visualization of HR distributions across different sleep stages and motion contexts, including heatmaps and stacked area charts to understand the subject's circadian rhythm and activity levels.

## Methodology & Functional Modeling

### 1. Functional Representation (B-Splines)
Discrete time-series data were converted into continuous functions $x(t)$ using **B-spline basis functions**.
* **Smoothing Penalty:** A penalized least squares approach was used. The optimal roughness penalty ($\lambda$) was selected via **Generalized Cross-Validation (GCV)** to balance curve smoothness with the need to capture rapid, high-intensity HR spikes during workouts.

### 2. Anomaly Detection (Kernel Density Estimation)
* A functional $L^2$ distance matrix was computed between all daily curves.
* **Gaussian Kernel Density Estimation (KDE)** was applied to the functional space to estimate pseudo-densities. Days falling into extreme low-density regions were flagged as functional anomalies (days where the routine deviated significantly from the norm).

### 3. Clustering & Conformal Prediction Bands
To classify daily routines and provide statistically valid bounds, we implemented an **Inductive Conformal Prediction** framework (60% Train / 40% Calibration split). We applied **Functional Principal Component Analysis (FPCA)** to reduce dimensionality before clustering.

We explored two distinct functional spaces:
* **Phase A: Time-Domain Analysis**
  * Clustering (Gaussian Mixture Models & K-Means) applied directly to the time-domain curves.
  * *Result:* Clusters were heavily influenced by **Phase Variation** (the *time* of day the workout occurred), rather than the overall intensity of the day.
* **Phase B: Quantile-Transformed Analysis**
  * Curves were transformed via monotone rearrangement (descending sorting), shifting the domain from "Time of Day" to "Duration spent above a given HR". This perfectly eliminated phase variation.
  * Clustering (GMM & K-Means) in this quantile space successfully grouped days based purely on physical intensity amplitude.
  * *Result:* **GMM on Quantile-transformed curves** proved to be the most robust method for creating meaningful Conformal Prediction Bands.

## Authors
- Simone Massinelli
- Massimo Caloro
- Andrea Saccol
