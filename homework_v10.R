# FUNCTIONAL DATA ANALYSIS: 24-HOUR HEART RATE PROFILES-----------

setwd("C:/Users/casam/OneDrive/Desktop/Simone/Stat_Brutti/Homework")

# install.packages(c("tidyverse", "lubridate", "gridExtra", "splines", "fda", 
#                    "cluster", "patchwork", "ggdendro", "mclust", "scales"))

library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(splines) 
library(fda)     
library(cluster)
library(patchwork)
library(ggdendro)
library(mclust)
library(scales)

# Dataset Upload and ''Feature Engineering''---------------------------

file_name <- "high_quality_data_filled.csv"

# We create a continuous time domain,strictly required for Functional 
# Data Analysis. We extract the absolute minute of the day (from 0 to 1439) 
# to use it as our common evaluation grid 't' for all functional curves.
hr_data <- read_csv(file_name, show_col_types = FALSE) %>%
  mutate(
    datetime      = make_datetime(Year, Month, Day, Hour, Minute, tz = "UTC"),
    date          = as_date(datetime),
    minute_of_day = as.numeric(difftime(datetime, floor_date(datetime, "day"), units = "mins"))
  ) %>%
  arrange(date, datetime) %>%
  mutate(day_id = as.integer(factor(date)))

# View(hr_data) (commented after we checked it was fine to avoid the visualization each time)

# Data Cleaning (Quantity and Physiological Range Filters)-----

# We did a quality check to ensure only reliable days are included in our FDA:
# 1. Quantitative Threshold (n >= 240): we choose to discard any day with less 
#    than 240 observations. Sparse data causes the B-spline penalty to wildly 
#    interpolate over large gaps, creating artificial variance.
# 2. Physiological Validation: if a single observation falls outside the human 
#    physiological range (30-200 BPM), the entire day is excluded.

hr_data <- hr_data %>%
  group_by(date) %>%
  filter(
    n() >= 240,                                           
    all(HeartRate >= 30 & HeartRate <= 200, na.rm = TRUE) 
  ) %>%
  ungroup()

cat("Cleaning Summary:\n")
cat("Final valid days remaining:", length(unique(hr_data$date)), "\n") # 43..no days deleted

# Exploratory Data Analysis (EDA)------------

# Conversion of categorical features for proper plotting and grouping
hr_data <- hr_data %>%
  mutate(
    MotionContext = factor(MotionContext),
    SleepStatus   = factor(SleepStatus),
    SleepStage    = factor(SleepStage)
  )

# Summary heart rate both in general and considering different situations 
cat("\nSummary Heart Rate\n")
print(summary(hr_data$HeartRate))

cat("\nHeart Rate per Sleep Stage:\n")
print(tapply(hr_data$HeartRate, hr_data$SleepStage, summary))
# 195 while sleeping.... what a nightmare... Or was Andrew busy with... something else???

cat("\nHeart Rate per Sleep Status:\n")
print(tapply(hr_data$HeartRate, hr_data$SleepStatus, summary))

cat("\nHeart Rate per Motion Context:\n")
print(tapply(hr_data$HeartRate, hr_data$MotionContext, summary))
# Overall, the results here are quite close to what we expected

# Heart rate distribution visualization (...Having some fun with ggplot...)
# From the histogram we can see that the heart rate distribution is right-skewed 
# with a median of 85 BPM. While most observations fall within the 60-100 BPM 
# range (resting state), the long right tail represents high-intensity 
# physiological spikes during sport activity.
p_hist <- ggplot(hr_data, aes(x = HeartRate)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(hr_data$HeartRate, na.rm = TRUE),
             colour = "firebrick", linewidth = 0.8, linetype = "dashed") +
  annotate("text",
           x     = median(hr_data$HeartRate, na.rm = TRUE) + 1,
           y     = Inf, vjust = 1.5, hjust = 0, size = 3,
           label = paste0("Median = ", round(median(hr_data$HeartRate, na.rm = TRUE), 1))) +
  labs(title = "Distribution Heart Rate", x = "HR (bpm)", y = "Count") +
  theme_minimal()

p_box_stage <- ggplot(hr_data %>% filter(!is.na(SleepStage)),
                      aes(x = SleepStage, y = HeartRate, fill = SleepStage)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "HR per Sleep Stage", x = "Sleep Stage", y = "HR (bpm)") +
  theme_minimal() + theme(legend.position = "none")

p_box_status <- ggplot(hr_data %>% filter(!is.na(SleepStatus)),
                       aes(x = SleepStatus, y = HeartRate, fill = SleepStatus)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "HR per Sleep Status", x = "Sleep Status", y = "HR (bpm)") +
  theme_minimal() + theme(legend.position = "none")

# Sleep analysis shows significantly lower and more stable HR during 
# deeper stages compared to awake one. 
# The reduced variance in sleep boxplots reflects HR stability during rest, 
# whereas the awake status introduces higher variability and elevated heart rates.

p_box_motion <- ggplot(hr_data %>% filter(!is.na(MotionContext)),
                       aes(x = MotionContext, y = HeartRate, fill = MotionContext)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "HR per Motion Context", x = "Motion Context", y = "HR (bpm)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

# The motion context boxplot reveals a clear correlation between physical movement 
# and heart rate. HR levels clearly increase moving from sedentary positions (0)
# to active motion(2), with 'Running' or 'Walking' showing the highest medians 
# and the widest interquartile ranges.
# This boxplot also reveals significant high-intensity outliers within the '0' 
# (Sedentary) category. These points indicate heart rate spikes (reaching 150-180 BPM) 
# occurring in the absence of physical movement. This situation suggests events such 
# as isometric exercise, high emotional stress, or post-workout recovery, where 
# the heart rate remains elevated while the body is stationary.

grid.arrange(p_hist, p_box_stage, p_box_status, p_box_motion, ncol = 2)

# Daily HR heatmap
df_heat <- hr_data %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  group_by(day_id, date, time_bin) %>%
  summarise(avg_HR = mean(HeartRate, na.rm = TRUE), .groups = "drop")

p_heatmap <- ggplot(df_heat,
                    aes(x = time_bin, y = factor(day_id), fill = avg_HR)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("midnightblue", "lightgoldenrod", "firebrick"), name = "HR") +
  scale_x_continuous(breaks = seq(0, 1440, by = 180),
                     labels = seq(0, 1440, by = 180)) +
  labs(title = "Daily HR Heatmap",
       x = "Minute of the day", y = "Day ID") +
  theme_minimal()

print(p_heatmap)
# The Daily HR Heatmap illustrates the longitudinal heart rate patterns across the study period. 
# A consistent blue band in the early hours (0-480 min) shows the obvious low HR during sleep. 
# Daytime periods show higher variability, with distinct red points in the late afternoon 
# (900-1200 min) identifying high-intensity exercise sessions. This visualization confirms 
# a stable circadian rhythm while highlighting sporadic physical activities as the main functional deviations.

# Motion Context through the day plot
df_motion_time <- hr_data %>%
  filter(!is.na(MotionContext)) %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  dplyr::count(time_bin, MotionContext) %>% 
  group_by(time_bin) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_motion_time <- ggplot(df_motion_time,
                        aes(x = time_bin, y = prop, fill = as.factor(MotionContext))) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 1440, by = 180),
                     labels = seq(0, 1440, by = 180)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Motion Context Evolution Through the Day",
       subtitle = "Proportion of physical activity levels across 1440 minutes",
       x = "Minutes of the day", y = "Proportion", fill = "Motion Type") +
  theme_minimal()

print(p_motion_time)
# With this plot we can visualize the distribution of movement intensities 
# throughout the 24-hour cycle. By normalizing the counts into proportions,
# the plot effectively shows the dominance of sedentary states (Motion 0) 
# during the night and early morning, followed by the emergence of diverse 
# physical activities during daylight hours. This temporal profile provides 
# context for the heart rate spikes observed in the previous analysis, 
# confirming that high-intensity HR observations align with the periods where 
# the subject is more physically active.

# START OF THE FUNCTIONAL ANALYSIS

# OPTIMIZED B-SPLINE PARAMETERS (Penalized Least Squares)----------------------

# Functional Analysis only works with continuous functions. For this reason,
# we needed to create continuous functions starting from our data.
# We chose to do it with B-Spline regression, which fit the best polynomial of 
# specified degree for each interval (the number of basis functions has to be specified as well).

# Parameters defining the continuous functional domain
time_range <- c(0, 1440)
grid_min   <- seq(0, 1439, by = 1)

days_list <- hr_data %>% 
  group_by(date) %>% 
  group_split()

# Generalized Cross-Validation (GCV) calculation function.
# It mathematically approximates the process
# of testing the curve against unseen points, allowing us to find the optimal 
# smoothing penalty without the computational cost of re-fitting the model point by point.
#
# This fits a smoothing spline and computes the average GCV across all valid days 
# to find the optimal roughness penalty.
compute_avg_gcv <- function(K, lambda_val) {
  basis_obj <- create.bspline.basis(rangeval = time_range, nbasis = K, norder = 4)
  fdPar_obj <- fdPar(basis_obj, Lfdobj = 2, lambda = lambda_val)
  
  gcv_vec <- numeric(length(days_list))
  valid   <- 0
  
  for (day_data in days_list) {
    if (nrow(day_data) < 30 || sd(day_data$HeartRate, na.rm = TRUE) < 2) next
    
    smooth_fit <- tryCatch(
      smooth.basis(argvals  = day_data$minute_of_day,
                   y        = day_data$HeartRate,
                   fdParobj = fdPar_obj),
      error = function(e) NULL
    )
    
    if (!is.null(smooth_fit) && length(smooth_fit$gcv) > 0) {
      valid <- valid + 1
      gcv_vec[valid] <- mean(smooth_fit$gcv, na.rm = TRUE)
    }
  }
  if (valid == 0) return(Inf)
  mean(gcv_vec[1:valid])
}

# GRID SEARCH 2D -----------------------------------------------------------
# (Commented out to save computation time, but kept to show the rigorous process)

# k_values    <- seq(30, 200, by = 5)  
# lambda_vals <- 10^seq(-4, 4, length.out = 20)
# gcv_mat <- outer(k_values, lambda_vals, Vectorize(compute_avg_gcv))
# best_idx    <- which(gcv_mat == min(gcv_mat, na.rm = TRUE), arr.ind = TRUE)
# best_K      <- k_values[best_idx[1]]
# best_lambda <- lambda_vals[best_idx[2]]
# 
# cat(sprintf("\n--- OPTIMAL PARAMETERS (GCV) ---\nK (nbasis) = %d\nlambda     = %.2e\n", 
#             best_K, best_lambda))
#
# gcv_vs_k <- apply(gcv_mat, 1, min)
# p_gcv_k <- ggplot(data.frame(K = k_values, GCV = gcv_vs_k), aes(K, GCV)) +
#   geom_line(color = "steelblue", linewidth = 1.1) + geom_point(color = "firebrick", size = 3) +
#   geom_vline(xintercept = best_K, color = "firebrick", linetype = "dashed", linewidth = 1) +
#   labs(title = "GCV vs Number of Basis Functions (K)", subtitle = paste("Optimal K =", best_K),
#        x = "K (nbasis)", y = "Average GCV") + theme_minimal(base_size = 14) +
#   theme(plot.title = element_text(face = "bold"))
#
# idx_k <- which(k_values == best_K)
# gcv_vs_lam <- gcv_mat[idx_k, ]
# p_gcv_lam <- ggplot(data.frame(Lambda = lambda_vals, GCV = gcv_vs_lam), 
#                     aes(log10(Lambda), GCV)) +
#   geom_line(color = "forestgreen", linewidth = 1.1) + geom_point(color = "firebrick", size = 3) +
#   geom_vline(xintercept = log10(best_lambda), color = "firebrick", linetype = "dashed", linewidth = 1) +
#   labs(title = "GCV vs Smoothing Penalty (λ)", subtitle = paste("Optimal log10(λ) =", round(log10(best_lambda), 2)),
#        x = "log10(λ)", y = "Average GCV") + theme_minimal(base_size = 14) +
#   theme(plot.title = element_text(face = "bold"))
#
# grid.arrange(p_gcv_k, p_gcv_lam, ncol = 2)

# Grid search results was unstable, so we fix K = 50. 
# This roughly fits a degree 3 polynomial for every 30 minutes(~), keeping things simple 
# while maintaining enough flexibility to capture ''rapid'' HR spikes.

# 1D OPTIMIZATION FOR LAMBDA (Fixed K) -------------------------------------

best_K <- 50
lambda_vals <- 10^seq(-4, 4, length.out = 50)
gcv_scores  <- numeric(length(lambda_vals))

for (i in seq_along(lambda_vals)) {
  gcv_scores[i] <- compute_avg_gcv(K = best_K, lambda_val = lambda_vals[i])
}

best_lambda <- lambda_vals[which.min(gcv_scores)]
cat(sprintf("Optimal lambda for K=50 is: %.2e\n", best_lambda))

# FINAL SMOOTHING OBJECT ---------------------------------------------------

final_basis <- create.bspline.basis(rangeval = time_range, nbasis = best_K, norder = 4)
final_fdPar <- fdPar(final_basis, Lfdobj = 2, lambda = best_lambda)

# Matrix for evaluated curves and valid dates
daily_matrix <- matrix(NA_real_, nrow = length(days_list), ncol = length(grid_min))
good_dates   <- character(length(days_list))
row_idx <- 1

# Apply smoothing day by day
for (day_data in days_list) {
  date_str <- as.character(day_data$date[1])
  
  # Skip days with too few observations to avoid extreme interpolation
  if (nrow(day_data) < 30) next
  
  # Fit smoothing spline to raw data
  smooth_fit <- tryCatch(
    smooth.basis(argvals = day_data$minute_of_day, 
                 y       = day_data$HeartRate, 
                 fdParobj = final_fdPar), 
    error = function(e) NULL
  )
  
  if (is.null(smooth_fit)) next
  
  # Evaluate curve on the 1-min continuous grid
  hr_grid <- as.vector(eval.fd(grid_min, smooth_fit$fd))
  
  # Filter out physiological anomalies and boundary artifacts caused by splines
  if (all(hr_grid >= 35 & hr_grid <= 215, na.rm = TRUE)) {
    daily_matrix[row_idx, ] <- hr_grid
    good_dates[row_idx]     <- date_str
    row_idx <- row_idx + 1
  }
}

# Clean up unused rows from the matrix
if (row_idx > 1) {
  daily_matrix <- daily_matrix[1:(row_idx - 1), , drop = FALSE]
  good_dates   <- good_dates[1:(row_idx - 1)]
} else {
  stop("No valid days remained.")
}

cat("\nFinal clean matrix contains:", nrow(daily_matrix), "days\n")

# Convert the clean matrix back to an fd object for analysis
fd_obj <- Data2fd(argvals  = grid_min, 
                  y        = t(daily_matrix), 
                  basisobj = final_basis)

# VISUALIZATION OF OBTAINED FUNCTIONS -----------------------------------------

plot(fd_obj, lty = 1, col = 'steelblue',
     main = "Optimized Daily HR Curves (B-spline + GCV)",
     xlab = "Minute of the Day", ylab = "Heart Rate (bpm)")

# FUNCTIONAL COMPARISON: WEEKDAYS VS WEEKENDS ------------------------------

# This section does not belong to the functional analysis but it's useful to 
# understand how different scenarios can give us different types of functions and thus create 
# different clusters.

# Identify the type of day for each valid date
day_of_week <- weekdays(as.Date(good_dates)) 
is_weekend  <- day_of_week %in% c("Saturday", "Sunday", "sabato", "domenica") 

n_weekend  <- sum(is_weekend)
n_weekday  <- sum(!is_weekend)

cat(sprintf("Weekdays: %d | Weekends: %d\n", n_weekday, n_weekend))

# Setup human-readable colors and line widths
col_weekday <- scales::alpha("steelblue", 0.35)
col_weekend <- scales::alpha("firebrick", 0.55)

plot_colors <- ifelse(is_weekend, col_weekend, col_weekday)
plot_widths <- ifelse(is_weekend, 1.5, 1)

plot(fd_obj,
     col  = plot_colors,
     lwd  = plot_widths,
     lty  = 1,
     main = "Functional Profiles: Weekdays vs Weekends",
     xlab = "Minute of the Day",
     ylab = "Heart Rate (bpm)")

# Functional mean for weekdays
mean_weekday <- mean.fd(fd_obj[which(!is_weekend)])
lines(mean_weekday, col = "navy", lwd = 3, lty = 2)

# Functional mean for weekends
mean_weekend <- mean.fd(fd_obj[which(is_weekend)])
lines(mean_weekend, col = "darkred", lwd = 3, lty = 2)

# Global functional mean
lines(mean.fd(fd_obj), col = "black", lwd = 3, lty = 1)

legend("topleft",
       legend = c(
         paste0("Weekday (n=", n_weekday, ")"),
         paste0("Weekend (n=", n_weekend, ")"),
         "Weekday Mean",
         "Weekend Mean",
         "Global Mean"
       ),
       col = c(col_weekday, col_weekend, "navy", "darkred", "black"),
       lwd = c(1, 1.5, 3, 3, 3),
       lty = c(1, 1, 2, 2, 1),
       bty = "n",
       cex = 0.7)

# This view generally highlights afternoon sports occurring slightly earlier 
# during weekends, alongside different nightlife or sleep patterns.

# CONFORMAL PREDICTION (PSEUDO-DENSITY & ALPHA ANALYSIS)---------

# We calculate functional L2 distances to find days that deviate completely 
# from normal behavior. By using a Gaussian Kernel to estimate the density 
# of curves in the functional space, we can identify low-density outliers.

matrix_smooth <- t(eval.fd(grid_min, fd_obj))
dist_matrix   <- as.matrix(dist(matrix_smooth) / sqrt(dim(matrix_smooth)[2]))

# Kernel Density Estimation with bandwidth based on the 15th percentile of distances
h_param <- quantile(dist_matrix, 0.15) 
kernel_gaussian <- function(u) { (1 / sqrt(2 * pi)) * exp(-0.5 * u^2) }

pseudo_density <- numeric(nrow(matrix_smooth))
for(i in seq_len(nrow(matrix_smooth))) {
  pseudo_density[i] <- mean(kernel_gaussian(dist_matrix[i, ] / h_param))
}

alpha_levels <- c(0.1, 0.5, 0.9)
par(mfrow = c(1, 3)) 

for(alpha_val in alpha_levels) {
  # Calculate the density threshold for the given alpha
  lambda_alpha <- quantile(pseudo_density, alpha_val)
  subset_idx   <- which(pseudo_density >= lambda_alpha)
  
  if(length(subset_idx) > 1) {
    # Draw the conformal confidence set of curves
    matplot(grid_min, t(matrix_smooth[subset_idx, ]), 
            type = "l", 
            lty = 1, 
            col = scales::alpha("gray40", 0.3), 
            ylim = range(matrix_smooth),
            main = paste("Alpha =", alpha_val, "\n(Confidence Set)"),
            ylab = "BPM", 
            xlab = "Minutes")
    # Add the specific mean of this subset (red line)
    lines(grid_min, colMeans(matrix_smooth[subset_idx, ]), col = "firebrick", lwd = 2.5)
    mtext(paste("Included days:", length(subset_idx)), side=3, line=-1.5, cex=0.7)
  }
}

par(mfrow = c(1, 1))

cat("\nAnomaly Detection via Confidence Threshold\n")
alpha_outlier <- 0.05
density_threshold <- quantile(pseudo_density, alpha_outlier)

# Identify outliers (days living in extreme low-density regions)
outlier_indices <- which(pseudo_density <= density_threshold)
n_detected      <- length(outlier_indices)
outlier_dates   <- good_dates[outlier_indices]

cat(sprintf("Calculated density threshold: %.6f\n", density_threshold))
cat(sprintf("Number of outliers detected (alpha=%.2f): %d\n", alpha_outlier, n_detected))

# Prepare colors: a distinct palette for outliers, gray for normal days
outlier_colors <- rainbow(n_detected)
plot_colors    <- rep(scales::alpha("gray60", 0.4), length(good_dates))
plot_linewidths <- rep(1, length(good_dates))

for(i in seq_len(n_detected)) {
  plot_colors[outlier_indices[i]] <- outlier_colors[i]
  plot_linewidths[outlier_indices[i]] <- 3 
}

# FINAL ANOMALY PLOT
plot(fd_obj, 
     col = plot_colors, 
     lwd = plot_linewidths, 
     lty = 1,
     main = paste("Functional Anomalies Detected (Alpha =", alpha_outlier, ")"),
     xlab = "Minutes (0-1440)", 
     ylab = "BPM")

lines(mean.fd(fd_obj), col = "black", lwd = 3, lty = 1)

# Dynamic legend adapting to the number of outliers found
if(n_detected <= 5) {
  legend_labels <- c("Normal Days", as.character(outlier_dates), "Global Mean")
  legend_colors <- c("gray60", outlier_colors, "black")
  legend_lwds   <- c(1, rep(3, n_detected), 3)
} else {
  legend_labels <- c("Normal Days", paste(n_detected, "Outliers detected"), "Global Mean")
  legend_colors <- c("gray60", "red", "black")
  legend_lwds   <- c(1, 3, 3)
}

legend("topleft", 
       legend = legend_labels, 
       col = legend_colors, 
       lwd = legend_lwds, 
       lty = 1, 
       bty = "n", 
       cex = 0.7)

print(data.frame(Date = outlier_dates, Density = pseudo_density[outlier_indices]))

# Of course those days can't really be considered as ouliers in the strict 
# sense of the word... they are not anomalies, but instead the days in which 
# Andrea behaved more differently respect to his 'classical routine' (black function in the graph)

# CONFIGURABLE PARAMETERS & TRAIN/CALIBRATION SPLIT------

set.seed(1235)

VAR_THRESHOLD   <- 0.85      # Cumulative variance threshold to select PCs
N_PC_MANUAL     <- NULL      # NULL = auto-select based on VAR_THRESHOLD
BAND_COVERAGE   <- 0.90      # 90% confidence level for conformal bands
MAX_K           <- 6         # Maximum number of clusters to search
K_MANUAL        <- NULL      # NULL = auto-select K based on Silhouette/BIC
CENTERING       <- TRUE      # Global functional centering (subtracting mean curve)
STANDARDIZE     <- FALSE     # Global functional standardization
SPLIT_RATIO     <- 0.6       # 60% train / 40% calibration for inductive CP
DIAGONAL_GMM    <- TRUE      # To avoid errors for high number of clusters computatins

# INDUCTIVE CONFORMAL SETUP ------------------
# We split the data into a training set (to safely learn the principal components 
# and cluster centroids without data leakage) and a calibration set (to compute 
# non-conformity scores and empirical thresholds for the valid conformal bands).

n_days     <- nrow(daily_matrix)
train_size <- round(n_days * SPLIT_RATIO)
train_idx  <- sample(seq_len(n_days), train_size, replace = FALSE)
calib_idx  <- setdiff(seq_len(n_days), train_idx)

# Raw matrices before any transformation
matrix_train_raw <- daily_matrix[train_idx, , drop = FALSE]
matrix_calib_raw <- daily_matrix[calib_idx, , drop = FALSE]
good_dates_train <- good_dates[train_idx]
good_dates_calib <- good_dates[calib_idx]

cat(sprintf("\nInductive split (60/40): %d train days | %d calibration days\n", nrow(matrix_train_raw), nrow(matrix_calib_raw)))

# PHASE A: EXACT ANALYSIS ON ORIGINAL TIME-DOMAIN CURVES-------

cat("PHASE A: EXACT PIPELINE ON ORIGINAL TIME-DOMAIN CURVES\n")

#FPCA 

# 1. Global Mean & SD estimated ONLY on TRAIN to avoid data leakage
mean_time <- colMeans(matrix_train_raw, na.rm = TRUE)

# 2. Centering: Training parameters applied to both sets
train_cent_time <- sweep(matrix_train_raw, 2, mean_time, "-")
calib_cent_time <- sweep(matrix_calib_raw, 2, mean_time, "-")

# 3. FPCA fitted ONLY on TRAIN
fpca_time <- prcomp(train_cent_time, center = FALSE, scale. = FALSE)
eigenvecs_time <- fpca_time$rotation
cum_var_time   <- cumsum(fpca_time$sdev^2) / sum(fpca_time$sdev^2)

n_pc_time <- which(cum_var_time >= VAR_THRESHOLD)[1]
if (is.na(n_pc_time)) n_pc_time <- min(6, ncol(eigenvecs_time))

scores_train_time <- fpca_time$x[, 1:n_pc_time, drop = FALSE]
scores_calib_time <- calib_cent_time %*% eigenvecs_time[, 1:n_pc_time, drop = FALSE]
Phi_time          <- eigenvecs_time[, 1:n_pc_time, drop = FALSE]

cat(sprintf("Selected TIME-DOMAIN PCs: %d | Explained variance: %.1f%%\n", n_pc_time, cum_var_time[n_pc_time] * 100))

# Time-Domain Scree Plot
var_df_time <- data.frame(
  PC     = seq_along(cum_var_time),
  VarExp = (fpca_time$sdev^2) / sum(fpca_time$sdev^2) * 100,
  CumVar = cum_var_time * 100
)

p_scree_time <- ggplot(var_df_time[1:min(12, nrow(var_df_time)), ], aes(x = PC)) +
  geom_col(aes(y = VarExp), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = CumVar), color = "firebrick", linewidth = 1) +
  geom_point(aes(y = CumVar), color = "firebrick", size = 2.5) +
  geom_hline(yintercept = VAR_THRESHOLD * 100, linetype = "dashed", color = "firebrick", alpha = 0.6) +
  geom_vline(xintercept = n_pc_time, linetype = "dotted", color = "navy", linewidth = 0.9) +
  scale_y_continuous(name = "Explained variance (%)", sec.axis = sec_axis(~., name = "Cumulative variance (%)")) +
  labs(title = "Time-Domain Scree Plot (fit on train)", x = "Principal component") +
  theme_minimal(base_size = 13)
print(p_scree_time)

alpha_cp <- 1 - BAND_COVERAGE   
n_calib  <- nrow(scores_calib_time)  
n_grid   <- length(grid_min)

# We will use both GMM and K-Means clustering algorithms.

# Our goal is not just to compute different clusters, but also to create interpretable ones.
# For this reason, knowing how Andrea's routines can be classified,
# we set the goal to classify days in 2 or 3 different clusters, which will most likely represent
# rest and phisical activity (morning activity and afternoon activity if 3) in the time domain phase.
# For the Quantile FA instead, the third cluster would may represent days in which Andrea moved a lot,
# but constantly (not just a training).

# STRATEGY 1: GMM CLUSTERING (TIME-DOMAIN) ---------------------------------
cat("\nTIME-DOMAIN: GMM CONFORMAL PREDICTION BANDS\n")

gmm_modeltype <- if (DIAGONAL_GMM) "VVI" else "VVV"

# We evaluate the BIC for G = 1 to MAX_K (6) to visualize the full statistical trend
bic_time <- mclustBIC(scores_train_time, G = 1:MAX_K, modelNames = gmm_modeltype)
plot(bic_time, main = "Time-Domain GMM BIC (1 to 6 Clusters)")

# However, we strictly restrict the final model choice between G=2 and G=3 for interpretability
# as previously explained.
gmm_fit_time  <- Mclust(scores_train_time, G = 2:3, modelNames = gmm_modeltype, verbose = FALSE)

# Safety fallback if the full covariance matrix (VVV) is unstable for the data
if (is.null(gmm_fit_time) || is.null(gmm_fit_time$G)) {
  cat("VVV model unstable. Switching to VVI (Diagonal covariance)...\n")
  gmm_fit_time  <- Mclust(scores_train_time, G = 2:3, modelNames = "VVI", verbose = FALSE)
}

K_gmm_time  <- gmm_fit_time$G
pi_hat_time <- gmm_fit_time$parameters$pro
mu_hat_time <- gmm_fit_time$parameters$mean   

get_sigma_k_time <- function(k) {
  S <- gmm_fit_time$parameters$variance$sigma[,, k]
  if (DIAGONAL_GMM) diag(diag(S)) else S
}

sigma_inv_time <- lapply(seq_len(K_gmm_time), function(k) solve(get_sigma_k_time(k)))
log_det_time   <- sapply(seq_len(K_gmm_time), function(k) as.numeric(determinant(get_sigma_k_time(k), logarithm = TRUE)$modulus))

# Conformity scores (log-scale max-component)
scores_conf_gmm_time <- numeric(n_calib)
for (i in seq_len(n_calib)) {
  xi <- as.numeric(scores_calib_time[i, ])
  vals <- sapply(seq_len(K_gmm_time), function(k) {
    d <- xi - mu_hat_time[, k]
    maha <- as.numeric(t(d) %*% sigma_inv_time[[k]] %*% d)
    log(pi_hat_time[k]) - 0.5 * (n_pc_time * log(2 * pi) + log_det_time[k] + maha)
  })
  scores_conf_gmm_time[i] <- max(vals)
}

lambda_gmm_time <- quantile(scores_conf_gmm_time, alpha_cp)

# Ellipsoid to functional bands
upper_gmm_time <- matrix(-Inf, nrow = n_grid, ncol = K_gmm_time)
lower_gmm_time <- matrix( Inf, nrow = n_grid, ncol = K_gmm_time)

for (k in seq_len(K_gmm_time)) {
  c_sq <- -2 * (lambda_gmm_time - log(pi_hat_time[k]) + 0.5 * (n_pc_time * log(2 * pi) + log_det_time[k]))
  if (is.finite(c_sq) && c_sq > 0) {
    Sigma_k <- get_sigma_k_time(k)
    mid   <- as.numeric(mean_time + Phi_time %*% mu_hat_time[, k])
    width <- sqrt(c_sq * diag(Phi_time %*% Sigma_k %*% t(Phi_time)))
    upper_gmm_time[, k] <- mid + width
    lower_gmm_time[, k] <- mid - width
  }
}

active_k_time <- which(apply(upper_gmm_time, 2, function(x) any(is.finite(x))))
best_k_gmm_time <- apply(scores_calib_time, 1, function(xi) {
  which.max(sapply(seq_len(K_gmm_time), function(k) {
    d <- xi - mu_hat_time[, k]
    maha <- as.numeric(t(d) %*% sigma_inv_time[[k]] %*% d)
    log(pi_hat_time[k]) - 0.5 * (n_pc_time * log(2 * pi) + log_det_time[k] + maha)
  }))
})

outlier_idx_gmm_time <- which(scores_conf_gmm_time < lambda_gmm_time)
gmm_colors_time <- c("purple", "darkorange", "cyan")[1:length(active_k_time)]

par(mfrow = c(1, length(active_k_time)))
for (k in active_k_time) {
  idx_k <- which(best_k_gmm_time == k)
  out_k <- intersect(idx_k, outlier_idx_gmm_time)
  inl_k <- setdiff(idx_k, outlier_idx_gmm_time)
  
  ylim_k <- range(c(upper_gmm_time[, k], lower_gmm_time[, k], matrix_calib_raw[idx_k, ]), na.rm = TRUE)
  plot(grid_min, upper_gmm_time[, k], type = "n", ylim = ylim_k,
       main = sprintf("Time-Domain GMM Cluster %d\n%.0f%% Band", k, BAND_COVERAGE * 100), xlab = "Minute", ylab = "HR")
  
  for (i in inl_k) lines(grid_min, matrix_calib_raw[i, ], col = alpha(gmm_colors_time[k], 0.3), lwd = 1)
  for (i in out_k) lines(grid_min, matrix_calib_raw[i, ], col = "firebrick", lwd = 2)
  
  polygon(c(grid_min, rev(grid_min)), c(upper_gmm_time[, k], rev(lower_gmm_time[, k])), col = alpha(gmm_colors_time[k], 0.2), border = NA)
  lines(grid_min, upper_gmm_time[, k], col = gmm_colors_time[k], lwd = 2, lty = 2)
  lines(grid_min, lower_gmm_time[, k], col = gmm_colors_time[k], lwd = 2, lty = 2)
  lines(grid_min, as.numeric(mean_time + Phi_time %*% mu_hat_time[, k]), col = "black", lwd = 2)
}
par(mfrow = c(1, 1))

# STRATEGY 2: K-MEANS CLUSTERING (TIME-DOMAIN) -----------------------------
cat("\nTIME-DOMAIN: K-MEANS CONFORMAL PREDICTION BANDS\n")

max_k_eval  <- min(MAX_K, nrow(scores_train_time) - 1)
k_range_wss <- 1:max_k_eval   # WSS can be calculated starting from 1 cluster
k_range_sil <- 2:max_k_eval   # Silhouette strictly requires at least 2 clusters

wss_time <- sapply(k_range_wss, function(k) kmeans(scores_train_time, centers = k, nstart = 25)$tot.withinss)
sil_width_time <- sapply(k_range_sil, function(k) mean(silhouette(kmeans(scores_train_time, centers = k, nstart = 25)$cluster, dist(scores_train_time))[, 3]))

# We restrict the optimal K choice between 2 and 3 for interpretability
valid_indices_time <- which(k_range_sil %in% 2:3)
optimal_k_time <- if (!is.null(K_MANUAL)) as.integer(K_MANUAL) else k_range_sil[valid_indices_time[which.max(sil_width_time[valid_indices_time])]]

p_sil_time <- ggplot(data.frame(k = k_range_sil, sil = sil_width_time), aes(x = k, y = sil)) +
  geom_line(color = "steelblue", linewidth = 1) + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k_time, linetype = "dashed", color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = k_range_sil) +
  labs(title = "Time-Domain Silhouette", subtitle = "Selection restricted to [2, 3]", y = "Average width", x = "k") + theme_minimal()

p_elbow_time <- ggplot(data.frame(k = k_range_wss, wss = wss_time), aes(x = k, y = wss)) +
  geom_line(color = "steelblue", linewidth = 1) + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k_time, linetype = "dashed", color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = k_range_wss) +
  labs(title = "Time-Domain Elbow", subtitle = "WSS evaluated from 1 to 6", y = "Within SS", x = "k") + theme_minimal()

print(p_sil_time + p_elbow_time)

kmeans_train_time  <- kmeans(scores_train_time, centers = optimal_k_time, nstart = 50)
centers_train_time <- kmeans_train_time$centers

# Conformity scores (negative distance to closest centroid)
scores_conf_km_time <- apply(scores_calib_time, 1, function(xi) -min(rowSums(sweep(centers_train_time, 2, xi)^2)))
best_k_km_time <- apply(scores_calib_time, 1, function(xi) which.min(rowSums(sweep(centers_train_time, 2, xi)^2)))

lambda_km_time <- quantile(scores_conf_km_time, alpha_cp)
r_sq_time      <- -lambda_km_time

upper_km_time <- matrix(-Inf, nrow = n_grid, ncol = optimal_k_time)
lower_km_time <- matrix( Inf, nrow = n_grid, ncol = optimal_k_time)

for (k in seq_len(optimal_k_time)) {
  mid   <- as.numeric(mean_time + Phi_time %*% centers_train_time[k, ])
  width <- sqrt(r_sq_time) * sqrt(rowSums(Phi_time^2))
  upper_km_time[, k] <- mid + width
  lower_km_time[, k] <- mid - width
}

outlier_idx_km_time <- which(scores_conf_km_time < lambda_km_time)
km_colors_time <- c("seagreen", "indianred", "dodgerblue")[1:optimal_k_time]

par(mfrow = c(1, optimal_k_time))
for (k in seq_len(optimal_k_time)) {
  idx_k <- which(best_k_km_time == k)
  out_k <- intersect(idx_k, outlier_idx_km_time)
  inl_k <- setdiff(idx_k, outlier_idx_km_time)
  
  ylim_k <- range(c(upper_km_time[, k], lower_km_time[, k], matrix_calib_raw[idx_k, ]), na.rm = TRUE)
  plot(grid_min, upper_km_time[, k], type = "n", ylim = ylim_k,
       main = sprintf("Time-Domain K-Means Cluster %d\n%.0f%% Band", k, BAND_COVERAGE * 100), xlab = "Minute", ylab = "HR")
  
  for (i in inl_k) lines(grid_min, matrix_calib_raw[i, ], col = alpha(km_colors_time[k], 0.3), lwd = 1)
  for (i in out_k) lines(grid_min, matrix_calib_raw[i, ], col = "firebrick", lwd = 2)
  
  polygon(c(grid_min, rev(grid_min)), c(upper_km_time[, k], rev(lower_km_time[, k])), col = alpha(km_colors_time[k], 0.2), border = NA)
  lines(grid_min, upper_km_time[, k], col = km_colors_time[k], lwd = 2, lty = 2)
  lines(grid_min, lower_km_time[, k], col = km_colors_time[k], lwd = 2, lty = 2)
  lines(grid_min, as.numeric(mean_time + Phi_time %*% centers_train_time[k, ]), col = "black", lwd = 2)
}
par(mfrow = c(1, 1))

# Final Combined Time-Domain Plot
plot_combined <- function(mat, labels, title_txt, pal, sub_txt) {
  df <- as.data.frame(mat)
  colnames(df) <- grid_min
  df$DayID <- seq_len(nrow(df))
  df$Cluster <- factor(labels)
  df_long <- pivot_longer(df, cols = -c(DayID, Cluster), names_to = "Minute", values_to = "HR") %>% mutate(Minute = as.numeric(Minute))
  
  ggplot(df_long, aes(x = Minute, y = HR, group = DayID, color = Cluster)) +
    geom_line(alpha = 0.65, linewidth = 0.4) +
    scale_x_continuous(breaks = seq(0, 1440, by = 180), labels = seq(0, 1440, by = 180)) +
    scale_color_manual(values = pal) +
    labs(title = title_txt, subtitle = sub_txt, x = "Minute of the Day (0-1440)", y = "Heart Rate (bpm)") +
    theme_minimal(base_size = 14) + theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
}

p_td_gmm <- plot_combined(matrix_calib_raw, best_k_gmm_time, "Time-Domain GMM Clustering", gmm_colors_time, "Clusters driven by phase variation (WHEN spikes occur)")
p_td_km  <- plot_combined(matrix_calib_raw, best_k_km_time, "Time-Domain K-Means Clustering", km_colors_time, "Clusters driven by phase variation (WHEN spikes occur)")
grid.arrange(p_td_gmm, p_td_km, ncol = 1)

# CONCLUSION PHASE A: Because workouts happen at different times, the models clustered
# days based on the 'time' of exercise, not the overall physical intensity. 
# This problem is known as Phase Variation and didn't allow us to correctly compute both 
# clusters and bands.
# The solution can be to align the amplitude using Quantiles.

# PHASE B: EXACT ANALYSIS ON QUANTILE-TRANSFORMED CURVES----------

# By applying a monotone rearrangement (sorting the heart rates descendingly), 
# we transform the domain from "Time of Day" to "Duration spent at or above a given HR".
# This completely eliminates Phase Variation. Distances in this functional space 
# strictly represent the difference in overall daily intensity amplitude.

# Same algorithms will be applied, only thing changing are the functions with which 
# the algorithms work.

cat("PHASE B: EXACT PIPELINE ON QUANTILE-TRANSFORMED CURVES\n")

# Transform training and calibration matrices independently to avoid data leakage
matrix_train_quant <- t(apply(matrix_train_raw, 1, sort, decreasing = TRUE))
matrix_calib_quant <- t(apply(matrix_calib_raw, 1, sort, decreasing = TRUE))

# Visualizing transformed quantile curves
plot_quant_df <- as.data.frame(matrix_train_quant) %>%
  mutate(DayID = row_number()) %>%
  pivot_longer(-DayID, names_to = "TimeIndex", values_to = "HR") %>%
  mutate(TimeIndex = as.numeric(str_remove(TimeIndex, "V")))

p_transformed <- ggplot(plot_quant_df, aes(x = TimeIndex, y = HR, group = DayID)) +
  geom_line(alpha = 0.2, color = "purple") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 1.2) +
  labs(title = "Transformed Curves (Functional Quantiles)",
       subtitle = "Descending HR sorted. Phase variation perfectly removed.",
       x = "Duration (minutes)", y = "Heart Rate (bpm)") +
  theme_minimal(base_size = 14)
print(p_transformed)

# 1. Global mean estimated ONLY on TRAIN
mean_quant <- colMeans(matrix_train_quant, na.rm = TRUE)

# 2. Centering 
train_cent_quant <- sweep(matrix_train_quant, 2, mean_quant, "-")
calib_cent_quant <- sweep(matrix_calib_quant, 2, mean_quant, "-")

# 3. FPCA fitted ONLY on TRAIN
fpca_quant <- prcomp(train_cent_quant, center = FALSE, scale. = FALSE)
eigenvecs_quant <- fpca_quant$rotation          
cum_var_quant   <- cumsum(fpca_quant$sdev^2) / sum(fpca_quant$sdev^2)

n_pc_quant <- which(cum_var_quant >= VAR_THRESHOLD)[1]
if (is.na(n_pc_quant)) n_pc_quant <- min(6, ncol(eigenvecs_quant))

scores_train_quant <- fpca_quant$x[, 1:n_pc_quant, drop = FALSE]
scores_calib_quant <- calib_cent_quant %*% eigenvecs_quant[, 1:n_pc_quant, drop = FALSE]
Phi_quant <- eigenvecs_quant[, 1:n_pc_quant, drop = FALSE]

cat(sprintf("Selected QUANTILE-DOMAIN PCs: %d | Explained variance: %.1f%%\n", n_pc_quant, cum_var_quant[n_pc_quant] * 100))

# Quantile-Domain Scree Plot
var_df_quant <- data.frame(
  PC     = seq_along(cum_var_quant),
  VarExp = (fpca_quant$sdev^2) / sum(fpca_quant$sdev^2) * 100,
  CumVar = cum_var_quant * 100
)

p_scree_quant <- ggplot(var_df_quant[1:min(12, nrow(var_df_quant)), ], aes(x = PC)) +
  geom_col(aes(y = VarExp), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = CumVar), color = "firebrick", linewidth = 1) +
  geom_point(aes(y = CumVar), color = "firebrick", size = 2.5) +
  geom_hline(yintercept = VAR_THRESHOLD * 100, linetype = "dashed", color = "firebrick", alpha = 0.6) +
  geom_vline(xintercept = n_pc_quant, linetype = "dotted", color = "navy", linewidth = 0.9) +
  scale_y_continuous(name = "Explained variance (%)", sec.axis = sec_axis(~., name = "Cumulative variance (%)")) +
  labs(title = "Quantile-Domain Scree Plot (fit on train)", x = "Principal component") +
  theme_minimal(base_size = 13)
print(p_scree_quant)

# STRATEGY 1: GMM CLUSTERING (QUANTILE-DOMAIN) -----------------------------
cat("\nQUANTILE-DOMAIN: GMM CONFORMAL PREDICTION BANDS\n")

# We evaluate the BIC for G = 1 to MAX_K (6) to visualize the full trend
bic_quant <- mclustBIC(scores_train_quant, G = 1:MAX_K, modelNames = gmm_modeltype)
plot(bic_quant, main = "Quantile-Domain GMM BIC (1 to 6 Clusters)")

# Restrict the final model choice to G=2 or G=3 for physiological interpretability
# (Rest vs Sport / Rest, Morning, Afternoon workouts)
gmm_fit_quant <- Mclust(scores_train_quant, G = 2:3, modelNames = gmm_modeltype, verbose = FALSE)

# Safety fallback if the full covariance matrix (VVV) is unstable
if (is.null(gmm_fit_quant) || is.null(gmm_fit_quant$G)) {
  cat("VVV model unstable. Switching to VVI (Diagonal covariance)...\n")
  gmm_fit_quant <- Mclust(scores_train_quant, G = 2:3, modelNames = "VVI", verbose = FALSE)
}

K_gmm_quant  <- gmm_fit_quant$G
pi_hat_quant <- gmm_fit_quant$parameters$pro
mu_hat_quant <- gmm_fit_quant$parameters$mean   

get_sigma_k_quant <- function(k) {
  S <- gmm_fit_quant$parameters$variance$sigma[,, k]
  if (DIAGONAL_GMM) diag(diag(S)) else S
}

sigma_inv_quant <- lapply(seq_len(K_gmm_quant), function(k) solve(get_sigma_k_quant(k)))
log_det_quant   <- sapply(seq_len(K_gmm_quant), function(k) as.numeric(determinant(get_sigma_k_quant(k), logarithm = TRUE)$modulus))

# Conformity scores (log-scale max-component)
scores_conf_gmm_quant <- numeric(n_calib)
for (i in seq_len(n_calib)) {
  xi <- as.numeric(scores_calib_quant[i, ])
  vals <- sapply(seq_len(K_gmm_quant), function(k) {
    d <- xi - mu_hat_quant[, k]
    maha <- as.numeric(t(d) %*% sigma_inv_quant[[k]] %*% d)
    log(pi_hat_quant[k]) - 0.5 * (n_pc_quant * log(2 * pi) + log_det_quant[k] + maha)
  })
  scores_conf_gmm_quant[i] <- max(vals)
}

lambda_gmm_quant <- quantile(scores_conf_gmm_quant, alpha_cp)

upper_gmm_quant <- matrix(-Inf, nrow = n_grid, ncol = K_gmm_quant)
lower_gmm_quant <- matrix( Inf, nrow = n_grid, ncol = K_gmm_quant)

for (k in seq_len(K_gmm_quant)) {
  c_sq <- -2 * (lambda_gmm_quant - log(pi_hat_quant[k]) + 0.5 * (n_pc_quant * log(2 * pi) + log_det_quant[k]))
  if (is.finite(c_sq) && c_sq > 0) {
    Sigma_k <- get_sigma_k_quant(k)
    mid   <- as.numeric(mean_quant + Phi_quant %*% mu_hat_quant[, k])
    width <- sqrt(c_sq * diag(Phi_quant %*% Sigma_k %*% t(Phi_quant)))
    upper_gmm_quant[, k] <- mid + width
    lower_gmm_quant[, k] <- mid - width
  }
}

active_k_quant <- which(apply(upper_gmm_quant, 2, function(x) any(is.finite(x))))
best_k_gmm_quant <- apply(scores_calib_quant, 1, function(xi) {
  which.max(sapply(seq_len(K_gmm_quant), function(k) {
    d <- xi - mu_hat_quant[, k]
    maha <- as.numeric(t(d) %*% sigma_inv_quant[[k]] %*% d)
    log(pi_hat_quant[k]) - 0.5 * (n_pc_quant * log(2 * pi) + log_det_quant[k] + maha)
  }))
})

outlier_idx_gmm_quant <- which(scores_conf_gmm_quant < lambda_gmm_quant)
gmm_colors_quant <- scales::hue_pal()(length(active_k_quant))

par(mfrow = c(1, length(active_k_quant)))
for (k in active_k_quant) {
  idx_k <- which(best_k_gmm_quant == k)
  out_k <- intersect(idx_k, outlier_idx_gmm_quant)
  inl_k <- setdiff(idx_k, outlier_idx_gmm_quant)
  
  ylim_k <- range(c(upper_gmm_quant[, k], lower_gmm_quant[, k], matrix_calib_quant[idx_k, ]), na.rm = TRUE)
  plot(grid_min, upper_gmm_quant[, k], type = "n", ylim = ylim_k,
       main = sprintf("Quantile GMM Cluster %d\n%.0f%% Band", k, BAND_COVERAGE * 100), xlab = "Duration", ylab = "Sorted HR")
  
  for (i in inl_k) lines(grid_min, matrix_calib_quant[i, ], col = alpha(gmm_colors_quant[k], 0.3), lwd = 1)
  for (i in out_k) lines(grid_min, matrix_calib_quant[i, ], col = "firebrick", lwd = 2)
  
  polygon(c(grid_min, rev(grid_min)), c(upper_gmm_quant[, k], rev(lower_gmm_quant[, k])), col = alpha(gmm_colors_quant[k], 0.2), border = NA)
  lines(grid_min, upper_gmm_quant[, k], col = gmm_colors_quant[k], lwd = 2, lty = 2)
  lines(grid_min, lower_gmm_quant[, k], col = gmm_colors_quant[k], lwd = 2, lty = 2)
  lines(grid_min, as.numeric(mean_quant + Phi_quant %*% mu_hat_quant[, k]), col = "black", lwd = 2)
}
par(mfrow = c(1, 1))

# STRATEGY 2: K-MEANS CLUSTERING (QUANTILE-DOMAIN) -------------------------
cat("\nQUANTILE-DOMAIN: K-MEANS CONFORMAL PREDICTION BANDS\n")

wss_quant <- sapply(k_range_wss, function(k) kmeans(scores_train_quant, centers = k, nstart = 25)$tot.withinss)
sil_width_quant <- sapply(k_range_sil, function(k) mean(silhouette(kmeans(scores_train_quant, centers = k, nstart = 25)$cluster, dist(scores_train_quant))[, 3]))

# We restrict the optimal K choice between 2 and 3 for interpretability:
# 2 clusters: Rest vs Sport
# 3 clusters: Rest, Morning Workout, Afternoon Workout (or continuous mix)
valid_indices_quant <- which(k_range_sil %in% 2:3)
optimal_k_quant <- if (!is.null(K_MANUAL)) as.integer(K_MANUAL) else k_range_sil[valid_indices_quant[which.max(sil_width_quant[valid_indices_quant])]]

p_sil_quant <- ggplot(data.frame(k = k_range_sil, sil = sil_width_quant), aes(x = k, y = sil)) +
  geom_line(color = "steelblue", linewidth = 1) + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k_quant, linetype = "dashed", color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = k_range_sil) +
  labs(title = "Quantile-Domain Silhouette", subtitle = "Selection restricted to [2, 3]", y = "Average width", x = "k") + theme_minimal()

p_elbow_quant <- ggplot(data.frame(k = k_range_wss, wss = wss_quant), aes(x = k, y = wss)) +
  geom_line(color = "steelblue", linewidth = 1) + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k_quant, linetype = "dashed", color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = k_range_wss) +
  labs(title = "Quantile-Domain Elbow", subtitle = "WSS evaluated from 1 to 6", y = "Within SS", x = "k") + theme_minimal()

print(p_sil_quant + p_elbow_quant)

kmeans_train_quant  <- kmeans(scores_train_quant, centers = optimal_k_quant, nstart = 50)
centers_train_quant <- kmeans_train_quant$centers

# Conformity scores (negative distance to closest centroid)
scores_conf_km_quant <- apply(scores_calib_quant, 1, function(xi) -min(rowSums(sweep(centers_train_quant, 2, xi)^2)))
best_k_km_quant <- apply(scores_calib_quant, 1, function(xi) which.min(rowSums(sweep(centers_train_quant, 2, xi)^2)))

lambda_km_quant <- quantile(scores_conf_km_quant, alpha_cp)
r_sq_quant      <- -lambda_km_quant

upper_km_quant <- matrix(-Inf, nrow = n_grid, ncol = optimal_k_quant)
lower_km_quant <- matrix( Inf, nrow = n_grid, ncol = optimal_k_quant)

for (k in seq_len(optimal_k_quant)) {
  mid   <- as.numeric(mean_quant + Phi_quant %*% centers_train_quant[k, ])
  width <- sqrt(r_sq_quant) * sqrt(rowSums(Phi_quant^2))
  upper_km_quant[, k] <- mid + width
  lower_km_quant[, k] <- mid - width
}

outlier_idx_km_quant <- which(scores_conf_km_quant < lambda_km_quant)
km_colors_quant <- scales::hue_pal()(optimal_k_quant)

par(mfrow = c(1, optimal_k_quant))
for (k in seq_len(optimal_k_quant)) {
  idx_k <- which(best_k_km_quant == k)
  out_k <- intersect(idx_k, outlier_idx_km_quant)
  inl_k <- setdiff(idx_k, outlier_idx_km_quant)
  
  ylim_k <- range(c(upper_km_quant[, k], lower_km_quant[, k], matrix_calib_quant[idx_k, ]), na.rm = TRUE)
  plot(grid_min, upper_km_quant[, k], type = "n", ylim = ylim_k,
       main = sprintf("Quantile K-Means Cluster %d\n%.0f%% Band", k, BAND_COVERAGE * 100), xlab = "Duration", ylab = "Sorted HR")
  
  for (i in inl_k) lines(grid_min, matrix_calib_quant[i, ], col = alpha(km_colors_quant[k], 0.3), lwd = 1)
  for (i in out_k) lines(grid_min, matrix_calib_quant[i, ], col = "firebrick", lwd = 2)
  
  polygon(c(grid_min, rev(grid_min)), c(upper_km_quant[, k], rev(lower_km_quant[, k])), col = alpha(km_colors_quant[k], 0.2), border = NA)
  lines(grid_min, upper_km_quant[, k], col = km_colors_quant[k], lwd = 2, lty = 2)
  lines(grid_min, lower_km_quant[, k], col = km_colors_quant[k], lwd = 2, lty = 2)
  lines(grid_min, as.numeric(mean_quant + Phi_quant %*% centers_train_quant[k, ]), col = "black", lwd = 2)
}
par(mfrow = c(1, 1))

# Final Combined Quantile-Domain Plot mapped back to the original curves
p_quant_gmm <- plot_combined(matrix_calib_raw, best_k_gmm_quant, "Quantile GMM Mapped to Original Curves", gmm_colors_quant, "Bands strictly cluster by daily physical intensity amplitude")
p_quant_km  <- plot_combined(matrix_calib_raw, best_k_km_quant, "Quantile K-Means Mapped to Original Curves", km_colors_quant, "Bands strictly cluster by daily physical intensity amplitude")
grid.arrange(p_quant_gmm, p_quant_km, ncol = 1)

# Out of all the tried models (Time-Domain and Quantiles, GMM and KMeans combinations),
# the GMM performed on the Quantiles functions seems to be the best one by far,
# reflecting the fact that functional analysis on biological data with no transformations
# apllied rarely returns significant results. 