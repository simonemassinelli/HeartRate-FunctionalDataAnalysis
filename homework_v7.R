setwd("C:/Users/casam/OneDrive/Desktop/Simone/Stat_Brutti/Homework")
#install.packages(c("tidyverse", "lubridate", "gridExtra", "splines", "fda", 
#                   "cluster", "patchwork", "ggdendro", "mclust"))
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

# Dataset upload and feature engineering ----------------------------------

file_name <- "high_quality_data_filled.csv"

df <- read_csv(file_name, show_col_types = FALSE) %>%
  mutate(
    datetime      = make_datetime(Year, Month, Day, Hour, Minute, tz = "UTC"),
    date          = as_date(datetime),
    minute_of_day = as.numeric(difftime(datetime, floor_date(datetime, "day"), units = "mins"))
  ) %>%
  arrange(date, datetime) %>%
  mutate(day_id = as.integer(factor(date)))

View(df)

# Data Cleaning (Quantity and physiological range filters) ----------------

#We did a quality check to ensure only reliable days are included in our Functional Data Analysis:
#Quantitative Threshold (n>= 240): we choose to discard any day with fewer than 240 observations. 
#Physiological Validation: if a single observation falls outside the physiological range (30-200 Heart-Rate), 
#the entire day is excluded.

df <- df %>%
  group_by(date) %>%
  filter(
    n() >= 240,                                           
    all(HeartRate >= 30 & HeartRate <= 200, na.rm = TRUE) 
  ) %>%
  ungroup()

cat("Cleaning Summary:\n")
cat("Final valid days remaining:", length(unique(df$date)), "\n")

# Exploratory Data Analysis -----------------------------------------------

# Conversion of categorical features
df <- df %>%
  mutate(
    MotionContext = factor(MotionContext),
    SleepStatus   = factor(SleepStatus),
    SleepStage    = factor(SleepStage)
  )

# Summary heart rate both in general and considering different situations 

cat("\nSummary Heart Rate\n")
print(summary(df$HeartRate))

cat("\nHeart Rate per SleepStage:\n")
print(tapply(df$HeartRate, df$SleepStage, summary))
#195 while sleeping.... what a nightmare...Or were you busy with... something else???

cat("\nHeart Rate per SleepStatus:\n")
print(tapply(df$HeartRate, df$SleepStatus, summary))

cat("\nHeart Rate per MotionContext:\n")
print(tapply(df$HeartRate, df$MotionContext, summary))

#Heart rate distribution visualization (...Having some fun with ggplot...)
p_hist <- ggplot(df, aes(x = HeartRate)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(df$HeartRate, na.rm = TRUE),
             colour = "firebrick", linewidth = 0.8, linetype = "dashed") +
  annotate("text",
           x     = median(df$HeartRate, na.rm = TRUE) + 1,
           y     = Inf, vjust = 1.5, hjust = 0, size = 3,
           label = paste0("Median = ", round(median(df$HeartRate, na.rm = TRUE), 1))) +
  labs(title = "Distribution Heart Rate", x = "HR (bpm)", y = "Count") +
  theme_minimal()
#From the histogram we can see that the heart rate distribution is right-skewed with a median of 85 BPM. 
#While most observations fall within the 60-100 BPM range (resting state), 
#the long right tail represents high-intensity physiological spikes during sport activity

p_box_stage <- ggplot(df %>% filter(!is.na(SleepStage)),
                      aes(x = SleepStage, y = HeartRate, fill = SleepStage)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "HR per Sleep Stage", x = "Sleep Stage", y = "HR (bpm)") +
  theme_minimal() + theme(legend.position = "none")

p_box_status <- ggplot(df %>% filter(!is.na(SleepStatus)),
                       aes(x = SleepStatus, y = HeartRate, fill = SleepStatus)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "HR per Sleep Status", x = "Sleep Status", y = "HR (bpm)") +
  theme_minimal() + theme(legend.position = "none")

#Sleep analysis shows significantly lower and more stable HR during 
#deeper stages (2,3) compared to 'Awake' states (0,1). 
#The reduced variance in sleep boxplots reflects HR stability during rest, 
#whereas the 'Awake' status introduces higher variability and elevated heart rates.

p_box_motion <- ggplot(df %>% filter(!is.na(MotionContext)),
                       aes(x = MotionContext, y = HeartRate, fill = MotionContext)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "HR per Motion Context", x = "Motion Context", y = "HR (bpm)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

#The motion context boxplot reveals a clear correlation between physical movement 
#and heart rate. HR levels clearly increase moving from sedentary positions (0)
#to active motion(2), with 'Running' or 'Walking' showing the highest medians and the widest interquartile ranges.
#This boxplot also reveals significant high-intensity outliers within the '0' (Sedentary) category. These points indicate 
#heart rate spikes (reaching 150-180 BPM) occurring in the absence of physical movement. This situation suggests events such 
#as isometric exercise, high emotional stress, or post-workout recovery , where the heart rate remains elevated while the body is stationary

grid.arrange(p_hist, p_box_stage, p_box_status, p_box_motion, ncol = 2)

# Daily HR heatmap
df_heat <- df %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  group_by(day_id, date, time_bin) %>%
  summarise(avg_HR = mean(HeartRate, na.rm = TRUE), .groups = "drop")

p_heatmap <- ggplot(df_heat,
                    aes(x = time_bin, y = factor(day_id), fill = avg_HR)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#313695", "#ffffbf", "#a50026"), name = "HR") +
  scale_x_continuous(breaks = seq(0, 1440, 180),
                     labels = seq(0, 1440, 180)) +
  labs(title = "Daily HR Heatmap",
       x = "Minute of the day", y = "Day ID") +
  theme_minimal()

print(p_heatmap)
#The Daily HR Heatmap illustrates the longitudinal heart rate patterns across the study period. 
#A consistent blue band in the early hours (0-480 min) shows the obvious low HR during sleep. 
#Daytime periods show higher variability, with distinct red points in the late afternoon (900-1200 min) 
#identifying high-intensity exercise sessions. This visualization confirms a stable circadian rhythm while 
#highlighting sporadic physical activities as the main functional deviations.

# Motion Context through the day plot

df_motion_time <- df %>%
  filter(!is.na(MotionContext)) %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  dplyr::count(time_bin, MotionContext) %>% 
  group_by(time_bin) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_motion_time <- ggplot(df_motion_time,
                        aes(x = time_bin, y = prop, fill = as.factor(MotionContext))) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 1440, 180),
                     labels = seq(0, 1440, 180)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Motion Context Evolution Through the Day",
       subtitle = "Proportion of physical activity levels across 1440 minutes",
       x = "Minutes of the day", y = "Proportion", fill = "Motion Type") +
  theme_minimal()

print(p_motion_time)
#With this plot we can visualize the distribution of movement intensities 
#throughout the 24-hour cycle. By normalizing the counts into proportions,
#the plot effectively shows the dominance of sedentary states (Motion 0) 
#during the night and early morning, followed by the emergence of diverse 
#physical activities during daylight hours. This temporal profile provides 
#context for the heart rate spikes observed in the previous analysis, 
#confirming that high-intensity HR observations align with the periods where the subject is more physically active.

# OPTIMIZED B-SPLINE PARAMETERS (Penalized Least Squares) -----------------

# Parameters
rangeval  <- c(0, 1440)
grid_min  <- seq(0, 1439, 1)

days_list <- df %>% 
  group_by(date) %>% 
  group_split()

# GCV calculation function
compute_avg_gcv <- function(K, lambda_val) {
  basis_obj <- create.bspline.basis(rangeval = rangeval, nbasis = K, norder = 4)
  fdPar_obj <- fdPar(basis_obj, Lfdobj = 2, lambda = lambda_val)
  
  gcv_vec <- numeric(length(days_list))
  valid   <- 0
  
  for (dati in days_list) {
    if (nrow(dati) < 30 || sd(dati$HeartRate, na.rm = TRUE) < 2) next
    
    sm <- tryCatch(
      smooth.basis(argvals  = dati$minute_of_day,
                   y        = dati$HeartRate,
                   fdParobj = fdPar_obj),
      error = function(e) NULL
    )
    
    if (!is.null(sm) && length(sm$gcv) > 0) {
      valid <- valid + 1
      gcv_vec[valid] <- mean(sm$gcv, na.rm = TRUE)
    }
  }
  if (valid == 0) return(Inf)
  mean(gcv_vec[1:valid])
}

# GRID SEARCH 2D ----------------------------------------------------------
# (Commented out to save computation time)

# k_values    <- seq(30, 200, 5)  
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
#   geom_line(color = "steelblue", linewidth = 1.1) + geom_point(color = "red", size = 3) +
#   geom_vline(xintercept = best_K, color = "red", linetype = "dashed", linewidth = 1) +
#   labs(title = "GCV vs Number of Basis Functions (K)", subtitle = paste("Optimal K =", best_K),
#        x = "K (nbasis)", y = "Average GCV") + theme_minimal(base_size = 14) +
#   theme(plot.title = element_text(face = "bold"))
#
# idx_k <- which(k_values == best_K)
# gcv_vs_lam <- gcv_mat[idx_k, ]
# p_gcv_lam <- ggplot(data.frame(Lambda = lambda_vals, GCV = gcv_vs_lam), 
#                     aes(log10(Lambda), GCV)) +
#   geom_line(color = "darkgreen", linewidth = 1.1) + geom_point(color = "red", size = 3) +
#   geom_vline(xintercept = log10(best_lambda), color = "red", linetype = "dashed", linewidth = 1) +
#   labs(title = "GCV vs Smoothing Penalty (λ)", subtitle = paste("Optimal log10(λ) =", round(log10(best_lambda), 2)),
#        x = "log10(λ)", y = "Average GCV") + theme_minimal(base_size = 14) +
#   theme(plot.title = element_text(face = "bold"))
#
# grid.arrange(p_gcv_k, p_gcv_lam, ncol = 2)

# Grid search results can be unstable, so we fix K = 50. 
# This roughly fits a degree 3 polynomial for every ~30 minutes, keeping things simple.

# 1D OPTIMIZATION FOR LAMBDA (Fixed K = 50) -------------------------------

best_K <- 50
lambda_vals <- 10^seq(-4, 4, length.out = 50)
gcv_scores  <- numeric(length(lambda_vals))

for (i in seq_along(lambda_vals)) {
  gcv_scores[i] <- compute_avg_gcv(K = best_K, lambda_val = lambda_vals[i])
}

best_lambda <- lambda_vals[which.min(gcv_scores)]
cat(sprintf("Optimal lambda for K=50 is: %.2e\n", best_lambda))

# FINAL SMOOTHING OBJECT --------------------------------------------------

final_basis <- create.bspline.basis(rangeval = rangeval, nbasis = best_K, norder = 4)
final_fdPar <- fdPar(final_basis, Lfdobj = 2, lambda = best_lambda)

# Matrix for evaluated curves and valid dates
daily_matrix <- matrix(NA_real_, nrow = length(days_list), ncol = length(grid_min))
good_dates   <- character(length(days_list))
row_i <- 1

# Apply smoothing day by day
for (dati in days_list) {
  date_str <- as.character(dati$date[1])
  
  # Skip days with too few observations
  if (nrow(dati) < 30) next
  
  # Fit smoothing spline to raw data
  sm <- tryCatch(
    smooth.basis(argvals = dati$minute_of_day, 
                 y       = dati$HeartRate, 
                 fdParobj = final_fdPar), 
    error = function(e) NULL
  )
  
  if (is.null(sm)) next
  
  # Evaluate curve on 1-min grid
  hr_grid <- as.vector(eval.fd(grid_min, sm$fd))
  
  # Filter out physiological anomalies and boundary artifacts
  if (all(hr_grid >= 35 & hr_grid <= 215, na.rm = TRUE)) {
    daily_matrix[row_i, ] <- hr_grid
    good_dates[row_i]     <- date_str
    row_i <- row_i + 1
  }
}

# Remove unused rows from the matrix
if (row_i > 1) {
  daily_matrix <- daily_matrix[1:(row_i - 1), , drop = FALSE]
  good_dates   <- good_dates[1:(row_i - 1)]
} else {
  stop("No valid days remained.")
}

cat("\nFinal clean matrix:", nrow(daily_matrix), "days\n")

# Convert clean matrix to fd object for FPCA/clustering
fd_obj <- Data2fd(argvals  = grid_min, 
                  y        = t(daily_matrix), 
                  basisobj = final_basis)

# VISUALIZATION OF DAILY FUNCTIONS ----------------------------------------

plot(fd_obj, lty = 1, col = 'blue',
     main = "Optimized Daily HR Curves (B-spline + GCV)",
     xlab = "Minute of the Day", ylab = "Heart Rate (bpm)")

# CONFORMAL PREDICTION (PSEUDO-DENSITY & ALPHA ANALYSIS) ------------------

# Compute pairwise L2 distances on the smoothed curves.
# Evaluating fd_obj on the grid yields continuous representations.
matrix_smooth <- t(eval.fd(grid_min, fd_obj))
dist_matrix   <- as.matrix(dist(matrix_smooth))

# Pseudo-density estimation (Kernel Density Estimation)
# h_param (bandwidth) defines the locality of the density measure
h_param <- quantile(dist_matrix, 0.15) 
kernel_gaussian <- function(u) { (1 / sqrt(2 * pi)) * exp(-0.5 * u^2) }

pseudo_density <- numeric(nrow(matrix_smooth))
for (i in seq_len(nrow(matrix_smooth))) {
  pseudo_density[i] <- mean(kernel_gaussian(dist_matrix[i, ] / h_param))
}

# Level sets for different alpha values (0.1, 0.5, 0.9)
# Theory: Alpha represents the error level (e.g., Alpha = 0.1 yields a 90% confidence set).
# Higher Alpha values restrict the set to the highest density ("typical") curves.

alpha_levels <- c(0.1, 0.5, 0.9)
par(mfrow = c(1, 3))

for (alpha in alpha_levels) {
  lambda_alpha <- quantile(pseudo_density, alpha)
  subset_idx <- which(pseudo_density >= lambda_alpha)
  
  if (length(subset_idx) > 1) {
    matplot(grid_min, t(matrix_smooth[subset_idx, ]), 
            type = "l", lty  = 1, col  = scales::alpha("grey40", 0.3), 
            ylim = range(matrix_smooth),
            main = paste("Alpha =", alpha, "\n(Confidence Set)"),
            ylab = "Heart Rate (bpm)", xlab = "Minute of the Day")
    
    lines(grid_min, colMeans(matrix_smooth[subset_idx, ]), col = "red", lwd = 2.5)
    mtext(paste("Days included:", length(subset_idx)), side = 3, line = -1.5, cex = 0.7)
  }
}

par(mfrow = c(1, 1))

# ANOMALY DETECTION (STATISTICAL CRITERION) -------------------------------

# Criterion: We define the 5% of observations with the lowest pseudo-density as outliers.
# This alpha_outlier (0.05) acts as our functional "p-value" for abnormality.
alpha_outlier  <- 0.05
density_cutoff <- quantile(pseudo_density, alpha_outlier)

# Identify the indices and count the outlier curves
outlier_indices <- which(pseudo_density <= density_cutoff)
n_outliers      <- length(outlier_indices)
outlier_dates   <- good_dates[outlier_indices] 

cat(sprintf("Calculated density cutoff: %.6f\n", density_cutoff))
cat(sprintf("Number of outliers detected (alpha=%.2f): %d\n", alpha_outlier, n_outliers))

outlier_colors <- rainbow(n_outliers)
plot_colors    <- rep(scales::alpha("grey", 0.3), length(good_dates))
plot_widths    <- rep(1, length(good_dates))

for (i in seq_len(n_outliers)) {
  plot_colors[outlier_indices[i]] <- outlier_colors[i]
  plot_widths[outlier_indices[i]] <- 3 
}

# Final outlier plot
par(mfrow = c(1, 1))

plot(fd_obj, 
     col  = plot_colors, 
     lwd  = plot_widths, 
     lty  = 1,
     main = paste("Detected Functional Anomalies (Alpha Threshold =", alpha_outlier, ")"),
     xlab = "Minute of the Day", 
     ylab = "Heart Rate (bpm)")

lines(mean.fd(fd_obj), col = "black", lwd = 3, lty = 1)

if (n_outliers <= 5) {
  legend_labels <- c("Normal Days", as.character(outlier_dates), "Global Mean")
  legend_colors <- c("grey", outlier_colors, "black")
  legend_lwds   <- c(1, rep(3, n_outliers), 3)
} else {
  legend_labels <- c("Normal Days", paste(n_outliers, "Outliers detected"), "Global Mean")
  legend_colors <- c("grey", "red", "black")
  legend_lwds   <- c(1, 3, 3)
}

legend("topleft", legend = legend_labels, col = legend_colors, 
       lwd = legend_lwds, lty = 1, bty = "n", cex = 0.6)

cat("\nOutlier Details\n")
print(data.frame(Date = outlier_dates, Density = pseudo_density[outlier_indices]))

# FUNCTIONAL COMPARISON: WEEKDAYS VS WEEKENDS -----------------------------

# Identify the type of day for each valid date
day_of_week <- weekdays(as.Date(good_dates)) 
is_weekend  <- day_of_week %in% c("Saturday", "Sunday", "sabato", "domenica") 

n_weekend  <- sum(is_weekend)
n_weekday  <- sum(!is_weekend)

cat(sprintf("Weekdays: %d | Weekends: %d\n", n_weekday, n_weekend))

# Setup colors and line widths
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
lines(mean_weekday, col = "blue", lwd = 3, lty = 2)

# Functional mean for weekends
mean_weekend <- mean.fd(fd_obj[which(is_weekend)])
lines(mean_weekend, col = "red", lwd = 3, lty = 2)

# Global functional mean
lines(mean.fd(fd_obj), col = "black", lwd = 3, lty = 1)

# Legend
legend("topleft",
       legend = c(
         paste0("Weekday (n=", n_weekday, ")"),
         paste0("Weekend (n=", n_weekend, ")"),
         "Weekday Mean",
         "Weekend Mean",
         "Global Mean"
       ),
       col = c(col_weekday, col_weekend, "blue", "red", "black"),
       lwd = c(1, 1.5, 3, 3, 3),
       lty = c(1, 1, 2, 2, 1),
       bty = "n",
       cex = 0.7)

# This view generally highlights afternoon sports during weekends 
# and different nightlife or sleep patterns.

# CONFIGURABLE PARAMETERS -------------------------------------------------

VAR_THRESHOLD   <- 0.95      # Cumulative variance for PC selection
N_PC_MANUAL     <- NULL      # NULL = auto
BAND_COVERAGE   <- 0.90
MAX_K           <- 4
K_MANUAL        <- NULL
N_SAMPLE_CURVES <- 43
SEED            <- 123
CENTERING       <- TRUE      # Global functional centering (recommended)
STANDARDIZE     <- FALSE     # Global functional standardization (divide by sd curve)

set.seed(SEED)

# SHARED PHASE: FPCA ON FUNCTIONAL QUANTILES ------------------------------

# 1. Evaluate the functional object fd_obj on the grid -> clean smoothed matrix
matrix_smooth <- t(eval.fd(grid_min, fd_obj))   # n_days x n_grid

# Monotone rearrangement (Functional Quantiles)
# Sort the heart rates of each day (row) in descending order.
matrix_smooth <- t(apply(matrix_smooth, 1, sort, decreasing = TRUE))

# Domain restriction
# We only keep the first 120 minutes (the most intense 2 hours of the day).
# This drops the remaining 21 resting hours, which would otherwise wash out the signal.
matrix_smooth <- matrix_smooth[, 10:130] 

# --- NEW: Visualizing transformed quantile curves ---
cat("\n--- Plotting Transformed Quantile Curves ---\n")
plot_quantiles_df <- as.data.frame(matrix_smooth) %>%
  mutate(DayID = row_number()) %>%
  pivot_longer(-DayID, names_to = "TimeIndex", values_to = "HR") %>%
  mutate(TimeIndex = as.numeric(str_remove(TimeIndex, "V")))

p_transformed <- ggplot(plot_quantiles_df, aes(x = TimeIndex, y = HR, group = DayID)) +
  geom_line(alpha = 0.2, color = "purple") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 1.2) +
  labs(title = "Transformed Curves (Functional Quantiles)",
       subtitle = "Descending HR sorted, restricted to minutes 10-130",
       x = "Duration (minutes)", y = "Heart Rate (bpm)") +
  theme_minimal(base_size = 14)
print(p_transformed)
# ----------------------------------------------------

# 2. Calculate global mean and sd (on the truncated curves)
global_mean <- colMeans(matrix_smooth, na.rm = TRUE)
global_sd   <- apply(matrix_smooth, 2, sd, na.rm = TRUE)
global_sd[global_sd < 1e-6] <- 1   

# 3. Apply the desired transformations
working_mat <- matrix_smooth
if (CENTERING) {
  working_mat <- sweep(working_mat, 2, global_mean, "-")
  cat("-> Global centering applied\n")
}
if (STANDARDIZE) {
  working_mat <- sweep(working_mat, 2, global_sd, "/")
  cat("-> Global standardization applied (divided by pointwise sd)\n")
}

# 4. Discrete FPCA on the processed matrix
fpca_res  <- prcomp(working_mat, center = FALSE, scale. = FALSE)
eigenvecs <- fpca_res$rotation
sdev_sq   <- fpca_res$sdev^2
cum_var   <- cumsum(sdev_sq) / sum(sdev_sq)

var_df <- data.frame(
  PC     = seq_along(cum_var),
  VarExp = sdev_sq / sum(sdev_sq) * 100,
  CumVar = cum_var * 100
)

# Principal Component selection
if (!is.null(N_PC_MANUAL)) {
  n_pc <- as.integer(N_PC_MANUAL)
} else {
  n_pc <- which(cum_var >= VAR_THRESHOLD)[1]
  if (is.na(n_pc)) n_pc <- min(6, ncol(eigenvecs))
}

scores <- fpca_res$x[, 1:n_pc]    
colnames(scores) <- paste0("PC", seq_len(n_pc))

cat(sprintf("Selected PCs: %d | Explained variance: %.1f%%\n", n_pc, cum_var[n_pc]*100))

# Scree plot
n_show  <- min(12, nrow(var_df))
p_scree <- ggplot(var_df[1:n_show, ], aes(x = PC)) +
  geom_col(aes(y = VarExp), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = CumVar), color = "firebrick", linewidth = 1) +
  geom_point(aes(y = CumVar), color = "firebrick", size = 2.5) +
  geom_hline(yintercept = VAR_THRESHOLD * 100, linetype = "dashed", color = "firebrick", alpha = 0.6) +
  geom_vline(xintercept = n_pc, linetype = "dotted", color = "navy", linewidth = 0.9) +
  annotate("text", x = n_pc + 0.3, y = max(var_df$VarExp[1:n_show]) * 0.9,
           label = paste0("PC", n_pc, " selected"), hjust = 0, color = "navy", size = 3.5) +
  scale_y_continuous(name = "Explained variance (%)",
                     sec.axis = sec_axis(~., name = "Cumulative variance (%)")) +
  labs(title = "Scree Plot + Cumulative Variance", x = "Principal Component") +
  theme_minimal(base_size = 13)
print(p_scree)

# STRATEGY 1: GMM CLUSTERING ----------------------------------------------
# (Pure data-driven + pseudo-density conformal)

cat("\n--- STRATEGY 1: GMM CLUSTERING ---\n")

bic_selection <- mclustBIC(scores)
gmm_fit       <- Mclust(scores, x = bic_selection)
G_opt         <- gmm_fit$G
res_class     <- gmm_fit$classification

# Rest = most frequent cluster (baseline); everything else = Active
rest_idx <- names(which.max(table(res_class)))
cluster_data_gmm <- data.frame(
  PC1         = scores[,1],
  PC2         = scores[,2],
  Label       = factor(ifelse(res_class != rest_idx, "Active", "Rest")),
  MaxHR       = apply(daily_matrix, 1, max, na.rm = TRUE),
  Uncertainty = gmm_fit$uncertainty
)

cat(sprintf("GMM optimal components: %d | Rest cluster: %s\n", G_opt, rest_idx))

# Uniform PCA scatter (K-Means style + uncertainty size from GMM)
p_pca_gmm <- ggplot(cluster_data_gmm, aes(x = PC1, y = PC2, color = Label)) +
  geom_point(aes(size = 1 - Uncertainty), alpha = 0.85) +
  stat_ellipse(level = 0.95, linetype = 2, linewidth = 1.1) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.12, bins = 8) +
  scale_color_manual(values = c("Active" = "orange", "Rest" = "steelblue")) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  scale_size_continuous(range = c(1, 5), name = "Certainty") +
  labs(title = paste("GMM Clustering (G =", G_opt, ")"),
       subtitle = "High-density regions + uncertainty sizing",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_pca_gmm)

# Peak distribution
p_peak_gmm <- ggplot(cluster_data_gmm, aes(x = PC1, y = MaxHR, color = Label)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Active" = "orange", "Rest" = "steelblue")) +
  labs(title = "Peak HR Distribution by Cluster", y = "Max BPM") +
  theme_minimal(base_size = 14)
print(p_peak_gmm)

# Dendrogram to check if GMM results are reliable
hc_gmm       <- hclust(dist(scores), method = "ward.D2")
dend_df_gmm  <- dendro_data(hc_gmm, type = "rectangle")
label_df_gmm <- dend_df_gmm$labels
label_df_gmm$Label <- cluster_data_gmm$Label[as.integer(label_df_gmm$label)]

p_dend_gmm <- ggplot() +
  geom_segment(data = segment(dend_df_gmm),
               aes(x = x, y = y, xend = xend, yend = yend),
               linewidth = 0.5, color = "grey40") +
  geom_text(data = label_df_gmm,
            aes(x = x, y = y - 0.02 * max(segment(dend_df_gmm)$y),
                label = label, color = Label),
            size = 2.8, angle = 90, hjust = 1, vjust = 0.5) +
  scale_color_manual(values = c("Active" = "orange", "Rest" = "steelblue")) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.05))) +
  labs(title = "Dendrogram (Ward.D2 on FPCA scores)",
       subtitle = "Leaves colored by GMM label",
       x = NULL, y = "Height") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom")
print(p_dend_gmm)

# STRATEGY 2: K-MEANS CLUSTERING ------------------------------------------
# (Silhouette-driven + conformal bands)

cat("\n--- STRATEGY 2: K-MEANS CLUSTERING ---\n")

max_k <- min(MAX_K, nrow(scores) - 1)

# 1. Calculate metrics with separate ranges (Elbow starts at 1, Silhouette at 2)
k_range_elbow <- 1:max_k
wss <- sapply(k_range_elbow, function(k) {
  kmeans(scores, centers = k, nstart = 25, iter.max = 50)$tot.withinss
})

k_range_sil <- 2:max_k
sil_width <- sapply(k_range_sil, function(k) {
  km <- kmeans(scores, centers = k, nstart = 25, iter.max = 50)
  mean(silhouette(km$cluster, dist(scores))[, 3])
})

optimal_k <- if (!is.null(K_MANUAL)) as.integer(K_MANUAL) else k_range_sil[which.max(sil_width)]

cat(sprintf("K-Means optimal k: %d\n", optimal_k))

# 2. Silhouette + Elbow Plots
p_sil <- ggplot(data.frame(k = k_range_sil, silhouette = sil_width),
                aes(x = k, y = silhouette)) +
  geom_line(color = "steelblue", linewidth = 1) + 
  geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = 1:max_k, limits = c(1, max_k)) +
  labs(title = "Silhouette", y = "Average width", x = "k") + 
  theme_minimal(base_size = 13)

p_elbow <- ggplot(data.frame(k = k_range_elbow, wss = wss),
                  aes(x = k, y = wss)) +
  geom_line(color = "steelblue", linewidth = 1) + 
  geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = 1:max_k, limits = c(1, max_k)) +
  labs(title = "Elbow (WSS)", y = "Within sum of squares", x = "k") + 
  theme_minimal(base_size = 13)

print(p_sil + p_elbow)

# 3. Final clustering
kmeans_res <- kmeans(scores, centers = optimal_k, nstart = 50)

cluster_data_km <- data.frame(
  as.data.frame(scores),
  Cluster = factor(kmeans_res$cluster)
)

# Binary labeling when k=2 (same logic as GMM)
if (optimal_k == 2) {
  mean_act <- tapply(rowMeans(daily_matrix, na.rm = TRUE),
                     cluster_data_km$Cluster, mean)
  active_cl <- names(which.max(mean_act))
  cluster_data_km$Label <- ifelse(cluster_data_km$Cluster == active_cl,
                                  "Active", "Rest")
} else {
  cluster_data_km$Label <- paste("Cluster", cluster_data_km$Cluster)
}

cluster_labels  <- sort(unique(cluster_data_km$Label))
cluster_palette <- setNames(scales::hue_pal()(length(cluster_labels)), cluster_labels)

# Uniform PCA scatter
p_pca_km <- ggplot(cluster_data_km, aes(x = PC1, y = PC2, color = Label)) +
  geom_point(size = 3, alpha = 0.85) +
  stat_ellipse(aes(group = Label), level = 0.90, type = "norm", linewidth = 1.1) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.12, bins = 8) +
  scale_color_manual(values = cluster_palette) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = paste("K-Means Clustering (k =", optimal_k, ")"),
       subtitle = sprintf("High-density regions | %d PCs (%.1f%% var)", n_pc, cum_var[n_pc]*100),
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")

print(p_pca_km)

# Dendrogram for same reason
hc_km       <- hclust(dist(scores), method = "ward.D2")
dend_df_km  <- dendro_data(hc_km, type = "rectangle")
label_df_km <- dend_df_km$labels
label_df_km$Label <- cluster_data_km$Label[as.integer(label_df_km$label)]

p_dend_km <- ggplot() +
  geom_segment(data = segment(dend_df_km),
               aes(x = x, y = y, xend = xend, yend = yend),
               linewidth = 0.5, color = "grey40") +
  geom_text(data = label_df_km,
            aes(x = x, y = y - 0.02 * max(segment(dend_df_km)$y),
                label = label, color = Label),
            size = 2.8, angle = 90, hjust = 1, vjust = 0.5) +
  scale_color_manual(values = cluster_palette) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.05))) +
  labs(title = "Dendrogram (Ward.D2 on FPCA scores)",
       subtitle = sprintf("Leaves colored by K-Means label (k=%d)", optimal_k),
       x = NULL, y = "Height") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom")

print(p_dend_km) # looks like we have an anomaly

# 4. K-Means Conformal Prediction Bands (Time domain: 10 to 130)
cat("\n--- K-Means Conformal Prediction Bands ---\n")

# Reconstruct curves using ONLY the selected PCs
reconstructed    <- scores %*% t(eigenvecs[, 1:n_pc])
approx_functions <- sweep(reconstructed, 2, global_mean, "+")

# Calculate spatial conformity scores
nonconf_scores <- sqrt(rowSums(scores^2))          
alpha          <- 1 - BAND_COVERAGE
threshold      <- quantile(nonconf_scores, 1 - alpha, type = 1)
in_band        <- nonconf_scores <= threshold

# Keep only the curves that fall within the conformal limit
mat_in <- approx_functions[in_band, , drop = FALSE]

# Explicitly define the time grid from 10 to 130
time_grid <- 10:130

band_df <- data.frame(
  Time       = time_grid,
  Mean       = colMeans(approx_functions, na.rm = TRUE),
  OuterLower = apply(mat_in, 2, quantile, probs = alpha/2, na.rm = TRUE),
  OuterUpper = apply(mat_in, 2, quantile, probs = 1 - alpha/2, na.rm = TRUE),
  IQRLower   = apply(mat_in, 2, quantile, probs = 0.25, na.rm = TRUE),
  IQRUpper   = apply(mat_in, 2, quantile, probs = 0.75, na.rm = TRUE),
  Median     = apply(mat_in, 2, median, na.rm = TRUE)
)

samp_idx  <- sample(nrow(approx_functions), min(N_SAMPLE_CURVES, nrow(approx_functions)))
samp_long <- do.call(rbind, lapply(samp_idx, function(i) {
  data.frame(Time  = time_grid,
             Value = approx_functions[i, ],
             ID    = factor(i),
             Label = cluster_data_km$Label[i])
}))

p_bands <- ggplot(band_df, aes(x = Time)) +
  geom_line(data = samp_long, aes(y = Value, group = ID, color = Label),
            alpha = 0.40, linewidth = 0.45) +
  geom_ribbon(aes(ymin = OuterLower, ymax = OuterUpper), fill = "tomato", alpha = 0.18) +
  geom_ribbon(aes(ymin = IQRLower, ymax = IQRUpper), fill = "seagreen", alpha = 0.38) +
  geom_line(aes(y = Median), color = "darkgreen", linewidth = 1.2) +
  geom_line(aes(y = Mean), color = "steelblue", linetype = "dashed", linewidth = 0.9) +
  scale_color_manual(values = cluster_palette) +
  scale_x_continuous(breaks = seq(10, 130, 20), limits = c(10, 130)) +
  labs(title = sprintf("Conformal Prediction Bands %d%% (K-Means)", round(BAND_COVERAGE*100)),
       subtitle = sprintf("n_pc = %d | alpha = %.2f", n_pc, alpha),
       x = "Duration (Minutes 10 to 130)", y = "Heart Rate (BPM)",
       caption = "Dark green = median | Dashed blue = mean | Light green = IQR | Red = conformal band") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")

print(p_bands)

# FINAL VISUALIZATION: ORIGINAL 24H CURVES COLORED BY CLUSTER -------------

cat("\n--- Plotting Original 24h Curves by Cluster (Combined) ---\n")

# Retrieve the ORIGINAL 24-hour smoothed curves (before quantile transformation)
matrix_smooth_original <- t(eval.fd(grid_min, fd_obj))

# Helper function to generate the plot for a specific model without faceting
plot_final_clusters_combined <- function(mat_orig, cluster_labels, title_prefix, palette) {
  plot_df <- as.data.frame(mat_orig)
  colnames(plot_df) <- grid_min
  plot_df$DayID   <- seq_len(nrow(plot_df))
  plot_df$Cluster <- cluster_labels
  
  plot_df_long <- plot_df %>%
    pivot_longer(cols = -c(DayID, Cluster), 
                 names_to = "Minute", 
                 values_to = "HR") %>%
    mutate(Minute = as.numeric(Minute))
  
  p <- ggplot() +
    # Plot all individual curves on the same axis
    # Alpha increased to 0.65 to make them darker
    geom_line(data = plot_df_long, 
              aes(x = Minute, y = HR, group = DayID, color = Cluster), 
              alpha = 0.65, linewidth = 0.4) +
    scale_x_continuous(breaks = seq(0, 1440, 180),
                       labels = seq(0, 1440, 180)) +
    scale_color_manual(values = palette) +
    labs(title = title_prefix,
         subtitle = "Original 24h profiles colored by estimated cluster",
         x = "Minute of the Day (0-1440)", 
         y = "Heart Rate (bpm)") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10))
  
  return(p)
}

# Ensure the GMM palette matches what we defined earlier
gmm_palette <- c("Active" = "orange", "Rest" = "steelblue")

# 1. Generate plot for GMM
p_final_gmm <- plot_final_clusters_combined(matrix_smooth_original, 
                                            cluster_data_gmm$Label, 
                                            "GMM Clustering", 
                                            gmm_palette)

# 2. Generate plot for K-Means
p_final_km <- plot_final_clusters_combined(matrix_smooth_original, 
                                           cluster_data_km$Label, 
                                           "K-Means Clustering", 
                                           cluster_palette)

# Combine them vertically into a single visualization (2 rows, 1 column)
grid.arrange(p_final_gmm, p_final_km, ncol = 1)
# IT looks like the GMM model performed a bit better than k-means, even if they
# give the same cluster at the majority of the observations.