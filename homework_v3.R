setwd("C:/Users/casam/OneDrive/Desktop/Simone/Stat_Brutti/Homework")
install.packages(c("tidyverse", "lubridate", "gridExtra", "splines", "fda", 
                   "cluster", "patchwork", "ggdendro", "mclust"))
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

# =========================================================================
# Dataset upload and feature engineering
# =========================================================================
file_name <- "high_quality_data_filled.csv"

df <- read_csv(file_name, show_col_types = FALSE) %>%
  mutate(
    datetime      = make_datetime(Year, Month, Day, Hour, Minute, tz = "UTC"),
    date          = as_date(datetime),
    minute_of_day = as.numeric(difftime(datetime, floor_date(datetime, "day"), units = "mins"))
  ) %>%
  arrange(date, datetime) %>%
  mutate(day_id = as.integer(factor(date)))

glimpse(df)
View(df)

# ==============================================================================
# Data Cleaning (Quantity and physiological range filters)
# ==============================================================================

days_counts <- df %>% dplyr::count(date)
valid_quantity_dates <- days_counts %>% filter(n >= 240) %>% pull(date)

physiological_valid_dates <- df %>%
  group_by(date) %>%
  summarise(
    is_valid = all(HeartRate >= 30 & HeartRate <= 200, na.rm = TRUE)
  ) %>%
  filter(is_valid == TRUE) %>%
  pull(date)

valid_dates <- intersect(valid_quantity_dates, physiological_valid_dates)

df <- df %>% filter(date %in% valid_dates)

cat("Cleaning Summary:\n")
cat("Final valid days remaining:", length(unique(df$date)), "\n")

# =========================================================================
# EDA
# =========================================================================

# Conversion of categorical features
df <- df %>%
  mutate(
    MotionContext = factor(MotionContext),
    SleepStatus   = factor(SleepStatus),
    SleepStage    = factor(SleepStage)
  )

# Summary of heart rate overall and by context
cat("\nSummary Heart Rate\n")
print(summary(df$HeartRate))

cat("\nHeart Rate per SleepStage:\n")
print(tapply(df$HeartRate, df$SleepStage, summary))
# 195 while sleeping.... what a nightmare...don't touch my family's tractor!

cat("\nHeart Rate per SleepStatus:\n")
print(tapply(df$HeartRate, df$SleepStatus, summary))

cat("\nHeart Rate per MotionContext:\n")
print(tapply(df$HeartRate, df$MotionContext, summary))

# Heart rate distribution and boxplots
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

p_box_motion <- ggplot(df %>% filter(!is.na(MotionContext)),
                       aes(x = MotionContext, y = HeartRate, fill = MotionContext)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.75) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "HR per Motion Context", x = "Motion Context", y = "HR (bpm)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

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

# Categorical trends throughout the day
df_motion_time <- df %>%
  filter(!is.na(MotionContext)) %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  dplyr::count(time_bin, MotionContext) %>% 
  group_by(time_bin) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_motion_time <- ggplot(df_motion_time,
                        aes(x = time_bin, y = prop, fill = MotionContext)) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 1440, 180),
                     labels = seq(0, 1440, 180)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "MotionContext through the day",
       x = "Minutes of the day", y = "Proportion", fill = "Motion") +
  theme_minimal()

df_status_time <- df %>%
  filter(!is.na(SleepStatus)) %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  dplyr::count(time_bin, SleepStatus) %>%
  group_by(time_bin) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_status_time <- ggplot(df_status_time,
                        aes(x = time_bin, y = prop, fill = SleepStatus)) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 1440, 180),
                     labels = seq(0, 1440, 180)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "SleepStatus through the day",
       x = "Minutes of the day", y = "Proportion", fill = "Status") +
  theme_minimal()

df_stage_time <- df %>%
  filter(!is.na(SleepStage)) %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  dplyr::count(time_bin, SleepStage) %>%
  group_by(time_bin) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_stage_time <- ggplot(df_stage_time,
                       aes(x = time_bin, y = prop, fill = SleepStage)) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 1440, 180),
                     labels = seq(0, 1440, 180)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "SleepStage through the day",
       x = "Minutes of the day", y = "Proportion", fill = "Stage") +
  theme_minimal()

grid.arrange(p_motion_time, p_status_time, p_stage_time, ncol = 1)

# Median HR by time bin, stratified by SleepStage
df_hr_stage_time <- df %>%
  filter(!is.na(SleepStage)) %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  group_by(time_bin, SleepStage) %>%
  summarise(med_HR = median(HeartRate, na.rm = TRUE), .groups = "drop")

p_hr_stage_line <- ggplot(df_hr_stage_time,
                          aes(x = time_bin, y = med_HR,
                              colour = SleepStage, group = SleepStage)) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 1440, 180),
                     labels = seq(0, 1440, 180)) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "HR median by time bin and Sleep Stage",
       x = "Minutes of the day", y = "HR median (bpm)", colour = "Stage") +
  theme_minimal()

print(p_hr_stage_line)

# ==============================================================================
# OPTIMIZED B-SPLINE PARAMETERS (Penalized Least Squares)
# ==============================================================================

# Parameters
rangeval  <- c(0, 1440)
grid_min  <- seq(0, 1439, 1)

days_list <- df %>% 
  group_by(date) %>% 
  group_split()

# GCV Calculation Function
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

# ------------------------------------------------------------------------------
# GRID SEARCH 2D (Commented out to save computation time)
# ------------------------------------------------------------------------------
# k_values    <- seq(30, 200, 5)  
# lambda_vals <- 10^seq(-4, 4, length.out = 20)
# gcv_mat <- outer(k_values, lambda_vals, Vectorize(compute_avg_gcv))
# best_idx    <- which(gcv_mat == min(gcv_mat, na.rm = TRUE), arr.ind = TRUE)
# best_K      <- k_values[best_idx[1]]
# best_lambda <- lambda_vals[best_idx[2]]
# 
# cat(sprintf("\n=== OPTIMAL PARAMETERS (GCV) ===\nK (nbasis) = %d\nlambda     = %.2e\n", 
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

# this method doesn't give us any good result, so we choose k = 50
# for simplicity because it would fit (with a poly of degree 3 still)
# each 30 mins (something about)

# ==============================================================================
# 1D OPTIMIZATION FOR LAMBDA (Fixed K = 50)
# ==============================================================================

best_K <- 50

lambda_vals <- 10^seq(-4, 4, length.out = 50)
gcv_scores  <- numeric(length(lambda_vals))

for (i in seq_along(lambda_vals)) {
  gcv_scores[i] <- compute_avg_gcv(K = best_K, lambda_val = lambda_vals[i])
}

best_lambda <- lambda_vals[which.min(gcv_scores)]
cat(sprintf("Optimal lambda for K=50 is: %.2e\n", best_lambda))

# ==============================================================================
# FINAL SMOOTHING OBJECT
# ==============================================================================

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

# Remove unused rows from matrix
if (row_i > 1) {
  daily_matrix <- daily_matrix[1:(row_i - 1), , drop = FALSE]
  good_dates   <- good_dates[1:(row_i - 1)]
} else {
  stop("No valid days remained.")
}

cat("\nFinal clean matrix:", nrow(daily_matrix), "days\n")

# Convert clean matrix to fd object for FPCA/clustering
fd_obj <- Data2fd(argvals = grid_min, 
                  y        = t(daily_matrix), 
                  basisobj = final_basis)

# ==============================================================================
# VISUALIZATION
# ==============================================================================
plot(fd_obj, lty = 1, col = 'blue',
     main = "Optimized Daily HR Curves (B-spline + GCV)",
     xlab = "Minute of the Day", ylab = "Heart Rate (bpm)")
