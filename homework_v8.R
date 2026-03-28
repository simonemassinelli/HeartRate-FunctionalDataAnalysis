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
library(scales)

# Dataset upload and feature engineering ----------------------------------

file_name <- "dataset/high_quality_data_filled.csv"

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

# 1D OPTIMIZATION FOR LAMBDA (Fixed K) -------------------------------

best_K <- 35
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

#CONFORMAL PREDICTION (PSEUDO-DENSITY & ALPHA ANALYSIS) ---------
# 1. Calcolo distanze sulle curve
matrix_smooth <- t(eval.fd(grid_min, fd_obj))
dist_matrix <- as.matrix(dist(matrix_smooth)/sqrt(dim(matrix_smooth)[2]))

# Kernel Density Estimation 
h_param <- quantile(dist_matrix, 0.15) 
kernel_gaussian <- function(u) { (1 / sqrt(2 * pi)) * exp(-0.5 * u^2) }

pseudo_density <- numeric(nrow(matrix_smooth))
for(i in 1:nrow(matrix_smooth)) {
  pseudo_density[i] <- mean(kernel_gaussian(dist_matrix[i, ] / h_param))
}

alpha_levels <- c(0.1, 0.5, 0.9)
par(mfrow = c(1, 3)) # Prepariamo il pannello per 3 grafici affiancati

for(alpha in alpha_levels) {
  # Calcoliamo il valore soglia di densita
  lambda_alpha <- quantile(pseudo_density, alpha)
  subset_idx <- which(pseudo_density >= lambda_alpha)
  
  if(length(subset_idx) > 1) {
    # Disegniamo il set di curve "conformi"
    matplot(grid_min, t(matrix_smooth[subset_idx, ]), 
            type = "l", 
            lty = 1,              # Linee continue
            col = alpha("grey40", 0.3), 
            ylim = range(matrix_smooth),
            main = paste("Alpha =", alpha, "\n(Confidence Set)"),
            ylab = "BPM", 
            xlab = "Minuti")
    # Aggiungiamo la media specifica di questo sotto-insieme (linea rossa)
    lines(grid_min, colMeans(matrix_smooth[subset_idx, ]), col = "red", lwd = 2.5)
    # Info sul numero di giorni inclusi nel set
    mtext(paste("Giorni inclusi:", length(subset_idx)), side=3, line=-1.5, cex=0.7)
  }
}

par(mfrow = c(1, 1))
cat("\n--- FASE 8: Rilevamento Anomalie tramite Soglia di Confidenza ---\n")
alpha_outlier <- 0.05
soglia_densita <- quantile(pseudo_density, alpha_outlier)
# Identifichiamo quali e quanti sono gli outlier
outlier_indices <- which(pseudo_density <= soglia_densita)
n_rilevati <- length(outlier_indices)
outlier_dates <- good_dates[outlier_indices]

cat(sprintf("Soglia di densit? calcolata: %.6f\n", soglia_densita))
cat(sprintf("Numero di outlier rilevati (alpha=%.2f): %d\n", alpha_outlier, n_rilevati))

# Prepariamo i colori: una palette per gli outlier, grigio per le normali
# Usiamo rainbow o heat.colors se sono molti, cos? da distinguerli
colori_outlier <- rainbow(n_rilevati)
colori_plot <- rep(alpha("grey", 0.3), length(good_dates))
spessori_plot <- rep(1, length(good_dates))

# Coloriamo ogni outlier in modo unico
for(i in 1:n_rilevati) {
  colori_plot[outlier_indices[i]] <- colori_outlier[i]
  spessori_plot[outlier_indices[i]] <- 3 # Linee spesse per gli outlier
}

# PLOT FINALE
par(mfrow=c(1,1))
plot(fd_obj, 
     col = colori_plot, 
     lwd = spessori_plot, 
     lty = 1,
     main = paste("Anomalie Funzionali Rilevate (Soglia Alpha =", alpha_outlier, ")"),
     xlab = "Minuti (0-1440)", 
     ylab = "BPM")

# Aggiungiamo la media globale in nero
lines(mean.fd(fd_obj), col = "black", lwd = 3, lty = 1)

# Legenda Dinamica (si adatta al numero di outlier trovati)
# Se gli outlier sono troppi, limitiamo la legenda ai primi o usiamo una nota
if(n_rilevati <= 5) {
  legend_labels <- c("Giornate Normali", as.character(outlier_dates), "Media Globale")
  legend_colors <- c("grey", colori_outlier, "black")
  legend_lwds   <- c(1, rep(3, n_rilevati), 3)
} else {
  legend_labels <- c("Giornate Normali", paste(n_rilevati, "Outliers rilevati"), "Media Globale")
  legend_colors <- c("grey", "red", "black")
  legend_lwds   <- c(1, 3, 3)
}

legend("topleft", 
       legend = legend_labels, 
       col = legend_colors, 
       lwd = legend_lwds, 
       lty = 1, 
       bty = "n", 
       cex = 0.5)

# Stampa i dettagli delle anomalie
print(data.frame(Data = outlier_dates, Density = pseudo_density[outlier_indices]))



# CONFIGURABLE PARAMETERS -------------------------------------------------
SEED            <- 123
set.seed(SEED)

VAR_THRESHOLD   <- 0.85      # cumulative variance for PC selection
N_PC_MANUAL     <- NULL     # NULL = auto
BAND_COVERAGE   <- 0.90
MAX_K           <- 3
K_MANUAL        <- NULL
CENTERING       <- TRUE     # Global functional centering (recommended)
STANDARDIZE     <- FALSE    # Global functional standardization (divide by sd at t time)

SPLIT_RATIO     <- 0.6      # 60% train / 40% calibration for inductive CP
ALPHA_OUTLIER   <- 0.01
DIAGONAL_GMM    <- FALSE    # TRUE = diagonal covariance (VVI), FALSE = full (default mclust)
GRID_TRASFORM<- TRUE      #ranked value change otherwise normal analisis

# INDUCTIVE CONFORMAL SETUP: 60/40 TRAIN/CALIBRATION SPLIT ---------

n_days <- nrow(daily_matrix)
train_size <- round(n_days * SPLIT_RATIO)
train_idx <- sample(seq_len(n_days), train_size, replace = FALSE)
calib_idx <- setdiff(seq_len(n_days), train_idx)

matrix_train <- daily_matrix[train_idx, , drop = FALSE]
matrix_calib <- daily_matrix[calib_idx, , drop = FALSE]
good_dates_train <- good_dates[train_idx]
good_dates_calib <- good_dates[calib_idx]

cat(sprintf("Inductive split (60/40): %d train days | %d calibration days\n", nrow(matrix_train), nrow(matrix_calib)))

# SHARED PHASE: FPCA ON FUNCTIONAL QUANTILES ------------------------------

# Split PRIMA di qualsiasi trasformazione per evitare data leakage
matrix_smooth_train_raw <- matrix_smooth[train_idx, , drop = FALSE]
matrix_smooth_calib_raw <- matrix_smooth[calib_idx, , drop = FALSE]

# ---> FUNCTIONAL QUANTILES (Monotone Rearrangement) <---
# Applicato indipendentemente a train e calib (operazione row-wise: no leakage)
if (GRID_TRASFORM) {
  matrix_smooth_train_raw <- t(apply(matrix_smooth_train_raw, 1, sort, decreasing = TRUE))
  matrix_smooth_calib_raw <- t(apply(matrix_smooth_calib_raw, 1, sort, decreasing = TRUE))
  cat("-> DATA TRASFORMATION")
}

# --- NEW: Visualizing transformed quantile curves ---
cat("\n--- Plotting Transformed Quantile Curves ---\n")
plot_quantiles_df <- as.data.frame(matrix_smooth_train_raw) %>%
  mutate(DayID = row_number()) %>%
  pivot_longer(-DayID, names_to = "TimeIndex", values_to = "HR") %>%
  mutate(TimeIndex = as.numeric(str_remove(TimeIndex, "V")))

p_transformed <-ggplot(plot_quantiles_df, aes(x = TimeIndex, y = HR, group = DayID)) +
  geom_line(alpha = 0.2, color = "purple") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 1.2) +
  labs(title = "Transformed Curves (Functional Quantiles)",
       subtitle = "Descending HR sorted, restricted to minutes 10-130",
       x = "Duration (minutes)", y = "Heart Rate (bpm)") +
  theme_minimal(base_size = 14)
print(p_transformed)

# 2. Global mean e sd stimati SOLO sul TRAIN
global_mean <- colMeans(matrix_smooth_train_raw, na.rm = TRUE)
global_sd   <- apply(matrix_smooth_train_raw, 2, sd, na.rm = TRUE)
global_sd[global_sd < 1e-6] <- 1

# 3. Centering / Standardization: parametri dal TRAIN, applicati a entrambi
working_mat_train <- matrix_smooth_train_raw
working_mat_calib <- matrix_smooth_calib_raw

if (CENTERING) {
  working_mat_train <- sweep(working_mat_train, 2, global_mean, "-")
  working_mat_calib <- sweep(working_mat_calib, 2, global_mean, "-")  # stessa media train
  cat("-> Global centering applied (train mean)\n")
}
if (STANDARDIZE) {
  working_mat_train <- sweep(working_mat_train, 2, global_sd, "/")
  working_mat_calib <- sweep(working_mat_calib, 2, global_sd, "/")    # stesso sd train
  cat("-> Global standardization applied (train sd)\n")
}

# 4. FPCA fittata SOLO sul TRAIN
fpca_res  <- prcomp(working_mat_train, center = FALSE, scale. = FALSE)
eigenvecs <- fpca_res$rotation          # autovettori appresi dal train
sdev_sq   <- fpca_res$sdev^2
cum_var   <- cumsum(sdev_sq) / sum(sdev_sq)

# Selezione numero di PC (basata sulla varianza del train)
if (!is.null(N_PC_MANUAL)) {
  n_pc <- as.integer(N_PC_MANUAL)
} else {
  n_pc <- which(cum_var >= VAR_THRESHOLD)[1]
  if (is.na(n_pc)) n_pc <- min(6, ncol(eigenvecs))
}

# Proiezione: scores TRAIN (da prcomp) e CALIB (proiezione manuale sugli assi train)
scores_train <- fpca_res$x[, 1:n_pc, drop = FALSE]
scores_calib <- working_mat_calib %*% eigenvecs[, 1:n_pc, drop = FALSE]

colnames(scores_train) <- paste0("PC", seq_len(n_pc))
colnames(scores_calib) <- paste0("PC", seq_len(n_pc))

cat(sprintf("Selected PCs: %d | Explained variance (train): %.1f%%\n", n_pc, cum_var[n_pc] * 100))

# Scree plot (invariato, basato su varianza train)-----
var_df <- data.frame(
  PC     = seq_along(cum_var),
  VarExp = sdev_sq / sum(sdev_sq) * 100,
  CumVar = cum_var * 100
)

n_show <- min(12, nrow(var_df))
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
  labs(title = "Scree plot + Cumulative variance (fit on train)", x = "Principal component") +
  theme_minimal(base_size = 13)
print(p_scree)

alpha_cp <- 1 - BAND_COVERAGE   # es. 0.10 per bande al 90%
n2       <- nrow(scores_calib)  # dimensione calibration set
n_grid   <- length(grid_min)

# Phi: matrice di autovettori  (n_grid x n_pc)  — proiezione dallo spazio PC
# al dominio funzionale (colonne = PC1...PCn_pc, righe = punti griglia)
Phi <- eigenvecs[, 1:n_pc, drop = FALSE]

# HELPER 1: banda da ellissoide (per GMM)
#   Ellissoide: { ξ : (ξ-μ)ᵀ Σ⁻¹ (ξ-μ) ≤ c² }
#   Ricostruzione curva: X(t) = global_mean[t] + Phi[t,] %*% ξ
#   => sup X(t) = global_mean[t] + Phi[t,]%*%μ + c * sqrt( Phi[t,]%*%Σ%*%Phi[t,]ᵀ )
band_from_ellipsoid <- function(mu, Sigma, c_sq) {
  mid   <- as.numeric(global_mean + Phi %*% mu)
  width <- sqrt(c_sq * diag(Phi %*% Sigma %*% t(Phi)))
  list(upper = mid + width, lower = mid - width)
}


# HELPER 2: banda da palla (per K-Means)
#   Palla: { ξ : ||ξ - v||² ≤ r² }
#   => sup X(t) = global_mean[t] + Phi[t,]%*%v + r * ||Phi[t,]||₂
band_from_ball <- function(v, r_sq) {
  mid   <- as.numeric(global_mean + Phi %*% v)
  width <- sqrt(r_sq) * sqrt(rowSums(Phi^2))
  list(upper = mid + width, lower = mid - width)
}



# STRATEGY 1: GMM CLUSTERING ----------------------------------------------
# (Pure data-driven + pseudo-density conformal)
cat("\n=== STRATEGY 1: GMM CONFORMAL PREDICTION BANDS ===\n")

gmm_modeltype <- if (DIAGONAL_GMM) "VVI" else "VVV"

bic_selection <- mclustBIC(scores_train, modelNames = gmm_modeltype)
gmm_fit_train <- Mclust(scores_train, x = bic_selection,
                        modelNames = gmm_modeltype, verbose = FALSE)

K_gmm  <- gmm_fit_train$G
pi_hat <- gmm_fit_train$parameters$pro
mu_hat <- gmm_fit_train$parameters$mean   # n_pc x K

get_sigma_k <- function(k) {
  S <- gmm_fit_train$parameters$variance$sigma[,, k]
  if (DIAGONAL_GMM) diag(diag(S)) else S
}

# Precalcola inversa e log-det di ogni Sigma_k (usati sia per score che per bande)
sigma_inv_list <- lapply(seq_len(K_gmm), function(k) solve(get_sigma_k(k)))
log_det_list   <- sapply(seq_len(K_gmm), function(k) {
  as.numeric(determinant(get_sigma_k(k), logarithm = TRUE)$modulus)
})

cat(sprintf("GMM on train | G=%d | model=%s\n", K_gmm, gmm_modeltype))

# --- Conformity scores (log-scale max-component, eq. 8) ---
scores_conf_gmm <- numeric(nrow(scores_calib))
for (i in seq_len(nrow(scores_calib))) {
  xi   <- as.numeric(scores_calib[i, ])
  vals <- sapply(seq_len(K_gmm), function(k) {
    d       <- xi - mu_hat[, k]
    maha    <- as.numeric(t(d) %*% sigma_inv_list[[k]] %*% d)
    log_phi <- -0.5 * (n_pc * log(2 * pi) + log_det_list[k] + maha)
    log(pi_hat[k]) + log_phi
  })
  scores_conf_gmm[i] <- max(vals)
}

# --- Soglia inductive CP (in log-scale, coerente con scores_conf_gmm) ---
lambda_gmm <- quantile(scores_conf_gmm, alpha_cp)
cat(sprintf("  lambda (GMM, %.0f%% band, log-scale): %.4f\n", BAND_COVERAGE * 100, lambda_gmm))

# --- Ellissoidi -> bande funzionali ---

upper_gmm <- matrix(-Inf, nrow = n_grid, ncol = K_gmm)
lower_gmm <- matrix( Inf, nrow = n_grid, ncol = K_gmm)
c_sq_gmm  <- rep(NA_real_, K_gmm)   # salva c_sq per ogni componente

for (k in seq_len(K_gmm)) {
  # c_k² in log-scale: formula IDENTICA a quella usata per scores_conf_gmm
  c_sq <- -2 * (lambda_gmm
                - log(pi_hat[k])
                + 0.5 * (n_pc * log(2 * pi) + log_det_list[k]))
  
  if (is.finite(c_sq) && c_sq > 0) {
    c_sq_gmm[k] <- c_sq
    Sigma_k <- get_sigma_k(k)
    mid     <- as.numeric(global_mean + Phi %*% mu_hat[, k])
    # Proiezione dell'ellissoide: sup/inf di Phi(t)ᵀ ξ con Mahal(ξ,μ_k) ≤ c_k
    width   <- sqrt(c_sq * diag(Phi %*% Sigma_k %*% t(Phi)))
    upper_gmm[, k] <- mid + width
    lower_gmm[, k] <- mid - width
  } else {
    cat(sprintf("  [Note] Component k=%d skipped: c_sq=%.4f (lambda too high for this component)\n", k, c_sq))
  }
}

# Controllo: almeno un componente deve aver contribuito
active_k <- which(apply(upper_gmm, 2, function(x) any(is.finite(x))))
if (length(active_k) == 0) stop("No GMM component produced a finite band. Check lambda_gmm or BAND_COVERAGE.")

upper_band_gmm <- apply(upper_gmm[, active_k, drop = FALSE], 1, max)
lower_band_gmm <- apply(lower_gmm[, active_k, drop = FALSE], 1, min)

# --- Outlier detection ---
# Assegna ogni curva di calibrazione al componente k con score più alto
best_k_gmm <- apply(scores_calib, 1, function(xi) {
  which.max(sapply(seq_len(K_gmm), function(k) {
    d    <- xi - mu_hat[, k]
    maha <- as.numeric(t(d) %*% sigma_inv_list[[k]] %*% d)
    log(pi_hat[k]) - 0.5 * (n_pc * log(2 * pi) + log_det_list[k] + maha)
  }))
})

outlier_idx_gmm   <- which(scores_conf_gmm < lambda_gmm)
n_outliers_gmm    <- length(outlier_idx_gmm)
outlier_dates_gmm <- good_dates_calib[outlier_idx_gmm]

cat(sprintf("  Active components in band: %d / %d\n", length(active_k), K_gmm))
cat(sprintf("  Outliers: %d / %d (%.1f%%)\n",
            n_outliers_gmm, n2, 100 * n_outliers_gmm / n2))

# Palette distinta per ogni componente attivo
gmm_colors <- setNames(scales::hue_pal()(length(active_k)), active_k)

par(mfrow = c(1, length(active_k)))

for (k in active_k) {
  # Curve appartenenti al cluster k (assegnazione soft: best_k_gmm)
  idx_k     <- which(best_k_gmm == k)
  out_k     <- intersect(idx_k, outlier_idx_gmm)
  inlier_k  <- setdiff(idx_k, outlier_idx_gmm)
  
  upper_k <- upper_gmm[, k]
  lower_k <- lower_gmm[, k]
  
  ylim_k <- range(c(upper_k, lower_k,
                    working_mat_calib[idx_k, , drop = FALSE] + global_mean),
                  na.rm = TRUE)
  
  plot(grid_min, upper_k, type = "n", ylim = ylim_k,
       main = sprintf("GMM — Component %d  (π=%.2f)\n%.0f%% Conformal Band",
                      k, pi_hat[k], BAND_COVERAGE * 100),
       xlab = "Sorted intensity rank", ylab = "Heart Rate (bpm)")
  
  # Curve inlier del cluster k
  for (i in inlier_k) {
    lines(grid_min, working_mat_calib[i, ] + global_mean,
          col = alpha(gmm_colors[as.character(k)], 0.25), lwd = 0.8)
  }
  # Curve outlier del cluster k
  for (i in out_k) {
    lines(grid_min, working_mat_calib[i, ] + global_mean,
          col = alpha("firebrick", 0.85), lwd = 2)
  }
  
  # Banda dell'ellissoide k
  polygon(c(grid_min, rev(grid_min)), c(upper_k, rev(lower_k)),
          col = alpha(gmm_colors[as.character(k)], 0.20), border = NA)
  lines(grid_min, upper_k, col = gmm_colors[as.character(k)], lwd = 2, lty = 2)
  lines(grid_min, lower_k, col = gmm_colors[as.character(k)], lwd = 2, lty = 2)
  
  # Media del cluster (ricostruita dai pesi mu_hat)
  lines(grid_min, as.numeric(global_mean + Phi %*% mu_hat[, k]),
        col = "black", lwd = 2.5)
  
  legend("topright",
         legend = c(sprintf("%.0f%% Band", BAND_COVERAGE * 100),
                    "Component mean",
                    sprintf("Inliers (n=%d)",  length(inlier_k)),
                    sprintf("Outliers (n=%d)", length(out_k))),
         col = c(gmm_colors[as.character(k)], "black",
                 gmm_colors[as.character(k)], "firebrick"),
         lwd = c(4, 2.5, 1.5, 2), bty = "n", cex = 0.75)
}
par(mfrow = c(1, 1))

# Uniform PCA scatter (K-Means style + uncertainty size from GMM)
#p_pca_gmm <- ggplot(cluster_data_gmm, aes(x = PC1, y = PC2, color = Label)) +
#  geom_point(aes(size = 1 - Uncertainty), alpha = 0.85) +
#  stat_ellipse(level = 0.95, linetype = 2, linewidth = 1.1) +
#  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.12, bins = 8) +
#  scale_color_manual(values = c("Active" = "orange", "Rest" = "steelblue")) +
#  scale_fill_gradient(low = "white", high = "darkblue") +
#  scale_size_continuous(range = c(1, 5), name = "Certainty") +
#  labs(title = paste("GMM Clustering (G =", G_opt, ")"),
#       subtitle = "High-density regions + uncertainty sizing",
#       x = "PC1", y = "PC2") +
#  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
#print(p_pca_gmm)


# STRATEGY 2: K-MEANS CLUSTERING ------------------------------------------
# (Silhouette-driven + conformal bands)
cat("\n=== STRATEGY 2: K-MEANS CONFORMAL PREDICTION BANDS ===\n")

# --- Silhouette + Elbow (su scores_train) ---
max_k   <- min(MAX_K, nrow(scores_train) - 1)
k_range <- 2:max_k

sil_width <- sapply(k_range, function(k) {
  km <- kmeans(scores_train, centers = k, nstart = 25, iter.max = 50)
  mean(silhouette(km$cluster, dist(scores_train))[, 3])
})
wss <- sapply(k_range, function(k) {
  kmeans(scores_train, centers = k, nstart = 25, iter.max = 50)$tot.withinss
})

optimal_k <- if (!is.null(K_MANUAL)) as.integer(K_MANUAL) else k_range[which.max(sil_width)]
cat(sprintf("K-Means optimal k: %d\n", optimal_k))

p_sil <- ggplot(data.frame(k = k_range, sil = sil_width), aes(x = k, y = sil)) +
  geom_line(color = "steelblue") + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick") +
  labs(title = "Silhouette (train)", y = "Average width", x = "k") +
  theme_minimal(base_size = 13)

p_elbow <- ggplot(data.frame(k = k_range, wss = wss), aes(x = k, y = wss)) +
  geom_line(color = "steelblue") + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick") +
  labs(title = "Elbow / WSS (train)", y = "Within sum of squares", x = "k") +
  theme_minimal(base_size = 13)

print(p_sil + p_elbow)

# --- Fit finale su train ---
kmeans_train  <- kmeans(scores_train, centers = optimal_k, nstart = 50)
centers_train <- kmeans_train$centers   # optimal_k x n_pc

# --- Conformity scores sul calibration set ---
# Score = -min_k ||ξ - v_k||²   (più alto = più vicino al centroide più prossimo)
scores_conf_km <- apply(scores_calib, 1, function(xi) {
  -min(rowSums(sweep(centers_train, 2, xi)^2))
})

# Assegna ogni curva di calibrazione al centroide più vicino
best_k_km <- apply(scores_calib, 1, function(xi) {
  which.min(rowSums(sweep(centers_train, 2, xi)^2))
})

# --- Soglia inductive CP ---
lambda_km <- quantile(scores_conf_km, alpha_cp)
r_sq      <- -lambda_km
cat(sprintf("  lambda (K-Means, %.0f%% band): %.4f | radius: %.4f\n",
            BAND_COVERAGE * 100, lambda_km, sqrt(max(r_sq, 0))))


# --- Palle -> bande funzionali ---
upper_km <- matrix(-Inf, nrow = n_grid, ncol = optimal_k)
lower_km <- matrix( Inf, nrow = n_grid, ncol = optimal_k)

for (k in seq_len(optimal_k)) {
  mid   <- as.numeric(global_mean + Phi %*% centers_train[k, ])
  width <- sqrt(r_sq) * sqrt(rowSums(Phi^2))
  upper_km[, k] <- mid + width
  lower_km[, k] <- mid - width
}

upper_band_km <- apply(upper_km, 1, max)
lower_band_km <- apply(lower_km, 1, min)

# --- Outlier detection ---
outlier_idx_km   <- which(scores_conf_km < lambda_km)
n_outliers_km    <- length(outlier_idx_km)
outlier_dates_km <- good_dates_calib[outlier_idx_km]

cat(sprintf("  Outliers: %d / %d (%.1f%%)\n",
            n_outliers_km, n2, 100 * n_outliers_km / n2))

# Palette distinta per ogni cluster
km_colors <- setNames(scales::hue_pal()(optimal_k), seq_len(optimal_k))

par(mfrow = c(1, optimal_k))

for (k in seq_len(optimal_k)) {
  # Curve assegnate al cluster k (hard assignment: centroide più vicino)
  idx_k    <- which(best_k_km == k)
  out_k    <- intersect(idx_k, outlier_idx_km)
  inlier_k <- setdiff(idx_k, outlier_idx_km)
  
  upper_k <- upper_km[, k]
  lower_k <- lower_km[, k]
  
  ylim_k <- range(c(upper_k, lower_k,
                    working_mat_calib[idx_k, , drop = FALSE] + global_mean),
                  na.rm = TRUE)
  
  plot(grid_min, upper_k, type = "n", ylim = ylim_k,
       main = sprintf("K-Means — Cluster %d  (n=%d)\n%.0f%% Conformal Band",
                      k, length(idx_k), BAND_COVERAGE * 100),
       xlab = "Sorted intensity rank", ylab = "Heart Rate (bpm)")
  
  # Curve inlier del cluster k
  for (i in inlier_k) {
    lines(grid_min, working_mat_calib[i, ] + global_mean,
          col = alpha(km_colors[as.character(k)], 0.25), lwd = 0.8)
  }
  # Curve outlier del cluster k
  for (i in out_k) {
    lines(grid_min, working_mat_calib[i, ] + global_mean,
          col = alpha("firebrick", 0.85), lwd = 2)
  }
  
  # Banda della palla k
  polygon(c(grid_min, rev(grid_min)), c(upper_k, rev(lower_k)),
          col = alpha(km_colors[as.character(k)], 0.20), border = NA)
  lines(grid_min, upper_k, col = km_colors[as.character(k)], lwd = 2, lty = 2)
  lines(grid_min, lower_k, col = km_colors[as.character(k)], lwd = 2, lty = 2)
  
  # Centroide k ricostruito nello spazio funzionale
  lines(grid_min, as.numeric(global_mean + Phi %*% centers_train[k, ]),
        col = "black", lwd = 2.5)
  
  legend("topright",
         legend = c(sprintf("%.0f%% Band", BAND_COVERAGE * 100),
                    "Centroid curve",
                    sprintf("Inliers (n=%d)",  length(inlier_k)),
                    sprintf("Outliers (n=%d)", length(out_k))),
         col = c(km_colors[as.character(k)], "black",
                 km_colors[as.character(k)], "firebrick"),
         lwd = c(4, 2.5, 1.5, 2), bty = "n", cex = 0.75)
}
par(mfrow = c(1, 1))


# FINAL VISUALIZATION: ORIGINAL 24H CURVES COLORED BY CLUSTER -------------

cat("\n--- Plotting Original 24h Curves by Cluster (Combined) ---\n")

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


p_final_gmm <- plot_final_clusters_combined(matrix_smooth_calib_raw, 
                                            factor(best_k_gmm), 
                                            "GMM Clustering", 
                                            gmm_colors)

# 2. Generate plot for K-Means
p_final_km <- plot_final_clusters_combined(matrix_smooth_calib_raw, 
                                           factor(best_k_km), 
                                           "K-Means Clustering", 
                                           km_colors)

# Combine them vertically into a single visualization (2 rows, 1 column)
grid.arrange(p_final_gmm, p_final_km, ncol = 1)
