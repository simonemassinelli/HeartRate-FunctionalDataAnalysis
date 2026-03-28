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
# Dataset upload and ""feature engineering""
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
# Data Cleaning (Filtering by quantity and physiological validity)
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

# Coversion of categorical features from numeric to categorical

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
# 195 while sleeping.... what a nightmare...don't touch my family's tractor!

cat("\nHeart Rate per SleepStatus:\n")
print(tapply(df$HeartRate, df$SleepStatus, summary))

cat("\nHeart Rate per MotionContext:\n")
print(tapply(df$HeartRate, df$MotionContext, summary))

# Distribution and box-plot Heart Rate
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

# Daily Heatmap HR
df_heat <- df %>%
  mutate(time_bin = floor(minute_of_day / 30) * 30) %>%
  group_by(day_id, date, time_bin) %>%
  summarise(avg_HR = mean(HeartRate, na.rm = TRUE), .groups = "drop")

p_heatmap <- ggplot(df_heat,
                    aes(x = time_bin, y = factor(day_id), fill = avg_HR)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#313695", "#ffffbf", "#a50026"), name = "HR") +
  scale_x_continuous(breaks = seq(0, 1440, by = 180),
                     labels = seq(0, 1440, by = 180)) +
  labs(title = "Heatmap Intensità HR Giornaliera",
       x = "Minuti del giorno", y = "ID Giorno") +
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
  scale_x_continuous(breaks = seq(0, 1440, by = 180),
                     labels = seq(0, 1440, by = 180)) +
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
  scale_x_continuous(breaks = seq(0, 1440, by = 180),
                     labels = seq(0, 1440, by = 180)) +
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
  scale_x_continuous(breaks = seq(0, 1440, by = 180),
                     labels = seq(0, 1440, by = 180)) +
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
  scale_x_continuous(breaks = seq(0, 1440, by = 180),
                     labels = seq(0, 1440, by = 180)) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "HR median by time bin and Sleep Stage",
       x = "Minutes of the day", y = "HR median (bpm)", colour = "Stage") +
  theme_minimal()

print(p_hr_stage_line)

# ==============================================================================
# OPTIMIZED B-SPLINE PARAMETERS (Penalized Least Squares Approach)
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

# Grid search
k_values    <- seq(30, 200, 5)  
lambda_vals <- 10^seq(-4, 4, length.out = 20)

gcv_mat <- outer(k_values, lambda_vals, Vectorize(compute_avg_gcv))

# Identify optimal parameters
best_idx    <- which(gcv_mat == min(gcv_mat, na.rm = TRUE), arr.ind = TRUE)
best_K      <- k_values[best_idx[1]]
best_lambda <- lambda_vals[best_idx[2]]

cat(sprintf("\n=== OPTIMAL PARAMETERS (GCV) ===\nK (nbasis) = %d\nlambda     = %.2e\n", 
            best_K, best_lambda))

# ==============================================================================
# PLOTS
# ==============================================================================

# GCV curve for K (evaluated at the optimal lambda)
gcv_vs_k <- apply(gcv_mat, 1, min)

p_gcv_k <- ggplot(data.frame(K = k_values, GCV = gcv_vs_k), aes(K, GCV)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = best_K, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "GCV vs Number of Basis Functions (K)",
       subtitle = paste("Optimal K =", best_K),
       x = "K (nbasis)", y = "Average GCV") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# GCV curve for lambda (evaluated at the optimal K)
idx_k <- which(k_values == best_K)
gcv_vs_lam <- gcv_mat[idx_k, ]

p_gcv_lam <- ggplot(data.frame(Lambda = lambda_vals, GCV = gcv_vs_lam), 
                    aes(log10(Lambda), GCV)) +
  geom_line(color = "darkgreen", linewidth = 1.1) +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = log10(best_lambda), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "GCV vs Smoothing Penalty (λ)",
       subtitle = paste("Optimal log10(λ) =", round(log10(best_lambda), 2)),
       x = "log10(λ)", y = "Average GCV") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

grid.arrange(p_gcv_k, p_gcv_lam, ncol = 2)

# this method doesn't give us any good result, so we choose k = 50
# for simplicity because it would fit (with a poly of degree 3 still)
# each 30 mins (something about)

# ==============================================================================
# OPTIMIZATION FOR LAMBDA (Given fixed K = 50)
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

# Initialize matrix to store evaluated curves and a vector for valid dates
daily_matrix <- matrix(NA_real_, nrow = length(days_list), ncol = length(grid_min))
good_dates   <- character(length(days_list))
row_i <- 1

# Smooth each day individually using the optimal parameters
for (dati in days_list) {
  date_str <- as.character(dati$date[1])
  
  # Skip days with insufficient raw data points
  if (nrow(dati) < 30) next
  
  # Fit the smoothing spline directly to the raw, irregularly spaced data
  sm <- tryCatch(
    smooth.basis(argvals = dati$minute_of_day, 
                 y       = dati$HeartRate, 
                 fdParobj = final_fdPar), 
    error = function(e) NULL
  )
  
  if (is.null(sm)) next
  
  # Evaluate the smoothed curve onto the regular 1-minute grid
  hr_grid <- as.vector(eval.fd(grid_min, sm$fd))
  
  # Strict physiological filter to catch edge effects (spline dives)
  if (all(hr_grid >= 35 & hr_grid <= 215, na.rm = TRUE)) {
    daily_matrix[row_i, ] <- hr_grid
    good_dates[row_i]     <- date_str
    row_i <- row_i + 1
  }
}

# Truncate the pre-allocated matrix and vector to keep only the valid days
if (row_i > 1) {
  daily_matrix <- daily_matrix[1:(row_i - 1), , drop = FALSE]
  good_dates   <- good_dates[1:(row_i - 1)]
} else {
  stop("Error: No valid days remained after applying the physiological filter.")
}

cat("\nFinal clean functional matrix:", nrow(daily_matrix), "days\n")

# Bundle the clean matrix into a single functional data object (fd)
# This prepares the data for downstream tasks like FPCA or conformal prediction
fd_obj <- smooth.basis(argvals = grid_min, 
                       y        = t(daily_matrix), 
                       fdParobj = final_fdPar)$fd

# ==============================================================================
# VISUALIZATION
# ==============================================================================
plot(fd_obj, lty = 1, col = 'blue',
     main = "Optimized Daily HR Curves (B-spline + GCV)",
     xlab = "Minute of the Day", ylab = "Heart Rate (bpm)")

# ????????? 7. CONFORMAL PREDICTION: PSEUDO-DENSIT? & ALPHA ANALYSIS ??????????????????????????????????????????
cat("\n--- FASE 7: Conformal Prediction (Level Sets per Alpha) ---\n")

# 1. Calcolo distanze sulle curve ottimizzate (Smooth Matrix)
# Valutiamo l'oggetto fd_obj sulla griglia per avere le curve 'pulite' dalla GCV
matrix_smooth <- eval.fd(grid_min, fd_obj) %>% t()
dist_matrix <- as.matrix(dist(matrix_smooth))

# 2. Stima della Pseudo-Densit? (Kernel Density Estimation - Sezione 4 del Paper)
# h_param (bandwidth) determina quanto siamo 'locali' nel misurare la densit?
h_param <- quantile(dist_matrix, 0.15) 
kernel_gaussian <- function(u) { (1 / sqrt(2 * pi)) * exp(-0.5 * u^2) }

pseudo_density <- numeric(nrow(matrix_smooth))
for(i in 1:nrow(matrix_smooth)) {
  pseudo_density[i] <- mean(kernel_gaussian(dist_matrix[i, ] / h_param))
}

# 3. Grafici Level Sets per diversi valori di Alpha (0.1, 0.5, 0.9)
# Teoria: Alpha ? il livello di errore. Un Alpha = 0.1 indica un insieme di 
# confidenza al 90%. Pi? Alpha cresce, pi? il set si stringe sulle curve "tipiche".

alpha_levels <- c(0.1, 0.5, 0.9)
par(mfrow = c(1, 3)) # Prepariamo il pannello per 3 grafici affiancati

for(alpha in alpha_levels) {
  # Calcoliamo il valore soglia di densit? (Lambda) per il livello alpha scelto
  lambda_alpha <- quantile(pseudo_density, alpha)
  
  # Selezioniamo solo le curve la cui densit? ? maggiore o uguale alla soglia
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

# Reset del layout grafico per i passaggi successivi
par(mfrow = c(1, 1))

# ????????? 8. IDENTIFICAZIONE AUTOMATICA & PLOT OUTLIERS (CRITERIO STATISTICO) ???????????????
cat("\n--- FASE 8: Rilevamento Anomalie tramite Soglia di Confidenza ---\n")

# CRITERIO: Definiamo come outlier il 5% delle osservazioni con la densit? pi? bassa.
# Questo alpha_outlier (0.05) ? il nostro "p-value" funzionale.
alpha_outlier <- 0.05
soglia_densita <- quantile(pseudo_density, alpha_outlier)

# Identifichiamo quali e quanti sono gli outlier
outlier_indices <- which(pseudo_density <= soglia_densita)
n_rilevati <- length(outlier_indices)
outlier_dates <- dates[outlier_indices]

cat(sprintf("Soglia di densit? calcolata: %.6f\n", soglia_densita))
cat(sprintf("Numero di outlier rilevati (alpha=%.2f): %d\n", alpha_outlier, n_rilevati))

# Prepariamo i colori: una palette per gli outlier, grigio per le normali
# Usiamo rainbow o heat.colors se sono molti, cos? da distinguerli
colori_outlier <- rainbow(n_rilevati)
colori_plot <- rep(alpha("grey", 0.3), length(dates))
spessori_plot <- rep(1, length(dates))

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

# ========= PLOT: FERIALE vs WEEKEND =========
cat("\n--- FASE 8 (mod): Confronto Funzionale Feriale vs Weekend ---\n")

# Identifichiamo il tipo di giorno per ogni data
day_of_week <- weekdays(as.Date(dates))  # Nomi dei giorni
is_weekend  <- day_of_week %in% c("Saturday", "Sunday",
                                  "sabato", "domenica")  # compatibile con locale IT/EN

n_weekend   <- sum(is_weekend)
n_feriale   <- sum(!is_weekend)

cat(sprintf("Giorni feriali: %d | Giorni weekend: %d\n", n_feriale, n_weekend))

# Palette colori
col_feriale <- alpha("steelblue", 0.35)
col_weekend <- alpha("firebrick", 0.55)

colori_plot   <- ifelse(is_weekend, col_weekend, col_feriale)
spessori_plot <- ifelse(is_weekend, 1.5, 1)

# PLOT PRINCIPALE
plot(fd_obj,
     col  = colori_plot,
     lwd  = spessori_plot,
     lty  = 1,
     main = "Profili Funzionali: Giorni Feriali vs Weekend",
     xlab = "Minuti (0-1440)",
     ylab = "BPM")

# Media per feriali
mean_feriale <- mean.fd(fd_obj[which(!is_weekend)])
lines(mean_feriale, col = "blue", lwd = 3, lty = 2)

# Media per weekend
mean_weekend <- mean.fd(fd_obj[which(is_weekend)])
lines(mean_weekend, col = "red", lwd = 3, lty = 2)

# Media globale
lines(mean.fd(fd_obj), col = "black", lwd = 3, lty = 1)

# Legenda
legend("topleft",
       legend = c(
         paste0("Feriale (n=", n_feriale, ")"),
         paste0("Weekend (n=", n_weekend, ")"),
         "Media Feriale",
         "Media Weekend",
         "Media Globale"
       ),
       col = c(col_feriale, col_weekend, "blue", "red", "black"),
       lwd = c(1, 1.5, 3, 3, 3),
       lty = c(1, 1, 2, 2, 1),
       bty = "n",
       cex = 0.7)
#importante sport pomeridiano tanto nel weekend e ovviamente media vita notturna 





# ==============================================================================
# CONFIGURABLE PARAMETERS (aggiornate)
# ==============================================================================
VAR_THRESHOLD   <- 0.6     # cumulative variance for PC selection
N_PC_MANUAL     <- NULL     # NULL = auto
BAND_COVERAGE   <- 0.90
MAX_K           <- 4
K_MANUAL        <- NULL
N_SAMPLE_CURVES <- 43
SEED            <- 123

CENTERING       <- TRUE     # Global functional centering (consigliato)
STANDARDIZE     <- FALSE     # NEW: Global functional standardization (divide by sd curve)

set.seed(SEED)

# ==============================================================================
# SHARED PHASE: FPCA sulle funzioni B-spline (fd_obj) con centramento + standardizzazione
# ==============================================================================
cat("\n=== SHARED: FPCA on B-spline fd_obj (centering + optional standardization) ===\n")

# 1. Valutiamo l'oggetto funzionale fd_obj sulla griglia → matrice smoothed pulita
matrix_smooth <- t(eval.fd(grid_min, fd_obj))   # n_days x n_grid  (stessa forma di daily_matrix)

# 2. Calcoliamo mean e sd globali (sempre, per permettere la ricostruzione)
global_mean <- colMeans(matrix_smooth, na.rm = TRUE)
global_sd   <- apply(matrix_smooth, 2, sd, na.rm = TRUE)
global_sd[global_sd < 1e-6] <- 1   # safeguard contro sd=0

# 3. Applichiamo le trasformazioni desiderate
working_mat <- matrix_smooth
if (CENTERING) {
  working_mat <- sweep(working_mat, 2, global_mean, "-")
  cat("→ Global centering applied\n")
}
if (STANDARDIZE) {
  working_mat <- sweep(working_mat, 2, global_sd, "/")
  cat("→ Global standardization applied (divided by pointwise sd)\n")
}

# 4. FPCA discreta sulla matrice processata (working_mat)
fpca_res  <- prcomp(working_mat, center = FALSE, scale. = FALSE)
eigenvecs <- fpca_res$rotation
sdev_sq   <- fpca_res$sdev^2
cum_var   <- cumsum(sdev_sq) / sum(sdev_sq)

var_df <- data.frame(
  PC     = seq_along(cum_var),
  VarExp = sdev_sq / sum(sdev_sq) * 100,
  CumVar = cum_var * 100
)

# Selezione numero di PC
if (!is.null(N_PC_MANUAL)) {
  n_pc <- as.integer(N_PC_MANUAL)
} else {
  n_pc <- which(cum_var >= VAR_THRESHOLD)[1]
  if (is.na(n_pc)) n_pc <- min(6, ncol(eigenvecs))
}

scores <- fpca_res$x[, 1:n_pc]          # scores già calcolati da prcomp
colnames(scores) <- paste0("PC", seq_len(n_pc))

cat(sprintf("Selected PCs: %d | Explained variance: %.1f%%\n", n_pc, cum_var[n_pc]*100))

# Scree plot (invariato)
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
  labs(title = "Scree plot + Cumulative variance", x = "Principal component") +
  theme_minimal(base_size = 13)
print(p_scree)







# ==============================================================================
# STRATEGY 1: GMM CLUSTERING (pure data-driven + pseudo-density conformal)
# ==============================================================================
cat("\n=== STRATEGY 1: GMM CLUSTERING ===\n")

bic_selection <- mclustBIC(scores)
gmm_fit <- Mclust(scores, x = bic_selection)
G_opt   <- gmm_fit$G
res_class <- gmm_fit$classification

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

# Uniform PCA scatter (KMEANS style + uncertainty size from GMM)
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

# Peak distribution (kept from GMM, uniform theme)
p_peak_gmm <- ggplot(cluster_data_gmm, aes(x = PC1, y = MaxHR, color = Label)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Active" = "orange", "Rest" = "steelblue")) +
  labs(title = "Peak HR distribution by cluster", y = "Max BPM") +
  theme_minimal(base_size = 14)
print(p_peak_gmm)

# Uniform dendrogram (ggplot style from KMEANS.R)
hc_gmm      <- hclust(dist(scores), method = "ward.D2")
dend_df_gmm <- dendro_data(hc_gmm, type = "rectangle")
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



# ==============================================================================
# STRATEGY 2: K-MEANS CLUSTERING (silhouette-driven + conformal bands)
# ==============================================================================
cat("\n=== STRATEGY 2: K-MEANS CLUSTERING ===\n")

max_k   <- min(MAX_K, nrow(scores) - 1)
k_range <- 2:max_k

sil_width <- sapply(k_range, function(k) {
  km <- kmeans(scores, centers = k, nstart = 25, iter.max = 50)
  mean(silhouette(km$cluster, dist(scores))[, 3])
})
wss <- sapply(k_range, function(k) {
  kmeans(scores, centers = k, nstart = 25, iter.max = 50)$tot.withinss
})

optimal_k <- if (!is.null(K_MANUAL)) as.integer(K_MANUAL) else k_range[which.max(sil_width)]

cat(sprintf("K-Means optimal k: %d\n", optimal_k))

# Silhouette + Elbow (only for K-Means)
p_sil <- ggplot(data.frame(k = k_range, silhouette = sil_width),
                aes(x = k, y = silhouette)) +
  geom_line(color = "steelblue") + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick") +
  labs(title = "Silhouette", y = "Average width", x = "k") + theme_minimal(base_size = 13)

p_elbow <- ggplot(data.frame(k = k_range, wss = wss),
                  aes(x = k, y = wss)) +
  geom_line(color = "steelblue") + geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick") +
  labs(title = "Elbow (WSS)", y = "Within sum of squares", x = "k") + theme_minimal(base_size = 13)
print(p_sil + p_elbow)

# Final clustering
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

# Uniform PCA scatter (same code as GMM but without uncertainty sizing)
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

# Uniform dendrogram (identical style)
hc_km      <- hclust(dist(scores), method = "ward.D2")
dend_df_km <- dendro_data(hc_km, type = "rectangle")
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
print(p_dend_km)

# K-Means conformal prediction bands (best visual from KMEANS.R)
cat("\n--- K-Means Conformal Prediction Bands ---\n")
reconstructed    <- scores %*% t(eigenvecs[, 1:n_pc])
approx_functions <- sweep(reconstructed, 2, global_mean, "+")

nonconf_scores <- sqrt(rowSums(scores^2))          # conformity = distance from origin in PC space
alpha          <- 1 - BAND_COVERAGE
threshold      <- quantile(nonconf_scores, 1 - alpha, type = 1)
in_band        <- nonconf_scores <= threshold

mat_in <- approx_functions[in_band, , drop = FALSE]

band_df <- data.frame(
  Time       = seq_len(ncol(daily_matrix)),
  Mean       = colMeans(approx_functions, na.rm = TRUE),
  OuterLower = apply(mat_in, 2, quantile, probs = alpha/2, na.rm = TRUE),
  OuterUpper = apply(mat_in, 2, quantile, probs = 1 - alpha/2, na.rm = TRUE),
  IQRLower   = apply(mat_in, 2, quantile, probs = 0.25, na.rm = TRUE),
  IQRUpper   = apply(mat_in, 2, quantile, probs = 0.75, na.rm = TRUE),
  Median     = apply(mat_in, 2, median, na.rm = TRUE)
)

time_grid <- seq_len(ncol(daily_matrix))
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
  labs(title = sprintf("Conformal Prediction Bands %d%% (K-Means)", round(BAND_COVERAGE*100)),
       subtitle = sprintf("n_pc = %d | alpha = %.2f", n_pc, alpha),
       x = "Minutes", y = "BPM",
       caption = "Dark green = median | Dashed blue = mean | Light green = IQR | Red = conformal band") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
print(p_bands)

