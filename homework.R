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
file_name <- "high_quality_data.csv"

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
# Initial Smoothing & Matrix Generation
# We create a preliminary daily_matrix to perform optimization later
# ==============================================================================

# Setting initial knots (set it smaller than the total amount of 
# datapoints per day to avoid overfitting)
K_init         <- 100
interior_knots <- seq(0.05, 0.95, length.out = K_init - 4)

df_bs <- df %>%
  group_by(date) %>%
  group_modify(~ {
    dati_giorno <- .x
    t_norm  <- dati_giorno$minute_of_day / 1440
    
    # Generate B-spline basis for observed data
    B_obs   <- bs(t_norm, knots = interior_knots, degree = 3,
                  intercept = TRUE, Boundary.knots = c(0, 1))
    
    # Fit linear model to estimate HR profile
    mod     <- lm(dati_giorno$HeartRate ~ B_obs - 1)
    
    # Predict HR over a regular 1-minute grid
    B_grid  <- bs(grid_min / 1440, knots = interior_knots, degree = 3,
                  intercept = TRUE, Boundary.knots = c(0, 1))
    
    tibble(minute_of_day = grid_min,
           HR = as.numeric(B_grid %*% coef(mod)))
  }) %>%
  ungroup()

# Reshape to wide format and convert to matrix
daily_matrix <- df_bs %>%
  pivot_wider(names_from = minute_of_day, values_from = HR) %>%
  dplyr::select(-date) %>%
  as.matrix()

# Check for mathematical errors (NA or Inf)
is_finite_row <- apply(daily_matrix, 1, function(x) all(is.finite(x)))

# Check for physiological errors in the generated splines
# This catches the splines that "dive" to 0 because of missing morning/evening data
no_spline_dives <- apply(daily_matrix, 1, function(x) all(x >= 30 & x <= 200, na.rm=TRUE))

keep_rows <- is_finite_row & no_spline_dives
daily_matrix <- daily_matrix[keep_rows, ]
dates        <- unique(df_bs$date)[keep_rows]

cat("\nFinal clean matrix size:", nrow(daily_matrix), "days\n")

# ==============================================================================
# Hyperparameter Optimization (GCV)
# Finding the optimal number of basis functions (K) and smoothing penalty (Lambda)
# ==============================================================================

t_vals  <- grid_min
y_vals  <- t(daily_matrix)
range_t <- c(0, 1440)

# Optimizing K
k_values <- seq(10, 100, by = 2)
gcv_k    <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  basis_temp  <- create.bspline.basis(rangeval = range_t, nbasis = k_values[i], norder = 4)
  smooth_temp <- smooth.basis(t_vals, y_vals, basis_temp)
  gcv_k[i]    <- sum(smooth_temp$gcv)
}

best_K_raw <- k_values[which.min(gcv_k)]
final_K    <- max(best_K_raw + 4, 25) 

p_k <- ggplot(data.frame(K = k_values, GCV = gcv_k), aes(K, GCV)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = best_K_raw, color = "red", linetype = "dashed") +
  coord_cartesian(ylim = c(min(gcv_k) * 0.95, min(gcv_k) * 20)) + 
  theme_minimal() +
  labs(title = "GCV Score by Number of Basis (K)", x = "K (Number of Basis)", y = "Total GCV")
# Optimizing Lambda
basis_final <- create.bspline.basis(rangeval = range_t, nbasis = final_K, norder = 4)
lambda_vals <- 10^seq(-4, 4, by = 0.5)
gcv_lambda  <- numeric(length(lambda_vals))

for (i in seq_along(lambda_vals)) {
  fdPar_obj   <- fdPar(basis_final, Lfdobj = 2, lambda = lambda_vals[i])
  smooth_pen  <- smooth.basis(t_vals, y_vals, fdPar_obj)
  gcv_lambda[i] <- sum(smooth_pen$gcv)
}

best_lambda <- lambda_vals[which.min(gcv_lambda)]

p_lam <- ggplot(data.frame(L = log10(lambda_vals), GCV = gcv_lambda), aes(L, GCV)) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = log10(best_lambda), color = "red", linetype = "dashed") +
  coord_cartesian(ylim = c(min(gcv_lambda), min(gcv_lambda) * 1.5)) + 
  theme_minimal() +
  labs(title = "GCV Score by Penalty (Lambda)", x = "log10(Lambda)", y = "Total GCV")

# Display optimization plots
grid.arrange(p_k, p_lam, ncol = 2)

# Final SMOOTH functional object
final_fdPar <- fdPar(basis_final, Lfdobj = 2, lambda = best_lambda)
fd_obj <- smooth.basis(t_vals, y_vals, final_fdPar)$fd
plot(fd_obj, lty=1, col="steelblue", main="Daily Heart Rate Curves (All Days)")

# ????????? 6. PCA & Clustering ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
cat("\n--- FASE 6: PCA Funzionale & Clustering ---\n")
pca_res <- pca.fd(fd_obj, nharm = 2)
scores <- pca_res$scores
colnames(scores) <- c("PC1", "PC2")
set.seed(123)
kmeans_res <- kmeans(scores, centers = 2)
cluster_data <- data.frame(Date = dates, scores, Cluster = factor(kmeans_res$cluster))
mean_c1 <- mean(daily_matrix[cluster_data$Cluster == 1, ], na.rm=T)
mean_c2 <- mean(daily_matrix[cluster_data$Cluster == 2, ], na.rm=T)
cluster_data$Label <- ifelse(cluster_data$Cluster == (if(mean_c1 > mean_c2) 1 else 2), "Sport/Active", "Rest/Normal")

p_pca <- ggplot(cluster_data, aes(x = PC1, y = PC2, color = Label)) +
  geom_point(size = 3) + stat_ellipse() + labs(title = "Clustering Sport vs Riposo") + theme_minimal()
print(p_pca)

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
# CONFIGURABLE PARAMETERS (merged & improved from both originals)
# ==============================================================================
VAR_THRESHOLD   <- 0.75     # cumulative variance for PC selection
N_PC_MANUAL     <- NULL     # NULL = auto | number to force
BAND_COVERAGE   <- 0.90     # conformal band coverage
MAX_K           <- 8
K_MANUAL        <- NULL     # NULL = auto (max silhouette)
N_SAMPLE_CURVES <- 43
SEED            <- 123

set.seed(SEED)

# ==============================================================================
# SHARED PHASE: GLOBAL CENTERING + DISCRETIZED FPCA
# ==============================================================================
cat("\n=== SHARED: GLOBAL CENTERING + FPCA (90% variance threshold) ===\n")

global_mean  <- colMeans(daily_matrix, na.rm = TRUE)
centered_mat <- sweep(daily_matrix, 2, global_mean, "-")

fpca_res  <- prcomp(centered_mat, center = FALSE, scale. = FALSE)
eigenvecs <- fpca_res$rotation
sdev_sq   <- fpca_res$sdev^2
cum_var   <- cumsum(sdev_sq) / sum(sdev_sq)

var_df <- data.frame(
  PC     = seq_along(cum_var),
  VarExp = sdev_sq / sum(sdev_sq) * 100,
  CumVar = cum_var * 100
)

# PC selection
if (!is.null(N_PC_MANUAL)) {
  n_pc <- as.integer(N_PC_MANUAL)
} else {
  n_pc <- which(cum_var >= VAR_THRESHOLD)[1]
  if (is.na(n_pc)) n_pc <- min(6, ncol(eigenvecs))
}

scores <- centered_mat %*% eigenvecs[, 1:n_pc]
colnames(scores) <- paste0("PC", seq_len(n_pc))

cat(sprintf("Selected PCs: %d | Explained variance: %.1f%%\n", n_pc, cum_var[n_pc]*100))

# Scree plot (best visual from KMEANS.R)
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

# GMM conformal (pseudo-density from Ferraty & Vieu / paper)
cat("\n--- GMM Conformal Prediction (pseudo-density alpha levels) ---\n")
dist_L2  <- dist(centered_mat)
h_param  <- quantile(as.matrix(dist_L2), 0.15)
kernel_gaussian <- function(u) (1 / sqrt(2 * pi)) * exp(-0.5 * u^2)

pseudo_density <- sapply(seq_len(nrow(centered_mat)), function(i) {
  mean(kernel_gaussian(as.matrix(dist_L2)[i, ] / h_param))
})

alpha_levels <- c(0.1, 0.5, 0.9)
par(mfrow = c(1, 3), mar = c(4,4,3,1))
for (a in alpha_levels) {
  idx_a <- which(pseudo_density >= quantile(pseudo_density, a))
  matplot(t(daily_matrix[idx_a, , drop = FALSE]), type = "l", lty = 1,
          col = scales::alpha("grey40", 0.3),
          main = paste("GMM Alpha =", a), ylab = "BPM", xlab = "Minutes")
  lines(colMeans(daily_matrix[idx_a, , drop = FALSE], na.rm = TRUE),
        col = "red", lwd = 2.5)
}

# Final all-curves plot (GMM style)
cat("\n--- GMM Final visualization (clusters + statistical outliers) ---\n")
outlier_idx   <- which(pseudo_density <= quantile(pseudo_density, 0.05))
colori_plot   <- ifelse(cluster_data_gmm$Label == "Active", "red",
                        scales::alpha("grey", 0.35))
spessori_plot <- ifelse(cluster_data_gmm$Label == "Active", 2, 1)
colori_plot[outlier_idx]   <- "darkred"
spessori_plot[outlier_idx] <- 3

# If you have fd_obj from fda package:
# plot(fd_obj, col = colori_plot, lwd = spessori_plot, lty = 1,
#      main = "GMM Result: Clustering + Outliers", xlab = "Minutes (0-1440)", ylab = "BPM")
# lines(mean.fd(fd_obj), col = "black", lwd = 3)
# legend("topleft", legend = c("Rest", "Active", "Outliers", "Mean"),
#        col = c("grey", "red", "darkred", "black"), lwd = c(1,3,3,3), bty = "n")

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

