# 04_shap_analysis.R
# Fig. 6: SHAP analysis of environmental drivers for large- and small-sized phytoplankton assemblages
library(readxl); library(dplyr); library(tidyr); library(purrr)
library(xgboost); library(writexl); library(ggplot2); library(ggbeeswarm)

# ---- Setup ----
file_path <- "shap_input.xlsx"
out_dir <- dirname(file_path)
raw <- as.data.frame(read_excel(file_path, col_names = FALSE))

sample_time <- as.character(unlist(raw[1, 3:ncol(raw)]))
sample_time[is.na(sample_time) | sample_time == "NA" | sample_time == ""] <- paste0("sample_", seq_along(sample_time))
sample_id <- make.unique(sample_time, sep = "_")
all_data_rows <- raw[-1, , drop = FALSE]
col1 <- as.character(all_data_rows[, 1])
col2 <- as.character(all_data_rows[, 2])

size_keywords <- c("small-sized", "mid-sized", "large-sized")
env_keywords  <- c("temp", "PO4-P", "SiO4-Si", "DIN", "N/P", "Si/N")

is_species <- col2 %in% size_keywords
is_env     <- col2 %in% env_keywords
species_data_raw <- all_data_rows[is_species, , drop = FALSE]
env_data_raw     <- all_data_rows[is_env, , drop = FALSE]

# ---- Build environment data frame ----
env_mat <- as.data.frame(lapply(env_data_raw[, 3:ncol(env_data_raw), drop = FALSE],
  function(x) suppressWarnings(as.numeric(x))))
rownames(env_mat) <- as.character(env_data_raw[, 2])
colnames(env_mat) <- sample_id

env_df <- as.data.frame(t(as.matrix(env_mat)))
env_df$sample_id <- rownames(env_df)

keep_env <- names(env_df)[names(env_df) != "sample_id"]
keep_env <- keep_env[sapply(keep_env, function(f) mean(!is.na(env_df[[f]])) >= 0.15)]
env_df <- env_df[, c("sample_id", keep_env), drop = FALSE]

for (v in keep_env) {
  env_df[[v]] <- suppressWarnings(as.numeric(env_df[[v]]))
  env_df[[v]][is.na(env_df[[v]])] <- median(env_df[[v]], na.rm = TRUE)
  env_df[[v]] <- if (sd(env_df[[v]], na.rm = TRUE) == 0) 0 else as.numeric(scale(env_df[[v]]))
}

env_name_map <- data.frame(original = keep_env, clean = make.names(keep_env, unique = TRUE), stringsAsFactors = FALSE)
colnames(env_df)[match(env_name_map$original, colnames(env_df))] <- env_name_map$clean
env_vars_clean <- env_name_map$clean
clean_to_original <- setNames(env_name_map$original, env_name_map$clean)

# ---- Build species data ----
species_data_values <- species_data_raw[, 3:ncol(species_data_raw), drop = FALSE]
sp_col1 <- as.character(species_data_raw[, 1])
sp_col2 <- as.character(species_data_raw[, 2])
sp_labels <- ifelse(is.na(sp_col1) | sp_col1 == "", paste0(sp_col2, "_", seq_len(nrow(species_data_raw))), sp_col1)
sp_ids <- make.unique(sp_labels, sep = "_")
species_mat <- as.data.frame(lapply(species_data_values, function(x) suppressWarnings(as.numeric(x))))
rownames(species_mat) <- sp_ids; colnames(species_mat) <- sample_id

species_size_info <- data.frame(original_label = sp_labels, size_class = sp_col2, unique_id = sp_ids, stringsAsFactors = FALSE)

# ---- SHAP function ----
run_shap <- function(y_raw, sp_uid, sample_ids, env_df, env_vars_clean, clean_to_original, min_samples = 15, min_nonzero = 5) {
  dat_sp <- data.frame(sample_id = sample_ids, y = as.numeric(y_raw), stringsAsFactors = FALSE) %>%
    left_join(env_df, by = "sample_id") %>% filter(!is.na(y))
  if (nrow(dat_sp) < min_samples || sum(dat_sp$y > 0, na.rm = TRUE) < min_nonzero) return(NULL)
  if (sd(dat_sp$y, na.rm = TRUE) == 0) return(NULL)
  
  dat_sp$y_model <- log1p(dat_sp$y)
  X <- as.matrix(dat_sp[, env_vars_clean, drop = FALSE])
  y <- dat_sp$y_model
  
  keep_vars <- names(which(apply(X, 2, var, na.rm = TRUE) > 1e-8))
  if (length(keep_vars) < 2) return(NULL)
  X <- X[, keep_vars, drop = FALSE]
  
  dtrain <- xgb.DMatrix(data = X, label = y, missing = NA)
  params <- list(objective = "reg:squarederror", eval_metric = "rmse", eta = 0.05, max_depth = 3,
                 min_child_weight = 2, subsample = 0.8, colsample_bytree = 0.8)
  set.seed(123)
  
  cv_fit <- xgb.cv(params = params, data = dtrain, nrounds = 300, nfold = 5, early_stopping_rounds = 10, verbose = 0)
  best_nr <- ifelse(is.null(cv_fit$best_iteration) || cv_fit$best_iteration == 0, 50, cv_fit$best_iteration)
  
  model <- xgb.train(params = params, data = dtrain, nrounds = best_nr, verbose = 0)
  shap_mat <- predict(model, X, predcontrib = TRUE)
  shap_df <- as.data.frame(shap_mat)
  bias_col <- intersect(c("BIAS", "(Intercept)"), colnames(shap_df))
  if (length(bias_col) > 0) shap_df <- shap_df[, setdiff(colnames(shap_df), bias_col), drop = FALSE]
  
  mean_abs_shap <- colMeans(abs(shap_df), na.rm = TRUE)
  sp_info <- species_size_info[species_size_info$unique_id == sp_uid, , drop = FALSE]
  
  summary_out <- data.frame(unique_id = sp_uid, size_class = sp_info$size_class,
    factor_clean = names(mean_abs_shap), mean_abs_shap = as.numeric(mean_abs_shap),
    stringsAsFactors = FALSE) %>%
    mutate(factor = clean_to_original[factor_clean], rel_importance = mean_abs_shap / sum(mean_abs_shap))
  
  shap_long <- shap_df %>% mutate(sample_id = dat_sp$sample_id) %>%
    pivot_longer(all_of(colnames(shap_df)), names_to = "factor_clean", values_to = "shap_value") %>%
    mutate(factor = clean_to_original[factor_clean])
  
  X_long <- as.data.frame(X) %>% mutate(sample_id = dat_sp$sample_id) %>%
    pivot_longer(all_of(colnames(X)), names_to = "factor_clean", values_to = "feature_value") %>%
    mutate(factor = clean_to_original[factor_clean])
  
  details_out <- shap_long %>% left_join(X_long, by = c("sample_id", "factor")) %>%
    mutate(unique_id = sp_uid, size_class = sp_info$size_class)
  
  return(list(summary = summary_out, details = details_out))
}

# ---- Run SHAP for all species ----
shap_summary_list <- list(); shap_details_list <- list()
for (sp_id in rownames(species_mat)) {
  res <- run_shap(as.numeric(species_mat[sp_id, ]), sp_id, colnames(species_mat),
                  env_df, env_vars_clean, clean_to_original)
  if (!is.null(res)) {
    shap_summary_list[[sp_id]] <- res$summary
    shap_details_list[[sp_id]] <- res$details
  }
}
shap_summary <- bind_rows(shap_summary_list)
shap_details <- bind_rows(shap_details_list)

# ---- Aggregate by size class ----
shap_group <- shap_summary %>%
  group_by(size_class, factor) %>%
  summarise(mean_abs_shap = mean(mean_abs_shap, na.rm = TRUE), .groups = "drop") %>%
  group_by(size_class) %>% arrange(size_class, desc(mean_abs_shap)) %>% ungroup()

# ---- Generate integrated SHAP plots (Fig 6) ----
for (size in unique(shap_group$size_class)) {
  bar_data <- shap_group %>% filter(size_class == size) %>%
    arrange(mean_abs_shap) %>% mutate(feature = factor(factor, levels = factor))
  
  feature_order <- levels(bar_data$feature)
  details_sub <- shap_details %>% filter(size_class == size) %>%
    mutate(feature = factor(factor, levels = feature_order))
  
  if (nrow(bar_data) == 0 || nrow(details_sub) == 0) next
  
  shap_lim <- max(abs(details_sub$shap_value), na.rm = TRUE) * 1.08
  shap_min <- -shap_lim; shap_max <- shap_lim
  bar_max <- max(bar_data$mean_abs_shap, na.rm = TRUE)
  
  map_to_shap <- function(x, smin, smax, bmax) { smin + (x / bmax) * (smax - smin) }
  bar_data <- bar_data %>% mutate(x_bar = map_to_shap(mean_abs_shap, shap_min, shap_max, bar_max))
  
  p <- ggplot() +
    geom_segment(data = bar_data, aes(x = shap_min, xend = x_bar, y = feature, yend = feature),
                 linewidth = 12, color = "#6CA3D4", alpha = 0.35, lineend = "butt") +
    geom_quasirandom(data = details_sub, aes(x = shap_value, y = feature, color = feature_value),
                     groupOnX = FALSE, width = 0.28, size = 2.0, alpha = 0.65) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    scale_x_continuous(name = "SHAP value (impact on model output)", limits = c(shap_min, shap_max),
      sec.axis = sec_axis(trans = ~ (. - shap_min) / (shap_max - shap_min) * bar_max,
                          name = "Mean Absolute SHAP Value", breaks = pretty(c(0, bar_max), n = 6))) +
    scale_color_gradient(low = "#2C7FB8", high = "#D7191C", name = "Feature value") +
    labs(title = NULL, y = NULL) +
    theme_bw(base_size = 22) + theme(
      text = element_text(family = "Times New Roman"), plot.title = element_blank(),
      axis.text.y = element_text(size = 20, color = "black"), axis.title.y = element_blank(),
      axis.text.x.bottom = element_text(size = 18, color = "black"),
      axis.title.x.bottom = element_text(size = 20, margin = margin(t = 8)),
      axis.text.x.top = element_text(size = 18, color = "black"),
      axis.title.x.top = element_text(size = 20, margin = margin(b = 8)),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey85"),
      legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
      legend.direction = "horizontal", legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      legend.background = element_rect(fill = "white", colour = "gray80", linewidth = 0.5),
      panel.border = element_rect(linewidth = 1.5, colour = "black"))
  
  ggsave(file.path(out_dir, paste0("Fig6_SHAP_", size, ".png")), p, width = 10, height = 5, dpi = 300)
}

# ---- Export results ----
write_xlsx(list(SHAP_importance = shap_group, Details = shap_summary, Sample_level = shap_details),
           path = file.path(out_dir, "SHAP_results.xlsx"))
cat("Done: Fig 6 SHAP plots saved\n")
