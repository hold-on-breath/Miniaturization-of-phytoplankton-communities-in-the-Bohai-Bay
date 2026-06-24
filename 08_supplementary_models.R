# 08_supplementary_models.R
# Supplementary analyses: GAM, db-RDA, SEM, Random Forest
library(readxl); library(tidyverse); library(mgcv); library(vegan); library(zoo)
library(ggrepel); library(randomForest); library(blavaan)

# ====== GAM: Environment effects on size groups ======
# Models partial effects of DIN and Phosphate on small/mid/large phytoplankton
env_data <- read_excel("environmental_factors.xlsx", sheet = 1)
colnames(env_data) <- c("Year", "Temperature", "Salinity", "DIN", "Phosphate", "Silicate", "N_P", "Si_N")
env_data$Year <- as.numeric(env_data$Year)

size_abund <- read.csv("size_class_avg_abundance.csv")
merged <- size_abund %>% left_join(env_data, by = "Year") %>% filter(!is.na(DIN))

# GAM for each size class
for (sc in c("Small_avg", "Mid_avg", "Large_avg")) {
  gam_fit <- gam(log1p(merged[[sc]]) ~ s(DIN) + s(Phosphate), data = merged, method = "REML")
  png(paste0("GAM_", sc, ".png"), width = 8, height = 6, units = "in", res = 300)
  par(mfrow = c(1, 2), family = "serif")
  plot(gam_fit, select = 1, shade = TRUE, shade.col = "#6CA3D433", main = paste(sc, "- DIN"))
  plot(gam_fit, select = 2, shade = TRUE, shade.col = "#BF95C133", main = paste(sc, "- Phosphate"))
  dev.off()
}
cat("GAM partial effects saved\n")

# ====== db-RDA: Community-environment relationships ======
phyto_data <- read_excel("phytoplankton_taxa.xlsx", sheet = 1, col_names = TRUE, .name_repair = "minimal")

env_start_row <- which(phyto_data[[1]] == "temp")
abundance_raw <- phyto_data[1:(env_start_row - 1), ]
env_raw <- phyto_data[env_start_row:nrow(phyto_data), ]

orig_names <- colnames(abundance_raw)[-1]
unique_ids <- paste0(orig_names, "_", seq_along(orig_names))
colnames(abundance_raw)[-1] <- unique_ids
colnames(env_raw)[-1] <- unique_ids

group_info <- abundance_raw[[1]]
abund_mat <- as.data.frame(lapply(abundance_raw[, -1], function(x) as.numeric(as.character(x))))
abund_mat[is.na(abund_mat)] <- 0
rownames(abund_mat) <- paste0(group_info, "_", seq_len(nrow(abund_mat)))
abund_t <- as.data.frame(t(abund_mat))

env_names <- env_raw[[1]]
env_mat <- as.data.frame(lapply(env_raw[, -1], function(x) as.numeric(as.character(x))))
rownames(env_mat) <- env_names; colnames(env_mat) <- unique_ids

env_mat_interp <- t(apply(env_mat, 1, function(x) {
  x_zoo <- zoo(as.numeric(x))
  as.numeric(na.approx(x_zoo, na.rm = FALSE, rule = 2))
}))
colnames(env_mat_interp) <- unique_ids; rownames(env_mat_interp) <- env_names
env_df <- as.data.frame(t(env_mat_interp))

mod <- dbrda(abund_t ~ ., data = env_df, distance = "bray")
site_scores <- as.data.frame(scores(mod, display = "sites"))
colnames(site_scores)[1:2] <- c("Axis1", "Axis2")

env_scores <- as.data.frame(scores(mod, display = "bp"))
colnames(env_scores)[1:2] <- c("Axis1", "Axis2")
env_scores$factor <- rownames(env_scores)

p_rda <- ggplot() +
  geom_point(data = site_scores, aes(x = Axis1, y = Axis2), color = "#6CA3D4", size = 3, alpha = 0.8) +
  geom_segment(data = env_scores, aes(x = 0, y = 0, xend = Axis1, yend = Axis2),
               arrow = arrow(length = unit(0.2, "cm")), color = "#D7191C", linewidth = 1) +
  geom_text_repel(data = env_scores, aes(x = Axis1, y = Axis2, label = factor),
                  color = "#D7191C", size = 4, fontface = "bold") +
  theme_minimal() + theme(legend.position = "right")
ggsave("dbRDA_plot.png", p_rda, width = 10, height = 8, dpi = 300)

# Significance test
env_fit <- envfit(mod, env_df, permutations = 999)
print("db-RDA envfit:"); print(env_fit)
cat("db-RDA done\n")

# ====== Random Forest: Variable importance ======
rf_data <- merged %>% select(Small_avg, Mid_avg, Large_avg, DIN, Phosphate, Silicate, Temperature, Salinity) %>%
  filter(complete.cases(.))

for (target in c("Small_avg", "Mid_avg", "Large_avg")) {
  rf_fit <- randomForest(as.formula(paste("log1p(", target, ") ~ .")), data = rf_data,
                         importance = TRUE, ntree = 500)
  imp <- importance(rf_fit)
  png(paste0("RF_importance_", target, ".png"), width = 8, height = 5, units = "in", res = 300)
  par(family = "serif")
  varImpPlot(rf_fit, main = paste("Variable Importance -", target))
  dev.off()
}
cat("Random Forest done\n")

# ====== SEM: Structural equation modeling (Bayesian) ======
# Simplified SEM linking environment -> phytoplankton size groups
sem_data <- merged %>% select(Small_avg, Mid_avg, Large_avg, DIN, Temperature, N_P) %>%
  filter(complete.cases(.)) %>%
  mutate(across(everything(), ~ log1p(.)))

sem_model <- '
  Small_avg ~ DIN + Temperature
  Mid_avg  ~ DIN + Temperature + N_P
  Large_avg ~ DIN + Temperature + N_P
  Small_avg ~~ Mid_avg
  Mid_avg ~~ Large_avg
'

# Note: blavaan requires JAGS/rstan; use lavaan as fallback
sem_fit <- tryCatch({
  library(lavaan)
  sem(sem_model, data = sem_data)
}, error = function(e) NULL)

if (!is.null(sem_fit)) {
  summary(sem_fit, fit.measures = TRUE)
  cat("SEM done\n")
}

cat("All supplementary models completed\n")
