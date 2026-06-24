# 03_esd_carbon_flux.R
# Fig. 5: Interannual variation in ESD and RSCVF (Equivalent Spherical Diameter + Real Specific Carbon Vertical Flux)
library(readxl); library(tidyverse); library(mgcv); library(patchwork)

# ---- Load data ----
df_esd <- read_excel("esd_rscvf_data.xlsx", sheet = "esd", col_names = TRUE) %>%
  pivot_longer(everything(), names_to = "year", values_to = "esd") %>%
  mutate(year = as.numeric(year), esd = as.numeric(esd)) %>% filter(!is.na(esd))

df_rscvf <- read_excel("esd_rscvf_data.xlsx", sheet = "RSCVF", col_names = TRUE) %>%
  pivot_longer(everything(), names_to = "year", values_to = "rscvf") %>%
  mutate(year = as.numeric(year), rscvf = as.numeric(rscvf)) %>% filter(!is.na(rscvf))

df_avg_raw <- read_excel("esd_rscvf_data.xlsx", sheet = "Average_RSCVF", col_names = FALSE)
df_avg <- data.frame(
  year = as.numeric(df_avg_raw[1, ]),
  mean = as.numeric(df_avg_raw[2, ]),
  sd   = as.numeric(df_avg_raw[3, ])) %>% filter(!is.na(mean) & !is.na(sd))

all_years <- sort(unique(c(df_esd$year, df_rscvf$year, df_avg$year)))

# ---- Common theme ----
base_theme <- theme_minimal() + theme(
  text = element_text(family = "serif", face = "bold"),
  plot.title = element_blank(), axis.title.x = element_blank(),
  axis.title.y = element_text(family = "serif", face = "bold", size = 14),
  axis.text = element_text(family = "serif", face = "bold", size = 12),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  panel.grid.major = element_line(color = "gray85", linewidth = 0.4),
  panel.grid.minor = element_blank(), legend.position = "none")

# ---- Panel A: ESD boxplot + GAM trend ----
gam_esd <- gam(esd ~ s(year), data = df_esd)
new_esd <- data.frame(year = seq(min(df_esd$year), max(df_esd$year), length.out = 200))
pred_esd <- predict(gam_esd, newdata = new_esd, se.fit = TRUE)
new_esd$pred <- pred_esd$fit
new_esd$lower <- pred_esd$fit - 1.96 * pred_esd$se.fit
new_esd$upper <- pred_esd$fit + 1.96 * pred_esd$se.fit

p_esd <- ggplot(df_esd, aes(x = year, y = esd)) +
  geom_boxplot(aes(group = year), fill = "#6CA3D4", outlier.color = "darkgray", alpha = 0.7, linewidth = 0.8) +
  geom_jitter(color = "#BF95C1", size = 1.5, alpha = 0.6, width = 0.2) +
  geom_ribbon(data = new_esd, aes(x = year, ymin = lower, ymax = upper), fill = "#61C1BF", alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = new_esd, aes(x = year, y = pred), color = "#61C1BF", linewidth = 1.2) +
  labs(y = expression(ESD~(mu*m))) +
  scale_x_continuous(breaks = all_years, labels = all_years) + base_theme + theme(axis.text.x = element_blank())

# ---- Panel B: RSCVF boxplot + GAM trend ----
gam_rscvf <- gam(rscvf ~ s(year), data = df_rscvf)
new_rscvf <- data.frame(year = seq(min(df_rscvf$year), max(df_rscvf$year), length.out = 200))
pred_rscvf <- predict(gam_rscvf, newdata = new_rscvf, se.fit = TRUE)
new_rscvf$pred <- pred_rscvf$fit
new_rscvf$lower <- pred_rscvf$fit - 1.96 * pred_rscvf$se.fit
new_rscvf$upper <- pred_rscvf$fit + 1.96 * pred_rscvf$se.fit

p_rscvf <- ggplot(df_rscvf, aes(x = year, y = rscvf)) +
  geom_boxplot(aes(group = year), fill = "#6CA3D4", outlier.color = "darkgray", alpha = 0.7, linewidth = 0.8) +
  geom_jitter(color = "#BF95C1", size = 1.5, alpha = 0.6, width = 0.2) +
  geom_ribbon(data = new_rscvf, aes(x = year, ymin = lower, ymax = upper), fill = "#61C1BF", alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = new_rscvf, aes(x = year, y = pred), color = "#61C1BF", linewidth = 1.2) +
  labs(y = expression(RSCVF~(mgC~m^{-2}~d^{-1}))) +
  scale_x_continuous(breaks = all_years, labels = all_years) + base_theme + theme(axis.text.x = element_blank())

# ---- Panel C: Average RSCVF with linear trend ----
lm_avg <- lm(mean ~ year, data = df_avg)
r2_avg <- round(summary(lm_avg)$r.squared, 3)

p_avg <- ggplot(df_avg, aes(x = year)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, linewidth = 0.8, color = "#6CA3D4") +
  geom_point(aes(y = mean), size = 4, color = "#BF95C1") +
  geom_smooth(aes(y = mean), method = "lm", se = TRUE, fill = "#61C1BF", color = "#61C1BF",
              alpha = 0.2, linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = max(df_avg$year), y = max(df_avg$mean + df_avg$sd),
           label = paste("R\u00b2 =", r2_avg), hjust = 1, vjust = 1,
           family = "serif", fontface = "bold", size = 12 / .pt, color = "black") +
  labs(y = expression(Average~RSCVF~(mgC~m^{-2}~d^{-1}))) +
  scale_x_continuous(breaks = all_years, labels = all_years) + base_theme

# ---- Combine and save ----
combined <- p_esd / p_rscvf / p_avg + plot_layout(heights = c(1, 1, 1))
ggsave("Fig5_ESD_RSCVF.png", combined, width = 9, height = 9, dpi = 300)
cat("Done: Fig 5 (A-C panels) saved\n")
