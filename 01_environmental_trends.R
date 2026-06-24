# 01_environmental_trends.R
# Fig. 2: Long-term trends in temperature, salinity, nutrients, and stoichiometric ratios
# Fig. S2: Chl-a interannual variation
library(readxl); library(ggplot2); library(dplyr); library(tidyr); library(patchwork)

# ---- Load data (skip header + units rows) ----
df <- read_excel("environmental_time_series.xlsx", sheet = 1, skip = 2, col_names = FALSE)
colnames(df) <- c("Year", "Temperature", "Salinity", "Nitrite", "Ammonium",
                  "Nitrate", "DIN", "Phosphate", "Silicate", "N_P", "Si_N", "Chl_a")
df <- df %>% filter(Year >= 2000 & Year <= 2020)

# ---- Common theme ----
my_theme <- theme_minimal(base_size = 14) + theme(
  text = element_text(family = "serif"),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
  axis.text = element_text(size = 16, face = "bold", family = "serif"),
  plot.margin = margin(10, 10, 10, 10),
  axis.title = element_blank()
)

# ========== Panel A: Temperature & Salinity ==========
df_ts <- df %>% select(Year, Temperature, Salinity) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>% drop_na(Value)
df_ts$Variable <- factor(df_ts$Variable, levels = c("Temperature", "Salinity"))

r2_t <- round(summary(lm(Value ~ Year, df_ts %>% filter(Variable == "Temperature")))$r.squared, 3)
r2_s <- round(summary(lm(Value ~ Year, df_ts %>% filter(Variable == "Salinity")))$r.squared, 3)
cols_ts <- c("Temperature" = "#6CA3D4", "Salinity" = "#BF95C1")
ymax_ts <- max(df_ts$Value, na.rm = TRUE)

pA <- ggplot(df_ts, aes(x = Year, y = Value, color = Variable)) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 0.8, aes(fill = Variable)) +
  annotate("text", x = 2014, y = ymax_ts * 0.99,
           label = paste("Temperature R² =", r2_t), hjust = 1, family = "serif", size = 5, fontface = "bold", color = cols_ts[1]) +
  annotate("text", x = 2014, y = ymax_ts * 0.97,
           label = paste("Salinity R² =", r2_s), hjust = 1, family = "serif", size = 5, fontface = "bold", color = cols_ts[2]) +
  scale_color_manual(values = cols_ts, labels = c("Temperature (\u2103)", "Salinity")) +
  scale_fill_manual(values = cols_ts, guide = "none") +
  my_theme + theme(
    legend.position = c(0.95, 0.93), legend.justification = c(1, 1),
    legend.direction = "vertical", legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 16, family = "serif")
  )
ggsave("Fig2_Temperature_Salinity.png", pA, width = 12, height = 4, dpi = 300)

# ========== Panel B-D: DIN, PO4-P, SiO4-Si ==========
df_nut <- df %>% select(Year, DIN, Phosphate, Silicate) %>%
  rename(PO4_P = Phosphate, SiO4_Si = Silicate) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>% drop_na(Value)

cols_nut <- c("DIN" = "#6CA3D4", "PO4_P" = "#BF95C1", "SiO4_Si" = "#61C1BF")
r2_nut <- df_nut %>% group_by(Variable) %>%
  summarise(r2 = round(summary(lm(Value ~ Year))$r.squared, 3))
y_max_nut <- df_nut %>% group_by(Variable) %>% summarise(y_max = max(Value, na.rm = TRUE))

make_nut_plot <- function(var_name, var_color, r2_val, y_max) {
  data_var <- df_nut %>% filter(Variable == var_name)
  ggplot(data_var, aes(x = Year, y = Value)) +
    geom_line(color = var_color, linewidth = 1) + geom_point(color = var_color, size = 3) +
    geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 0.8, color = var_color, fill = var_color) +
    annotate("text", x = 2010, y = y_max * 0.99, label = paste0(var_name, " (\u00b5mol/L)"),
             hjust = 0, family = "serif", size = 5, fontface = "bold", color = var_color) +
    annotate("text", x = 2015, y = y_max * 0.90, label = paste("R² =", r2_val),
             hjust = 1, family = "serif", size = 5, fontface = "bold", color = var_color) +
    my_theme
}

pB <- make_nut_plot("DIN", cols_nut["DIN"], r2_nut$r2[r2_nut$Variable == "DIN"], y_max_nut$y_max[y_max_nut$Variable == "DIN"])
pC <- make_nut_plot("PO4_P", cols_nut["PO4_P"], r2_nut$r2[r2_nut$Variable == "PO4_P"], y_max_nut$y_max[y_max_nut$Variable == "PO4_P"])
pD <- make_nut_plot("SiO4_Si", cols_nut["SiO4_Si"], r2_nut$r2[r2_nut$Variable == "SiO4_Si"], y_max_nut$y_max[y_max_nut$Variable == "SiO4_Si"])

nutplot <- pB + pC + pD + plot_layout(ncol = 3) & theme(legend.position = "none")
ggsave("Fig2_Nutrients.png", nutplot, width = 15, height = 4, dpi = 300)

# ========== Panel E-F: N/P, Si/N + Chl-a ==========
df_ratio <- df %>% select(Year, N_P, Si_N, Chl_a) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>% drop_na(Value)

cols_ratio <- c("N_P" = "#6CA3D4", "Si_N" = "#BF95C1", "Chl_a" = "#61C1BF")
r2_ratio <- df_ratio %>% filter(Variable != "Chl_a") %>%
  group_by(Variable) %>% summarise(r2 = round(summary(lm(Value ~ Year))$r.squared, 3))
y_max_ratio <- df_ratio %>% group_by(Variable) %>% summarise(y_max = max(Value, na.rm = TRUE))

make_ratio_plot <- function(var_name, var_color, r2_val, y_max) {
  data_var <- df_ratio %>% filter(Variable == var_name)
  label_map <- c("N_P" = "N/P", "Si_N" = "Si/N", "Chl_a" = "Chl-a (\u00b5g/L)")
  lab <- ifelse(var_name %in% names(label_map), label_map[var_name], var_name)
  
  if (var_name == "Chl_a") {
    p <- ggplot(data_var, aes(x = Year, y = Value)) +
      geom_col(fill = var_color, color = "black", width = 0.7, linewidth = 0.2)
  } else {
    p <- ggplot(data_var, aes(x = Year, y = Value)) +
      geom_line(color = var_color, linewidth = 1) + geom_point(color = var_color, size = 3) +
      geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 0.8, color = var_color, fill = var_color)
    if (var_name == "N_P") {
      p <- p + geom_hline(yintercept = 16, linetype = "dashed", color = "#A5D395", linewidth = 1) +
        annotate("text", x = 2000, y = 25.5, label = "16:1", hjust = 0, family = "serif", size = 5, fontface = "bold", color = "black")
    }
    if (var_name == "Si_N") {
      p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "#A5D395", linewidth = 1) +
        annotate("text", x = 2019, y = 1.1, label = "1:1", hjust = 0, family = "serif", size = 5, fontface = "bold", color = "black")
    }
    p <- p + annotate("text", x = 2015, y = y_max * 0.99,
                      label = paste("R² =", r2_val), hjust = 1, family = "serif", size = 5, fontface = "bold", color = var_color)
  }
  p + annotate("text", x = 2000, y = y_max * 0.99, label = lab,
               hjust = 0, family = "serif", size = 5, fontface = "bold", color = var_color) + my_theme
}

pE <- make_ratio_plot("N_P", cols_ratio["N_P"], r2_ratio$r2[r2_ratio$Variable == "N_P"], y_max_ratio$y_max[y_max_ratio$Variable == "N_P"])
pF <- make_ratio_plot("Si_N", cols_ratio["Si_N"], r2_ratio$r2[r2_ratio$Variable == "Si_N"], y_max_ratio$y_max[y_max_ratio$Variable == "Si_N"])
pS2 <- make_ratio_plot("Chl_a", cols_ratio["Chl_a"], NULL, y_max_ratio$y_max[y_max_ratio$Variable == "Chl_a"])

ratioplot <- pE + pF + pS2 + plot_layout(ncol = 3) & theme(legend.position = "none")
ggsave("Fig2_NP_SiN_Chla.png", ratioplot, width = 15, height = 4, dpi = 300)
ggsave("FigS2_Chla.png", pS2, width = 6, height = 4, dpi = 300)

cat("Done: Fig 2 (A-F panels) + Fig S2 saved\n")
