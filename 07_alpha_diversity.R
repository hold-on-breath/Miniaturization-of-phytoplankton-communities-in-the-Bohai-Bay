# 07_alpha_diversity.R
# Fig. S1: Alpha diversity indices and long-term phytoplankton abundance trends
library(readxl); library(vegan); library(dplyr); library(tidyr); library(ggplot2); library(purrr); library(mgcv)

# ---- Load per-year phytoplankton data ----
file_path <- "phytoplankton_annual_abundance.xlsx"
sheet_names <- excel_sheets(file_path)
year_sheets <- sheet_names[!sheet_names %in% c("original", "diversity_index", "dominance_index", "taxa_groups", "annual_diversity")]

calc_alpha <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name, col_types = "text")
  colnames(df)[1] <- "species"
  df <- df %>% filter(!is.na(species) & species != "") %>%
    mutate(across(-species, ~ replace_na(as.numeric(.), 0)))
  df <- as.data.frame(df); rownames(df) <- df$species; df <- df[, -1, drop = FALSE]
  df <- df[, colSums(df) > 0, drop = FALSE]
  if (ncol(df) == 0) return(NULL)
  
  richness <- colSums(df > 0)
  shannon <- diversity(t(df), index = "shannon")
  simpson <- diversity(t(df), index = "simpson")
  pielou <- ifelse(richness > 0, shannon / log(richness), NA)
  
  chao1_est <- apply(df, 2, function(x) {
    x <- x[x > 0]; S_obs <- length(x)
    if (S_obs == 0) return(NA)
    f1 <- sum(x == 1); f2 <- sum(x == 2)
    if (f2 > 0) S_obs + f1^2 / (2 * f2) else S_obs + f1 * (f1 - 1) / 2
  })
  
  data.frame(Year = sheet_name, Sample = colnames(df), Observed = richness,
    Shannon = shannon, Simpson = simpson, Pielou = pielou, Chao1 = chao1_est,
    stringsAsFactors = FALSE)
}

alpha_list <- map(year_sheets, ~ calc_alpha(.x))
alpha_all <- bind_rows(alpha_list) %>% mutate(Year = as.numeric(Year))

alpha_long <- alpha_all %>%
  pivot_longer(c(Observed, Shannon, Simpson, Pielou, Chao1), names_to = "Index", values_to = "Value") %>%
  filter(!is.na(Value))
alpha_long$Index <- factor(alpha_long$Index,
  levels = c("Observed", "Shannon", "Simpson", "Pielou", "Chao1"),
  labels = c("Observed species", "Shannon", "Simpson", "Pielou", "Chao1"))

alpha_mean <- alpha_long %>% group_by(Index, Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

p_S1 <- ggplot() +
  geom_boxplot(data = alpha_long, aes(x = factor(Year), y = Value), fill = "#6CA3D4",
               outlier.shape = NA, width = 0.6, show.legend = FALSE) +
  geom_line(data = alpha_mean, aes(x = factor(Year), y = Mean, group = Index),
            color = "#BF95C1", linewidth = 1) +
  geom_point(data = alpha_mean, aes(x = factor(Year), y = Mean), color = "#BF95C1", size = 2) +
  geom_smooth(data = alpha_long, aes(x = as.numeric(Year), y = Value),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 4),
              se = TRUE, color = "#61C1BF", fill = "#61C1BF", alpha = 0.5) +
  facet_wrap(~ Index, scales = "free_y", ncol = 5) +
  theme_bw(base_size = 12) + theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold", size = 32),
    panel.grid.minor = element_blank(),
    axis.title = element_blank())

ggsave("FigS1_alpha_diversity.png", p_S1, width = 25, height = 5, dpi = 300)
cat("Done: Fig S1 saved\n")
