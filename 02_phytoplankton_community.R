# 02_phytoplankton_community.R
# Fig. 3: Decadal changes in phytoplankton community (bubble plot + size-class proportions)
# Fig. 4: Community structure (dominance heatmap + size-class area chart)
library(readxl); library(tidyverse); library(ggtext); library(scales)

# ====== Fig 3A: Bubble plot of species abundance ======
df_bubble <- read_excel("bubble_plot.xlsx", sheet = "abundance", col_names = TRUE)
names(df_bubble)[1:2] <- c("SizeClass", "Species")

df_bubble <- df_bubble %>%
  mutate(SizeClass = ifelse(is.na(SizeClass) | SizeClass == "", "Overall", SizeClass)) %>%
  pivot_longer(-c(SizeClass, Species), names_to = "Year", values_to = "Abundance") %>%
  mutate(Year = as.numeric(Year), Abundance = as.numeric(Abundance)) %>%
  filter(!is.na(Abundance) & Abundance > 0)

species_order <- unique(df_bubble$Species)
size_levels <- c("Large-sized", "Middle-sized", "Small-sized", "Overall")
df_bubble$Species <- factor(df_bubble$Species, levels = species_order)
df_bubble$SizeClass <- factor(df_bubble$SizeClass, levels = size_levels)

fill_cols <- c("Large-sized" = "#61C1BF", "Middle-sized" = "#BF95C1",
               "Small-sized" = "#6CA3D4", "Overall" = "gray70")

dino_spp <- c("Noctiluca scintillans", "Ceratium furca", "Ceratium boehmii")
df_bubble <- df_bubble %>% mutate(Community = case_when(
  Species %in% dino_spp ~ "Dinoflagellate",
  Species == "Dinoflagellate" ~ "Dinoflagellate",
  Species == "Diatom" ~ "Diatom",
  Species == "Total" ~ NA_character_, TRUE ~ "Diatom"
))

y_labels <- ifelse(species_order %in% c("Total", "Diatom", "Dinoflagellate"),
  paste0('bold("', species_order, '")'),
  paste0("italic('", gsub("'", "\\\\'", species_order), "')"))
names(y_labels) <- species_order

p3A <- ggplot(df_bubble, aes(x = Year, y = Species, size = Abundance)) +
  geom_point(aes(fill = SizeClass, color = Community), shape = 21, stroke = 1.2, alpha = 0.8) +
  scale_size_continuous(range = c(0.2, 10), trans = "log10",
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = function(x) sapply(round(log10(x)), function(e) as.expression(bquote(10^.(e))))) +
  scale_fill_manual(values = fill_cols, name = "Size class",
    breaks = c("Large-sized", "Middle-sized", "Small-sized")) +
  scale_color_manual(values = c("Dinoflagellate" = "#0074B3", "Diatom" = "#A5D395"),
    name = "Community", na.value = "transparent", breaks = c("Dinoflagellate", "Diatom")) +
  scale_x_continuous(breaks = unique(df_bubble$Year)) +
  scale_y_discrete(labels = function(x) parse(text = y_labels[as.character(x)]),
    expand = expansion(mult = c(0.05, 0.05))) +
  guides(fill = guide_legend(override.aes = list(size = 5)),
    color = guide_legend(override.aes = list(size = 5)),
    size = guide_legend(override.aes = list(shape = 16))) +
  labs(size = expression(Abundance~(cells/m^3))) +
  theme_minimal() + theme(
    text = element_text(family = "serif", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "serif", size = 12),
    axis.text.y = element_text(size = 12, family = "serif", face = "plain"),
    axis.title = element_blank(), plot.title = element_blank(),
    legend.text = element_text(family = "serif", size = 12),
    legend.title = element_text(family = "serif", size = 12))
ggsave("Fig3A_bubble_plot.png", p3A, width = 15, height = 6, dpi = 300)

# ====== Fig 3B + Fig 4B: Size-class abundance proportions ======
file_path <- "phytoplankton_size_abundance.xlsx"
sheet_names <- excel_sheets(file_path)
results <- data.frame(Year = character(), Small_avg = numeric(),
  Middle_avg = numeric(), Large_avg = numeric(), stringsAsFactors = FALSE)

for (sheet in sheet_names) {
  df_s <- read_excel(file_path, sheet = sheet, col_types = "text")
  esd <- as.numeric(df_s[[1]])
  abund_mat <- apply(as.matrix(df_s[, -c(1, 2), drop = FALSE]), 2, as.numeric)
  abund_mat[is.na(abund_mat)] <- 0
  total_abund <- rowSums(abund_mat, na.rm = TRUE)
  sp_data <- data.frame(esd = esd, total = total_abund) %>% filter(!is.na(esd)) %>%
    mutate(class = case_when(esd <= 20 ~ "small", esd > 20 & esd <= 50 ~ "middle", esd > 50 ~ "large"))
  ct <- sp_data %>% group_by(class) %>% summarise(total = sum(total, na.rm = TRUE)) %>%
    complete(class = c("small", "middle", "large"), fill = list(total = 0))
  n_st <- ncol(df_s) - 2
  results <- rbind(results, data.frame(
    Year = sheet, Small_avg = ct$total[ct$class == "small"] / n_st,
    Middle_avg = ct$total[ct$class == "middle"] / n_st,
    Large_avg = ct$total[ct$class == "large"] / n_st))
}

df_prop <- results %>%
  pivot_longer(-Year, names_to = "SizeClass", values_to = "Abundance") %>%
  mutate(Year = as.numeric(Year), SizeClass = factor(SizeClass,
    levels = c("Small_avg", "Middle_avg", "Large_avg"),
    labels = c("Small-sized", "Mid-sized", "Large-sized"))) %>%
  group_by(Year) %>% mutate(Proportion = Abundance / sum(Abundance)) %>% ungroup()

prop_cols <- c("Small-sized" = "#61C1BF", "Mid-sized" = "#BF95C1", "Large-sized" = "#6CA3D4")

p3B <- ggplot(df_prop, aes(x = Year, y = Abundance, fill = SizeClass)) +
  geom_area(alpha = 0.8) + scale_fill_manual(values = prop_cols) +
  labs(y = expression(Abundance~(cells/m^3))) +
  theme_minimal(base_size = 14) + theme(
    text = element_text(family = "serif", face = "bold"),
    axis.text = element_text(family = "serif", size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "right")
ggsave("Fig3B_size_proportions.png", p3B, width = 10, height = 5, dpi = 300)

p4B <- ggplot(df_prop, aes(x = Year, y = Proportion * 100, fill = SizeClass)) +
  geom_area(alpha = 0.8) + scale_fill_manual(values = prop_cols) +
  labs(y = "Proportion (%)") +
  theme_minimal(base_size = 14) + theme(
    text = element_text(family = "serif", face = "bold"),
    axis.text = element_text(family = "serif", size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "right")
ggsave("Fig4B_proportion_area.png", p4B, width = 10, height = 5, dpi = 300)

write.csv(df_prop, "size_class_proportions.csv", row.names = FALSE)

# ====== Fig 4A: Dominance (Y) heatmap ======
df_dom <- read_excel("species_dominance.xlsx", sheet = "dominance")
names(df_dom)[1:3] <- c("Group", "SizeClass", "Species")
year_cols <- as.character(2004:2018)

df_dom_long <- df_dom %>%
  pivot_longer(cols = all_of(year_cols), names_to = "Year", values_to = "Y") %>%
  mutate(Year = as.numeric(Year), Y = as.numeric(Y)) %>% filter(!is.na(Y))

size_cols <- c("Small-sized" = "#61C1BF", "Middle-sized" = "#BF95C1", "Large-sized" = "#6CA3D4")

species_info <- df_dom_long %>% distinct(Species, SizeClass, Group) %>%
  mutate(Label = sprintf("<span style='color:%s;'><i>%s</i></span>", size_cols[SizeClass], Species))

sz_levels <- c("Small-sized", "Middle-sized", "Large-sized")
species_order_dom <- species_info %>%
  arrange(factor(SizeClass, levels = sz_levels), Group, Species) %>% pull(Species)

df_dom_long$Species <- factor(df_dom_long$Species, levels = species_order_dom)
label_map <- setNames(species_info$Label, species_info$Species)
df_dom_long$Y_label <- sprintf("%.3f", df_dom_long$Y)

p4A <- ggplot(df_dom_long, aes(x = Year, y = Species, fill = Y)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = Y_label), size = 5, family = "serif", fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#b2182b", na.value = "grey90", name = "Y") +
  scale_y_discrete(labels = label_map) +
  scale_x_continuous(breaks = seq(2004, 2018, 1)) +
  theme_minimal(base_size = 16) + theme(
    text = element_text(family = "serif"),
    axis.text.y = element_markdown(family = "serif", size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, family = "serif", face = "bold"),
    axis.title = element_blank(), panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(family = "serif", size = 20),
    legend.text = element_text(family = "serif", size = 20),
    plot.margin = margin(10, 10, 10, 10), plot.title = element_blank())

legend_data <- df_dom_long %>% distinct(SizeClass)
p4A <- p4A +
  geom_point(data = legend_data, aes(x = 2004, y = 1, color = SizeClass),
             alpha = 0, show.legend = TRUE, inherit.aes = FALSE) +
  scale_color_manual(values = size_cols, name = "Size Class",
    labels = c("Small-sized", "Middle-sized", "Large-sized")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, shape = 15, size = 8)))
ggsave("Fig4A_dominance_heatmap.png", p4A, width = 15, height = 6, dpi = 300)

cat("Done: Fig 3 (A-B) + Fig 4 (A-B) saved\n")
