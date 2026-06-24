# 05_correlation_heatmap.R
# Fig. 7: Pearson correlations between top 20 abundant species and environmental factors
library(readxl); library(dplyr); library(tidyr); library(pheatmap)

file_path <- "correlation_matrix.xlsx"
raw <- as.data.frame(read_excel(file_path, col_names = FALSE))

n_samples <- ncol(raw) - 2
sample_ids <- as.character(unlist(raw[1, 3:ncol(raw)]))
sample_ids[is.na(sample_ids) | sample_ids == ""] <- paste0("S", seq_len(n_samples))

species_labels <- as.character(raw[2:nrow(raw), 1])
size_labels <- as.character(raw[2:nrow(raw), 2])

# Identify the break between species and environment rows
env_start <- which(species_labels == "temp" | species_labels == "Temperature")
if (length(env_start) == 0) {
  env_start <- which(grepl("^(temp|DIN|PO4|SiO4|N/P|Si/N|Temperature|Salinity)$", species_labels))[1]
}
if (is.na(env_start) || env_start <= 1) stop("Could not identify environment factor boundary")

# Species abundance matrix
sp_rows <- 1:(env_start - 1)
abund_mat <- as.data.frame(lapply(raw[sp_rows + 1, 3:ncol(raw), drop = FALSE],
  function(x) suppressWarnings(as.numeric(x))))
rownames(abund_mat) <- species_labels[sp_rows]
colnames(abund_mat) <- sample_ids

# Environment matrix
env_rows <- env_start:nrow(raw[-1, , drop = FALSE])
env_mat <- as.data.frame(lapply(raw[env_rows + 1, 3:ncol(raw), drop = FALSE],
  function(x) suppressWarnings(as.numeric(x))))
rownames(env_mat) <- species_labels[env_rows]
colnames(env_mat) <- sample_ids

# Keep top 20 species by total abundance
sp_totals <- rowSums(abund_mat, na.rm = TRUE)
top20 <- names(sort(sp_totals, decreasing = TRUE))[1:min(20, length(sp_totals))]

# Compute Pearson correlations
cor_matrix <- matrix(NA, nrow = length(top20), ncol = nrow(env_mat))
rownames(cor_matrix) <- top20
colnames(cor_matrix) <- rownames(env_mat)

for (i in seq_along(top20)) {
  for (j in 1:nrow(env_mat)) {
    sp_vals <- as.numeric(abund_mat[top20[i], ])
    env_vals <- as.numeric(env_mat[j, ])
    valid <- !is.na(sp_vals) & !is.na(env_vals) & is.finite(sp_vals) & is.finite(env_vals)
    if (sum(valid) >= 5 && sd(sp_vals[valid], na.rm = TRUE) > 0 && sd(env_vals[valid], na.rm = TRUE) > 0) {
      cor_matrix[i, j] <- cor(sp_vals[valid], env_vals[valid], method = "pearson")
    }
  }
}

# Keep only species with >= 2 valid correlations
valid_rows <- rowSums(!is.na(cor_matrix)) >= 2
cor_matrix <- cor_matrix[valid_rows, , drop = FALSE]

# Draw heatmap
pheatmap(cor_matrix, color = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100),
         breaks = seq(-1, 1, length.out = 101),
         cluster_rows = TRUE, cluster_cols = TRUE,
         display_numbers = TRUE, number_format = "%.2f", number_color = "black",
         fontsize_number = 10, fontsize_row = 12, fontsize_col = 12,
         fontfamily = "serif", border_color = "white",
         main = "", angle_col = 45,
         filename = "Fig7_correlation_heatmap.png", width = 8, height = 10, dpi = 300)

cat("Done: Fig 7 saved\n")
