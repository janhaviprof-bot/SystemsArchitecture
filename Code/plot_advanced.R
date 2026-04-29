library(readr)

input_file <- "fullevaluation.csv"
df <- read_csv(input_file, show_col_types = FALSE)

# Keep only 4 metrics and drop rows with NA/Inf
metrics <- df[, c("cost", "certification", "reliability", "durability")]
metrics <- metrics[complete.cases(metrics), , drop = FALSE]
metrics <- as.data.frame(lapply(metrics, as.numeric))

if (nrow(metrics) == 0) {
  stop("No valid rows found in fullevaluation.csv for plotting.")
}

# Normalize helper (0..1)
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

norm_metrics <- as.data.frame(lapply(metrics, minmax))

# ------------------------------------------------------------
# 1) Scatter-matrix style view (all pairwise metric relations)
# ------------------------------------------------------------
png("advanced_pairs_matrix.png", width = 1400, height = 1200)
pairs(
  metrics,
  pch = 16,
  cex = 0.4,
  col = "black",
  main = "Pairs Matrix: cost, certification, reliability, durability"
)
dev.off()

# ------------------------------------------------------------
# 2) Color-code two other metrics in one 2D chart
#    X = cost, Y = certification
#    Color = reliability, Size = durability
# ------------------------------------------------------------
reliab_col <- rgb(
  red = 1 - norm_metrics$reliability,
  green = 0.1,
  blue = norm_metrics$reliability,
  alpha = 0.7
)
dur_size <- 0.5 + 2.0 * norm_metrics$durability

png("advanced_cost_cert_colorcoded.png", width = 1200, height = 900)
plot(
  metrics$cost, metrics$certification,
  pch = 19,
  cex = dur_size,
  col = reliab_col,
  xlab = "cost",
  ylab = "certification",
  main = "Cost vs Certification (color: reliability, size: durability)"
)
legend(
  "topright",
  legend = c("Low reliability", "High reliability"),
  pch = 19,
  col = c(rgb(1, 0.1, 0, 0.7), rgb(0, 0.1, 1, 0.7)),
  bty = "n"
)
dev.off()

# ------------------------------------------------------------
# 3) Parallel coordinates (normalized metrics)
# ------------------------------------------------------------
png("advanced_parallel_coordinates.png", width = 1400, height = 900)
matplot(
  t(as.matrix(norm_metrics)),
  type = "l",
  lty = 1,
  col = rgb(0, 0, 0, 0.15),
  xaxt = "n",
  xlab = "Metrics",
  ylab = "Normalized value (0-1)",
  main = "Parallel Coordinates (normalized)"
)
axis(1, at = 1:4, labels = colnames(norm_metrics))
dev.off()

# ------------------------------------------------------------
# 4) Radar-style comparison for representative points
#    Choose 5 points spread across cost quantiles
# ------------------------------------------------------------
pick_idx <- unique(round(quantile(seq_len(nrow(norm_metrics)), probs = c(0.05, 0.25, 0.5, 0.75, 0.95))))
pick_idx <- pick_idx[pick_idx >= 1 & pick_idx <= nrow(norm_metrics)]
pick <- norm_metrics[pick_idx, , drop = FALSE]

radar_plot <- function(df_norm, file_name) {
  n_axes <- ncol(df_norm)
  theta <- seq(0, 2 * pi, length.out = n_axes + 1)
  axis_labels <- colnames(df_norm)
  
  png(file_name, width = 1000, height = 1000)
  plot(0, 0, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n",
       main = "Radar-style Comparison (selected architectures)")
  
  # Grid rings
  for (r in seq(0.2, 1.0, by = 0.2)) {
    lines(r * cos(theta), r * sin(theta), col = "grey85")
  }
  
  # Axes + labels
  for (i in seq_len(n_axes)) {
    lines(c(0, cos(theta[i])), c(0, sin(theta[i])), col = "grey60")
    text(1.1 * cos(theta[i]), 1.1 * sin(theta[i]), labels = axis_labels[i], cex = 0.9)
  }
  
  cols <- c("red", "blue", "darkgreen", "purple", "orange")
  for (i in seq_len(nrow(df_norm))) {
    vals <- as.numeric(df_norm[i, ])
    vals <- c(vals, vals[1])
    lines(vals * cos(theta), vals * sin(theta), lwd = 2, col = cols[(i - 1) %% length(cols) + 1])
  }
  
  legend("bottomleft", legend = paste("row", rownames(df_norm)), col = cols[seq_len(nrow(df_norm))], lwd = 2, bty = "n")
  dev.off()
}

radar_plot(pick, "advanced_radar_selected.png")

cat("Saved advanced plots:\n")
cat("- advanced_pairs_matrix.png\n")
cat("- advanced_cost_cert_colorcoded.png\n")
cat("- advanced_parallel_coordinates.png\n")
cat("- advanced_radar_selected.png\n")
