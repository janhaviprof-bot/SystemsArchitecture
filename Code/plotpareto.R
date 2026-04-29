library(readr)

input_file <- "fullevaluation.csv"
solution_file <- "final_solution_arch_metrics.csv"
output_file <- "final_population_metric_pairs_pareto.png"

df <- read_csv(input_file, show_col_types = FALSE)
solution_df <- read_csv(solution_file, show_col_types = FALSE)

# Align reliability units between datasets (fraction vs percent).
if (max(df$reliability, na.rm = TRUE) <= 1 && max(solution_df$reliability, na.rm = TRUE) > 1) {
  solution_df$reliability <- solution_df$reliability / 100
}

m1 <- df$cost
m2 <- df$certification
m3 <- df$reliability
m4 <- df$durability

# Compute 2D Pareto front indices for any objective directions.
# minimize_x/minimize_y: TRUE means minimize, FALSE means maximize.
pareto_front_2d <- function(x, y, minimize_x = TRUE, minimize_y = TRUE) {
  x2 <- if (minimize_x) x else -x
  y2 <- if (minimize_y) y else -y
  
  ord <- order(x2, y2)
  x2s <- x2[ord]
  y2s <- y2[ord]
  
  keep <- rep(FALSE, length(x2s))
  best_y <- Inf
  
  for (i in seq_along(x2s)) {
    if (y2s[i] <= best_y) {
      keep[i] <- TRUE
      best_y <- y2s[i]
    }
  }
  
  ord[keep]
}

plot_pair_with_solution_and_pareto <- function(
    x, y, sx, sy, xlab, ylab, main,
    minimize_x = TRUE, minimize_y = TRUE
) {
  plot(
    x, y,
    pch = 16,
    cex = 0.7,
    col = "black",
    xlab = xlab,
    ylab = ylab,
    main = main
  )
  
  # Overlay final solution points (yellow)
  points(sx, sy, pch = 19, cex = 1.8, col = "yellow")
  
  # Overlay pairwise Pareto front (red)
  p_idx <- pareto_front_2d(x, y, minimize_x = minimize_x, minimize_y = minimize_y)
  px <- x[p_idx]
  py <- y[p_idx]
  
  if (length(px) > 1) {
    # Draw front line in sorted order by x for visual continuity.
    s <- order(px)
    lines(px[s], py[s], col = "red", lwd = 2)
  }
  points(px, py, pch = 1, cex = 1.2, col = "red", lwd = 1.5)
}

save_pair_plot <- function(
    filename, x, y, sx, sy, xlab, ylab, main,
    minimize_x = TRUE, minimize_y = TRUE
) {
  png(filename = filename, width = 900, height = 700)
  plot_pair_with_solution_and_pareto(
    x, y, sx, sy, xlab, ylab, main,
    minimize_x = minimize_x, minimize_y = minimize_y
  )
  dev.off()
}

png(filename = output_file, width = 1400, height = 1200)
old_par <- par(no.readonly = TRUE)
on.exit({
  par(old_par)
  dev.off()
}, add = TRUE)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot_pair_with_solution_and_pareto(
  m1, m2, solution_df$cost, solution_df$certification,
  "cost", "certification", "cost vs certification",
  minimize_x = TRUE, minimize_y = TRUE
)
plot_pair_with_solution_and_pareto(
  m1, m3, solution_df$cost, solution_df$reliability,
  "cost", "reliability", "cost vs reliability",
  minimize_x = TRUE, minimize_y = FALSE
)
plot_pair_with_solution_and_pareto(
  m2, m3, solution_df$certification, solution_df$reliability,
  "certification", "reliability", "certification vs reliability",
  minimize_x = TRUE, minimize_y = FALSE
)
plot_pair_with_solution_and_pareto(
  m3, m4, solution_df$reliability, solution_df$durability,
  "reliability", "durability", "reliability vs durability",
  minimize_x = FALSE, minimize_y = FALSE
)
plot_pair_with_solution_and_pareto(
  m4, m1, solution_df$durability, solution_df$cost,
  "durability", "cost", "durability vs cost",
  minimize_x = FALSE, minimize_y = TRUE
)

save_pair_plot(
  "plot_pareto_cost_vs_certification.png",
  m1, m2, solution_df$cost, solution_df$certification,
  "cost", "certification", "cost vs certification",
  minimize_x = TRUE, minimize_y = TRUE
)
save_pair_plot(
  "plot_pareto_cost_vs_reliability.png",
  m1, m3, solution_df$cost, solution_df$reliability,
  "cost", "reliability", "cost vs reliability",
  minimize_x = TRUE, minimize_y = FALSE
)
save_pair_plot(
  "plot_pareto_certification_vs_reliability.png",
  m2, m3, solution_df$certification, solution_df$reliability,
  "certification", "reliability", "certification vs reliability",
  minimize_x = TRUE, minimize_y = FALSE
)
save_pair_plot(
  "plot_pareto_reliability_vs_durability.png",
  m3, m4, solution_df$reliability, solution_df$durability,
  "reliability", "durability", "reliability vs durability",
  minimize_x = FALSE, minimize_y = FALSE
)
save_pair_plot(
  "plot_pareto_durability_vs_cost.png",
  m4, m1, solution_df$durability, solution_df$cost,
  "durability", "cost", "durability vs cost",
  minimize_x = FALSE, minimize_y = TRUE
)

cat("Saved plot:", output_file, "\n")
cat("Saved separate Pareto files:\n")
cat("- plot_pareto_cost_vs_certification.png\n")
cat("- plot_pareto_cost_vs_reliability.png\n")
cat("- plot_pareto_certification_vs_reliability.png\n")
cat("- plot_pareto_reliability_vs_durability.png\n")
cat("- plot_pareto_durability_vs_cost.png\n")
