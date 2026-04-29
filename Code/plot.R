library(readr)

input_file <- "fullevaluation.csv"
solution_file <- "final_solution_arch_metrics.csv"
output_file <- "final_population_metric_pairs.png"

df <- read_csv(input_file, show_col_types = FALSE)
solution_df <- read_csv(solution_file, show_col_types = FALSE)

# Align reliability units between datasets (fraction vs percent).
# If one looks like percent and the other fraction, convert solution to fraction.
if (max(df$reliability, na.rm = TRUE) <= 1 && max(solution_df$reliability, na.rm = TRUE) > 1) {
  solution_df$reliability <- solution_df$reliability / 100
}

m1 <- df$cost
m2 <- df$certification
m3 <- df$reliability
m4 <- df$durability

plot_pair_with_solution <- function(x, y, sx, sy, xlab, ylab, main) {
  plot(
    x, y,
    pch = 16,
    cex = 0.7,
    col = "black",
    xlab = xlab,
    ylab = ylab,
    main = main
  )
  points(sx, sy, pch = 19, cex = 1.8, col = "yellow")
}

save_pair_plot <- function(filename, x, y, sx, sy, xlab, ylab, main) {
  png(filename = filename, width = 900, height = 700)
  plot_pair_with_solution(x, y, sx, sy, xlab, ylab, main)
  dev.off()
}

png(filename = output_file, width = 1400, height = 1200)
old_par <- par(no.readonly = TRUE)
on.exit({
  par(old_par)
  dev.off()
}, add = TRUE)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot_pair_with_solution(m1, m2, solution_df$cost, solution_df$certification, "cost", "certification", "cost vs certification")
plot_pair_with_solution(m1, m3, solution_df$cost, solution_df$reliability, "cost", "reliability", "cost vs reliability")
plot_pair_with_solution(m2, m3, solution_df$certification, solution_df$reliability, "certification", "reliability", "certification vs reliability")
plot_pair_with_solution(m3, m4, solution_df$reliability, solution_df$durability, "reliability", "durability", "reliability vs durability")
plot_pair_with_solution(m4, m1, solution_df$durability, solution_df$cost, "durability", "cost", "durability vs cost")

save_pair_plot("plot_cost_vs_certification.png", m1, m2, solution_df$cost, solution_df$certification, "cost", "certification", "cost vs certification")
save_pair_plot("plot_cost_vs_reliability.png", m1, m3, solution_df$cost, solution_df$reliability, "cost", "reliability", "cost vs reliability")
save_pair_plot("plot_certification_vs_reliability.png", m2, m3, solution_df$certification, solution_df$reliability, "certification", "reliability", "certification vs reliability")
save_pair_plot("plot_reliability_vs_durability.png", m3, m4, solution_df$reliability, solution_df$durability, "reliability", "durability", "reliability vs durability")
save_pair_plot("plot_durability_vs_cost.png", m4, m1, solution_df$durability, solution_df$cost, "durability", "cost", "durability vs cost")

cat("Saved plot:", output_file, "\n")
cat("Saved separate files:\n")
cat("- plot_cost_vs_certification.png\n")
cat("- plot_cost_vs_reliability.png\n")
cat("- plot_certification_vs_reliability.png\n")
cat("- plot_reliability_vs_durability.png\n")
cat("- plot_durability_vs_cost.png\n")
