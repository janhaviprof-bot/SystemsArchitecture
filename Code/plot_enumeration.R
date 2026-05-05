# Metric pair plots for the full enumerated architecture set.
# Same layout as plot.R, but a single data source: fullevaluation.csv
# (columns d1..d8 + cost, certification, reliability, durability).

library(readr)

input_candidates <- c(
  "CodeOld/fullevaluation.csv",
  "../CodeOld/fullevaluation.csv",
  "fullevaluation.csv"
)
existing_inputs <- input_candidates[file.exists(input_candidates)]
if (length(existing_inputs) == 0) {
  stop(
    "Could not find fullevaluation.csv. Tried: ",
    paste(input_candidates, collapse = ", ")
  )
}
input_file <- existing_inputs[[1]]
output_file <- "enumeration_metric_pairs.png"

df <- read_csv(input_file, show_col_types = FALSE)

arch_cols <- c(
  "d1", "d2", "d3", "d4_1", "d4_2", "d4_3", "d4_4", "d4_5", "d4_6",
  "d5", "d6", "d7_1", "d7_2", "d7_3", "d7_4", "d7_5", "d7_6", "d8"
)
metric_cols <- c("cost", "certification", "reliability", "durability")
missing <- setdiff(c(arch_cols, metric_cols), names(df))
if (length(missing) > 0) {
  stop("fullevaluation.csv is missing columns: ", paste(missing, collapse = ", "))
}

m1 <- df$cost
m2 <- df$certification
m3 <- df$reliability
m4 <- df$durability

plot_pair_enum <- function(x, y, xlab, ylab, main) {
  plot(
    x, y,
    pch = ".",
    cex = 1.1,
    col = "black",
    xlab = xlab,
    ylab = ylab,
    main = main
  )
}

save_pair_plot <- function(filename, x, y, xlab, ylab, main) {
  png(filename = filename, width = 900, height = 700)
  plot_pair_enum(x, y, xlab, ylab, main)
  dev.off()
}

cat(
  "Enumeration plot data: n =", nrow(df),
  "architectures, metrics from", input_file, "\n"
)

png(filename = output_file, width = 1400, height = 1200)
old_par <- par(no.readonly = TRUE)
on.exit(
  {
    par(old_par)
    dev.off()
  },
  add = TRUE
)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot_pair_enum(m1, m2, "cost", "certification", "cost vs certification (enumeration)")
plot_pair_enum(m1, m3, "cost", "reliability", "cost vs reliability (enumeration)")
plot_pair_enum(m2, m3, "certification", "reliability", "certification vs reliability (enumeration)")
plot_pair_enum(m3, m4, "reliability", "durability", "reliability vs durability (enumeration)")
plot_pair_enum(m4, m1, "durability", "cost", "durability vs cost (enumeration)")

save_pair_plot("enumeration_plot_cost_vs_certification.png", m1, m2, "cost", "certification", "cost vs certification (enumeration)")
save_pair_plot("enumeration_plot_cost_vs_reliability.png", m1, m3, "cost", "reliability", "cost vs reliability (enumeration)")
save_pair_plot("enumeration_plot_certification_vs_reliability.png", m2, m3, "certification", "reliability", "certification vs reliability (enumeration)")
save_pair_plot("enumeration_plot_reliability_vs_durability.png", m3, m4, "reliability", "durability", "reliability vs durability (enumeration)")
save_pair_plot("enumeration_plot_durability_vs_cost.png", m4, m1, "durability", "cost", "durability vs cost (enumeration)")

cat("Saved plot:", output_file, "\n")
cat("Saved separate files:\n")
cat("- enumeration_plot_cost_vs_certification.png\n")
cat("- enumeration_plot_cost_vs_reliability.png\n")
cat("- enumeration_plot_certification_vs_reliability.png\n")
cat("- enumeration_plot_reliability_vs_durability.png\n")
cat("- enumeration_plot_durability_vs_cost.png\n")
