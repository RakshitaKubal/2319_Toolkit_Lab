
# IMC503 Lab - Interactive & PDF plots with colors (works in RStudio)
# CSV path (Windows):
file_path <- "C:\\Users\\RAKSHITA.KUBAL\\Downloads\\country_wise_latest - country_wise_latest.csv"

# --- Dependencies ---
suppressPackageStartupMessages({
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
  library(tidyverse)
  library(RColorBrewer)
})

# ---- Safety check & read ----
if (!file.exists(file_path)) {
  stop(paste0("CSV not found at:\n", file_path, "\nPlease check the path and try again."))
}
df <- readr::read_csv(file_path, show_col_types = FALSE)
names(df) <- trimws(names(df))

# ---- helper to find columns robustly ----
find_col <- function(pattern, df_names = names(df)) {
  matches <- df_names[grepl(pattern, df_names, ignore.case = TRUE)]
  if (length(matches) == 0) return(NA_character_)
  exact <- matches[tolower(matches) == tolower(gsub("[^a-zA-Z0-9]","",pattern))]
  if (length(exact) >= 1) return(exact[1])
  return(matches[1])
}

country_col   <- find_col("country\\b|country/region|country_region|country.region")
confirmed_col <- find_col("^confirmed$|confirmed")
deaths_col    <- find_col("^deaths$|deaths")
recovered_col <- find_col("^recovered$|recovered")
active_col    <- find_col("^active$|active")

if (is.na(country_col) || is.na(confirmed_col)) {
  stop("Could not detect required columns (Country and Confirmed). Columns present:\n",
       paste(names(df), collapse = ", "))
}

# ---- coerce numeric columns safely ----
coerce_numeric_safe <- function(df, colname) {
  if (is.na(colname)) return(df)
  df[[colname]] <- as.numeric(gsub(",", "", df[[colname]]))
  df[[colname]][is.na(df[[colname]])] <- 0
  df
}
for (col in c(confirmed_col, deaths_col, recovered_col, active_col)) {
  df <- coerce_numeric_safe(df, col)
}

# ---- derived metrics ----
df <- df %>%
  mutate(
    .confirmed = .data[[confirmed_col]],
    .deaths    = if (!is.na(deaths_col)) .data[[deaths_col]] else 0,
    .recovered = if (!is.na(recovered_col)) .data[[recovered_col]] else 0,
    CFR = ifelse(.confirmed > 0, .deaths / .confirmed, NA_real_),
    RecoveryRate = ifelse(.confirmed > 0, .recovered / .confirmed, NA_real_)
  )

# ---- prepare top-10 data ----
top_confirmed <- df %>% arrange(desc(.data[[confirmed_col]])) %>% slice(1:10)
top_deaths    <- if (!is.na(deaths_col)) df %>% arrange(desc(.data[[deaths_col]])) %>% slice(1:10) else tibble()

# ---- colors & palettes ----
bar_colors <- brewer.pal(min(10, max(3, nrow(top_confirmed))), "Set3")
pie_colors <- brewer.pal(4, "Pastel2")
line_colors <- c("sienna","goldenrod","deepskyblue")

# ---- INTERACTIVE PLOTS (RStudio) ----
# When you run this script in RStudio the plots will show in the Plots pane.
# Plot A: Base-R barplot (Top 10 Confirmed) with legend
if (nrow(top_confirmed) > 0) {
  par(mar = c(6, 6, 4, 2) + 0.1)
  bc_vals <- top_confirmed[[confirmed_col]]
  bc_names <- top_confirmed[[country_col]]
  barplot_heights <- rev(bc_vals) # reverse for horizontal ordering
  bar_names <- rev(bc_names)
  bar_colors_use <- rep(bar_colors, length.out = length(bar_names))
  barplot(barplot_heights,
          names.arg = bar_names,
          horiz = TRUE,
          las = 1,
          col = bar_colors_use,
          border = "black",
          main = "Top 10 Countries by Confirmed Cases (Base-R)",
          xlab = "Confirmed cases")
  legend("topright", legend = bar_names, fill = bar_colors_use, cex = 0.7, title = "Country")
}

# Plot B: Pie chart of totals (base-R)
totals <- c(
  Confirmed = sum(df[[confirmed_col]], na.rm = TRUE),
  Recovered = if (!is.na(recovered_col)) sum(df[[recovered_col]], na.rm = TRUE) else 0,
  Deaths    = if (!is.na(deaths_col)) sum(df[[deaths_col]], na.rm = TRUE) else 0,
  Active    = if (!is.na(active_col)) sum(df[[active_col]], na.rm = TRUE) else 0
)
totals <- totals[totals > 0]
if (length(totals) > 0) {
  pie_labels <- paste0(names(totals), "\n", format(totals, big.mark = ","))
  pie(p_tot <- totals, labels = pie_labels, main = "Overall distribution: Confirmed/Recovered/Deaths/Active", col = pie_colors)
  legend("topright", legend = names(totals), fill = pie_colors[1:length(totals)], cex = 0.8)
}

# Plot C: ggplot2 scatter (Confirmed vs Deaths) with color ramp
if (!is.na(confirmed_col) && !is.na(deaths_col)) {
  library(ggplot2)
  p <- ggplot(df, aes(x = .data[[confirmed_col]], y = .data[[deaths_col]])) +
    geom_point(aes(size = pmax(1, .data[[confirmed_col]])), alpha = 0.6) +
    labs(title = "Confirmed vs Deaths (ggplot2)", x = "Confirmed", y = "Deaths") +
    scale_size_continuous(name = "Confirmed (size)") +
    theme_minimal()
  if (any(df[[confirmed_col]] > 0) && any(df[[deaths_col]] > 0)) {
    p <- p + scale_x_log10() + scale_y_log10() + labs(subtitle = "Log-Log scale applied")
  }
  print(p)
}

# Plot D: Histogram of CFR (ggplot2)
if ("CFR" %in% names(df)) {
  p2 <- ggplot(df %>% filter(!is.na(CFR) & is.finite(CFR)), aes(x = CFR)) +
    geom_histogram(bins = 30, fill = "purple", color = "black", alpha = 0.8) +
    labs(title = "Distribution of CFR (Deaths/Confirmed)", x = "CFR", y = "Count") +
    theme_minimal()
  print(p2)
}

# Plot E: RecoveryRate vs CFR (ggplot2)
if ("RecoveryRate" %in% names(df) && "CFR" %in% names(df)) {
  p3 <- ggplot(df %>% filter(!is.na(RecoveryRate) & !is.na(CFR)), aes(x = RecoveryRate, y = CFR)) +
    geom_point(alpha = 0.6, color = "darkgreen") +
    labs(title = "Recovery Rate vs CFR", x = "Recovery Rate", y = "CFR") +
    theme_minimal()
  print(p3)
}

# ---- Stacked bar example (similar to your energy example) using top 5 countries (if available) ----
if (nrow(top_confirmed) >= 3 && !is.na(recovered_col) && !is.na(deaths_col)) {
  # Use top 5 by confirmed and show stacked Confirmed as Recovered+Active+Deaths (approx)
  use_n <- min(5, nrow(top_confirmed))
  subset_top <- top_confirmed %>% slice(1:use_n)
  stacked_mat <- rbind(
    Recovered = subset_top[[recovered_col]],
    Deaths = if (!is.na(deaths_col)) subset_top[[deaths_col]] else rep(0, use_n),
    Active = if (!is.na(active_col)) subset_top[[active_col]] else rep(0, use_n)
  )
  colnames(stacked_mat) <- subset_top[[country_col]]
  source_colors <- c("forestgreen", "firebrick", "steelblue")
  barplot(stacked_mat, beside = FALSE, col = source_colors, border = "black",
          main = "Stacked: Recovered / Deaths / Active (Top countries)",
          ylab = "Counts")
  legend("topright", legend = rownames(stacked_mat), fill = source_colors, cex = 0.8)
}

# ---- SAVE same plots to PDF for submission ----
pdf("visualizations_report_R.pdf", width = 10, height = 7)
# Re-create a cleaner ggplot sequence to save
if (nrow(top_confirmed) > 0) {
  ggplot(top_confirmed, aes(x = reorder(!!sym(country_col), !!sym(confirmed_col)), y = !!sym(confirmed_col))) +
    geom_col(fill = bar_colors) + coord_flip() +
    labs(title = "Top 10 Countries by Confirmed Cases", x = "", y = "Confirmed") -> g1
  print(g1)
}
if (!is.na(deaths_col) && nrow(top_deaths) > 0) {
  ggplot(top_deaths, aes(x = reorder(!!sym(country_col), !!sym(deaths_col)), y = !!sym(deaths_col))) +
    geom_col(fill = "darkred") + coord_flip() +
    labs(title = "Top 10 Countries by Deaths", x = "", y = "Deaths") -> g2
  print(g2)
}
if (!is.na(confirmed_col) && !is.na(deaths_col)) {
  g3 <- ggplot(df, aes(x = .data[[confirmed_col]], y = .data[[deaths_col]])) +
    geom_point(alpha = 0.6) + labs(title = "Confirmed vs Deaths", x = "Confirmed", y = "Deaths")
  if (any(df[[confirmed_col]] > 0) && any(df[[deaths_col]] > 0)) g3 <- g3 + scale_x_log10() + scale_y_log10()
  print(g3)
}
if ("CFR" %in% names(df)) {
  g4 <- ggplot(df %>% filter(!is.na(CFR) & is.finite(CFR)), aes(x = CFR)) +
    geom_histogram(bins = 30, fill = "purple", color = "black", alpha = 0.8) +
    labs(title = "Distribution of CFR (Deaths/Confirmed)", x = "CFR", y = "Count")
  print(g4)
}
if ("RecoveryRate" %in% names(df) && "CFR" %in% names(df)) {
  g5 <- ggplot(df %>% filter(!is.na(RecoveryRate) & !is.na(CFR)), aes(x = RecoveryRate, y = CFR)) +
    geom_point(alpha = 0.6, color = "darkgreen") + labs(title = "Recovery Rate vs CFR", x = "Recovery Rate", y = "CFR")
  print(g5)
}
dev.off()

# ---- Console summary for quick verification ----
totals <- list(
  Confirmed = sum(df[[confirmed_col]], na.rm = TRUE),
  Deaths    = if (!is.na(deaths_col)) sum(df[[deaths_col]], na.rm = TRUE) else NA,
  Recovered = if (!is.na(recovered_col)) sum(df[[recovered_col]], na.rm = TRUE) else NA
)
cat("Summary totals:\n")
cat("Confirmed:", format(totals$Confirmed, big.mark = ","), "\n")
if (!is.na(totals$Deaths)) cat("Deaths:", format(totals$Deaths, big.mark = ","), "\n")
if (!is.na(totals$Recovered)) cat("Recovered:", format(totals$Recovered, big.mark = ","), "\n")
cat("\nInteractive plots shown in RStudio Plots pane. PDF saved as: visualizations_report_R.pdf\n")
