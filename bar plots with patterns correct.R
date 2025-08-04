# Load data
df <- read_csv("plot data.csv", col_select = -"id_row")

# List of variables to plot
vars <- c(
  "PLT", "HGB", "MCH", "MCV", "PCT", "RBC",
  "RDW-CV (%)", "RDW-SD", "WBC", "MCHC", "HCT"
)

# Order Treatment variables
df$Treatment <- factor(df$Treatment, 
                       levels = c("CONTROL", "IF", "5% PKSD", "IF + 15% PKSD", "15% PKSD"))

# Define treatment patterns
treatment_levels <- unique(df$Treatment)
pattern_types <- c("stripe", "crosshatch", "circle", "grid", "none", "horizontal")
pattern_map <- setNames(pattern_types[1:length(treatment_levels)], treatment_levels)

# Open PDF for output
pdf("ggpattern_Treatment_Plots.pdf", width = 6, height = 5)

for (v in vars) {
  # Summarize data
  df_summary <- df %>%
    group_by(Treatment) %>%
    summarise(
      Mean = mean(.data[[v]], na.rm = TRUE),
      SD = sd(.data[[v]], na.rm = TRUE)
    ) %>%
    mutate(Pattern = pattern_map[Treatment])
  
  # Plot
  p <- ggplot(df_summary, aes(x = Treatment, y = Mean, pattern = Pattern)) +
    geom_bar_pattern(stat = "identity",
                     fill = "white",
                     color = "black",
                     width = 0.7,
                     pattern_fill = "black",
                     pattern_color = "black",
                     pattern_density = 0.5,
                     pattern_spacing = 0.03) +
    geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.2, size = 0.8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  # Force y-axis to start at zero
    labs(x = "Treatment", y = v) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#fdf6e3", color = NA),
      plot.background = element_rect(fill = "#fdf6e3", color = NA),
      axis.line = element_line(color = "black", linewidth = 0.6),
      axis.ticks = element_line(color = "black")
    )
  
  print(p)
}

dev.off()


# Folder to save images
dir.create("plots", showWarnings = FALSE)

# Loop through variables and save each plot
for (v in vars) {
  # Summarize data
  df_summary <- df %>%
    group_by(Treatment) %>%
    summarise(
      Mean = mean(.data[[v]], na.rm = TRUE),
      SD = sd(.data[[v]], na.rm = TRUE)
    ) %>%
    mutate(Pattern = pattern_map[Treatment])
  
  # Create plot
  p <- ggplot(df_summary, aes(x = Treatment, y = Mean, pattern = Pattern)) +
    geom_bar_pattern(stat = "identity",
                     fill = "white",
                     color = "black",
                     width = 0.7,
                     pattern_fill = "black",
                     pattern_color = "black",
                     pattern_density = 0.5,
                     pattern_spacing = 0.03) +
    geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.2, size = 0.8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    labs(x = "Treatment", y = v) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#fdf6e3", color = NA),
      plot.background = element_rect(fill = "#fdf6e3", color = NA),
      axis.line = element_line(color = "black", linewidth = 0.6),
      axis.ticks = element_line(color = "black")
    )
  
  # Save as PNG
  ggsave(filename = paste0("plots/", v, "_barplot.png"),
         plot = p,
         width = 6, height = 5, dpi = 300)
}