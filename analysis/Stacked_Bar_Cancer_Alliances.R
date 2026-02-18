library(ggplot2)
library(tidyverse)
library(here)
library(patchwork)

cdk46Data <- readr::read_csv(here("data/oph_data_11_25_processed.csv")) |>
  mutate(Date = as.Date(Date))

# Aggregate the data to VTM & month for ICBs
cdk46ICBData <- cdk46Data |>
  group_by(year_month = Date, VTM = `VTM Name`, ICB_name = ICB) |>
  summarise(ddd = sum(Value), .groups = "drop")

# Extract each drug separately
ribo <- cdk46ICBData |>
  ungroup() |>
  filter(VTM == "Ribociclib") |>
  select(-VTM) |>
  rename(Ribociclib = ddd)

palbo <- cdk46ICBData |>
  ungroup() |>
  filter(VTM == "Palbociclib") |>
  select(-VTM) |>
  rename(Palbociclib = ddd)

abema <- cdk46ICBData |>
  ungroup() |>
  filter(VTM == "Abemaciclib") |>
  select(-VTM) |>
  rename(Abemaciclib = ddd)

combinedData <- ribo |>
  left_join(palbo) |>
  left_join(abema)

# Replace NA with 0 for all drugs (may not be available in all periods)
combinedData <- combinedData |>
  mutate(
    Ribociclib = replace_na(Ribociclib, 0),
    Palbociclib = replace_na(Palbociclib, 0),
    Abemaciclib = replace_na(Abemaciclib, 0)
  )

# Add Cancer Alliance information
alliances <- read_csv(here("data/CancerAlliances.csv")) |>
  mutate(ICB_upper = toupper(paste("NHS", ICS)))

combinedData <- combinedData |>
  rename(ICB = ICB_name) |>
  mutate(ICB_upper = toupper(ICB)) |>
  left_join(alliances, by = "ICB_upper") |>
  select(-ICB_upper)

# Filter to 2021 and 2024 data and aggregate by Cancer Alliance (Jan-Dec inclusive for each year)
allianceData <- combinedData |>
  filter(
    (year_month >= as.Date("2021-01-01") & year_month <= as.Date("2021-12-31")) |
    (year_month >= as.Date("2024-01-01") & year_month <= as.Date("2024-12-31"))
  ) |>
  mutate(year = year(year_month)) |>
  select(-year_month, -ICB, -Region, -ICS) |>
  group_by(year, Cancer_Alliance) |>
  summarise(across(c(Ribociclib, Palbociclib, Abemaciclib), sum), .groups = "drop")

# Calculate total usage for each year and alliance
allianceData <- allianceData |>
  mutate(total_usage = Ribociclib + Palbociclib + Abemaciclib)

# Calculate percentages for each drug relative to total usage of all three drugs
allianceData <- allianceData |>
  mutate(
    pct_ribociclib = 100 * Ribociclib / total_usage,
    pct_palbociclib = 100 * Palbociclib / total_usage,
    pct_abemaciclib = 100 * Abemaciclib / total_usage
  )

# Get alliance order from 2024 (by palbociclib percentage) - use this for both plots
alliance_order_2024 <- allianceData |>
  filter(year == 2024) |>
  arrange(desc(pct_palbociclib)) |>
  mutate(Cancer_Alliance = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
  pull(Cancer_Alliance) |>
  unique()

# Helper to prepare plot data with consistent alliance ordering
prepare_plot_data <- function(data, alliance_levels) {
  data |>
    select(Cancer_Alliance, pct_ribociclib, pct_palbociclib, pct_abemaciclib) |>
    mutate(Cancer_Alliance = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
    mutate(Cancer_Alliance = factor(Cancer_Alliance, levels = alliance_levels)) |>
    pivot_longer(
      cols = starts_with("pct_"),
      names_to = "drug",
      names_prefix = "pct_",
      values_to = "percentage"
    ) |>
    mutate(drug = str_to_title(drug)) |>
    mutate(drug = factor(drug, levels = c("Ribociclib", "Abemaciclib", "Palbociclib")))
}

# Get 2024 and 2021 data for plotting (2021 uses 2024 alliance order)
plotData2024 <- allianceData |>
  filter(year == 2024) |>
  prepare_plot_data(alliance_order_2024)

plotData2021 <- allianceData |>
  filter(year == 2021) |>
  prepare_plot_data(alliance_order_2024)

# Create stacked bar chart
bar_chart_theme <- list(
  scale_fill_manual(
    values = c("Palbociclib" = "#2a6ebb", "Ribociclib" = "#00b2a9", "Abemaciclib" = "#c50084"),
    name = "Drug"
  ),
  labs(x = NULL, y = "% of total CDK4/6 inhibitor issuing"),
  theme_minimal(),
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.margin = margin(5, 5, 5, 105),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.box.spacing = unit(-20, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ),
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
)

# Build individual plots (top plot has no x labels; bottom has shared x labels)
stacked_bar_plot_2021 <- ggplot(plotData2021, aes(x = Cancer_Alliance, y = percentage, fill = drug)) +
  geom_bar(stat = "identity", position = "stack") +
  bar_chart_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

stacked_bar_plot_2024 <- ggplot(plotData2024, aes(x = Cancer_Alliance, y = percentage, fill = drug)) +
  geom_bar(stat = "identity", position = "stack") +
  bar_chart_theme

# Combine into subplot with a) and b) tags, shared legend and x tick labels (only on bottom)
combined_plot <- stacked_bar_plot_2021 / stacked_bar_plot_2024 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "", tag_suffix = ")") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 24),
    plot.tag.position = c(-0.08, 1)
  )

summary_table_2024 <- allianceData |>
  filter(year == 2024) |>
  select(Cancer_Alliance, pct_ribociclib, pct_palbociclib, pct_abemaciclib) |>
  mutate(Cancer_Alliance = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
  arrange(desc(pct_palbociclib))

summary_table_2021 <- allianceData |>
  filter(year == 2021) |>
  select(Cancer_Alliance, pct_ribociclib, pct_palbociclib, pct_abemaciclib) |>
  mutate(Cancer_Alliance = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
  arrange(match(Cancer_Alliance, alliance_order_2024))

ggsave(here("outputs/Cancer_Alliance_Stacked_Bar.svg"), combined_plot, width = 12, height = 12)
ggsave(here("outputs/Cancer_Alliance_Stacked_Bar.png"), combined_plot, dpi = 350, width = 12, height = 12)

write_csv(summary_table_2024, here("outputs/Cancer_Alliance_Percentages_2024.csv"))
write_csv(summary_table_2021, here("outputs/Cancer_Alliance_Percentages_2021.csv"))

# Calculate change in each drug's percentage between 2021 and 2024
change_data <- allianceData |>
  select(Cancer_Alliance, year, pct_ribociclib, pct_palbociclib, pct_abemaciclib) |>
  mutate(Cancer_Alliance = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
  pivot_wider(
    names_from = year,
    values_from = c(pct_ribociclib, pct_palbociclib, pct_abemaciclib)
  ) |>
  mutate(
    change_ribociclib = pct_ribociclib_2024 - pct_ribociclib_2021,
    change_palbociclib = pct_palbociclib_2024 - pct_palbociclib_2021,
    change_abemaciclib = pct_abemaciclib_2024 - pct_abemaciclib_2021
  ) |>
  select(Cancer_Alliance, change_ribociclib, change_palbociclib, change_abemaciclib) |>
  arrange(desc(change_abemaciclib))

write_csv(change_data, here("outputs/Cancer_Alliance_Percentages_Change_2021_2024.csv"))
