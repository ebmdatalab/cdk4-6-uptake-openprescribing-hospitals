library(ggplot2)
library(tidyverse)
library(here)

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

# Filter to 2020 and 2024 data and aggregate by Cancer Alliance
allianceData <- combinedData |>
  filter(year_month >= as.Date("2020-01-01") & year_month <= as.Date("2024-12-31")) |>
  filter(year_month >= as.Date("2024-01-01") | year_month <= as.Date("2020-12-31")) |>
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

# Get 2024 data and prepare for plotting
plotData2024 <- allianceData |>
  filter(year == 2024) |>
  select(Cancer_Alliance, pct_ribociclib, pct_palbociclib, pct_abemaciclib) |>
  # Clean up alliance names by removing " Cancer Alliance" suffix
  mutate(Cancer_Alliance = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
  # Order by palbociclib percentage before converting to long format
  arrange(desc(pct_palbociclib)) |>
  mutate(Cancer_Alliance = factor(Cancer_Alliance, levels = unique(Cancer_Alliance))) |>
  # Convert to long format for ggplot
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "drug",
    names_prefix = "pct_",
    values_to = "percentage"
  ) |>
  # Clean up drug names and order for stacking (palbociclib on top)
  mutate(drug = str_to_title(drug)) |>
  mutate(drug = factor(drug, levels = c("Ribociclib", "Abemaciclib", "Palbociclib")))

# Create stacked bar chart
stacked_bar_plot <- ggplot(plotData2024, aes(x = Cancer_Alliance, y = percentage, fill = drug)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("Palbociclib"="#2a6ebb", "Ribociclib"="#00b2a9", "Abemaciclib"="#c50084"),
    name = "Drug"
  ) +
  labs(
    x = NULL,
    y = "Percentage of total CDK4/6 inhibitor issuing (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


summary_table_2024 <- allianceData |>
  filter(year == 2024) |>
  select(Cancer_Alliance, pct_ribociclib, pct_palbociclib, pct_abemaciclib) |>
  mutate(Cancer_Alliance = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
  arrange(desc(pct_palbociclib))

ggsave(here("outputs/Cancer_Alliance_Stacked_Bar.svg"), stacked_bar_plot, width = 12, height = 8)
ggsave(here("outputs/Cancer_Alliance_Stacked_Bar.png"), stacked_bar_plot, dpi = 350, width = 12, height = 8)

write_csv(summary_table_2024, here("outputs/Cancer_Alliance_Percentages_2024.csv"))
