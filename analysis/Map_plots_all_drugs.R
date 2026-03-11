
require(ggplot2)
require(tidyverse)
require(gtsummary)
require(gt)
require(sf)
require(cowplot)
require(gridExtra)

options(warn = -1)
options(message = FALSE)


cdk46Data <- readr::read_csv(here::here("data/oph_data_11_25_processed.csv")) |>
  mutate(Date = as.Date(Date)) |>
  filter(!is.na(Cancer_Alliance) & Cancer_Alliance != "")

# Aggregate directly by Cancer Alliance (data already has Cancer_Alliance from trust-information)
cdk46Data |>
  group_by(year_month = Date, VTM = `VTM Name`, Cancer_Alliance) |>
  summarise(ddd = sum(Value), .groups = "drop") -> cdk46AllianceData

head(cdk46AllianceData)

# Prepare combined data with all three drugs
ribo <- cdk46AllianceData |>
  filter(VTM == "Ribociclib") |>
  select(-VTM) |>
  rename(Ribociclib = ddd)

palbo <- cdk46AllianceData |>
  filter(VTM == "Palbociclib") |>
  select(-VTM) |>
  rename(Palbociclib = ddd)

abema <- cdk46AllianceData |>
  filter(VTM == "Abemaciclib") |>
  select(-VTM) |>
  rename(Abemaciclib = ddd)

# Combine all three drugs
combinedData <- ribo |>
  full_join(palbo, by = c("year_month", "Cancer_Alliance")) |>
  full_join(abema, by = c("year_month", "Cancer_Alliance"))

# Replace NA with 0 for all drugs (may not be available in all periods)
combinedData <- combinedData |>
  mutate(
    Ribociclib = replace_na(Ribociclib, 0),
    Palbociclib = replace_na(Palbociclib, 0),
    Abemaciclib = replace_na(Abemaciclib, 0)
  )

# Filter to 2020 and 2024 data and aggregate by Cancer Alliance
allianceData <- combinedData |>
  filter(year_month >= as.Date("2020-01-01") & year_month <= as.Date("2024-12-31")) |>
  filter(year_month >= as.Date("2024-01-01") | year_month <= as.Date("2020-12-31")) |>
  mutate(year = year(year_month)) |>
  select(-year_month) |>
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

make_change_table <- function(data, pct_col, new_name) {
  data |>
    select(year, Cancer_Alliance, !!sym(pct_col)) |>
    pivot_wider(id_cols = "Cancer_Alliance", names_from = year, values_from = !!sym(pct_col)) |>
    mutate(pct_pt_change = `2024` - `2020`) |>
    rename(
      !!paste0(new_name, "_2020") := `2020`,
      !!paste0(new_name, "_2024") := `2024`,
      !!paste0(new_name, "_change") := pct_pt_change
    )
}

# Create tables showing percentages for each year
ribociclib_pct <- allianceData |>
  select(year, Cancer_Alliance, pct_ribociclib) |>
  pivot_wider(id_cols = "Cancer_Alliance", names_from = year, values_from = pct_ribociclib) |>
  rename(
    ribociclib_2020 = `2020`,
    ribociclib_2024 = `2024`
  )

palbociclib_pct <- allianceData |>
  select(year, Cancer_Alliance, pct_palbociclib) |>
  pivot_wider(id_cols = "Cancer_Alliance", names_from = year, values_from = pct_palbociclib) |>
  rename(
    palbociclib_2020 = `2020`,
    palbociclib_2024 = `2024`
  )

abemaciclib_pct <- allianceData |>
  select(year, Cancer_Alliance, pct_abemaciclib) |>
  pivot_wider(id_cols = "Cancer_Alliance", names_from = year, values_from = pct_abemaciclib) |>
  rename(
    abemaciclib_2020 = `2020`,
    abemaciclib_2024 = `2024`
  )

allPercentages <- ribociclib_pct |>
  left_join(palbociclib_pct, by = "Cancer_Alliance") |>
  left_join(abemaciclib_pct, by = "Cancer_Alliance")

# Load shapefile and mapping for name corrections
shp <- read_sf(here::here("data/map/Cancer_Alliances_July_2023_Boundaries/CAL_JUL_2023_EN_BFE.shp"))
shapefile_mapping <- read_csv(here::here("data/shapefile_cancer_alliance_mapping.csv"))

# Map shapefile CAL23NM to trust_information names (for corrections and East of England merge)
shp <- shp |>
  left_join(shapefile_mapping, by = c("CAL23NM" = "shapefile_CAL23NM")) |>
  mutate(merged_name = coalesce(trust_information_name, CAL23NM))

# Combine East of England polygons (merged in trust-information but not yet in shapefile)
shp <- shp |>
  group_by(merged_name) |>
  summarise(.groups = "drop")

# Prepare map data: Cancer Alliance base name (strip " Cancer Alliance")
# Matches shapefile CAL23NM for most; mapping file handles exceptions
mapData <- allPercentages |>
  mutate(merged_name = str_remove(Cancer_Alliance, " Cancer Alliance")) |>
  select(-Cancer_Alliance)

shp_with_data <- shp |>
  left_join(mapData, by = c("merged_name" = "merged_name"))

# Create map plotting function
create_percentage_figure <- function(data, col_2024, pct_label, show_legend = TRUE) {

  pct_scale <- scale_fill_viridis_c(limits = c(0, 100),
                                    name = pct_label,
                                    option = "plasma",
                                    direction = -1)  # Reverse so higher values are darker

  # Percentage map with optional legend
  pctMap <- ggplot(data) +
    geom_sf(aes(fill = .data[[col_2024]]), color = "black") +
    pct_scale +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
    theme_void() +
    if (!show_legend) theme(legend.position = "none")

  return(pctMap)
}

# Create individual maps without legends
ribociclib_plot <- create_percentage_figure(
  shp_with_data,
  "ribociclib_2024",
  "% Ribociclib\n(of total CDK4/6 inhibitors)",
  show_legend = FALSE
) +
  labs(title = "Ribociclib")

palbociclib_plot <- create_percentage_figure(
  shp_with_data,
  "palbociclib_2024",
  "% Palbociclib\n(of total CDK4/6 inhibitors)",
  show_legend = FALSE
) +
  labs(title = "Palbociclib")

abemaciclib_plot <- create_percentage_figure(
  shp_with_data,
  "abemaciclib_2024",
  "% Abemaciclib\n(of total CDK4/6 inhibitors)",
  show_legend = FALSE
) +
  labs(title = "Abemaciclib")

# Create a custom legend plot with a tall color bar
legend_plot <- ggplot(shp_with_data) +
  geom_sf(aes(fill = ribociclib_2024), color = "black") +
  scale_fill_viridis_c(limits = c(0, 100),
                       name = "% of total CDK4/6 inhibitors",
                       option = "plasma",
                       direction = -1) +
  guides(fill = guide_colourbar(title.position = "top", 
                                title.hjust = 0.5,
                                barheight = grid::unit(4, "inches"),
                                barwidth = grid::unit(0.4, "inches"))) +
  theme_void() +
  theme(legend.position = "right")

shared_legend <- cowplot::get_legend(legend_plot)

# Combine the three maps side by side
combined_plot <- cowplot::plot_grid(
  ribociclib_plot,
  palbociclib_plot,
  abemaciclib_plot,
  nrow = 1,
  align = "h",
  axis = "tb"
)

# Add the shared legend to the right of the maps
final_plot <- cowplot::plot_grid(
  combined_plot,
  shared_legend,
  nrow = 1,
  rel_widths = c(1, 0.15),
  align = "v",
  axis = "l"
)


final_plot

# Summary tables
# Ribociclib percentage of total CDK4/6 inhibitors
ribociclib_pct |>
  gt() |>
  fmt_number(columns = c("ribociclib_2020", "ribociclib_2024"), decimals = 1) |>
  cols_label(
    ribociclib_2020 = "2020 (%)",
    ribociclib_2024 = "2024 (%)"
  ) |>
  tab_header(title = "Ribociclib % (of total CDK4/6 inhibitors)")

# Palbociclib percentage of total CDK4/6 inhibitors
palbociclib_pct |>
  gt() |>
  fmt_number(columns = c("palbociclib_2020", "palbociclib_2024"), decimals = 1) |>
  cols_label(
    palbociclib_2020 = "2020 (%)",
    palbociclib_2024 = "2024 (%)"
  ) |>
  tab_header(title = "Palbociclib % (of total CDK4/6 inhibitors)")

# Abemaciclib percentage of total CDK4/6 inhibitors
abemaciclib_pct |>
  gt() |>
  fmt_number(columns = c("abemaciclib_2020", "abemaciclib_2024"), decimals = 1) |>
  cols_label(
    abemaciclib_2020 = "2020 (%)",
    abemaciclib_2024 = "2024 (%)"
  ) |>
  tab_header(title = "Abemaciclib % (of total CDK4/6 inhibitors)")

ggsave(here::here("outputs/CDK46_Percentage_Maps_Combined.svg"), final_plot, width = 20, height = 6)
ggsave(here::here("outputs/CDK46_Percentage_Maps_Combined.png"), final_plot, dpi = 350, width = 20, height = 6)

# Save combined percentage table to a single CSV
write_csv(allPercentages, here::here("outputs/CDK46_Percentage_table.csv"))
