require(tidyverse)
require(ggplot2)

colours <- c("Palbociclib_125"="#1f5189", "Abemaciclib_150"="#88005b","C1"="#00B2A9FF", "Palbociclib_100" = "#2a6ebb", "Abemaciclib_100"="#c50084","C2"="#00B2A9AA",
                 "Palbociclib_75" = "#b4cfee", "Abemaciclib_50"="#ff83d6","C3"="#00B2A966")

cdk46Data <- readr::read_csv(here::here("data/oph_data_11_25_processed.csv")) |>
  mutate(Date = as.Date(Date))

# Dose Reductions

# Convert DDD values back to tablet quantities
# tablets = (Value * DDD_mg) / tablet_strength_mg
doseSummary <- cdk46Data |>
  mutate(year = year(Date)) |>
  rowwise() |>
  mutate(vtm = stringr::str_split_1(as.character(`VMP Name`), " ")[1]) |>
  mutate(strength = stringr::str_split_1(as.character(`VMP Name`), " ")[2]) |>
  mutate(strength_mg = as.numeric(stringr::str_remove(strength, "mg"))) |>
  # Extract DDD value from Unit column (e.g., "DDD (94.0 mg)" or "DDD (0.3 g)")
  mutate(ddd_value = as.numeric(stringr::str_extract(Unit, "\\d+\\.?\\d*"))) |>
  mutate(ddd_unit = stringr::str_extract(Unit, "(mg|g)")) |>
  # Convert to mg if unit is grams
  mutate(ddd_mg = if_else(ddd_unit == "g", ddd_value * 1000, ddd_value)) |>
  # Convert DDD to tablet quantity
  mutate(quantity = (Value * ddd_mg) / strength_mg) |>
  filter(vtm != "Ribociclib") |>
  filter(year == 2024) |>
  group_by(vtm, strength_mg) |>
  summarise(quantity = sum(quantity)) |>
  mutate(Dose_level = toOrdinal::toOrdinal(rank(-strength_mg))) |>
  mutate(percentage = 100*(quantity/sum(quantity)))

pl_data <- doseSummary |>
  mutate(Dose_level = as.factor(Dose_level)) |>
  mutate(fill = paste(vtm,strength_mg, sep="_")) |>
  mutate(fill = fct_inorder(fill)) |>
  arrange(Dose_level) |>
  mutate(label = glue::glue(
    "{Dose_level} dose level:\n{strength_mg}mg\n\n{round(percentage,1)}%")) |>
  mutate(cum_percentage = cumsum(percentage) ) |>
  mutate(label_pos = cumsum(percentage) - (percentage/2))

pl_data |>
  ggplot(aes(x=vtm, y=percentage, fill=fill)) +
  geom_col() +
  theme_bw() +
  scale_fill_manual(values = colours) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black") ,
        legend.position = "none") +
  ylab("Percentage of doses dispensed at this dose level") +
  xlab("CDK 4/6 Inhibitor") +
  coord_flip() +
  geom_text(aes(label=label, y= label_pos), size=3.5, color=c("white", "white", "white", "white", "black", "black"))

ggsave(here::here("outputs/DoseReductions.svg"), width = 8, height = 3)
ggsave(here::here("outputs/DoseReductions.png"), dpi = 350, width = 8, height = 3)

# Summary statistics
# Percentage of patients on abemaciclib on a reduced dose
abemaciclib_reduced <- round(sum(doseSummary$percentage[doseSummary$vtm == "Abemaciclib" & doseSummary$Dose_level != "1st"]), 1)
cat("Percentage of patients on abemaciclib on a reduced dose:", abemaciclib_reduced, "%\n")

# Percentage of patients on palbociclib on a reduced dose
palbociclib_reduced <- round(sum(doseSummary$percentage[doseSummary$vtm == "Palbociclib" & doseSummary$Dose_level != "1st"]), 1)
cat("Percentage of patients on palbociclib on a reduced dose:", palbociclib_reduced, "%\n")
