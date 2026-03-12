# Table 1 - Summary of Dispensing

library(tidyverse)
library(gtsummary)
library(flextable)

cdk46Data <- read_csv(here::here("data/oph_data_11_25_processed.csv")) |>
  rename(year_month = Date, vmp_code = `VMP Code`, vmp_name = `VMP Name`,
         vtm_name = `VTM Name`, ddd_quantity = Value)

cdk46Data |>
  mutate(year = year(year_month)) |>
  rowwise() |>
  mutate(vtm = vtm_name) |>
  mutate(vmp_short_name = paste(stringr::str_split_1(as.character(vmp_name), " ")[2:3], collapse = " ")) |>
  filter(year < 2025) |>
  group_by(vtm, vmp_short_name, vmp_code, year) |>
  summarise(quantity = sum(ddd_quantity)) |>
  pivot_wider(names_from = year, values_from = quantity) |>
  mutate(vmp = paste0(vmp_short_name, " (", vmp_code, ")"), .after = vtm) |>
  mutate(sort_value = as.numeric(stringr::str_split_1(vmp, "mg")[1])) |>
  ungroup() |>
  group_by(vtm) |>
  arrange(-sort_value, .by_group = TRUE) |>
  select(-sort_value) |>
  ungroup() |>
  select(-vmp_short_name, -vmp_code) |>
  rowwise() |>
  mutate(Total = sum(`2019`, `2020`, `2021`, `2022`, `2023`, `2024`, na.rm = TRUE), .after = `2024`) |>
  as_grouped_data(groups = c("vtm"), columns = NULL) |>
  select(-c(`2020`, `2021`, `2022`, `2023`)) |>
  mutate(DDD = c("300mg", NA, NA, NA, "94mg", NA, NA, NA, NA, NA, NA, "450mg", NA), .before = `2019`) -> coreTable

cdk46Data |>
  mutate(year = year(year_month)) |>
  mutate(vtm = vtm_name) |>
  group_by(vtm, year) |>
  summarise(ddd_quantity = sum(ddd_quantity)) |>
  pivot_wider(names_from = year, values_from = ddd_quantity) |>
  ungroup() |>
  rowwise() |>
  mutate(Total = sum(`2019`, `2020`, `2021`, `2022`, `2023`, `2024`, na.rm = TRUE), .after = `2024`) |>
  select(-c(`2020`, `2021`, `2022`, `2023`)) -> dddTable

coreTable |>
  rows_update(dddTable, by = "vtm") |>
  mutate(vtm = case_when(
    vtm == "Abemaciclib" ~ "Abemaciclib (774399006)",
    vtm == "Palbociclib" ~ "Palbociclib (777047009)",
    vtm == "Ribociclib" ~ "Ribociclib (777435007)",
    TRUE ~ NA
  )) |>
  rename("VTM (SNOMED CT Code)" = vtm, "VMP (dm+d Code)" = vmp, "Last year" = `2024`) -> coreTable

vtm_rows <- c(1, 5, 12)
grand_totals <- coreTable |>
  slice(vtm_rows) |>
  summarise(across(c(`2019`, `Last year`, Total), \(x) sum(x, na.rm = TRUE)))

fmt_count_pct <- function(count, denom) {
  if_else(is.na(count), "",
    if_else(is.na(denom) | denom == 0, format(round(count), big.mark = ",", trim = TRUE),
      paste0(format(round(count), big.mark = ",", trim = TRUE), " (", round(100 * count / denom, 1), "%)")
    ))
}

coreTable <- coreTable |>
  mutate(
    denom_2019 = grand_totals$`2019`[1],
    denom_last = grand_totals$`Last year`[1],
    denom_Total = grand_totals$Total[1]
  ) |>
  mutate(
    `2019` = fmt_count_pct(`2019`, denom_2019),
    `Last year` = fmt_count_pct(`Last year`, denom_last),
    Total = fmt_count_pct(Total, denom_Total)
  ) |>
  select(-denom_2019, -denom_last, -denom_Total)

ft <- coreTable |>
  flextable() |>
  bold(i = c(1, 5, 12)) |>
  flextable::footnote(
    i = c(1, 5, 12, 1, 5, 12, 1, 5, 12), j = c(4, 4, 4, 5, 5, 5, 6, 6, 6),
    value = as_paragraph(c("Defined daily doses")),
    ref_symbols = c("*"),
    part = "body", inline = TRUE
  ) |>
  flextable::footnote(
    i = c(6, 8, 10), j = c(2, 2, 2),
    value = as_paragraph(c("Palbociclib capsules were phased out in January 2021")),
    ref_symbols = c("†"),
    part = "body", inline = TRUE
  ) |>
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 8, part = "all") |>
  hline(i = c(4, 11), border = officer::fp_border(color = "black"), part = "body") |>
  width(width = c(2.4, 5.7, 1.9, 1.9, 1.9, 1.9), unit = "cm")

print(ft)

write_csv(coreTable, here::here("outputs/Table1.csv"))
