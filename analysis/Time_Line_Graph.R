require(dplyr)
require(tidyr)
require(ggplot2)
require(readr)
require(here)
require(scales)
require(stringr)
require(ggtext)

# Define BMJ colour scheme
BMJ_colours <- c("Palbociclib"="#2a6ebb", 
                 "Abemaciclib"="#c50084",
                 "Ribociclib"="#00b2a9", 
                 "Total"="#808080", 
                 "Palbociclib_line" = "#b4cfee", 
                 "Abemaciclib_line"="#ff83d6",
                 "Ribociclib_line"="#70fff8")

# Load and process data
cdk46Data <- readr::read_csv(here::here("data/oph_data_11_25_processed.csv")) |>
  mutate(Date = as.Date(Date)) |>
  filter(Date <= as.Date("2024-12-01"))

# Aggregate the data to VTM & month
cdk46MonthlyData <- cdk46Data |>
  group_by(year_month = Date, VTM = `VTM Name`) |>
  summarise(ddd = sum(Value))

# Add total line
cdk46MonthlyTotals <- cdk46MonthlyData |>
  group_by(year_month) |>
  summarise(ddd=sum(ddd)) |>
  mutate(VTM = "Total", .before=ddd)

cdk46MonthlyData <- cdk46MonthlyData |>
  rows_append(cdk46MonthlyTotals) |>
  arrange(year_month) |>
  mutate(VTM = factor(VTM, levels = c("Palbociclib", "Abemaciclib", "Ribociclib", "Total")))

# Load key dates for timeline markers
KeyDates <- readr::read_csv(here::here("data/KeyDates.csv")) |>
  mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  mutate(VTM = factor(VTM, levels = c("Palbociclib", "Abemaciclib", "Ribociclib"))) |>
  mutate(display_label = stringr::str_remove_all(coalesce(event_label, event), "HR [0-9.]+ \\([0-9.-]+\\)<br/>"))

# Create the base plot
cdk46Plot <- cdk46MonthlyData |>
  ggplot(aes(
         x=year_month,
         y=ddd,
         colour = VTM
       )) +
  geom_line(lwd=1.1) +
  theme_classic() +
  scale_color_manual(values = BMJ_colours, 
                     breaks = c("Palbociclib", "Abemaciclib", "Ribociclib", "Total")) +
  # Force x axis to show wider timeline, and better break points (match quantile_plots.py)
  scale_x_date("Month",
                     limits = c(as.Date("2019-01-01"), as.Date("2025-01-01")), 
                     date_breaks = "1 year",
                     date_minor_breaks = "1 month",
                     date_labels = "%Y",
               expand = expansion(mult = c(0, 0.02))
               ) +
  # Format the y-axis scales and set a better range
  scale_y_continuous("Defined Daily Doses (DDDs)",
                     labels = scales::label_comma(), # Add commas to numbers
                     limits = c(0, 350000), 
                     expand = c(0, 0),
                     breaks = seq(0, 350000, by = 50000)) +
  # Label x-axis
  xlab("Month") +
  # Theme adjustments
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.grid.major = element_line(colour = scales::alpha("grey", 0.5), linewidth = 0.5),
        panel.grid.minor.x = element_line(colour = scales::alpha("grey", 0.3), linewidth = 0.3),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        axis.ticks.length.x = unit(0.15, "cm"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.text = element_text(size = 12))

# Add timeline markers and labels
cdk46Plot <- cdk46Plot +
  # plot all the vertical marker lines
  geom_segment(data=KeyDates, 
               aes(x=as.Date(date), xend=as.Date(date), y = 0, yend= marker, colour=paste0(VTM,"_line")),
               lwd=0.8, lty=5, show.legend = FALSE) +
  # plot all the left horizontal marker lines
  geom_segment(data=filter(KeyDates, text_pos == "left"), 
               aes(x=as.Date(date), xend=as.Date(date)-60, y = marker, yend= marker, colour=paste0(VTM,"_line")),
               lwd=0.8, lty=5, show.legend = FALSE) +
  # plot all the right horizontal marker lines
  geom_segment(data=filter(KeyDates, text_pos == "right"), 
               aes(x=as.Date(date), xend=as.Date(date)+60, y = marker, yend= marker, colour=paste0(VTM,"_line")),
               lwd=0.8, lty=5, show.legend = FALSE) +
  # plot all the top horizontal marker lines
  geom_segment(data=filter(KeyDates, text_pos == "above"), 
               aes(x=as.Date(date)-30, xend=as.Date(date)+30, y = marker, yend= marker,
                   colour=paste0(VTM,"_line")), lwd=0.8, lty=5, show.legend = FALSE)  +
  # plot all the left text (use event_label like EventTimeLineSplit)
  geom_richtext(data=filter(KeyDates, text_pos == "left"), 
               aes(x=as.Date(date) - 65, y = marker, colour=VTM, 
                   label = display_label), 
            size = 3.5, hjust = 1, show.legend = FALSE,
            fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  # plot all the right text
  geom_richtext(data=filter(KeyDates, text_pos == "right"), 
               aes(x=as.Date(date) + 65, y = marker, colour=VTM, 
                   label = display_label), 
            size = 3.5, hjust = 0, show.legend = FALSE,
            fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  # plot all the above text
  geom_richtext(data=filter(KeyDates, text_pos == "above"), 
               aes(x=as.Date(date), y = marker+5000, colour=VTM, 
                   label = display_label), 
            size = 3.5, hjust = 0.5, vjust = 0, show.legend = FALSE,
            fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  # Redraw the lines so they are on top 
  geom_line(lwd=1.1)

# Export plot
ggsave(here::here("outputs/TimeLineFigure.svg"), cdk46Plot, width = 8, height = 6)
ggsave(here::here("outputs/TimeLineFigure.png"), cdk46Plot, dpi = 350, width = 8, height = 6)
