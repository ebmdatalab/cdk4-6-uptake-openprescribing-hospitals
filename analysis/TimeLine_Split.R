### Time line graph with 3 sepearte sections
require(tidyverse)
require(ggplot2)
require(ggh4x)
require(ggtext)

BMJ_colours_odd_even <- c(Palbociclib = "#2a6ebb", Abemaciclib = "#c50084", Ribociclib = "#00b2a9", 
  Palbociclib_1 = "#2a6ebb", Abemaciclib_1 = "#c50084", Ribociclib_1 = "#00b2a9", 
  Palbociclib_0 = "#b4cfee", Abemaciclib_0 = "#ff83d6", Ribociclib_0 = "#70fff8"
)

keyDates <- read_csv(here::here("data/KeyDates.csv"))

# Build a framework
axisData <- expand_grid(
  year = 2016:2024,
  VTM = factor(c("Palbociclib", "Abemaciclib", "Ribociclib"), levels = c("Palbociclib", "Abemaciclib", "Ribociclib"))
) |>
  mutate(x1 = as.Date(paste0(year,"-01-01"))) |>
  mutate(x2 = x1+365) |>
  mutate(VTM_fill = paste(VTM, year%%2, sep="_"))
                   
keyDates |>
  mutate(date = as.Date(date, format="%d/%m/%Y")) |>
  mutate(VTM = factor(VTM, levels = c("Palbociclib", "Abemaciclib", "Ribociclib"))) |>
  select(VTM, event_date = date, event_label, event_level, event_align) |>
ggplot() +
  theme_void() +
  ylim(-3.2,3.2) +
  xlim(as.Date("2016-01-01"), as.Date("2025-01-01")) +
#  geom_point(aes(x=x, y=y)) +
  facet_wrap2(~VTM, ncol=1, strip.position="left", strip=strip_themed(
    background_y = elem_list_rect(fill=BMJ_colours_odd_even[c("Palbociclib", "Abemaciclib", "Ribociclib")]))) +
  geom_rect(data=axisData,
            inherit.aes = F,
            aes(xmin=x1, xmax=x2, ymin=-0.5, ymax = 0.5, fill=VTM_fill))+
  scale_fill_manual(values=BMJ_colours_odd_even)+
  geom_text(data=axisData[axisData$year%%2==1,],
            inherit.aes = F,
            aes(x=x1+(365/2), y =0, label = year), size=4, colour="white")+
  geom_text(data=axisData[axisData$year%%2==0,],
            inherit.aes = F,
            aes(x=x1+(365/2), y =0, label = year), size=4, colour="black")+
  theme(legend.position = "none", strip.text = element_text(angle=90, colour="white",
        size = 16,
        margin = margin(0.25,0.25,0.25,0.25, "cm")),
        plot.caption = element_markdown(size=10)) +
  
  ## Now add the events
  geom_segment(aes(x=event_date, xend=event_date, y= if_else(event_level>0, 0.3, -0.3), yend= event_level),
               arrow=arrow(
                 angle = 30, 
                 length = unit(0.02, "inches"), 
                 ends = "first", 
                 type = "open"
                 )
  ) +
  geom_richtext(aes(x=event_date, y = if_else(event_level> 0, event_level + 0.1, event_level-0.1), 
                    label=event_label, hjust=event_align, vjust=if_else(event_level>0, 0,1)), size = 3.5, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) -> event_timeline_split
  
ggsave(here::here("outputs/EventTimeLineSplit.svg"), event_timeline_split, width = 8, height =6)
ggsave(here::here("outputs/EventTimeLineSplit.png"), event_timeline_split, dpi = 350, width = 8, height =6)