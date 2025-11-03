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
axisData <- tibble(year = rep(2016:2025,3),
                   VTM = rep(c("Abemaciclib","Palbociclib", "Ribociclib"),10)) |>
  mutate(x1 = as.Date(paste0(year,"-01-01"))) |>
  mutate(x2 = x1+365) |>
  mutate(VTM_fill = paste(VTM, year%%2, sep="_"))
                   
keyDates |>
  mutate(date = as.Date(date, format="%d/%m/%Y")) |>
  select(VTM, event_date = date, event_label, event_level, event_align) |>
ggplot() +
  theme_void() +
  ylim(-2,3.2) +
  xlim(as.Date("2016-01-01"), as.Date("2026-01-01")) +
#  geom_point(aes(x=x, y=y)) +
  facet_wrap2(~VTM, ncol=1, strip.position="left", strip=strip_themed(
    background_y = elem_list_rect(fill=sort(BMJ_colours_odd_even[1:3], decreasing=T)))) +
  geom_rect(data=axisData,
            inherit.aes = F,
            aes(xmin=x1, xmax=x2, ymin=-0.25, ymax = 0.25, fill=VTM_fill))+
  scale_fill_manual(values=BMJ_colours_odd_even)+
  geom_text(data=axisData[axisData$year%%2==1,],
            inherit.aes = F,
            aes(x=x1+(365/2), y =0, label = year), size=2.75, colour="white")+
  geom_text(data=axisData[axisData$year%%2==0,],
            inherit.aes = F,
            aes(x=x1+(365/2), y =0, label = year), size=2.75, colour="black")+
  theme(legend.position = "none", strip.text = element_text(angle=90, colour="white",
        margin = margin(0.25,0.25,0.25,0.25, "cm")),
        plot.caption = element_markdown()) +
  
  ## Now add the events
  geom_segment(aes(x=event_date, xend=event_date, y= if_else(event_level>0, 0.3, -0.3), yend= event_level),
               arrow=arrow(
                 angle = 30, 
                 length = unit(0.05, "inches"), 
                 ends = "first", 
                 type = "open")
  ) +
  geom_richtext(aes(x=event_date, y = if_else(event_level> 0, event_level + 0.1, event_level-0.1), 
                    label=event_label, hjust=event_align, vjust=if_else(event_level>0, 0,1)), size = 2.5, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  labs(caption = "**1L:** First line (with aromatase inhibitor),  **2L:** Second line (with fulvestrant),  *n.s.*: Not significant<br/>
       **OS:** Overall survival, **PFS:** Progression free survival") -> event_timeline_split
  
ggsave(here::here("r/outputs/EventTimeLineSplit.svg"), event_timeline_split, width = 8, height =6)
ggsave(here::here("r/outputs/EventTimeLineSplit.png"), event_timeline_split, dpi = 350, width = 8, height =6)
