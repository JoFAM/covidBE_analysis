# Plot for hospital situation by province
# author: Joris Meys
# Date last modified: 2021-04-20
source("scripts/downloadData.R")

smoother <- function(x,dates){
  ksmooth(dates,x,
          kernel = "normal",
          bandwidth = 7,
          n.points = length(x))$y
}

thelevels <- c("WestVlaanderen", "OostVlaanderen",
               "Antwerpen", "Limburg", "VlaamsBrabant",
               "BrabantWallon","Liège","Namur",
               "Luxembourg","Hainaut","Brussels",
               "België")

slice <- rawhospit %>%
  filter(! PROVINCE %in% c("All","unknown") |
           PROVINCE == "All" & REGION == 'Belgium') %>%
  select(-REGION) %>%
  mutate(DATE = as.Date(DATE),
         PROVINCE = recode(PROVINCE, "All" = "België"),
         PROVINCE = factor(PROVINCE,
                           levels = thelevels)) %>%
  group_by(PROVINCE) %>%
  mutate(
    smoothin = smoother(TOTAL_IN, DATE),
    smoothnew = smoother(NEW_IN, DATE),
    smoothicu = smoother(TOTAL_IN_ICU, DATE),
    fracicu = smoothicu / smoothin
  ) %>%
  filter(between(DATE,
                 as.Date("2021-02-01"),
                 Sys.Date()))

ggplot(slice, aes(x=DATE)) +
  geom_area(aes(y=smoothin, color = "# patiënten\nin hospitaal"),
            fill = alpha("#455053",0.5)) +
  geom_bar(aes(y = smoothicu,
               fill = fracicu),
           stat = "identity",
            width = 1) +
  geom_line(aes(y = smoothicu),
            color = "black") +
  facet_wrap(vars(PROVINCE),
             scales = "free_y",
             ) +
  scale_color_manual(
    "", values = "black"
  ) +
  scale_fill_viridis_c(option = "B",
                       direction = -1,
                       labels = label_percent()) +
  labs(fill = "# en fractie\nin ICU",
       y = "Aantal patiënten",
       x = "",
       title = "Belasting ziekenhuis en ICU per provincie",
       subtitle = paste("Data gedownload van Sciensano op", as.character(Sys.Date())),
       caption = "@JorisMeys") +
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  scale_x_date(expand = c(0,0),
               date_breaks = "1 month",
               date_labels = "%b %d") +
  coord_cartesian(xlim = c(as.Date("2021-02-01"),
                           Sys.Date() - 1)) +
  guides(color = guide_legend(order = 1),
         fill = guide_colourbar(order = 2))

ggsave("HospitaalBelasting.png",
       width = 8*1.5,
       height = 4.45*1.5)
