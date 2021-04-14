# Plot for ICU versus deaths in Belgium
# author: Joris Meys
# Date last modified: 2021-04-14
library(patchwork)
source("scripts/downloadData.R")

# Remove X axis from top 3 plots
theme_nox <- theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank())

# Process the data that's been downloaded 
hosptmp <- filter(rawhospit, PROVINCE == "All",REGION == "Belgium") %>%
  mutate(DATE = as.Date(DATE),
         smooth = ksmooth(DATE,TOTAL_IN_ICU,
                          kernel = "normal",
                          bandwidth = 7)$y)
deathtmp <- filter(rawdeaths, REGION == "Belgium",
                   SEX == "All",
                   !AGEGROUP %in% c("All","unknown") ) %>%
  mutate(DATE = as.Date(DATE)) %>%
  group_by(AGEGROUP) %>%
  mutate(smooth = ksmooth(DATE,DEATHS,
                          kernel = "normal",
                          bandwidth = 7)$y)

comb <- filter(rawdeaths, REGION == "Belgium",
               SEX == "All", AGEGROUP == "All") %>%
  select(DEATHS, DATE) %>%
  mutate(DATE = as.Date(DATE),
         smoothdeaths = ksmooth(DATE,DEATHS,
                                kernel = "normal",
                                bandwidth = 7)$y) %>%
  full_join(hosptmp, by = "DATE") %>%
  mutate(ratio = smoothdeaths/smooth)

# Create different plots
p1 <- ggplot(deathtmp,aes(x=DATE,y=smooth,
                    fill = AGEGROUP)) +
  geom_area(position = "fill") +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  scale_y_continuous(labels = label_percent(),
                     expand = c(0,0)) +
  labs(y = "% deaths by age",
       fill = "Age group") +
  theme_nox 

p2 <- ggplot(deathtmp,aes(x=DATE,y=smooth,
                          fill = AGEGROUP)) +
  geom_area() +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(y = "Deaths by age",
       fill = "Age group") +
  theme_nox

p3 <- ggplot(hosptmp, aes(x=DATE,y=smooth)) +
  geom_area() +
  theme_bw() +
  labs(y = "ICU patients") +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  theme_nox

p4 <- ggplot(na.omit(comb), aes(x=DATE,y=ratio)) +
  geom_area(fill = "darkred") +
  scale_y_continuous(labels = label_percent(),
                     expand = expansion(mult = c(0,0.05))) +
  theme_bw()

# Combine plots
(p1 / p2 / p3 / p4) * coord_cartesian(xlim = 
                                     as.Date(c("2020-03-15", "2021-04-15"))) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparison deaths and ICU patients Belgium",
    subtitle = paste("Data downloaded from https://epistat.wiv-isp.be/Covid/ on", Sys.Date()) ,
    caption = "@JorisMeys"
  )

ggsave("ICUvsDeaths.png",
       width = 8*1.5,
       height = 4.45*1.5)
