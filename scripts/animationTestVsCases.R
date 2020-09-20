# Create plot on tests and cases
library(viridis)
library(gganimate)
# For this, you need 
# source("scripts/downloadData.R")
regionalsmooth <- readRDS("Data/regionalsmooth.RDS")
# Make plot data
pdata <- filter(regionalsmooth, REGION == "Belgium",
                as.Date(DATE) > "2020-05-01",
                as.Date(DATE) < (Sys.Date() - 4)) %>%
  na.omit() %>%
  mutate(posrate = round(CASES/TESTS_ALL*100, 1),
         DATE = as.Date(DATE))

# Make raster
grid <- expand.grid(
 seq(0, max(pdata$TESTS_ALL)*1.05, length.out = 200),
 seq(0, max(pdata$CASES)*1.05, length.out = 200)
)

names(grid) <- c("y","x")
grid$val <- with(grid, x/y*100)
grid$val[grid$val > 100] <- 100
grid <- na.omit(grid)

# Make labels
labels <- data.frame(
  x=c(210,350,650,950, 950, 950),
  y = c(30000,30000,30000,30000,22000,18000),
  label = c("0.5%","1%","2%","3%","4%","5%")
)

p <- ggplot(pdata, aes(x=CASES, y = TESTS_ALL)) +
  geom_raster(mapping = aes(x=x,y=y,fill=val),
              data = grid) +
  geom_abline(intercept = 0, 
              slope = c(1/0.005,1/0.01, 1/0.02, 1/0.03, 1/0.04, 1/0.05),
              color = "lightgrey") +
  geom_path(lwd = 2, mapping = aes(color = DATE, group = 1)) +
  geom_point(show.legend = FALSE, size = 3) +
  labs(x = "number of cases",
       y = "number of tests",
       fill = "positivity\nrate %") +
  geom_text(mapping = aes(x=x,y=y,label=label),
            data = labels) +
  scale_y_continuous(limits = c(0,NA), expand = c(0,0.5) ) +
  scale_x_continuous(limits = c(0,NA), expand = c(0,0.5) ) +
  theme_classic() +
  scale_color_date(low = "blue", high = "white") +
  scale_fill_gradientn(colours = inferno(9,
                                         direction = -1,
                                         end = 1),
                       values = c(0,0.3,0.4,0.5,0.55,0.6,
                                  0.65,0.7,1),
                       trans = "log10") +
  labs(caption = "Data from https://epistat.wiv-isp.be/covid/")

anim <- p + transition_reveal(DATE) +
  labs(title = "Date: {pdata$DATE[round(frame)]} - positivity rate: {pdata$posrate[round(frame)]} % ")

animate(anim, nframes = nrow(pdata) + 20,
        fps = 5, end_pause = 20)
anim_save("testVsCases.gif",
          anim,
          path = "Data")
