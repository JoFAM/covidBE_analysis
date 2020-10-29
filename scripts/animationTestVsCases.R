# Create plot on tests and cases
library(ggplot2)
library(viridis)
library(gganimate)
library(dplyr)
# For this, you need 
# source("scripts/downloadData.R")
thefile <- dir("Processed", pattern = "regionalweekavg")
regionalsmooth <- readRDS(file.path("Processed", thefile))
# Make plot data
pdata <- filter(regionalsmooth, REGION == "Belgium",
 #               as.Date(DATE) > "2020-05-01",
                as.Date(DATE) < (Sys.Date() - 4)) %>%
  na.omit() %>%
  mutate(posrate = round(TESTS_ALL_POS/TESTS_ALL*100, 1),
         DATE = as.Date(DATE))

# Make raster
grid <- expand.grid(
 seq(0, max(pdata$TESTS_ALL)*1.05, length.out = 200),
 seq(0, max(pdata$TESTS_ALL_POS)*1.05, length.out = 200)
)

names(grid) <- c("x","y")
grid$val <- with(grid, y/x*100)
grid$val[grid$val > 100] <- 100
grid <- na.omit(grid)

# Make lines
maxprate <- max(pdata$posrate)

if(maxprate <= 10){
  refline <- c(0.005, seq(0.01, ceiling(max(pdata$posrate))/100,
                          by = 0.01))
} else{
  refline <- c(0.01,0.02,0.03,
               seq(0.05, ceiling(maxprate/5)/20,
                   by = 0.05))
}


# Make labels
maxx <- max(pdata$TESTS_ALL)*1.05
maxy <- max(pdata$TESTS_ALL_POS)*1.05

# Position
refvert <- head(refline, -2)
refhoriz <- tail(refline, 2)
ylab <- maxx * 0.75 * c(refvert,
                        rep(tail(refvert,1),2))+ maxy*0.02
xlab <- maxx* c(rep(0.75,length(refvert)),
                0.65,0.55)
labels <- data.frame(
  y=ylab,
  x = xlab,
  label = paste0(refline*100,"%")
)

p <- ggplot(pdata, aes(x=TESTS_ALL, y = TESTS_ALL_POS)) +
  geom_raster(mapping = aes(x=x,y=y,fill=val),
              data = grid) +
  geom_abline(intercept = 0, 
              slope = refline,
              color = "lightgrey") +
  geom_path(lwd = 2, mapping = aes(color = DATE, group = 1)) +
  geom_point(show.legend = FALSE, size = 3) +
  labs(x = "number of tests",
       y = "number of positive tests",
       fill = "positivity\nrate %") +
  geom_text(mapping = aes(x=x,y=y,label=label),
            data = labels,
            hjust = "right") +
  scale_y_continuous(limits = c(0,NA), expand = c(0,0.5) ) +
  scale_x_continuous(limits = c(0,NA), expand = c(0,0.5) ) +
  theme_classic() +
  scale_color_date(low = "blue", high = "white") +
  scale_fill_gradientn(colours = inferno(9,
                                         direction = -1,
                                         end = 1),
                       values = c(0,0.3,0.4,0.5,0.6,0.7,
                                  0.8,0.9,1),
                       trans = "log10") +
  labs(caption = paste("Data from https://epistat.wiv-isp.be/covid/,",
                       "downloaded on",Sys.Date()))

anim <- p + transition_reveal(DATE) +
  labs(title = "Date: {pdata$DATE[round(frame)]} - positivity rate: {sprintf('%1.1f', pdata$posrate[round(frame)]) } % - data for Belgium")

# In case you want to check it in the viewer.
# animate(anim, nframes = nrow(pdata) + 20,
#          fps = 5, end_pause = 20)

anim_save("testVsCases.gif",
          anim,
          path = "Data",
          nframes = nrow(pdata) + 100,
          fps = 20, end_pause = 100,
          width = 900,
          height = 900,
          res = 150)

