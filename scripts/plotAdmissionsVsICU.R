# Plot for new admissions versus ICU
# author: Joris Meys
# Date last modified: 2021-04-14
source("scripts/downloadData.R")

tmp <- full_join(filter(rawcases,
                        SEX == "All",
                        AGEGROUP == "All"),
                 rawhospit,
                 by = c("PROVINCE","REGION","DATE")) %>%
  filter(PROVINCE == "All")

slice <- tmp %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(REGION == "Belgium",
         between(DATE, as.Date("2020-03-01"),
                 Sys.Date())) %>%
  na.omit()

# Silly trick to get the areas plotted in the 
# correct order and create a manual legend.
# Legends in ggplot are a pain to do manually...
slice <- mutate(slice,
                filla = alpha("#455053",0.5),
                fillb = "black",
                smoothICU = ksmooth(DATE,TOTAL_IN_ICU,
                                    kernel = "normal",
                                    bandwidth = 7)$y,
                smoothNEW = ksmooth(DATE,NEW_IN,
                                    kernel = "normal",
                                    bandwidth = 7)$y)

# Plot :
# - kernel smooth
# - numbers are more or less eyeballed. 
# - lag analysis showed a peak in ICU abt 10 days after admissions
ggplot(slice, aes(x = DATE, y = smoothICU )) +
  geom_area(aes(fill = filla) ) + 
  geom_area(mapping = aes(y = smoothNEW, fill = fillb)) +
  theme_minimal() +
  labs(y = "Number of patients",
       fill = "Daily new\nhospitalisations",
       subtitle = paste("Data downloaded from https://epistat.wiv-isp.be/Covid/ on", Sys.Date()) ,
       caption = "@JorisMeys") + 
  ggtitle("Patients in ICU - Belgium") +
  scale_fill_identity(
    guide = guide_legend(
      title = "", direction = "horizontal"),
    breaks = c(alpha("#455053",0.5),"black"),
    labels = c("Total in ICU","Daily new\nhospitalisations"),
    drop = FALSE
  ) +
  theme(legend.position = "bottom") +
  geom_segment(x = as.Date("2020-03-01"),
               xend = as.Date("2020-04-08"),
               y = 900,
               yend = 900, col = "red", lty = 2) +
  geom_segment(x = as.Date("2020-03-29"),
               xend = as.Date("2020-03-29"),
               y = 0,
               yend = 1265, col = "red", lty = 2) +
  geom_segment(x = as.Date("2020-03-01"),
             xend = as.Date("2020-04-08"),
             y = 1265,
             yend = 1265, col = "blue", lty = 2) +
  geom_segment(x = as.Date("2020-04-08"),
               xend = as.Date("2020-04-08"),
               y = 0,
               yend = 1265, col = "blue", lty = 2) +
  # annotate(geom = "text",
  #          x = as.Date("2020-06-15"),
  #           y = 1150,
  #           label = "+40% 10 days after\npeak in new hospitalisations",
  #           color = "black") +
  geom_segment(x = as.Date("2020-03-01"),
               xend = as.Date("2020-08-21"),
               y = 74,
               yend = 74, col = "red", lty = 2) +
  geom_segment(x = as.Date("2020-08-11"),
               xend = as.Date("2020-08-11"),
               y = 0,
               yend = 85, col = "red", lty = 2) +
  geom_segment(x = as.Date("2020-03-01"),
               xend = as.Date("2020-08-21"),
               y = 85,
               yend = 85, col = "blue", lty = 2) +
  geom_segment(x = as.Date("2020-08-21"),
               xend = as.Date("2020-08-21"),
               y = 0,
               yend = 85, col = "blue", lty = 2) +
  # annotate(geom = "text",
  #          x = as.Date("2020-08-15"),
  #          y = 200,
  #          label = "+14% 10 days after\npeak in new hospitalisations",
  #          color = "black") +
  # New addition
  geom_segment(x = as.Date("2020-03-01"),
               xend = as.Date("2020-11-11"),
               y = 1161,
               yend = 1161, col = "red", lty = 2) +
  geom_segment(x = as.Date("2020-10-31"),
               xend = as.Date("2020-10-31"),
               y = 0,
               yend = 1465, col = "red", lty = 2) +
  geom_segment(x = as.Date("2020-03-01"),
               xend = as.Date("2020-11-10"),
               y = 1465,
               yend = 1465, col = "blue", lty = 2) +
  geom_segment(x = as.Date("2020-11-10"),
               xend = as.Date("2020-11-10"),
               y = 0,
               yend = 1465, col = "blue", lty = 2) +
  annotate(geom = "text",
           x = as.Date("2020-08-01"),
           y = 600,
           label = "Peak in ICU total 10 days after\npeak in new hospitalisations",
           color = "black") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b %d") +
  geom_segment(x = as.Date("2020-03-01"),
               xend = as.Date("2021-04-12"),
               y = 269,
               yend = 269, col = "red", lty = 2) +
  geom_segment(x = as.Date("2021-04-01"),
               xend = as.Date("2021-04-01"),
               y = 0,
               yend = 917, col = "red", lty = 2) +
  geom_segment(x = as.Date("2020-03-01"),
               xend = as.Date("2021-04-12"),
               y = 917,
               yend = 917, col = "blue", lty = 2) +
  geom_segment(x = as.Date("2021-04-12"),
               xend = as.Date("2021-04-12"),
               y = 0,
               yend = 917, col = "blue", lty = 2)


