library(dplyr)
library(ggplot2)
library(patchwork)

source("https://raw.githubusercontent.com/JoFAM/covidBE_analysis/master/functions/simulate_variants.R")

calc_p <- function(r,reff,immune){
  reff / (r * (1 - immune))
}

inc_kernel <- dlnorm(seq(1,14), mean = 1.63, sd = 0.5)
# Normalization to sum to 1:
inc_kernel <- inc_kernel / sum(inc_kernel)

# pars
all_t <- 90
reinf_ref <- 0.1
reinf <- 0.2
vacc_speed <- 40000

res1 <- replicate_series(n = 500,
                        t = all_t,
                        prev = seq(3850, 3750, length.out = 14),
                        kernel = inc_kernel,
                        tot_pop = 11e6,
                        vacc = 100000,
                        vacc_speed = vacc_speed,
                        immune = 3000000,
                        p_binom = 0.54,
                        r2 = 3.75,
                        p_reinfect = reinf_ref) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-01") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant")))

res2 <- replicate_series(n = 500,
                         t = all_t,
                         prev = seq(3850, 3750, length.out = 14),
                         kernel = inc_kernel,
                         tot_pop = 11e6,
                         vacc = 100000,
                         vacc_speed = vacc_speed,
                         immune = 3000000,
                         p_binom = 0.54,
                         r2 = 4.375,
                         p_reinfect = reinf_ref) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-01") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant")))

res3 <- replicate_series(n = 500,
                         t = all_t,
                         prev = seq(3850, 3750, length.out = 14),
                         kernel = inc_kernel,
                         tot_pop = 11e6,
                         vacc = 100000,
                         vacc_speed = vacc_speed,
                         immune = 3000000,
                         p_binom = 0.54,
                         r2 = 3.75,
                         p_reinfect = reinf) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-01") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant")))
res4 <- replicate_series(n = 100,
                         t = all_t,
                         prev = seq(3850, 3750, length.out = 14),
                         kernel = inc_kernel,
                         tot_pop = 11e6,
                         vacc = 100000,
                         vacc_speed = vacc_speed,
                         immune = 3000000,
                         p_binom = 0.54,
                         r2 = 4.375,
                         p_reinfect = reinf) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-01") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant"))
         )

p1 <- ggplot(res1, aes(x = t, fill = var)) +
  geom_area(aes(y = median, color = var), lwd = 1,
            show.legend = FALSE) +
  labs(x = "Date",
       y = "Confirmed cases / day",
       fill = "Variant") +
  theme_bw() +
  ggtitle(paste0("New variant: 50% more infections (R0 = 3.75) \n",reinf_ref*100,"% reinfection \nVaccination: ",vacc_speed/1000,"k per day on average")) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(ylim = c(0,15000))

p2 <- ggplot(res2, aes(x = t, fill = var)) +
  geom_area(aes(y = median, color = var), lwd = 1,
            show.legend = FALSE) +
  labs(x = "Date",
       y = "Confirmed cases / day",
       fill = "Variant") +
  theme_bw() +
  ggtitle(paste0("New variant: 75% more infections (R0 = 4.375) \n",reinf_ref*100,"% reinfection \nVaccination: ",vacc_speed/1000,"k per day on average")) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(ylim = c(0,15000))

p3 <- ggplot(res3, aes(x = t, fill = var)) +
  geom_area(aes(y = median, color = var), lwd = 1,
            show.legend = FALSE) +
  labs(x = "Date",
       y = "Confirmed cases / day",
       fill = "Variant") +
  theme_bw() +
  ggtitle(paste0("New variant: 50% more infections (R0 = 3.75) \n",reinf*100,"% reinfection \nVaccination: ",vacc_speed/1000,"k per day on average")) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(ylim = c(0,15000))

p4 <- ggplot(res4, aes(x = t, fill = var)) +
  geom_area(aes(y = median, color = var), lwd = 1,
            show.legend = FALSE) +
  labs(x = "Date",
       y = "Confirmed cases / day",
       fill = "Variant") +
  theme_bw() +
  ggtitle(paste0("New variant: 75% more infections (R0 = 4.375) \n",reinf*100,"% reinfection \nVaccination: ",vacc_speed/1000,"k per day on average")) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(ylim = c(0,15000))

library(patchwork)

allp <- (p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")
allp

ggsave(paste0("inf-",reinf,"vacc-",vacc_speed,"ref-",reinf_ref,".png"),
       allp,
       width = 12, height = 7)
