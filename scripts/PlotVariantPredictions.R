library(dplyr)
library(ggplot2)
library(patchwork)

## Get the functions needed
source("https://raw.githubusercontent.com/JoFAM/covidBE_analysis/master/functions/simulate_variants.R")

## Per jan 27, cases of the previous 2 weeks
## calculated as rolling average, multiplied by 2 to account
## for nondetected cases
prevcases <- c(2046, 2002, 2019, 2026, 1995, 1975, 1969, 
               1986, 1986, 2014, 2051, 2122, 2148, 2159)*2

calc_p <- function(r,reff,immune){
  reff / (r * (1 - immune))
}

inc_kernel <- dlnorm(seq(1,14), mean = 1.63, sd = 0.5)
# Normalization to sum to 1:
inc_kernel <- inc_kernel / sum(inc_kernel)

# Calculate p_binom
reff <- mean(prevcases[-c(1:3)] /zoo::rollmean(prevcases,4))
# Take into account part is already new variant.
p_binom <- calc_p(2.7, reff, 0.3)


# pars
all_t <- 90
reinf_ref <- 0.1
reinf <- 0.2
vacc_speed <- 10000
vaccs <- 229863
prop_newvar <- 0.25

res1 <- replicate_series(n = 500,
                        t = all_t,
                        prev = prevcases,
                        kernel = inc_kernel,
                        tot_pop = 11e6,
                        vacc = vaccs,
                        vacc_speed = vacc_speed,
                        immune = 3000000,
                        p_binom = p_binom,
                        r2 = 3.75,
                        p_reinfect = reinf_ref,
                        prop_newvar = prop_newvar) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-24") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant")))

res2 <- replicate_series(n = 500,
                         t = all_t,
                         prev = prevcases,
                         kernel = inc_kernel,
                         tot_pop = 11e6,
                         vacc = vaccs,
                         vacc_speed = vacc_speed,
                         immune = 3000000,
                         p_binom = p_binom,
                         r2 = 3.25,
                         p_reinfect = reinf_ref,
                         prop_newvar = prop_newvar) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-24") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant")))

res3 <- replicate_series(n = 500,
                         t = all_t,
                         prev = prevcases,
                         kernel = inc_kernel,
                         tot_pop = 11e6,
                         vacc = vaccs,
                         vacc_speed = vacc_speed,
                         immune = 3000000,
                         p_binom = p_binom,
                         r2 = 3.75,
                         p_reinfect = reinf,
                         prop_newvar = prop_newvar) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-24") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant")))
res4 <- replicate_series(n = 100,
                         t = all_t,
                         prev = prevcases,
                         kernel = inc_kernel,
                         tot_pop = 11e6,
                         vacc = vaccs,
                         vacc_speed = vacc_speed,
                         immune = 3000000,
                         p_binom = p_binom,
                         r2 = 3.25,
                         p_reinfect = reinf,
                         prop_newvar = prop_newvar) %>%
  mutate(across(c(ll, ul, median),
                function(x) x/2)) %>%
  filter(var != "tot") %>%
  mutate(t = as.Date("2021-01-24") + t,
         var = factor(var,
                      levels = c("v2","v1"),
                      labels = c("New variant",
                                 "Old variant"))
         )

p1 <- ggplot(res1, aes(x = t, fill = var)) +
  geom_area(aes(y = median, color = var), 
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
  geom_area(aes(y = median, color = var), 
            show.legend = FALSE) +
  labs(x = "Date",
       y = "Confirmed cases / day",
       fill = "Variant") +
  theme_bw() +
  ggtitle(paste0("New variant: 30% more infections (R0 = 3.25) \n",reinf_ref*100,"% reinfection \nVaccination: ",vacc_speed/1000,"k per day on average")) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(ylim = c(0,15000))

p3 <- ggplot(res3, aes(x = t, fill = var)) +
  geom_area(aes(y = median, color = var), 
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
  geom_area(aes(y = median, color = var), 
            show.legend = FALSE) +
  labs(x = "Date",
       y = "Confirmed cases / day",
       fill = "Variant") +
  theme_bw() +
  ggtitle(paste0("New variant: 30% more infections (R0 = 3.25) \n",reinf*100,"% reinfection \nVaccination: ",vacc_speed/1000,"k per day on average")) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(ylim = c(0,15000))

library(patchwork)

allp <- (p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")
allp

ggsave(paste0("inf-",reinf,"vacc-",vacc_speed,"ref-",reinf_ref,".png"),
       allp,
       width = 12, height = 7)
