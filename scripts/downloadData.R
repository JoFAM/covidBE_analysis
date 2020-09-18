# This script loads the data
source("scripts/checkPackages.R")
source("functions/refresh.R")
source("functions/helperfunctions.R")
source("functions/add_totals.R")
#--------------------------------------
# Read in the raw datasets

## For cases by province, region, gender and agegroup
rawcases<- refresh("cases",
                   "COVID19BE_CASES_AGESEX.csv",
                   process = function(x){
                     add_totals(x,
                                values = "CASES",
                                groups = c("PROVINCE","REGION",
                                           "SEX","AGEGROUP"),
                                along = c("DATE"),
                                name = c("All","Belgium","All","All")) %>%
                       filter(keep_combs(REGION,PROVINCE))
                   })

## For tests by province (and region)
rawtest <- refresh("tests",
                   "COVID19BE_tests.csv",
                   process = function(x){
                     mutate(x,
                            PROVINCE = add_unknown(PROVINCE),
                            REGION = add_region(PROVINCE)) %>%
                     add_totals(values = "TESTS_ALL",
                                groups = c("PROVINCE","REGION"),
                                along = "DATE",
                                name = c("All","Belgium")) %>%
                       filter(keep_combs(REGION,PROVINCE))
                   })

## For hospitalisations by province and region
rawhospit <- refresh("hospitalisations",
                     "COVID19BE_HOSP.csv",
                     process = function(x){
                       add_totals(x,
                                  values = c("TOTAL_IN",
                                             "TOTAL_IN_ICU",
                                             "TOTAL_IN_RESP",
                                             "TOTAL_IN_ECMO",
                                             "NEW_IN",
                                             "NEW_OUT"),
                                  groups = c("PROVINCE",
                                             "REGION"),
                                  along = "DATE",
                                  name = c("All","Belgium")) %>%
                         filter(keep_combs(REGION,PROVINCE))
                     })

## For deaths by region, sex and agegroup
rawdeaths <- refresh("deaths",
                     "COVID19BE_MORT.csv",
                     process = function(x){
                       add_totals(x,
                                  values = "DEATHS",
                                  groups = c("REGION",
                                             "SEX","AGEGROUP"),
                                  along = c("DATE"),
                                  name = c("Belgium","All","All")) 
                     })

## Numbers for each municipality
rawmunicipalities <- refresh("municipalities",
                             "COVID19BE_CASES_MUNI.csv")

#--------------------------------------
# Combine the raw datasets into a number of separate 
# datasets that can be used for analysis.
if(!dir.exists("Processed")) dir.create("Processed")

## Combine cases, tests, hospitalisations and deaths by date and region

casetemp <- filter(rawcases, PROVINCE == "All" & SEX == "All" &
                     AGEGROUP == "All") %>%
  select(-c(SEX, PROVINCE, AGEGROUP))

testtemp <- filter(rawtest, PROVINCE == "All") %>%
  select(-PROVINCE)

hosptemp <- filter(rawhospit, PROVINCE == "All") %>%
  select(-PROVINCE)

deathtemp <- filter(rawdeaths, SEX == "All" & AGEGROUP == "All") %>%
  select(-c(SEX, AGEGROUP))

regionalsmooth <- full_join(casetemp,
                          testtemp,
                          by = c("REGION","DATE")) %>%
  full_join(hosptemp, by = c("REGION","DATE")) %>%
  full_join(deathtemp, by = c("REGION","DATE")) %>%
  filter(as.Date(DATE) >= as.Date("2020-03-15")) %>%
  mutate(across(where(is.numeric),replaceby0)) %>%
  group_by(REGION) %>%
  mutate(across(where(is.numeric),
                ~ zoo::rollmean(., 7, align = "right", fill = NA))) %>%
  as.data.frame()

saveRDS(regionalsmooth, file = file.path("Processed",
                                         "regionalsmooth.RDS"))
