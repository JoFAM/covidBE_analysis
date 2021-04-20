# This script loads the data
source("scripts/checkPackages.R")
thefuns <- dir("functions", pattern = "\\.R")
for(i in thefuns){
  source(file.path("functions",i),
         encoding = "UTF8")
}


if(!dir.exists("Processed")) dir.create("Processed")
#--------------------------------------
# Read in the raw datasets
message("Start download data. This might take a minute.")
message("Processing cases data.")
## For cases by province, region, gender and agegroup
rawcases<- refresh("cases",
                   "COVID19BE_CASES_AGESEX.csv",
                   process = function(x){
                     calc_margins(x,
                                  values = "CASES",
                                  grouping = "SEX, AGEGROUP / PROVINCE / REGION",
                                  by = "DATE",
                                  names = c("All","All","All","Belgium"))
                   })

## For tests by province (and region)
message("Processing test information.")
rawtest <- refresh("tests",
                   "COVID19BE_tests.csv",
                   process = function(x){
                     mutate(x,
                            PROVINCE = add_unknown(PROVINCE),
                            REGION = add_region(PROVINCE)) %>%
                     add_totals(values = c("TESTS_ALL","TESTS_ALL_POS"),
                                groups = c("PROVINCE","REGION"),
                                along = "DATE",
                                name = c("All","Belgium")) %>%
                       filter(keep_combs(REGION,PROVINCE))
                   })

## For hospitalisations by province and region
message("Processing hospitalisations.")
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
message("Processing deaths.")
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
message("Processing municipalities data")
rawmunicipalities <- refresh("municipalities",
                             "COVID19BE_CASES_MUNI.csv") %>%
  mutate(DISTRICT = translate_districts(TX_ADM_DSTR_DESCR_NL,
                                        TX_ADM_DSTR_DESCR_NL,
                                        REGION),
         MUNTY = translate_munty(TX_DESCR_NL,
                                 TX_DESCR_FR,
                                 REGION),
         cd_munty_refnis = NIS5,
         binned = bin_cases(CASES)) %>%
  select(REGION, PROVINCE, DISTRICT,MUNTY,
         cd_munty_refnis, cases = CASES,
         binned,DATE) %>%
  na.omit()
  
replacefile(rawmunicipalities,
            "binnedmunty",
            "rds",
            "Processed")

#--------------------------------------
# Combine the raw datasets into a number of separate 
# datasets that can be used for analysis.
# Smooth the data using a 7 day window where possible.

## Combine cases, tests, hospitalisations and deaths by date and region
message("Combining data sets and saving.")
casetemp <- filter(rawcases, PROVINCE == "All" & SEX == "All" &
                     AGEGROUP == "All") %>%
  select(-c(SEX, PROVINCE, AGEGROUP))

testtemp <- filter(rawtest, PROVINCE == "All") %>%
  select(-PROVINCE)

hosptemp <- filter(rawhospit, PROVINCE == "All") %>%
  select(-PROVINCE)

deathtemp <- filter(rawdeaths, SEX == "All" & AGEGROUP == "All") %>%
  select(-c(SEX, AGEGROUP))

# Make raw regional data with changes.

regionalraw <- full_join(casetemp,
                          testtemp,
                          by = c("REGION","DATE")) %>%
  full_join(hosptemp, by = c("REGION","DATE")) %>%
  full_join(deathtemp, by = c("REGION","DATE")) %>%
  filter(as.Date(DATE) >= as.Date("2020-03-15")) %>%
  mutate(across(where(is.numeric),replaceby0)) %>%
  as.data.frame()

# Calculate per 100 k
pop_region <- read.csv("Data/pop_region.csv") %>%
  filter(SEX == "All", 
         AGEGROUP == "All") %>%
  select(REGION, POPULATION)

regionalper1M <- regionalraw %>%
  left_join(pop_region, by = "REGION") %>%
  mutate(across(where(is.numeric) & !contains("POPULATION"),
                ~ ./POPULATION * 1e6))

# Changes can be averaged as they're absolute changes.
regionalweekavg <- regionalraw %>%
  group_by(REGION) %>%
  mutate(across(where(is.numeric),
                ~ zoo::rollmean(., 7, align = "right", fill = NA)))  %>%
  mutate(across(where(is.numeric),
                changeabsolute,
                .names = "CHANGE_{.col}")) %>%
  mutate(POSRATE = TESTS_ALL/TESTS_ALL_POS) %>%
  as.data.frame()

# Add relative changes
# NEED AGEDIST FOR PROVINCES AS WELL !!!!
# n <- read.csv("Data/AgedistPopBe.csv")
# 
# agecases <- rawcases %>%
#   group_by(DATE, REGION, PROVINCE, AGEGROUP, SEX) %>%
#   mutate(across(where(is.numeric),
#                 ~ zoo::rollmean(., 7, align = "right", fill = NA)))

replacefile(regionalraw,
            "regionalraw",
            "rds",
            "Processed")

replacefile(regionalper1M,
            "regionalper1M",
            "rds",
            "Processed")

replacefile(regionalweekavg,
            "regionalweekavg",
            "rds",
            "Processed")
message("Succes!")

