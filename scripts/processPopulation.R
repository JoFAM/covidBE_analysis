#-----------------------------------------
# Construct the population dataset so we can calculate
# all numbers relative to population size.
library(dplyr)
library(tidyr)
source("functions/namesfunctions.R")
source("functions/add_totals.R")
# 1. download the necessary file from the official website
# of Statbel.

downloadsucces <- 
  download.file("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking%20naar%20woonplaats%2C%20nationaliteit%20burgelijke%20staat%20%2C%20leeftijd%20en%20geslacht/TF_SOC_POP_STRUCT_2020.zip",
                destfile = "Data/popdata.zip",
                mode = "wb")
if(downloadsucces == 0){
  popfile <- unzip("Data/popdata.zip",
                   exdir = "Data")
}

# 2. Prepare a raw data frame with the data for further
# processing
popraw <- read.table(popfile,
                     header = TRUE,
                     sep = ";") %>%
  select(-c(CD_NATLTY,TX_NATLTY_NL,TX_NATLTY_FR,
            CD_CIV_STS,TX_CIV_STS_NL,TX_CIV_STS_FR)) %>%
  mutate(AGEGROUP = cut(CD_AGE,
                        breaks = c(0,9,19,29,39,49,59,69,79,89,Inf),
                        labels = c("0-9","10-19","20-29",
                                   "30-39","40-49","50-59",
                                   "60-69","70-79","80-89",
                                   "90+"),
                        right = TRUE,
                        include.lowest = TRUE),
         AGEGROUP = as.character(AGEGROUP)) %>%
  mutate(REGION = translate_regions(TX_RGN_DESCR_NL),
         PROVINCE = translate_provinces(TX_PROV_DESCR_NL,
                                        TX_PROV_DESCR_FR,
                                        REGION),
         DISTRICT = translate_districts(TX_ADM_DSTR_DESCR_NL,
                                        TX_ADM_DSTR_DESCR_FR,
                                        REGION),
         MUNTY = translate_munty(TX_DESCR_NL,
                                 TX_DESCR_FR,
                                 REGION)
  ) 

## Store the information on munty for later
munty_info <- select(popraw,
                     MUNTY, REGION, PROVINCE,
                     DISTRICT, CD_REFNIS) %>%
  unique()

distr_info <- select(popraw,
                     REGION, PROVINCE, DISTRICT,
                     CD_DSTR_REFNIS) %>%
  unique()

prov_info <- select(popraw,
                    REGION, PROVINCE,
                    CD_PROV_REFNIS) %>%
  unique()


# 3. calculate totals of population
popraw <- popraw %>%
  select(REGION,
         PROVINCE,
         DISTRICT,
         MUNTY,
         AGEGROUP,
         POPULATION = MS_POPULATION,
         SEX = CD_SEX) %>%
  group_by(MUNTY,SEX,AGEGROUP) %>%
  summarise(POPULATION = sum(POPULATION),
            REGION = unique(REGION),
            PROVINCE = unique(PROVINCE),
            DISTRICT = unique(DISTRICT)) %>%
  ungroup() %>%
  mutate(PROVINCE = clean_prov(PROVINCE))

# 4. add information on the totals.
# For that, we need the munty info so we can
# attach it to the totals later.

levs <- c("MUNTY","DISTRICT","PROVINCE")
totallist <- lapply(levs,
               function(i){
                 add_totals(
                   popraw,
                   values = "POPULATION",
                   groups = c("AGEGROUP","SEX"),
                   along = i,
                   name = c("All","All"))
                 })
names(totallist) <- levs
totallist <- mapply(left_join,
               totallist,
               list(munty_info,
                    distr_info,
                    prov_info))

pop_region <- add_totals(popraw,
                         values = "POPULATION",
                         groups = c("AGEGROUP","SEX","REGION"),
                         name = c("All","All","Belgium"))

# Store that information
write.csv(munty_info,
          file = "Data/munty_info.csv",
          row.names = FALSE)
write.csv(distr_info,
          file = "Data/distr_info.csv",
          row.names = FALSE)
write.csv(prov_info,
          file = "Data/prov_info.csv",
          row.names = FALSE)

write.csv(totallist$MUNTY,
          file = "Data/pop_munty.csv",
          row.names = FALSE)
write.csv(totallist$DISTRICT,
          file = "Data/pop_district.csv",
          row.names = FALSE)
write.csv(totallist$PROVINCE,
          file = "Data/pop_province.csv",
          row.names = FALSE)
write.csv(pop_region,
          file = "Data/pop_region.csv",
          row.names = FALSE)
