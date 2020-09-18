# This script loads the data
source("scripts/checkPackages.R")
source("functions/refresh.R")
source("functions/helperfunctions.R")
source("functions/add_totals.R")
#--------------------------------------
# Read in the raw datasets

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
