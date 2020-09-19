#--------------------------------
# Get the shapes for the plots
# These are stored in the repository, to avoid having to
# do this with every opening of a shiny app.
# 
# WARNING: THIS TAKES TIME!!!
library(sf)
library(dplyr)
library(rmapshaper) # To simplify the shapes
library(tidyr)
source("functions/process_shapeinfo.R")

# Download the 2019 boundaries from StatBel (most recent)
download.file("https://statbel.fgov.be/sites/default/files/files/opendata/Statistische%20sectoren/sh_statbel_statistical_sectors_20190101.geojson.zip",
              destfile = "Data/Maps/statbel.zip")

# Unzip the file at location
thedir <- unzip("Data/Maps/statbel.zip",
      exdir = "Data/Maps/statbel")

geojsonfile <- grep("\\.geojson$", thedir, value = TRUE)

# Read all agglomarations
# Give it some time. A lot of time...
agglo <- st_read(geojsonfile)

# Keep the names
shapeinfo <- st_set_geometry(agglo, NULL) %>%
  select(cd_munty_refnis, tx_munty_descr_nl,
         cd_dstr_refnis, tx_adm_dstr_descr_nl, tx_adm_dstr_descr_fr,
         cd_prov_refnis, tx_prov_descr_nl, tx_prov_descr_fr, 
         cd_rgn_refnis, tx_rgn_descr_nl) %>%
  unique() %>%
  process_shapeinfo()

# This now contains all the statistical units down to neighbourhoods.

#--------------------------------------------
# CREATE SHAPES
# In the following code, these are combined into municipalities,
# provinces and regions
muntymap <- agglo %>% group_by(cd_munty_refnis) %>%
  summarise(do_union = TRUE) %>%
  ms_simplify(keep = 0.01, weighting = 0.9)%>%
  full_join(shapeinfo, by = "cd_munty_refnis")

dstrmap <- muntymap %>% group_by(cd_dstr_refnis) %>%
  summarise(do_union = TRUE) %>%
  full_join(unique(shapeinfo[c("cd_dstr_refnis","names_dstr",
                               "names_prov","names_rgn")]),
            by = "cd_dstr_refnis")

provmap <- muntymap %>% group_by(cd_prov_refnis) %>%
  summarise(do_union = TRUE) %>%
  full_join(unique(shapeinfo[c("cd_prov_refnis","names_prov",
                                "names_rgn")]),
            by = "cd_prov_refnis")

rgnmap <- muntymap %>% group_by(cd_rgn_refnis) %>%
  summarise(do_union = TRUE) %>%
  full_join(unique(shapeinfo[c("cd_rgn_refnis","names_rgn")]),
            by = "cd_rgn_refnis")

#---------------------------------------
# Save the result
saveRDS(muntymap,
        file = "Data/Maps/muntymap.RDS")
saveRDS(dstrmap,
        file = "Data/Maps/dstrmap.RDS")
saveRDS(provmap,
        file = "Data/Maps/provmap.RDS")
saveRDS(rgnmap,
        file = "Data/Maps/rgnmap.RDS")

#---------------------------------------
# Cleanup
unlink("Data/Maps/statbel.zip")
unlink("Data/Maps/statbel", recursive = TRUE)
