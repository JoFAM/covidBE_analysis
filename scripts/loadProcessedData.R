# This script loads processed data if available.
source("scripts/checkPackages.R")
source("functions/refresh.R")
source("functions/helperfunctions.R")
source("functions/add_totals.R")
source("functions/namesfunctions.R")

# Necessary checks
if(!dir.exists("Processed")){
  needdownload <- TRUE
} else {
  thefiles <- dir("Processed")
  needdownload <- !length(thefiles) || 
    !all(readstamp(dir("Processed")) == Sys.Date())
}

if(needdownload){
  source("scripts/downloadData.R")
}
message("Loading processed data.")
binnedmunty <- readRDS(file.path("Processed",
                                 stamp("binnedmunty",".RDS"))
                       )
regional <- readRDS(file.path("Processed",
                              stamp("regionalweekavg",".RDS"))
)
message("Succes!")
