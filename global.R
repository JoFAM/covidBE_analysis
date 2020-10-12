#----------------------------------------------------
# Interface for descriptive analysis Covid data for Belgium
# author: Joris Meys
#----------------------------------------------------

# Check whether all packages are available and load them.
source("scripts/checkPackages.R")

# Load the processed data
source("scripts/loadProcessedData.R")

# Load the necessary modules
mfiles <- dir("modules")
for(i in mfiles){
  source(file.path("modules",i))
}


