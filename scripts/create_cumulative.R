#-------------------------------------
# Create a dataset that contains the added numbers 
# for every municipality based on the historic datasets.
# This is a bit more exact than using the reported numbers,
# as they cut off at 5.
source("functions/stamp.R")
source("functions/process_munty.R")

library(dplyr)
library(tidyr)
# First check whether the file exists
fname <- dir("Data", pattern = "muntycumulative")
hasfile <- length(fname) > 0

# Create start date for reading
startdate <- if(hasfile) readstamp(fname)  else as.Date("2020-03-31")
if(startdate >= Sys.Date() )
  stop("You have the latest data already.")
# Create the links to be downloaded
# Don't download the latest file, as that can still change.
daterange <- seq(startdate,Sys.Date() - 1,
                 by = "1 day")
infile <- format(daterange, "%Y%m%d")

fnames <- paste0("https://epistat.sciensano.be/Data/",
                 infile,
                 "/COVID19BE_CASES_MUNI_CUM_",
                 infile,
                 ".csv")

# Create first dataset
if(hasfile){
  muntycumulative <- read.csv(file.path("Data",fname))
  
  tmpdate <- as.Date(muntycumulative$DATE)
  id <- which(tmpdate == max(tmpdate))
  oldtmp <- muntycumulative[id, c("names_munty","cases")]
  
} else {
  # read in the first
  muntycumulative <- read.csv(fnames[1]) %>%
    process_munty(daterange[1])
  muntycumulative$diffcases <- NA
  # correct the fnames and daterange
  fnames <- fnames[-1]
  daterange <- daterange[-1]
  #store data as oldtmp
  oldtmp <- muntycumulative[c("names_munty","cases")]
}

# Function to calculate the change in cases.
calc_cases <- function(x,y){
  tmp <- right_join(x,y,by = "names_munty")
  cx <- tmp$cases.x
  cy <- tmp$cases.y
  cx[cx == "<5"] <- NA
  cy[cy == "<5"] <- NA
  cx <- as.numeric(cx)
  cy <- as.numeric(cy)
  out <- cy - cx
  id <- is.na(out) & !is.na(cy)
  out[id] <- cy[id]
  return(data.frame(names_munty = tmp$names_munty,
                    diffcases = out))
}

for(i in seq_along(fnames)){
  message(paste("Downloading",daterange[i]))
  tmp <- read.csv(fnames[i]) %>%
    process_munty(daterange[i])
  diffcases <- calc_cases(oldtmp,
                              tmp[c("names_munty","cases")])
  
  muntycumulative <- rbind(muntycumulative,
                           left_join(tmp, diffcases, by = "names_munty"))
  oldtmp <- tmp[c("names_munty","cases")]
  Sys.sleep(1)
}
message("Save the file.")
write.csv(muntycumulative,
          file = file.path("Data",
                           stamp("muntycumulative")),
          row.names = FALSE)
if(hasfile) unlink(file.path("Data",fname))
message("Succes!")
