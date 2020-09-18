#' This file contains a number of convenience functions for processing

#----------------------------------
# Convert the date and cut the last day

cleanDate <- function(x, var = "DATE"){
  tmp <- as.Date(x[[var]])
  x[[var]] <- tmp
  id <- !is.na(tmp) & tmp < (Sys.Date() - 1)
  x[id,]
}

#---------------------------------
# Replace missing values by 0
replaceby0 <- function(x){
  x[is.na(x)] <- 0
  x
}

#---------------------------------
# Calculate the change compared to n values ago

calculate_change <- function(x,n=7,type=c("abs","rel")){
  type <- match.arg(type)
  nx <- length(x)
  if(n >= nx) stop("x isn't large enough.")
  tmp <- x[(n+1):nx] - x[1:(nx - n)]
  if(type == "rel"){
    tmp <- tmp / x[1:(nx - n)]
  }
  return(c(rep(NA,n),tmp))
}

#-------------------------------
# Clean up impossible combinations

keep_combs<- function(region,province){
  poss <- c(
    "Brussels-Brussels",
    "Antwerpen-Flanders",
    "BrabantWallon-Wallonia",
    "Hainout-Wallonia",
    "Liège-Wallonia",
    "Limburg-Flanders",
    "Luxembourg-Wallonia",
    "Namur-Wallonia",
    "VlaamsBrabant-Flanders",
    "OostVlaanderen-Flanders",
    "WestVlaanderen-Flanders"
  )
  tmp <- paste(province, region, sep = "-")
  id <- tmp %in% poss | grepl("All-",tmp)
}
