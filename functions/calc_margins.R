#' add margin sums to a table
#' 
#' @param x the original data frame
#' @param values a character vector with the names of the value variables
#' @param grouping the grouping formula
#' @param by a variable by which the sums should be calculated (eg a date)
#' @param names the names to be used for the margins
#' @param na.rm do we need to remove the NA values?
#' @param use.na if TRUE, NA is translated to "unknown"
#' 
#' @details
#' Grouping can be done as follows:
#' Groups for which all interactions should be calculated, should be added
#' first. The nested groups should be separated using a / symbol.
#' So:
#' Age, Sex / district / region / country
#' would calculate the totals of every combination of age and sex, and
#' this for every district, region and country, respecting the fact that
#' a district can only occur in one region.
calc_margins <- function(x, 
                          values,
                          grouping,
                          by = NULL,
                          names = "All",
                          na.rm = FALSE,
                          use.na = TRUE){

  # Read the groups
  tmp <- unlist(strsplit(grouping, "/"))
  groups <- trimws(unlist(strsplit(tmp[1],",")))
  nested <- trimws(tmp[-1])
  
  if(length(groups) && groups == ""){ 
    groups <- character(0)
    hasgroups <- FALSE
  } else if (length(groups)){
    hasgroups <- TRUE
  } else {
    hasgroups <- FALSE
  }
  
  ngroup <- length(groups)
  nnest <- length(nested)
  
  # check the names:
  if(length(names) == 1){
    names <- rep(names, ngroup + nnest)
  } else if(length(names) != ngroup + nnest) {
    stop("the names argument should contain 1 name, or 1 name for every grouping factor")
  }
  
  if(use.na){
    x[c(groups, nested, by)] <- lapply(x[c(groups, nested, by)], 
                                       add_unknown)
  }
  
  # Make the xtabs 
  make_xtabs <- function(v,nest){

    f <- as.formula(paste(v, " ~ ",
                    paste(c(groups, nest, by),
                          collapse = "+"))
                    )
    xt <- xtabs(f, data = x)
    xt <- replaceby0(xt)
  } # end make_xt
  
  # calculate totals
  calc_total <- function(val,nest){
    
    morevalues <- length(values) > 1
    v <- val[1]
    xt <- make_xtabs(v, nest)
    
    # Calculate the margins
    seqnest <- ngroup + seq_along(c(nest,by))
    
    out <- as.data.frame(xt,
                         responseName = v,
                         stringsAsFactors = FALSE)
    for(i in gdims <- c(0,seq_along(groups))){
      ms <- marginSums(xt, margin = c(i,seqnest))
      ms <- as.data.frame(ms,
                          responseName = v,
                          stringsAsFactors = FALSE)
      
      for(j in setdiff(gdims,c(0,i)) ){
        if(hasgroups)
          ms[[groups[j]]] <- names[j]
      }
      out <- rbind(out, ms)
    } # END for i in gdims
    
    if(morevalues){
      nsolution <- nrow(out)
      for(v in val[-1]){
        xt <- make_xtabs(v,nest)
        res <- numeric(0)
        for(i in gdims){
          res <- c(res,
                   as.vector(marginSums(xt, margin = c(i,seqnest)))
          )
        } # END gdims
        out[[v]] <- res
      } # END for v in val
    } # END if morevalues
    
    return(out)
  } #End calc_total
  
  
  if(length(nested)){
    nestinfo <- unique(x[nested])
    todo <- nested
    tofill <- character(0)
    
    final<- data.frame()
    
    for(i in nested){
      
        tmp <- calc_total(values, i)
      
      for(j in seq_along(tofill) ){
        tmp[[tofill[j] ]] <- names[ngroup+j]
      }
      
      final <- rbind(final,
                     left_join(tmp, unique(nestinfo[todo]), by = i))
      
      tofill <- c(tofill, todo[1])
      todo <- todo[-1]
    }
    
    tmp <- calc_total(values, character(0))
    
    for(j in seq_along(tofill) ){
      tmp[[tofill[j] ]] <- names[ngroup+j]
    }
    final <- rbind(final, tmp)
    
    
  } else {
    final <- calc_total(values, character(0))
  }
  return(final)
} # END function
