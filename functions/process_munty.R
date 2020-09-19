#-----------------------------------
# Function to process municipality data
#
source("functions/namesfunctions.R")
process_munty <- function(x, date){
  x <- na.omit(x, date)
  
  region <- translate_regions(x$TX_RGN_DESCR_NL)
  prov <- translate_provinces(x$TX_PROV_DESCR_NL,
                              x$TX_PROV_DESCR_FR,
                              region)
  dstr <- translate_districts(x$TX_ADM_DSTR_DESCR_NL,
                              x$TX_ADM_DSTR_DESCR_FR,
                              region)
  munty <- translate_munty(x$TX_DESCR_NL,
                           x$TX_DESCR_FR,
                           region)
  
  data.frame(
    cd_munty_refnis = x$NIS5,
    names_munty = munty,
    names_dstr = dstr,
    names_prov = prov,
    names_rgn = region,
    cases = x$CASES,
    DATE = date
  )
}
