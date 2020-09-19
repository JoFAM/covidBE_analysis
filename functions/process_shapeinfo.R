#-----------------------
# Function to process shapeinfo
library(stringr)
process_shapeinfo <- function(x){
  regs <- recode(x$tx_rgn_descr_nl,
                 "Vlaams Gewest" = "Flanders",
                 "Brussels Hoofdstedelijk Gewest" = "Brussels",
                 "Waals Gewest" = "Wallonia")
  provs <- case_when(
    regs == "Flanders" ~ str_remove(x$tx_prov_descr_nl,"Provincie "),
    regs == "Wallonia" ~ str_remove(x$tx_prov_descr_fr,"Province d[eu] "),
    regs == "Brussels" ~ "Brussel"
  )
  distr <- case_when(
    regs == "Flanders" ~ str_remove(x$tx_adm_dstr_descr_nl,
                                    "Arrondissement "),
    regs == "Wallonia" ~ str_remove(x$tx_adm_dstr_descr_fr,
                                    "Arrondissement d[eu']\\s?"),
    regs == "Brussels" ~ str_remove(x$tx_adm_dstr_descr_nl,"Arrondissement ")
  )
  x %>%
    select(cd_munty_refnis,
           names_munty = tx_munty_descr_nl,
           cd_dstr_refnis,
           cd_prov_refnis,
           cd_rgn_refnis) %>%
    mutate(names_dstr = distr,
           names_prov = provs,
           names_rgn = regs)
}
