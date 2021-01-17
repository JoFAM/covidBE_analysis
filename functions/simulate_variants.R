#' Simulate infections
#' 
#' @param k the dispersion parameter
#' @param r the R0 value
#' @param p_binom the proportion of avoided infections
#' @param p_immune the proportion of immune people
#' @param prev the number of previous infections
#' @param kernel the probability kernel

calc_infections <- function(k = 0.2, r = 2.5,
                            prev = rep(1,14),
                            kernel = rep(1/14,14),
                            p_binom = 0.6,
                            p_immune = 0.3
                            ){
  # refactor using prob for rnbinom
  p <- k / (k + r)
  n <- length(prev)
  # draw the theoretical infections
  i_theor <- rnbinom(n, size = k*prev, prob = p) * kernel *
    (1 - p_immune)
  # draw the real infections
  size = round(sum(i_theor))
  if(size == 0 ) {
    return(0)
  } else {
    return(rbinom(1, size = size, prob = p_binom))
  }
  
}

#' simulate a timeline of infections for two different strains
#' 
#' @param t the number of steps to calculate
#' @param tot_pop the total population
#' @param vacc the number vaccinated
#' @param vacc_speed the number vaccinated per day
#' @param immune the number already infected at the start of the time series
#' @param r1 the R value for the main variant
#' @param r2 the R value for the new variant
#' @param prop_newvar the proportion of the new variant at the start
#' @param ... parameters passed to calc_infections (apart from r)
simulate_series <- function(t, tot_pop = 11e6,
                            vacc = 100000,
                            vacc_speed = 1000,
                            immune = 3000000,
                            r1 = 2.5, r2 = 4,
                            k1 = 0.2, k2 = k1,
                            prop_newvar = 0.05,
                            prev = rep(1,14),
                            kernel = rep(1/14,14),
                            p_binom = 0.6){
  
  # Store output
  n_window <- length(prev)
  
  v1_inf <- numeric(t)
  v2_inf <- numeric(t)
  tot_inf <- numeric(t)
  
  #initialize 
  p_immune <- (vacc + immune) / tot_pop
  
  prev_v1 <- round(prev*(1-prop_newvar))
  prev_v2 <- round(prev*prop_newvar)
  
  # Generate samples
  
  for(i in seq_len(t)) {
    
    new_v1 <- calc_infections(k = k1, r = r1, 
                              prev = prev_v1,
                              kernel = kernel,
                              p_binom = p_binom,
                              p_immune = p_immune)
    
    new_v2 <- calc_infections(k = k2, r = r2, 
                              prev = prev_v2,
                              kernel = kernel,
                              p_binom = p_binom,
                              p_immune = p_immune)
    
    
    v1_inf[i] <- new_v1
    v2_inf[i] <- new_v2
    tot_inf[i] <- new_v1 + new_v2
    
    # Recalculate immune proportion
    vacc <- vacc + vacc_speed
    # the infected people cannot be reinfected now
    immune <- immune + tot_inf[i]
    p_immune <- (vacc + immune) / tot_pop
    # shift the window 1
    prev_v1 <- c(prev_v1[-1],new_v1)
    prev_v2 <- c(prev_v2[-1],new_v2)
    
  }
  
  return(list(v1 = v1_inf, 
              v2 = v2_inf, 
              tot = tot_inf))
}
