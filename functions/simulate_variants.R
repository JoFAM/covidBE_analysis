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
  i_theor <- suppressWarnings(rnbinom(n, size = k*prev, prob = p)) * kernel *
    (1 - p_immune)
  # draw the real infections
  size = round(sum(i_theor, na.rm = TRUE))
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
#' @param p_reinfect proportion of reinfection with variant 2 after immunity to variant 1 and vaccine
#' @param p_vac probability of protection by vaccine
#' @param ... parameters passed to calc_infections (apart from r)
simulate_series <- function(t, tot_pop = 11e6,
                            vacc = 100000,
                            vacc_speed = 10000,
                            immune = 3000000,
                            r1 = 2.5, r2 = 4,
                            k1 = 0.2, k2 = k1,
                            prop_newvar = 0.05,
                            prev = rep(1,14),
                            kernel = rep(1/14,14),
                            p_binom = 0.6,
                            p_reinfect = 0.05,
                            p_vac = 0.95){
  
  # Check p_binom
  if(length(p_binom) == 1 ){
    p_binom <- rep(p_binom,t)
  } else if(length(p_binom) != t){
    stop("p_binom has the wrong length.")
  }
  
  # Store output
  n_window <- length(prev)
  
  v1_inf <- numeric(t)
  v2_inf <- numeric(t)
  tot_inf <- numeric(t)
  
  #initialize 
  immune_1 <- immune
  immune_2 <- immune * (1 - p_reinfect)
  
  p_immune_1 <- immune_1 / tot_pop
  p_immune_1 <- (vacc * (1 - p_immune_1) * p_vac + immune_1) / tot_pop
  
  p_immune_2 <- immune_2 / tot_pop
  p_immune_2 <- (vacc * (1 - p_immune_2) * (1 - p_reinfect) * p_vac + immune_2) / tot_pop
  
  # take growth into account
  r_newinit <- r2*p_binom[1]*(1-p_immune_2)
  
  #find periodicity
  period <- which.max(inc_kernel) + 1
  multfact <- log((14:1)^(1/period))
  pnew_prev <- prop_newvar/exp(log(r_newinit)*multfact)
  prev_v1 <- round(prev*(1-pnew_prev))
  prev_v2 <- round(prev*pnew_prev)
  # Generate samples
  
  for(i in seq_len(t)) {
    
    new_v1 <- calc_infections(k = k1, r = r1, 
                              prev = prev_v1,
                              kernel = kernel,
                              p_binom = p_binom[i],
                              p_immune = p_immune_1)
    
    new_v2 <- calc_infections(k = k2, r = r2, 
                              prev = prev_v2,
                              kernel = kernel,
                              p_binom = p_binom[i],
                              p_immune = p_immune_2)
    
    
    v1_inf[i] <- new_v1
    v2_inf[i] <- new_v2
    tot_inf[i] <- new_v1 + new_v2
    
    # Recalculate immune proportion
    vacc <- vacc + vacc_speed
    # the infected people cannot be reinfected now
    immune_1 <- immune_1 + v1_inf[i] + v2_inf[i]*(1 - p_reinfect)
    immune_2 <- immune_2 + v2_inf[i] + v1_inf[i]*(1 - p_reinfect)
    # A fraction of vaccinated people is already immune
    p_immune_1 <- immune_1 / tot_pop
    p_immune_1 <- (vacc * (1 - p_immune_1) * p_vac + immune_1) / tot_pop
    
    p_immune_2 <- immune_2 / tot_pop
    p_immune_2 <- (vacc * (1 - p_immune_2) * (1 - p_reinfect) * p_vac + immune_2) / tot_pop
    
    # shift the window 1
    prev_v1 <- c(prev_v1[-1],new_v1)
    prev_v2 <- c(prev_v2[-1],new_v2)
    
  }
  
  return(list(v1 = v1_inf, 
              v2 = v2_inf, 
              tot = tot_inf))
}

#' Helper function for creating the data
to_data <- function(x, probs, var = "v"){
  tmp <- apply(x, 2, quantile, probs)
  t <- ncol(tmp)
  data.frame(
    ll = tmp[1,],
    median = tmp[2,],
    ul = tmp[3,],
    var = rep(var, t),
    t = seq_len(t)
  )
}

#' Replicate the time series
#'
#' @param n the number of replicates for each time step.
#' @param t the number of time steps
#' @param probs the probabilities returned
#' @param ... arguments passed on to simulate_series
#' 
replicate_series<- function(n, t, probs = c(0,0.5,1), ... ){
  
  tot <- v2 <- v1 <- matrix(numeric(n*t),ncol = t)
  
  for( i in seq_len(n)){
    res <- simulate_series(t = t, ...)
    
    v1[i,] <- res$v1
    v2[i,] <- res$v2
    tot[i,] <- res$tot
  }
  
  v1 <- to_data(v1, probs, var = "v1")
  v2 <- to_data(v2, probs, var = "v2")
  tot <- to_data(tot, probs, var = "tot")
  
  res <- rbind(v1, v2, tot)
  return(res)
}
