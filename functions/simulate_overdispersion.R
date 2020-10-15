# Functions to model an epidemic using a negative binomial
# distribution to take overdispersion into account.
#---------------------------------------------------------

# Simulate total number of infections after n steps
simul_cases <- function(nsteps = 5, nstart = 1,
                        k = 0.1, R = 1.3){
  n <- nstart
  i <- 1
  while(i <= nsteps){
    n <- sum(rnbinom(n,size = k, mu = R))
    i <- i + 1
  }
  n
}

# Make distribution for a specific simulation
dist_cases <- function(nsimul = 1000,
                       nsteps = 5, nstart = 1,
                       k = 0.1, R = 1.3){
  replicate(nsimul,
            simul_cases(nsteps = nsteps,
                        nstart = nstart,
                        k = k,
                        R = R))
}

# Make probability distribution of dying epidemic
simul_prob <- function(nstart = seq.int(50),
                       nsimul = 1000,
                       nsteps = 5,
                       k = 0.1, R = 1.3){
  nout <- length(nstart)
  p <- numeric(nout)
  
  for(i in seq.int(nout)){
    tmp <- dist_cases(nsimul = nsimul,
                      nsteps = nsteps,
                      nstart = nstart[i],
                      k = k, R = R)
    p[i] <- mean(tmp == 0)
  }
  tibble(nstart = nstart,
         p = p)
}

# Simulate an infection chain
# Function to simulate an infection chain
simul_chain <- function(k=0.1, R = 1.3, ninfected = 10, nsteps = 5){
  # Make the start cases
  start <- paste0("r1case0-",seq.int(ninfected))
  
  # initialize the vectors to store results
  infected_by <- rep("r0case", length(start))
  id <- c("r0case",start)
  current <- start
  nmax <- nsteps
  
  n <- 2
  # Do the algorithm
  while(n <= nmax){
    # re-initialize the new id's
    newid <- character()
    
    for(i in seq_along(current)){
      # Draw from a neg binomial
      nnew <- rnbinom(1,size = k, mu = R)
      ninfected <- c(ninfected,nnew)
      if(nnew > 0){
        tmp <- paste0("r",n,"case",i,"-",seq.int(nnew))
        id <- c(id,tmp)
        infected_by <- c(infected_by,rep(current[i],nnew))
        newid <- c(newid,tmp)
      }
    }
    if(!length(newid)) break
    current <- newid
    n <- n + 1
  }
  if(n == nmax + 1){
    ninfected <- c(ninfected, rep(0,length(current)))
  }
  # Create vertices and edges
  vert <- tibble(
    name = unique(id),
    ninfected = ninfected,
    level = gsub("r(\\d+)case.*","\\1",name)
  )
  edge <- data.frame(from = match(infected_by, vert$name),
                     to = match(id[-1],vert$name),
                     length = 1)
  
  # Return
  tbl_graph(nodes = vert, 
            edges = edge,
            directed = TRUE)
  
}
