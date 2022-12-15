# call specific libraries just in case the packages are not called beforehand
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function

# objective function to be optimized
objective_KL <- function(x, my_tableaux){
  # extract constraint names
  constraints <- colnames(my_tableaux)[4:16]
  
  # turn data frame into a list of matrices where each matrix is a single derivation
  tableaux <- my_tableaux %>% split(.$input) %>% map(~ (.x %>% dplyr::select(-input,-output))) %>% lapply(as.matrix)
  
  # frequencies of the candidates for each derivation
  frequencies <- lapply(tableaux,function(x){x[,"frequency"]})
  
  # calculate probabilities across the matrices in the list 
  probabilities <- sapply(tableaux, function(the_element) the_element[,constraints] %*% (x*-1)) %>% # calculate harmony values on negative terms
    sapply(function(the_element) exp(the_element)/sum(exp(the_element))) %>% # calculate the probability of each candidate
    lapply(function(x) {colnames(x) <- "probabilities"; x}) # rename the resulting column as probabilities
  
  # combine the frequencies and probabilities of each matrix in the list then transpose it for KLD. Calculate KLD scores
  sum_KL <- Map(cbind, frequencies, probabilities) %>% lapply(t) %>% lapply(KL) %>% unlist() %>% sum()
  
  # return the overall KLD for the whole set of derivations
  return(sum_KL)
  }

weight_optimize <- function(the_tableaux){ # turn the optimizing into a function to be used 
  # anything other than "input" and "output" vector for the data frame "the_tableaux" should be a numeric value. 
  # the data frame should only include the vectors for input, output, frequency, and constraint violations
  n_constraint <- length(the_tableaux) - 3 # only 3 vectors are not constraints: input, output, and frequency
  # box optimization
  optimal_weights <- optim(par = rep(0, n_constraint), # starting values for the weights is 0
                           fn = objective_KL, # objective function
                           my_tableaux = the_tableaux, # argument to be passed to the objective function
                           lower = rep(0, n_constraint), # the lowest that the constraint weights can get
                           upper = rep(100, n_constraint), # the highest that the constraint weights can get
                           method = "L-BFGS-B") # the method of optimization which allows lower bound
  # return the resulting optimization 
  return(optimal_weights)
  }