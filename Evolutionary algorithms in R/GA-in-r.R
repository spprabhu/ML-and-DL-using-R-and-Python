rm(list=ls(all=TRUE))

# Initital population generation
fnGenerateInitPop = function(dataSet, initPopSize){

  initPop = as.data.frame(matrix(nrow = 0, ncol = nrow(dataSet),
                                 dimnames = list(NULL, dataSet$item)))
  
  for (i in 1:initPopSize){
    individual = sample(0:1, nrow(dataSet), replace=TRUE)
    initPop[i,]= individual
  }
  
  rm(i, individual)
  
  return(initPop)
}

# Define the Objective function as follows.
fnEvaluate = function(individual){

  current_solution_survivalpoints = individual %*% dataSet$survivalpoints
  current_solution_weight = individual %*% dataSet$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) 
  else 
    return(current_solution_survivalpoints)
}

# Mutation : Pick one position and change the value to 0 or 1 as the case may be
fnMutate = function(individual){
  
  index = sample(1:length(individual), 1)
  individual[index] = 1 - individual[index]
  
  return(individual)
}

# Crossover : Randomly select a point and swap the tails
fnCrossOver = function(parent1, parent2){
  
  idx = sample(2:(length(parent1)-2), 1)
  child1 = c(parent1[1:idx], parent2[(idx+1):length(parent2)])
  child2 = c(parent2[1:idx], parent1[(idx+1):length(parent1)])
  
  return(list(child1, child2))
}

# Execute the genetic algorithm
fnRunGeneticAlgo = function(initPop, mutStartProb,
                            elitePercent, maxIterations,
                            maxSurvivalPoints){
  
  counter = 0   # Is used for stopping criteria
  
  cat("Max iterations = ", maxIterations, "\n")

  # How many winners from each generation?
  
  origPopSize = nrow(initPop)
  topElite = round(elitePercent*origPopSize, 0)
  
  initPop$fitN = apply(initPop, 1, fnEvaluate)
  initPop = initPop[order(initPop$fitN, decreasing = T),]
  currentFitN = initPop$fitN[1]
  initPop$fitN = NULL

  newPop = initPop  
    
  for (i in 1:maxIterations){
    
    cat("Iteration : ", i, "\n")

    elitePop = newPop[1:topElite,]
    
    newPop = newPop[0, ] # newPop = newPop[-(1:nrow(newPop)), ]
    
    mut = mutStartProb/i
    
    # Add mutated and bred forms of the winners
    while (nrow(newPop) < origPopSize) {

      # Mutation
      if (runif(1,0,1) < mut){
        idx = sample(1:topElite, 1)
        newPop[nrow(newPop)+1,] = fnMutate(elitePop[idx,])
        
        if (nrow(newPop) == origPopSize){break()}
      }
      else { #Crossover
        idx1 = sample(1:topElite, 1)
        idx2 = sample(1:topElite, 1)
        
        ls = fnCrossOver(elitePop[idx1,], elitePop[idx2, ])
        
        newPop[nrow(newPop)+1,] = ls[[1]]
        newPop[nrow(newPop)+1,] = ls[[2]]
        
        if (nrow(newPop) == origPopSize){break()}
      }
    }

    newPop$fitN = apply(newPop, 1, fnEvaluate)
    newPop = newPop[order(newPop$fitN, decreasing = T),]
    
    prevFitN = currentFitN
    currentFitN = newPop$fitN[1]
    
    newPop$fitN = NULL
        
    # stopping criteria 
    if(prevFitN == currentFitN){
      counter = counter+1
      if(counter==5){break()}
    }else{
      counter=0
    }
    
    if (currentFitN == maxSurvivalPoints) {break()}

    cat("Total survival points in iteration ", i, " = ", currentFitN, "\n")
  }
  
  return(newPop[1,])
}


fnExecuteMain = function(dataSet, mutStartProb,
                         elitePercent, maxIterations, 
                         maxSurvivalPoints){
  
  set.seed(1234)
  
  initPopSize = 100
  
  initPop = fnGenerateInitPop(dataSet, initPopSize)
  
  solution = fnRunGeneticAlgo(initPop, mutStartProb, 
                              elitePercent, maxIterations,
                              maxSurvivalPoints)
  
  finalSolution = as.numeric(solution)
  selectedItems = dataSet[finalSolution == 1, ]
  
  # solution vs available
  cat(paste(finalSolution %*% dataSet$survivalpoints, "/", 
            sum(dataSet$survivalpoints),"\n"))
  
  cat("Total Survivalpoints = ",
      sum(selectedItems$survivalpoints), "\n", 
      "Total weight = ",sum(selectedItems$weight))
  
  return(selectedItems)
}

dataSet = data.frame(item = c("pocketknife", "beans", "potatoes", 
                              "onions", "phone", "lemons",
                              "sleepingbag", "rope", "compass",
                              "umbrella", "sweater", "medicines", 
                              "others"), 
                     survivalpoints = c(15, 16, 13, 14, 20, 12, 17, 
                                        18, 17, 19, 10, 12, 11), 
                     weight = c(5, 6, 3, 4, 11, 2, 7, 
                                8, 10, 9, 1, 12, 11))

# Stopping criteria could be either maxSurvivalPoints or 
# If there is no much change for last n iterations you may stop
maxSurvivalPoints = sum(dataSet$survivalpoints)

weightlimit = 80

sum(dataSet$weight)

# Gene 0 or 1
# Chromosome = c(1, 0, 0, 1, 1, 0, 0)

mutStartProb  = 0.5
elitePercent  = 0.2
maxIterations = 10

result = fnExecuteMain(dataSet, mutStartProb, elitePercent,
                       maxIterations, maxSurvivalPoints)
