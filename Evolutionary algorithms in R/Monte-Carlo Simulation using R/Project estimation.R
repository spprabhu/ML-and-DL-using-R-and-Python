# Project management - estimation problem

rm(list=ls(all=TRUE))

# What is the most likely time to complete the project given the following conditions

# No. of modules in part of the project : 10 to 12
# No. of modules in full project        : 20 to 24
# Chance that we get the full project   : 10%
# No. of tasks per module               : 50 to 100  
# No. of people per module              : 3 to 7
# Time per task in days                 : 5 to 10

# Let us conduct 10, 100, 1000, 10000, 20000 simulations.  Then compute the average to estimate

# Function number of simulations as input and computes the needed time

timeNeeded = function(numSims){

  time = 0
  set.seed(123)
  for (i in 1:numSims) {
    
    partOrFull = runif(1, 0, 1)

    # 90-10 probability for geting full and part projects
    if (partOrFull <= 0.1){
      # Part project is possible as per probability
      totalModules = sample(10:12, 1)
    }else{ 
      # Full project is possible as per probability
      totalModules = sample(20:24, 1)
    }
    
    # Two vectors are created randomly to represent tasks and people per module
    tasksPerModule = sample(50:100, totalModules, replace=T)
    totalTasks = sum(tasksPerModule)
    
    # Compute the time for all tasks. Each task can take anywhere between 5 to 10 hours
    timeToDoTasks = sum(sample(5:10, totalTasks, replace=TRUE))
    
    # Resources for a project
    Resourcesperproject = sum(sample(3:7, 1)) 
    
    #Time needed according to this simulation
    time[i] = timeToDoTasks/Resourcesperproject
  }
  
  #The time vector is returned
  cat("Time Needed based on", numSims, "simulations= ", mean(time), "\n")
  return(mean(time))
}

simulations = c(10, 100, 1000, 10000, 20000)

start = Sys.time()
for (i in simulations) {
  Time=timeNeeded(i)
}
end=Sys.time()-start
end

# instead for loop use sapply
start = Sys.time()
time = sapply(simulations, timeNeeded)
end = Sys.time() - start
end
