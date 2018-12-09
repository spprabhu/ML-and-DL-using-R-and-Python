# Removing the environament variables 
```{r}
rm(list=ls(all = TRUE))
```
## Problem Statement 1: Using Monte Carlo simulation, find the probability that two random 
## individuals have the same week day as their day of birth.

# Defining the function to generate the birthdays
```{r}
findSameBirthDay= function(simualations){
  
  yes = 0
  person1 = sample(1:7, simualations, replace = TRUE)
  person2 = sample(1:7, simualations, replace = TRUE)
  
  yes = sum(person1 == person2)
  probabilitySameBirthday = yes/ simualations
  
  return (probabilitySameBirthday)
}
```
# Generating the simulations for ten, hundred, thousand, tenthousand and 1 lakh pair of people. 
```{r}
simualtions = c(10, 100, 1000, 10000, 100000)
sapply(simualtions, findSameBirthDay)
```

```{r}

for (i in simualtions){
  print(findSameBirthDay(i))
}

```