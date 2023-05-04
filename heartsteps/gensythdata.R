## Setup RData files
load("./RDS_files/ggdrive.RData")
load("./RDS_files/maxT.RData")

# ## Need gamm4 for generalized additive mixed effects model
if(!require("gamm4")){install.packages("gamm4")}
library("gamm4")

# Generative models
# Inputs: Current State, Action, and Prob

generate_initial_values <- function(id) {
  ## Input: id
  ## Output: State and Availability
  load("./RDS_files/initstates.RData")
  initial_state = init.states[init.states$id == 1,c(1:3,5:12)]
  availability = init.states[init.states$id == 1,4]
  initial_state$id = id
  return(list("state" = initial_state, "availability" = availability))
}

generate_reward_state <- function(probability, action, available, state) {
  # Input is a list with 3 arguments:
  # 1. State: (id, day, decision.time, dosage, engagement, other.location, 
  #            variation, temperature, logpresteps, sqrt.totalsteps, prior.anti)
  # 2. Availability: 0/1 
  # # 3. Action: 0/1
  # Ouput is a list with 3 arguments:
  # 1. Reward: Numeric value which is log of step-count with offset = 0.5
  # 2. State: Same as before
  # 3. Availability: for next decision time
  
  input = list("probability" = probability,
                "state" = state,
                "action" = action,
               "available" = available)
  
  pred.data = data.frame("id" = input$state[1])
  pred.data$day = as.numeric(input$state[2])
  pred.data$decision.time = as.numeric(input$state[3])
  pred.data$dosage = as.numeric(input$state[4])
  pred.data$engagement = as.numeric(input$state[5])
  pred.data$other.location = as.numeric(input$state[6])
  pred.data$variation = as.numeric(input$state[7])
  pred.data$temperature= as.numeric(input$state[8])
  pred.data$logpresteps = as.numeric(input$state[9])
  pred.data$sqrt.totalsteps = as.numeric(input$state[10])
  pred.data$prior.anti = as.logical(input$state[11])
  pred.data$action = input$action
  pred.data$probability = input$probability
  pred.data$ac = input$action - input$probability
  pred.data$acday = pred.data$ac * pred.data$day
  pred.data$acdosage = pred.data$ac * pred.data$dosage
  pred.data$acengagement = pred.data$ac * pred.data$engagement
  pred.data$aclocation = pred.data$ac * pred.data$other.location
  pred.data$acvariation = pred.data$ac * pred.data$variation
  pred.data$actemp = pred.data$ac * pred.data$temperature
  pred.data$aclogpresteps = pred.data$ac * pred.data$logpresteps
  pred.data$acsqrttotsteps = pred.data$ac * pred.data$sqrt.totalsteps
  pred.data$acprioranti = pred.data$ac * pred.data$prior.anti
  
  cohort = ceiling(pred.data$id/7)
  # print(paste("Cohort is",cohort))
  
  zeroavailable_reward_model = readRDS(file = paste("./RDS_files/zeroavailablereward_cohort_",cohort,".RDS", sep = ""))
  available_reward_model = readRDS(file = paste("./RDS_files/availablereward_cohort_",cohort,".RDS", sep = ""))
  zerounavailable_reward_model = readRDS(file = paste("./RDS_files/zerounavailablereward_cohort_",cohort,".RDS", sep = ""))
  unavailable_reward_model = readRDS(file = paste("./RDS_files/unavailablereward_cohort_",cohort,".RDS", sep = ""))
  expit <- function(x) {1/(1+exp(-x))}
  
  if(input$available == 1) {
    logit_zeroreward = predict(zeroavailable_reward_model$gam, pred.data) 
    no_reward = rbinom(n=1,size=1, prob = expit(logit_zeroreward))
    mean_reward = predict(available_reward_model$gam, pred.data) 
    sig = sigma(available_reward_model$lme)
    reward = (1-no_reward)*(mean_reward + rnorm(1, mean = 0, sd = sig)) + no_reward*log(0.5)
  } else {
    logit_zeroreward = predict(zerounavailable_reward_model$gam, pred.data) 
    no_reward = rbinom(n=1,size=1, prob = expit(logit_zeroreward))
    mean_reward = predict(unavailable_reward_model$gam, pred.data) 
    sig = sigma(unavailable_reward_model$lme)
    reward = (1-no_reward)*(mean_reward + rnorm(1, mean = 0, sd = sig)) + no_reward*log(0.5)
  }
  
  if(pred.data$decision.time > 5) {
    print("Decision time must be integer <= 5.")
  } else {
    pred.data$decision.time = (pred.data$decision.time + 1)%%5 
    if (pred.data$decision.time == 0) {pred.data$decision.time = 5}
    if (pred.data$decision.time == 1) {
      pred.data$day = pred.data$day + 1 
    } else {
      pred.data$day = pred.data$day
    }
  }
  
  current.row = which(ggdrive.data$id == pred.data$id & ggdrive.data$day == pred.data$day & ggdrive.data$decision.time == pred.data$decision.time)
  
  if(identical(current.row, integer(0))) {
    return(list("reward" = reward))
  } else {
    pred.data$temperature = ggdrive.data$temperature[current.row]
    pred.data$dosage = pred.data$dosage * 0.95 + 1*(pred.data$action == 1)
    pred.data$other.location = ggdrive.data$other.location[current.row]
  }
  
  engagement.model = readRDS(file = paste("./RDS_files/engagement_cohort_",cohort,".RDS", sep = ""))
  logit_engagement = predict(engagement.model$gam, pred.data)
  pred.data$engagement = rbinom(n=1, size = 1, prob = expit(logit_engagement))
  
  variation.model = readRDS(file = paste("./RDS_files/variation_cohort_",cohort,".RDS", sep = ""))
  logit_variation = predict(variation.model$gam, pred.data)
  pred.data$variation = rbinom(n=1, size = 1, prob = expit(logit_engagement))
  
  zerologpresteps.model = readRDS(file = paste("./RDS_files/zerologpresteps_cohort_",cohort,".RDS", sep = ""))
  logit_presteps = predict(zerologpresteps.model$gam, pred.data)
  nosteps = rbinom(n=1, size = 1, prob = expit(logit_presteps))
  logpresteps.model = readRDS(file = paste("./RDS_files/logpresteps_cohort_",cohort,".RDS", sep = ""))
  pos_reward = predict(logpresteps.model$gam, pred.data) + rnorm(1, mean = 0, sd = sigma(logpresteps.model$lme))
  pred.data$logpresteps = nosteps * log(0.5) + (1-nosteps) * pos_reward
  
  sqrttotalsteps.model = readRDS(file = paste("./RDS_files/sqrttotalsteps_cohort_",cohort,".RDS", sep = ""))
  if(pred.data$decision.time == 1) {
    pred.data$sqrt.totalsteps = predict(sqrttotalsteps.model$gam, pred.data) + rnorm(1, mean = 0, sd = sigma(sqrttotalsteps.model$lme))
  }
  
  prioranti.model = readRDS(file = paste("./RDS_files/prioranti_cohort_",cohort,".RDS", sep = ""))
  logit_prioranti = predict(prioranti.model$gam, pred.data)
  pred.data$prior.anti = as.logical(rbinom(n=1, size = 1, prob = expit(logit_prioranti)))
  
  availability_model = readRDS(file = paste("./RDS_files/availability_cohort_",cohort,".RDS", sep = ""))
  logit_avail = predict(availability_model$gam, pred.data)
  user_avail = rbinom(n = 1, size = 1, prob = expit(logit_avail))
  
  state = c(pred.data$id, pred.data$day, pred.data$decision.time, pred.data$dosage, 
            pred.data$engagement, pred.data$other.location, pred.data$variation,
            pred.data$temperature, pred.data$logpresteps, pred.data$sqrt.totalsteps,
            pred.data$prior.anti)
  
  output = list("reward" = reward,
                "state" = c(pred.data$id, pred.data$day, pred.data$decision.time, pred.data$dosage,
                            pred.data$engagement, pred.data$other.location, pred.data$variation,
                            pred.data$temperature, pred.data$logpresteps, pred.data$sqrt.totalsteps,
                            pred.data$prior.anti),
                "availability" = user_avail)
  
  return(output)
}
