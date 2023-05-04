### This code generates synthetic data arising from an MRT
source("gensythdata.R")
complete_data = rep(0,0)
set.seed(1907147)

for (id in 63:100) {
  ## For a given ID, we can generate data sequentially as follows
  print(paste("On participant", id))
  initial_values = generate_initial_values(id)
  state = initial_values$state
  available = initial_values$availability
  # print(state)
  # print(available)
  MRT = state
  MRT_avails = available
  MRT_outcome = rep(0,0)
  MRT_probs = rep(0,0)
  MRT_action = rep(0,0)
  
  for(iter in 1:10000) {
    if (iter %% 50 == 0) {
      print(iter)
      print(paste("Unique ids in here are:", unique(MRT$id)))
    }
    ### PART TO EDIT
    probt = 0.5  # CAN MAKE THIS MUCH MORE COMPLEX!
    action = rbinom(n = 1, size = 1, prob = probt)
    MRT_probs = rbind(MRT_probs, probt)
    MRT_action = rbind(MRT_action, action)
    ###
    
    output = generate_reward_state(probability = probt, action = action, state = state, available = available)
    MRT_outcome = rbind(MRT_outcome, output$reward)
    
    if((state[2] == maxT[id,]$day) & (state[3] == 5)) {break}
    MRT = rbind(MRT, output$state)
    MRT_avails = rbind(MRT_avails, output$availability)
    
    
    state = output$state
    available = output$availability
  }
  MRT$id = id
  participant_data = cbind(MRT, MRT_avails, MRT_probs, MRT_action, MRT_outcome)
  complete_data = rbind(complete_data, participant_data)
}

complete_data = data.frame(complete_data)
names(complete_data)[15] = c("MRT_reward")

write.csv(complete_data, file = "./HS_MRT_example_v2v3_combined.csv", row.names= FALSE)
