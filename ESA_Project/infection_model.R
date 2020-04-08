#rm(list=ls()) 
library(EpiModel)
library(ggplot2)
#I am using Susceptible-Infectious-Recovered (SIR) model from EpiModel package. SIR Model
# is currently being used around the globe for Covid-19 infection modelling.
# to see the full list of adjustable parameters for SIR Modelling, type ?param.dcm
# but for our ESA Project we will focus on the infection number played out in the game

simulate_infection<-function(infection){
  #As the counter records cards left in deck, we calculate the infection cards being played out via 20-N.
  
  infection<- 20-infection
  #for realistic replication, we assume one player in the game equals to one hundred real life players
  infection<- infection/20*4
  
  #we will assume about 4 people in the wider community of the 4 players
  init <- init.dcm(s.num = 4, i.num = infection, r.num = 0)
  
  #create differing levels of 'social interaction' to simulate
  act.rates <- seq(1, 0.2, by=-.05)
  #Deterministic Compartmental Model, We run simulation 100 times in 0.5 time increments
  control <- control.dcm(type = "SIR", nsteps = 250, dt = 0.5)
  
  #we then plot the chosen degree of interaction decided by players
  param <- param.dcm(inf.prob = 0.4, act.rate = 0.7, rec.rate = 1/20,
                     a.rate = 0, ds.rate = 0, di.rate = 1/80, dr.rate = 0)
  mod <- dcm(param, init, control) #run model
  mod.df <- as.data.frame(mod) # save into dataframe
  
  g<- ggplot(data=mod.df,
         aes(x=mod.df$time, y=mod.df$i.num))+
    geom_line() +
    labs(x="Turns since recording", y="Number of Infected")+
    coord_cartesian(xlim=c(0,20), ylim=c(0,4))
  print(g)
}


