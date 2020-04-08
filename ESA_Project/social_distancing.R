#rm(list=ls()) 
library(EpiModel)
#I am using Susceptible-Infectious-Recovered (SIR) model from EpiModel package. SIR Model
# is currently being used around the globe for Covid-19 infection modelling.
# to see the full list of adjustable parameters for SIR Modelling, type ?param.dcm
# but for this function for our ESA Project we will focus on act.rate — Average number of transmissible acts, like shaking hands.
degree=0.5
simulate_model<-function(degree=0.5){
  
  #to create graph plot area
  plot(NA, type="n", xlim=c(0,1000), ylim=c(0, 250),
       bty="n", axes=FALSE,
       xlab="Time Since First Case", ylab="Number Infected", 
       main="Flatten the Curve by Minimizing Interactions!\n(Social Distancing effects in the wider community)")
  axis(1, seq(0,1000,250), lwd=0, lwd.ticks = .5, pos = -5)
  axis(2, at=seq(0, 250, 50), lwd=0, lwd.ticks=.5, pos=-2)
  
  #we will assume about 1000 people in the wider community of the 4 players
  init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)

  #create differing levels of 'social interaction' to simulate
  act.rates <- seq(1, 0.2, by=-.05)
  #Deterministic Compartmental Model, We run simulation 100 times in 0.5 time increments
  control <- control.dcm(type = "SIR", nsteps = 1000, dt = 0.5)
  
  # Run simulation then plot, for each varying degrees of possible social interaction
  for (rt in act.rates) {
    param <- param.dcm(inf.prob = 0.2, act.rate = rt, rec.rate = 1/20,
                       a.rate = 0, ds.rate = 0, di.rate = 1/80, dr.rate = 0)
    mod <- dcm(param, init, control) #run model
    mod.df <- as.data.frame(mod) # save into dataframe
    #plot lines per varying degree of interaction
    lines(mod.df$time, mod.df$i.num,
          col=rgb(85/255,158/255,161/255,min(1-rt+.1, 1)),
          lwd=1+(1-rt))   
  }
  
  #we then plot the chosen degree of interaction decided by players
  param <- param.dcm(inf.prob = 0.2, act.rate = degree, rec.rate = 1/20,
                     a.rate = 0, ds.rate = 0, di.rate = 1/80, dr.rate = 0)
  mod <- dcm(param, init, control) #run model
  mod.df <- as.data.frame(mod) # save into dataframe
  lines(mod.df$time, mod.df$i.num,
        col=rgb(255/255,0/255,0/255,min(1-rt+.1, 1)),
        lwd=1+(1-rt))  
}
simulate_model(0.45)

