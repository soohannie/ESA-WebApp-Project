#install.packages(c("EpiModel", "extrafont", "animation"),dependencies = TRUE)
rm(list=ls()) 
library(EpiModel)

param <- param.dcm(inf.prob = 0.2, act.rate = 1, rec.rate = 1/20,
                   a.rate = 0, ds.rate = 0, di.rate = 1/80, dr.rate = 0)
init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
mod <- dcm(param, init, control)
mod.df <- as.data.frame(mod)

plot(NA, type="n", xlim=c(0,1000), ylim=c(0, 250),
     bty="n", axes=FALSE,
     xlab="Time Since First Case", ylab="Number Infected", 
     main="Flatten the Curve")
axis(1, seq(0,1000,250), lwd=0, lwd.ticks = .5, pos = -5)
axis(2, at=seq(0, 250, 50), lwd=0, lwd.ticks=.5, pos=-2)

act.rates <- seq(.8, 0, by=-.05)
control <- control.dcm(type = "SIR", nsteps = 1000, dt = 0.5)
# Run simulation then plot, for each acts per person
for (rt in act.rates) {
  param <- param.dcm(inf.prob = 0.2, act.rate = rt, rec.rate = 1/20,
                     a.rate = 0, ds.rate = 0, di.rate = 1/80, dr.rate = 0)
  mod <- dcm(param, init, control)
  mod.df <- as.data.frame(mod)
  lines(mod.df$time, mod.df$i.num,
        col=rgb(85/255,158/255,161/255,min(1-rt+.1, 1)),
        lwd=1+(1-rt))   
}
