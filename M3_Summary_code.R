#########################################
# ESE420/520 Mid-term 3                 #
# Zhiyang Chen, Bari Gordon, Meiqing Li #
#########################################

rm(list=ls())
setwd("C:/Users/straw/Google Drive/ESE420_520/Midterm 3/Code")

library(GGally)
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(logspline)
library(DistributionUtils)
library(readr)


# PDFs of risk and emotion factors
### type in the code here ###
data <- read_csv("SurveyData.csv")
sum(is.na(data))
##Empirical PDF

###Categorize questions into two different factors
data2 <- mutate(data, redLight_HV = E3 + E7 + E8)
data3 <- mutate(data2, redLight_Ped = E4 + E5 + E6)

###Scale the variable so that it has range from 0.03 to 1.03
range01 <- function(x){(x-min(x))/(max(x)-min(x)) + 0.03}

data3$redLight_HV_01 <- range01(data3$redLight_HV)
data3$redLight_Ped_01 <- range01(data3$redLight_Ped)

###Draw PDF for hv
hist(data3$redLight_HV_01,probability=TRUE)

###Draw PDF for ped
hist(data3$redLight_Ped_01,probability=TRUE)

##Fit the data (should we fit a theoretical probability distribution? or just empirical?)
#(reference: https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best)

###Try few distributions (HV)
fit.weibull <- fitdist(data3$redLight_HV_01, "weibull")
plot(fit.weibull)

fit.gamma <- fitdist(data3$redLight_HV_01, "gamma")
plot(fit.gamma)

fit.norm <- fitdist(data3$redLight_HV_01, "norm")
plot(fit.norm)

fit.exp <- fitdist(data3$redLight_HV_01, "exp")
plot(fit.exp)

###AIC of exponential fit is lower, we will use exponential
c(fit.weibull$aic, fit.gamma$aic, fit.norm$aic, fit.exp$aic)

######Try few distributions (Ped)
fit.1.weibull <- fitdist(data3$redLight_Ped_01, "weibull")
plot(fit.1.weibull)

fit.1.gamma <- fitdist(data3$redLight_Ped_01, "gamma")
plot(fit.1.gamma)

fit.1.norm <- fitdist(data3$redLight_Ped_01, "norm")
plot(fit.1.norm)

fit.1.exp <- fitdist(data3$redLight_Ped_01, "exp")
plot(fit.1.exp)

### AIC of normal fit is lower, we will use normal.
c(fit.1.weibull$aic, fit.1.gamma$aic, fit.1.norm$aic, fit.1.exp$aic)

##KS TEST for goodness of fit

###for hv, red-light weight is an exponential distribution with lambda = 2.310655, the error is summarized below.

summary(fit.exp)

###for ped, red-light weight is a normal distribution with mean = 0.5602 and sd = 0.3034, the error is summarized below.
summary(fit.1.norm)



##########
#Load RNetLogo Package, Start NetLogo
library("RNetLogo")
NLStart("C:/Program Files/NetLogo 6.0.2/app", gui = FALSE, nl.jarname = "netlogo-6.0.2.jar")

########################
#Monte Carlo Simulation#
########################

absolute.model.path <- "C:/Users/straw/Google Drive/ESE420_520/Midterm 3/Code/M3_Code_v5(riskrandom).nlogo"
NLLoadModel(absolute.model.path)

NLCommand("setup")
nruns <- 100

# we ran cominatorically the two PDFs
d_HV <- rexp(100,2.310655)
d_Ped <- rnorm(100,0.56,0.30)
traffic_offenses <- list()
HV <- list()
Ped <- list()

for(i in 1:100) {
  print (d_HV[i])
  print (d_Ped[i])
  NLCommand("setup")
  NLCommand("set red-light_Ped", d_Ped[i])
  NLCommand("set red-light_HV", d_HV[i])
  a <- NLReport("red-light_HV")
  print(a)
  b <- NLReport("red-light_Ped")
  print(b)
  NLDoCommand(1000,"go")
  traffic_offenses[[i]] <- NLReport(c("number_traffic_offenses"))
  HV[[i]] <- d_HV[i]
  Ped[[i]] <- d_Ped[i]}

output_HV <- data.frame(t(sapply(HV,c)))
output_Ped <- data.frame(t(sapply(Ped,c)))
output_traffic_offenses <- data.frame(t(sapply(traffic_offenses,c)))
output <- rbind(output_traffic_offenses,output_HV, output_Ped)

# output csv
write.csv(output, "Monte_carlo_timedata(100).csv")

# plot the results from Monte Carlo Simulation
sim <- read.csv("Monte_carlo_timedata(100).csv")

plot(sim$Offence~sim$HV, xlab = "perceived risk factor (HV)", ylab = "number of traffic offences")
plot(sim$Offence~sim$Ped, xlab = "perceived risk factor (Ped)", ylab = "number of traffic offences")

# plot empirical PDFs #
hist(sim$Offence, main= "", xlab = "number of traffic offences", ylab = "density",probability = TRUE)
lines(x, y, col = "red")
plot(density(sim$Offence),  main = "", xlab = "number of traffic offences", ylab = "density")

######### MEA ##############

# Factors
parkratio <- c(10,10,10,10,90,90,90,90)
pctAV <- c(10,10,90,90,10,10,90,90)
MEA_work <- c(0.2,0.2,1.8,1.8,1.8,1.8,0.2,0.2)
MEA_shop <- c(0.2,1.8,0.2,1.8,0.2,1.8,0.2,1.8)
MEA_school <- c(0.2,1.8,0.2,1.8,1.8,0.2,1.8,0.2)
MEA_house <- c(0.2,1.8,1.8,0.2,0.2,1.8,1.8,0.2) 

MEA_output_offence <- data.frame(matrix(ncol = 3, nrow = 8))
MEA_output_deaths <- data.frame(matrix(ncol = 3, nrow = 8))

for (i in 1:8){
  for (j in 1:3){
    NLCommand("setup")
    NLCommand("set parking-ratio", parkratio[i])
    NLCommand("set percentage-of-AVs", pctAV[i])
    NLCommand("set MEA_work", MEA_work[i])
    NLCommand("set MEA_shop", MEA_shop[i])
    NLCommand("set MEA_school", MEA_school[i])
    NLCommand("set MEA_house", MEA_house[i])
    NLDoCommand(1000,"go")
    MEA_output_offence[i,j] <- NLReport(c("number_traffic_offenses"))
    MEA_output_deaths[i,j] <- NLReport(c("number_deaths"))
  }
    
}
  
# output csv
write.csv(MEA_output_offence, "offence.csv")
write.csv(MEA_output_deaths, "deaths.csv")

##### Plot the MEA results#########

#rm(list=ls())

library(GGally)
library(ggplot2)
library(dplyr)
library(readr)
library(car)

dat <- read.csv("Timedata.csv")
output1 <- read.csv("offence.csv")
output2 <- read.csv("deaths.csv")

#Append input into the output
parkratio <- c(10,10,10,10,90,90,90,90)
pctAV <- c(10,10,90,90,10,10,90,90)
MEA_work <- c(0.2,0.2,1.8,1.8,1.8,1.8,0.2,0.2)
MEA_shop <- c(0.2,1.8,0.2,1.8,0.2,1.8,0.2,1.8)
MEA_school <- c(0.2,1.8,0.2,1.8,1.8,0.2,1.8,0.2)
MEA_house <- c(0.2,1.8,1.8,0.2,0.2,1.8,1.8,0.2) 

output1 <- mutate(output1, park_ratio = parkratio)
output1 <- mutate(output1, percent_av = pctAV)
output1 <- mutate(output1, work_u = MEA_work)
output1 <- mutate(output1, shop_u = MEA_shop)
output1 <- mutate(output1, school_u = MEA_school)
output1 <- mutate(output1, house_u = MEA_house)

output2 <- mutate(output2, park_ratio = parkratio)
output2 <- mutate(output2, percent_av = pctAV)
output2 <- mutate(output2, work_u = MEA_work)
output2 <- mutate(output2, shop_u = MEA_shop)
output2 <- mutate(output2, school_u = MEA_school)
output2 <- mutate(output2, house_u = MEA_house)

# Output 1: offence
fit1 <- lm(mean ~ park_ratio+percent_av+work_u+shop_u+school_u+house_u, data = output1)
summary(fit1)
Anova(fit1)

park_ratio_effect <- c((output1$mean[1]+output1$mean[2]+output1$mean[3]+output1$mean[4])/4, 
                       (output1$mean[5]+output1$mean[6]+output1$mean[7]+output1$mean[8])/4)
percent_av_effect <- c((output1$mean[1]+output1$mean[2]+output1$mean[5]+output1$mean[6])/4, 
                       (output1$mean[3]+output1$mean[4]+output1$mean[7]+output1$mean[8])/4)
work_effect <- c((output1$mean[1]+output1$mean[2]+output1$mean[7]+output1$mean[8])/4, 
                 (output1$mean[5]+output1$mean[6]+output1$mean[3]+output1$mean[4])/4)
shop_effect <- c((output1$mean[1]+output1$mean[3]+output1$mean[5]+output1$mean[7])/4, 
                 (output1$mean[2]+output1$mean[4]+output1$mean[6]+output1$mean[8])/4)
school_effect <- c((output1$mean[1]+output1$mean[3]+output1$mean[6]+output1$mean[8])/4, 
                   (output1$mean[2]+output1$mean[4]+output1$mean[5]+output1$mean[7])/4)
house_effect <- c((output1$mean[1]+output1$mean[4]+output1$mean[5]+output1$mean[8])/4, 
                  (output1$mean[2]+output1$mean[3]+output1$mean[6]+output1$mean[7])/4)

MEA.1 <- data.frame("x1" = c(10,90), "x2" = c(10,90), "x3" = c(0.2, 1.8), "x4" = c(0.2, 1.8), 
                    "x5" = c(0.2, 1.8), "x6" = c(0.2, 1.8), 
                    "y1" = park_ratio_effect, "y2" = percent_av_effect, "y3" = work_effect, 
                    "y4" = shop_effect, "y5" = school_effect, "y6" = house_effect)

plot1 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x1, y = y1), color = "red") +
  geom_line(data = MEA.1, aes(x = x2, y = y2), color = "blue") +
  xlab('value of factor') +
  ylab('number of offence')

plot2 <- ggplot() + geom_line(data = MEA.1, aes(x = x3, y = y3), color = "orange") +
  geom_line(data = MEA.1, aes(x = x4, y = y4), color = "black") +
  geom_line(data = MEA.1, aes(x = x5, y = y5), color = "green") +
  geom_line(data = MEA.1, aes(x = x6, y = y6), color = "yellow") +
  xlab('value of factor') +
  ylab('number of offence')

plot3 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x1, y = y1), color = "red") +
  geom_line(data = MEA.1, aes(x = x2, y = y2), color = "blue") +
  geom_line(data = MEA.1, aes(x = x3, y = y3), color = "orange") +
  geom_line(data = MEA.1, aes(x = x4, y = y4), color = "black") +
  geom_line(data = MEA.1, aes(x = x5, y = y5), color = "green") +
  geom_line(data = MEA.1, aes(x = x6, y = y6), color = "yellow") +
  xlab('value of factor') +
  ylab('number of offence')

# arrange multiple plots on canvas
library(gridExtra)
p1 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x1, y = y1), color = "blue") +
  xlab('parking ratio') +
  ylab('number of offence') + ylim (0,400)

p2 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x2, y = y2), color = "blue") +
  xlab('percentage of AVs') +
  ylab('number of offence') + ylim (0,400)

p3 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x3, y = y3), color = "blue") +
  xlab('work') +
  ylab('number of offence') + ylim (0,400)

p4 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x4, y = y4), color = "blue") +
  xlab('shop') +
  ylab('number of offence') + ylim (0,400)

p5 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x5, y = y5), color = "blue") +
  xlab('school') +
  ylab('number of offence') + ylim (0,400)

p6 <- ggplot() + 
  geom_line(data = MEA.1, aes(x = x6, y = y6), color = "blue") +
  xlab('house') +
  ylab('number of offence') + ylim (0,400)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, 
             top = "Main effects plot for number of offence" )

#Output 2: num death
fit2 <- lm(mean ~ park_ratio+percent_av+work_u+shop_u+school_u+house_u, data = output2)
summary(fit2)
Anova(fit2)

park_ratio_effect <- c((output2$mean[1]+output2$mean[2]+output2$mean[3]+output2$mean[4])/4, 
                       (output2$mean[5]+output2$mean[6]+output2$mean[7]+output2$mean[8])/4)
percent_av_effect <- c((output2$mean[1]+output2$mean[2]+output2$mean[5]+output2$mean[6])/4, 
                       (output2$mean[3]+output2$mean[4]+output2$mean[7]+output2$mean[8])/4)
work_effect <- c((output2$mean[1]+output2$mean[2]+output2$mean[7]+output2$mean[8])/4, 
                 (output2$mean[5]+output2$mean[6]+output2$mean[3]+output2$mean[4])/4)
shop_effect <- c((output2$mean[1]+output2$mean[3]+output2$mean[5]+output2$mean[7])/4, 
                 (output2$mean[2]+output2$mean[4]+output2$mean[6]+output2$mean[8])/4)
school_effect <- c((output2$mean[1]+output2$mean[3]+output2$mean[6]+output2$mean[8])/4, 
                   (output2$mean[2]+output2$mean[4]+output2$mean[5]+output2$mean[7])/4)
house_effect <- c((output2$mean[1]+output2$mean[4]+output2$mean[5]+output2$mean[8])/4, 
                  (output2$mean[2]+output2$mean[3]+output2$mean[6]+output2$mean[7])/4)

MEA.2 <- data.frame("x1" = c(10,90), "x2" = c(10,90), "x3" = c(0.2, 1.8), 
                    "x4" = c(0.2, 1.8), "x5" = c(0.2, 1.8), "x6" = c(0.2, 1.8),
                    "y1" = park_ratio_effect, "y2" = percent_av_effect, "y3" = work_effect, 
                    "y4" = shop_effect, "y5" = school_effect, "y6" = house_effect)

ggplot() + 
  geom_line(data = MEA.2, aes(x = x1, y = y1), color = "red") +
  geom_line(data = MEA.2, aes(x = x2, y = y2), color = "blue") +
  xlab('value of factor') +
  ylab('number of deaths')

ggplot() + geom_line(data = MEA.2, aes(x = x3, y = y3), color = "orange") +
  geom_line(data = MEA.2, aes(x = x4, y = y4), color = "black") +
  geom_line(data = MEA.2, aes(x = x5, y = y5), color = "green") +
  geom_line(data = MEA.2, aes(x = x6, y = y6), color = "yellow") +
  xlab('value of factor') +
  ylab('number of deaths')

ggplot() + 
  geom_line(data = MEA.2, aes(x = x1, y = y1), color = "red") +
  geom_line(data = MEA.2, aes(x = x2, y = y2), color = "blue") +
  geom_line(data = MEA.2, aes(x = x3, y = y3), color = "orange") +
  geom_line(data = MEA.2, aes(x = x4, y = y4), color = "black") +
  geom_line(data = MEA.2, aes(x = x5, y = y5), color = "green") +
  geom_line(data = MEA.2, aes(x = x6, y = y6), color = "yellow") +
  xlab('value of factor') +
  ylab('number of deaths')

p1 <- ggplot() + 
  geom_line(data = MEA.2, aes(x = x1, y = y1), color = "red") +
  xlab('parking ratio') +
  ylab('number of deaths') + ylim (0,10)

p2 <- ggplot() + 
  geom_line(data = MEA.2, aes(x = x2, y = y2), color = "red") +
  xlab('percentage of AVs') +
  ylab('number of deaths') + ylim (0,10)

p3 <- ggplot() + 
  geom_line(data = MEA.2, aes(x = x3, y = y3), color = "red") +
  xlab('work') +
  ylab('number of deaths') + ylim (0,10)

p4 <- ggplot() + 
  geom_line(data = MEA.2, aes(x = x4, y = y4), color = "red") +
  xlab('shop') +
  ylab('number of deaths') + ylim (0,10)

p5 <- ggplot() + 
  geom_line(data = MEA.2, aes(x = x5, y = y5), color = "red") +
  xlab('school') +
  ylab('number of deaths') + ylim (0,10)

p6 <- ggplot() + 
  geom_line(data = MEA.2, aes(x = x6, y = y6), color = "red") +
  xlab('house') +
  ylab('number of deaths') + ylim (0,10)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, 
             top = "Main effects plot for number of deaths" )

##################
#What-if Analysis#
##################

absolute.model.path <- "C:/Users/straw/Google Drive/ESE420_520/Midterm 3/Code/M3_Code_v7(risk4).nlogo"
NLLoadModel(absolute.model.path)

dat <- seq(0.2,1.8,0.01)

deaths <- list()
work_utility <- list()

## we ran the following simulation for three iterations
for (i in 1:161){
  NLCommand("setup")
  NLCommand("set MEA_work", dat[i])
  a <- dat[i]
  print(a)
  NLDoCommand(1000,"go")
  work_utility[[i]] <- dat[i]
  deaths[[i]] <- NLReport(c("number_deaths"))}

whatif_output_work <- data.frame(t(sapply(work_utility,c)))
whatif_output_deaths <- data.frame(t(sapply(deaths,c)))
whatif_output <- rbind(whatif_output_work,whatif_output_deaths)

# output csv
write.csv(whatif_output, "whatif_work_deaths.csv")

#### analyze the results from whatif
whatif <- read.csv("whatif_work_deaths.csv")
plot (whatif$deaths~whatif$work, xlab = "destination utility of work", ylab = "number of deaths")

# fit a linear regression
abline(lm(whatif$deaths~whatif$work), col = "blue")
summary(lm(whatif$deaths~whatif$work))

# fit a non-linear regression
nls_fit <- nls(deaths ~ a + b * work^(-c), whatif, start = list(a = -1, b = 5, c = 2))
lines(whatif$work, predict(nls_fit), col = "red")
summary(nls_fit)
