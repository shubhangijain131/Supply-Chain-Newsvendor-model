d<- read.csv("NewsvendorData.csv", header=T)
View(d)
hist(d$Demand)
install.packages("ks")
library("ks")

#Single Period Inventory
price = 10
cost = 5
N = 1000

ProfitProfile = c()
for(Q in 100:1000){
  u = runif(1000) #Generate Random probability values
  k = kde(d$Demand) #Estimate density of observed demands
  
  #plot(k)
  #Convert u and k into random demands
  D = qkde(u, k) #u - pvalue and k - density estimates
  #plot(density(D))
  
  Profit = c()
  for(i in 1:N){
    Pr = min(D[i],Q)*(price-cost) - max(0,(Q-D[i]))*cost
    Profit = c(Profit,Pr)
  }
  ExP = mean(Profit)
  ProfitProfile = c(ProfitProfile,ExP)
}

ProfitProfile
plot(seq(100,1000),ProfitProfile,type="l")

MaxProfit<- max(ProfitProfile)
MaxProfit
qmax<- which(ProfitProfile==max(ProfitProfile))
qmax

