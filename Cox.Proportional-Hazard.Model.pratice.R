library(MASS)
head(gehan)
library(survival)
#group(1 = treatment, 1 = relapsed)
#weeks(time of remission)

Treatment = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 
          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
Time = c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23,
          6,6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32, 32,34,35)
Status = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
           1,1,1,0,1,0,1,0,0,1,1, 0,0,0,1,1, 0,0,0,0,0)
logWBC = c(2.8, 5, 4.91, 4.48, 4.01, 4.36, 2.42, 3.49, 3.97, 3.52, 3.05, 2.32, 3.26, 3.49, 2.12, 1.5, 3.06, 2.3, 2.95, 2.73, 1.97,
           2.31, 4.06, 3.28, 3.2, 4.43, 2.8, 2.96, 2.7, 2.6, 2.88, 3.6, 2.16, 2.05, 2.01, 2.32, 2.57, 1.78, 2.2, 2.53, 1.47, 1.45)
  
length(Status)

Luek <- as.data.frame(cbind(Time, Status, Treatment, logWBC))
head(Luek)


#Model 1
surv_model = coxph(Surv(Luek$Time, Luek$Status) ~ Treatment, data = Luek, method = "breslow")
summary(surv_model)

surv_model$loglik[2]

#plot
plot(survfit(surv_model, newdata = data.frame(Treatment = c(0,1), logWBC = rep(mean(logWBC),2))), lty = c(1,2), xlab = "Time", ylab = "Estmiated S(t)", main = "Adjusted Survival Curves")

#Model 2
surv_model2 = coxph(Surv(Luek$Time, Luek$Status) ~ Treatment + logWBC, data = Luek, method = "breslow")
summary(surv_model2)

surv_model2$loglik[2]

#plot


#Model 3
surv_model3 = coxph(Surv(Luek$Time, Luek$Status) ~ Treatment * logWBC, data = Luek, method = "breslow")
summary(surv_model3)

surv_model3$loglik[2]

#plot
