
library(survival)


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
plot(survfit(surv_model2, newdata = data.frame(Treatment = c(0,1), logWBC = rep(mean(logWBC),2))), lty = c(1,2), xlab = "Time", ylab = "Estmiated S(t)", main = "Adjusted Survival Curves M2")


#Model 3
surv_model3 = coxph(Surv(Luek$Time, Luek$Status) ~ Treatment * logWBC, data = Luek, method = "breslow")
summary(surv_model3)

surv_model3$loglik[2]

#plot
plot(survfit(surv_model3, newdata = data.frame(Treatment = c(0,1), logWBC = rep(mean(logWBC),2))), lty = c(1,2), xlab = "Time", ylab = "Estmiated S(t)", main = "Adjusted Survival Curves M3")

#Compare
anova(surv_model, surv_model2) #Since pvlaue is below .001, we know that more terms (surv_model2) is better
anova(surv_model2, surv_model3) #Since pvalue is above .05 we know that surv_model2 was a better model than surv_model3

#Subconclusion: surv_model2 is the best model out of the three for the dataset. 


##########################################################################################################################
#Models with efron as ties method and not breslow 
##########################################################################################################################


#Model 1E
surv_modelE = coxph(Surv(Luek$Time, Luek$Status) ~ Treatment, data = Luek, method = "efron")
summary(surv_modelE) #both models have Treatment was sig. 

surv_modelE$loglik[2] #This is -85 and before using breslow this was .86. R squared is more in efron model than brewslow. 

#plot
plot(survfit(surv_modelE, newdata = data.frame(Treatment = c(0,1), logWBC = rep(mean(logWBC),2))), lty = c(1,2), xlab = "Time", ylab = "Estmiated S(t)", main = "Adjusted Survival Curves E")

#Model 2E
surv_model2E = coxph(Surv(Luek$Time, Luek$Status) ~ Treatment + logWBC, data = Luek, method = "efron")
summary(surv_model2E) #All sig like in previous model. log like is -69 and before was -72. r squared is .671 and before it was .644

surv_model2E$loglik[2]

#plot
plot(survfit(surv_model2E, newdata = data.frame(Treatment = c(0,1), logWBC = rep(mean(logWBC),2))), lty = c(1,2), xlab = "Time", ylab = "Estmiated S(t)", main = "Adjusted Survival Curves M2E")


#Model 3E
surv_model3E = coxph(Surv(Luek$Time, Luek$Status) ~ Treatment * logWBC, data = Luek, method = "efron")
summary(surv_model3E) #only LogWBC sig in both models. log like is -69 when before it was -72. The r squared is .674 and before it was .648

surv_model3E$loglik[2]

#plot
plot(survfit(surv_model3E, newdata = data.frame(Treatment = c(0,1), logWBC = rep(mean(logWBC),2))), lty = c(1,2), xlab = "Time", ylab = "Estmiated S(t)", main = "Adjusted Survival Curves M3E")

#compare
anova(surv_modelE, surv_model2E) #pvalue is below .05 so surv_model2E better
anova(surv_model2E, surv_model3E) #pvalue is above .05 so surv_model2E better

#Subconclusion: surv_model2E is the best model out of the three efron models. 


#Report Conclusion: After recreating the slides from class, I decided to determine using anova which model was best. I found that surv_model2 was best.
#It uses both Treatment and logWBC as factors, but not their interaction effect. Then I decided to switch from breslow to efron as my method of dealing
#with ties. I found that efron models were slightly better. That's because breslow has a tendency to underestimate parameters in Cox model. That's why
#if I had to pick the "best" overall model from the ones I produced, it would be surv_model2E, which uses Treatment, logWBC as facts and uses efron method to 
#deal with ties. 



