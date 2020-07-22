#573 Group Project
#By Cole Conte, Jiayou Chao, Peter Gonatas, Steven Geiser  

#Part 1: Set up environment, load data set
library(mgcv)
library(ISLR)
library(tidyverse)
library(GGally)
library(MASS)
library(ggplot2)
library(caret)
library(interactions)
df = Wage

#Part 2: Data manipulation

#Order education and health variables
df$education = factor(df$education, levels=c("1. < HS Grad", "2. HS Grad", "3. Some College",
                                                 "4. College Grad", "5. Advanced Degree"),ordered=T)
df$health = factor(df$health, order=T, levels = c("1. <=Good","2. >=Very Good"))
#Treat year as a factor, not a continuous variable
df$year = factor(df$year,order=T)
#Response variable health ins as a factor (0=No 1=Yes)
df$health_ins = factor(df$health_ins, order=T, levels = c("2. No","1. Yes"))


#Exploring Box-Cox transformation for wage
wage.lm = lm(wage~1,data=df)
b = boxcox(wage.lm,data=df)
lambda = b$x[which.max(b$y)]


#Part 3
#Exploratory Data Analysis

#Plot distribution of each variable
Map(function(x,name){
  if (is.factor(x)){
    ggplot() +
    geom_bar(mapping = aes(x = x,fill=..x..)) +
    labs(x=name,fill="Category")
    }
  else if (is.numeric(x)){
    ggplot() +
    geom_histogram(mapping = aes(x = x,fill=..x..)) +
    scale_fill_gradient(low="blue", high="red") +
    labs(x=name,fill=name)
  }
},df,names(df))

#Education vs Job Class count plot
ggplot(data = df) +
  geom_count(mapping = aes(x = jobclass, y = education,color=..n..))

#Log(Wage) vs Education plot
ggplot(df, aes(x=logwage, y=education)) + geom_point()


#Part 4: Single Predictor GLM vs GAM vs category-level GAM
#Removing wage (using logwage) and region (all mid-atlantic)
df = within(df,rm(wage,region))

#Generating single predictor GLM
wage.glm1 = glm(health_ins ~ logwage,family="binomial",data=df)
summary(wage.glm1)
wage.glm1.aic = AIC(wage.glm1)
wage.glm1.bic = BIC(wage.glm1)
wage.glm1.dev = deviance(wage.glm1)
wage.glm1.pred = predict(wage.glm1, new.data = data.frame(logwage), type="response")
wage.glm1.new.data = cbind(data.frame(df$logwage),wage.glm1.pred)
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
    labs(x="log(Wage)",y="Health Insurance")+
    geom_point() +
    labs(title = "Logistic Regression Model") +
    theme(plot.title = element_text(hjust = 0.5))+
    geom_line(data=wage.glm1.new.data,aes(x=df.logwage,y=wage.glm1.pred),color="blue")


#Generating single predictor GAM
wage.gam1 = mgcv::gam(health_ins ~ s(logwage),
                          family="binomial",data=df,method="REML")
summary(wage.gam1)
wage.gam1.aic = AIC(wage.gam1)
wage.gam1.bic = BIC(wage.gam1)
wage.gam1.dev = deviance(wage.gam1)
wage.gam1.pred = predict(wage.gam1, new.data = data.frame(logwage), type="response")
wage.gam1.new.data = cbind(data.frame(df$logwage),wage.gam1.pred)
wage.gam1$sp
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using REML Smoothing Parameter (0.0854)") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.new.data,aes(x=df.logwage,y=wage.gam1.pred),color="blue")


#Generating single predictor GAM by category
ggpairs(data=df,legend = 1, columns=c("maritl","logwage"), 
        mapping = aes(colour=health_ins)) +
  theme(legend.position = "bottom")
wage.gam1.bymaritl = gam(health_ins ~ s(logwage,by=maritl),
                                   family="binomial",data=df,method="REML")
summary(wage.gam1.bymaritl)
wage.gam1.bymaritl.aic = AIC(wage.gam1.bymaritl)
wage.gam1.bymaritl.bic = BIC(wage.gam1.bymaritl)
wage.gam1.bymaritl.dev = deviance(wage.gam1.bymaritl)


#Part 5: Varying smoothing function/ basis functions/ spline type
#Changing The Smoothing Parameter

#Raising the smoothing parameter
wage.gam1.highsmooth = gam(health_ins ~ s(logwage), data=df,
               family = binomial, method = "REML", sp = 10)
wage.gam1.highsmooth.pred = predict(wage.gam1.highsmooth, new.data = data.frame(logwage), type="response")
wage.gam1.highsmooth.new.data = cbind(data.frame(df$logwage),wage.gam1.highsmooth.pred)
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using Smoothing Parameter 10") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.highsmooth.new.data,aes(x=df.logwage,y=wage.gam1.highsmooth.pred),color="red")


#Lowering the smoothing parameter
wage.gam1.lowsmooth = gam(health_ins ~ s(logwage), data=df,
                           family = binomial, method = "REML", sp = 0.00001)
wage.gam1.lowsmooth.pred = predict(wage.gam1.lowsmooth, new.data = data.frame(logwage), type="response")
wage.gam1.lowsmooth.new.data = cbind(data.frame(df$logwage),wage.gam1.lowsmooth.pred)
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using Smoothing Parameter 0.00001") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.lowsmooth.new.data,aes(x=df.logwage,y=wage.gam1.lowsmooth.pred),color="red")


#Raising the number of basis functions
wage.gam1.highbasis = gam(health_ins ~ s(logwage,k=20), data=df,
                          family = binomial, method = "REML",sp=0.08543573)
wage.gam1.highbasis.pred = predict(wage.gam1.highbasis, new.data = data.frame(logwage), type="response")
wage.gam1.highbasis.new.data = cbind(data.frame(df$logwage),wage.gam1.highbasis.pred)
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using 19 Basis Functions") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.highbasis.new.data,aes(x=df.logwage,y=wage.gam1.highbasis.pred),color="red")

#Lowering the number of basis functions
wage.gam1.lowbasis = gam(health_ins ~ s(logwage,k=3), data=df,
                          family = binomial, method = "REML",sp=0.08543573)
wage.gam1.lowbasis.pred = predict(wage.gam1.lowbasis, new.data = data.frame(logwage), type="response")
wage.gam1.lowbasis.new.data = cbind(data.frame(df$logwage),wage.gam1.lowbasis.pred)
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using 2 Basis Functions") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.lowbasis.new.data,aes(x=df.logwage,y=wage.gam1.lowbasis.pred),color="red")


#Relabeling of orginal GAM
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using 9 Basis Functions") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.new.data,aes(x=df.logwage,y=wage.gam1.pred),color="green")

#Relabeling of orginal GAM
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using Thin-Plate Spline") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.new.data,aes(x=df.logwage,y=wage.gam1.pred),color="blue")

#Using Cubic Spline
wage.gam1.cubic = gam(health_ins ~ s(logwage,bs="cr"), data=df,
                         family = binomial, method = "REML")
wage.gam1.cubic.pred = predict(wage.gam1.cubic, new.data = data.frame(logwage), type="response")
wage.gam1.cubic.new.data = cbind(data.frame(df$logwage),wage.gam1.cubic.pred)
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "GAM Using Cubic Spline") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.cubic.new.data,aes(x=df.logwage,y=wage.gam1.cubic.pred),color="blue")


#Spline Comparison Plot
ggplot(data = df, aes(x=logwage,y=as.numeric(health_ins)-1))+
  labs(x="log(Wage)",y="Health Insurance")+
  geom_point() +
  labs(title = "Spline Comparison") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data=wage.gam1.cubic.new.data,aes(x=df.logwage,y=wage.gam1.cubic.pred,color="blue"))+
  geom_line(data=wage.gam1.new.data,aes(x=df.logwage,y=wage.gam1.pred,color="red")) +
  scale_color_discrete(name = "Spline", labels = c("Cubic", "Thin-Plate"))


#Generate all basis functions
matsum = rep(0,3000)
wage.gam1.mat = predict.gam(wage.gam1,type="lpmatrix")
for (i in 1:10) {
  matsum = matsum + wage.gam1.mat[,i]*coef(wage.gam1)[i]
}
#Plot the sum of all basis functions
ggplot(data=df)+
  geom_line(aes(x=logwage,y=matsum),color="brown")+
  labs(title = "Sum of All Basis Functions") +
  theme(plot.title = element_text(hjust = 0.5))

#Exponentiate to get our original model
ematsum = exp(matsum)/(1+exp(matsum))
ggplot(data=df)+
  geom_line(aes(x=logwage,y=ematsum),color="brown")+
  labs(title = "Sum of All Basis Functions",x="Log(Wage)",y="Health Insurance") +
  theme(plot.title = element_text(hjust = 0.5))


#Plotting individual basis functions
ggplot(data=df)+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).1"]*coef(wage.gam1)[2])/(1+exp(wage.gam1.mat[, "s(logwage).1"]*coef(wage.gam1)[2]))),color="blue")+
  labs(title = "1st Basis",y="Health Insurance") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data=df)+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).2"]*coef(wage.gam1)[3])/(1+exp(wage.gam1.mat[, "s(logwage).2"]*coef(wage.gam1)[3]))),color="red")+
  labs(title = "2nd Basis",y="Health Insurance") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data=df)+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).3"]*coef(wage.gam1)[4])/(1+exp(wage.gam1.mat[, "s(logwage).3"]*coef(wage.gam1)[4]))),color="green")+
  labs(title = "3rd Basis",y="Health Insurance") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting them all together
ggplot(data=df)+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).1"]*coef(wage.gam1)[2])/(1+exp(wage.gam1.mat[, "s(logwage).1"]*coef(wage.gam1)[2])),color="1st"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).2"]*coef(wage.gam1)[3])/(1+exp(wage.gam1.mat[, "s(logwage).2"]*coef(wage.gam1)[3])),color="2nd"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).3"]*coef(wage.gam1)[4])/(1+exp(wage.gam1.mat[, "s(logwage).3"]*coef(wage.gam1)[4])),color="3rd"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).4"]*coef(wage.gam1)[5])/(1+exp(wage.gam1.mat[, "s(logwage).4"]*coef(wage.gam1)[5])),color="4th"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).5"]*coef(wage.gam1)[6])/(1+exp(wage.gam1.mat[, "s(logwage).5"]*coef(wage.gam1)[6])),color="5th"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).6"]*coef(wage.gam1)[7])/(1+exp(wage.gam1.mat[, "s(logwage).6"]*coef(wage.gam1)[7])),color="6th"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).7"]*coef(wage.gam1)[8])/(1+exp(wage.gam1.mat[, "s(logwage).7"]*coef(wage.gam1)[8])),color="7th"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).8"]*coef(wage.gam1)[9])/(1+exp(wage.gam1.mat[, "s(logwage).8"]*coef(wage.gam1)[9])),color="8th"))+
  geom_line(aes(x=logwage,y=exp(wage.gam1.mat[, "s(logwage).9"]*coef(wage.gam1)[10])/(1+exp(wage.gam1.mat[, "s(logwage).9"]*coef(wage.gam1)[10])),color="9th"))+
  labs(title = "Basis Functions",x="Log(Wage)",y="Health Insurance",color="Function") +
  theme(plot.title = element_text(hjust = 0.5))
  
#Assessing Model Adequacy
plot(wage.gam1)
gam.check(wage.gam1)



#Part 6: Multivariate GAM
df$health_ins = as.numeric(df$health_ins) -1

#Smoothing Age
wage.gam.mv = gam(logwage ~ year + s(age) + maritl + education +
                        jobclass + health + health_ins,
                      data = df)
summary(wage.gam.mv)
plot(wage.gam.mv, main = 'Using s(age)')

#Smoothing Age^2
wage.gam2.mv = gam(logwage ~ year + s(age ^ 2) + maritl + education +
                        jobclass + health + health_ins,
                      data = df)
summary(wage.gam2.mv)
plot(wage.gam2.mv, main = 'Using s(age^2)')

#Smoothing log(Age)
wage.gam3.mv = gam(logwage ~ year + s(log(age)) + maritl + education +
                        jobclass + health + health_ins,
                      data = df)
summary(wage.gam3.mv)
plot(wage.gam3.mv, main = 'Using s(log(age))')

# Adding Factor-smooth interactions, tensor interactions
df$year = as.numeric(levels(df$year))[df$year] #year must be numeric for use with tensor interactions 
wage.gam4.mv = gam(
  logwage ~ year + s(age, by = education) + maritl + education +
  jobclass  + health_ins + age:health + te(age, year),
  data = df
)
summary(wage.gam4.mv)

#Plotting 
interact_plot(wage.gam4.mv, pred = age, modx = education)
interact_plot(wage.gam4.mv, pred = age, modx = health)
vis.gam(x = wage.gam4.mv,
        view = c("age", "year"),
        plot.type = "persp")
gam.check(wage.gam4.mv, old.style = TRUE)

#Model Selection
#Refit wage.gam4.mv without age:health because the interaction isn't significant
wage.gam5.mv = gam(
  logwage ~ year + s(age, by = education) + maritl + education +
    jobclass  + health_ins  + te(age, year),
  data = df, select = T
)
summary(wage.gam5.mv)

#Concurvity Check
concurvity(wage.gam5.mv)
concurvity(wage.gam5.mv,full=F)
