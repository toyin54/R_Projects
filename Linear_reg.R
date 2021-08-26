#load Data
library(pwr)
library(ggplot2)
library(descr)
library(dplyr)
library(lattice)
library(psych)
library(gmodels)
library(lattice)
library(psych)
library(gmodels)

#Load Data
h_exp <- read.csv("MultiRegDataset.csv")
head(h_exp)
# Mean, SD, Min/Max
prop.table(table(h_exp$sex))
prop.table(table(h_exp$smoker))
prop.table(table(h_exp$region))
mean(h_exp$age)
mean(h_exp$bmi)
mean(h_exp$expenses)
sd(h_exp$expenses)

#Average, Min and Max Expenses data Grouped by Smoker and Filtered by Sex(Male)
h_exp %>% filter(sex =="male") %>%group_by(sex,smoker) %>% summarize(Expenses_Average = mean(expenses),Age_Avergae = mean(age), Minimum_Expenses = min(expenses),Maximum_Expenses = max(expenses))

#Average, Min and Max Expenses data Grouped by Smoker and Filtered by Sex(Female)
h_exp %>%filter(sex =="female") %>%group_by(sex,smoker) %>% summarize(Expenses_Average = mean(expenses),Age_Average = mean(age),Minimum_Expenses = min(expenses),Maximum_Expenses = max(expenses))



#Histogram of Expenses
x=h_exp$expenses
h<-hist(x, breaks=10, col="red", xlab="Expenses",
        main="Expenses Data")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)




#Expenses vs. NO Children point plot
ggplot(data=h_exp, aes(y = expenses, x = age , color = sex )) + geom_point()

#T-Test



sample = h_exp$expenses
summary(sample)
#One Sample t-test
t.test(sample, mu=10000)

#Linear Regression
ggplot(h_exp, aes(x = smoker, y = expenses))+
  geom_point(colour = "red") + geom_smooth(method = "lm", fill = NA) +
  labs(title = "Original Model",
       x = "Smoker",
       y = "Expenses")

#Linear Model - No Transformation
lmodel <- lm(expenses ~ smoker, data = h_exp)
summary(lmodel)



df2 <- df1 %>% select(THANK_YOU_STATUS, GMV , BOOKINGS) %>% filter(THANK_YOU_STATUS %in% "thank_you_call")
count(df2)

df3 <- df1 %>% select(THANK_YOU_STATUS, GMV , BOOKINGS) %>% filter(THANK_YOU_STATUS %in% "no_call")
count(df3)



