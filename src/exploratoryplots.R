library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(lme4)
library(akima)

#Reading in the data
changes_Sound <- load(file = "data/sounds.rdata")
changes_Sound <- data.frame(data)
attach(changes_Sound)
summary(changes_Sound)
str(changes_Sound)
#Names of the variables
names(changes_Sound)

#Suimmary Statistics
couunt <-changes_Sound %>% group_by(Following.POA) %>% summarize(Count = n()) %>% arrange(desc(Count))
AgeDecadeGender <-  changes_Sound %>% group_by(Gender, Age, Decade) %>% summarise(Average = mean(F1), Variance = (sd(F1)^2))
write.csv(AgeDecadeGender, file = "data/Mean_Age_Gender_Decade.csv")


#No interaction terms
Model_1 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + (1|Speaker)+(1|Word))
Model_9 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Age:Decade + Gender:Age + Vowel8:Preceding.POA + Vowel8:Following.POA  + (1|Speaker)+(1|Word))

anova(Model_9)
plot(Model_9)
qqnorm(resid(Model_9))
qqline(resid(Model_9))
# Interaction terms: Vowel8 and Preceding.POA and Following.POA
Model_2 <- lmer(F1~Age+Decade+Gender+Vowel8*Preceding.POA*Following.POA + (1|Speaker)+(1|Word))
plot(Model_2)
# Interaction of Decade, Age and Gender
Model_3 <- lmer(F1~Vowel8*Preceding.POA*Following.POA +Gender*Decade*Age + (1|Speaker)+(1|Word))
plot(Model_3)
# Interaction with Vowel8 and Preceding.POA and Following.POA and
# Decade, Age and Gender
Model_4 <- lmer(F1~ (1|Speaker)+(1|Word))
plot(Model_4)
qqnorm(resid(Model_4))
qqline(resid(Model_4))

#Interaction term of Gender and Vowel
Model_5 <- lmer(F1~Age+Gender+Vowel8+Preceding.POA+Following.POA +Gender*Vowel8 + (1|Speaker)+(1|Word))

#Interaction Decade*Age + Decage*Gender + Age*Gender
Model_6 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Decade*Age + Decade*Gender + Age*Gender + (1|Speaker)+(1|Word))

#Interaction Vowel8*Preceding.POA + Vowel8*Following.POA 
#+ Preceding.POA*Following.POA
Model_7 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Vowel8*Preceding.POA + Vowel8*Following.POA + Preceding.POA*Following.POA + (1|Speaker)+(1|Word))

Model_8 <- lmer(F1~(Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA)^6  + (1|Speaker)+(1|Word))


summary(Model_2)

anova(Model_2)
#Model 1
residual_4 <- resid(Model_2)
qqnorm(residual_4)
qqline(residual_4)   
hist(residual_4)
df_res_1 <- data.frame(residual_4)
normaltest <- ggplot(df_res_1, aes(x = residual_4))
normaltest+geom_density()
#Model 2
residual_2 <- resid(Model_2)
qqnorm(residual_2)
qqline(residual_2)   
hist(residual_2)
df_res_2 <- data.frame(residual_2)
normaltest <- ggplot(df_res_2, aes(x = residual_2))
normaltest+geom_density()
#Model 3
residual_3 <- resid(Model_3)
qqnorm(residual_3)
qqline(residual_3)   
hist(residual_3)
df_res_3 <- data.frame(residual_3)
normaltest <- ggplot(df_res_3, aes(x = residual_3))
normaltest+geom_density()
#Model 4
residual_4 <- resid(Model_4)
qqnorm(residual_4)
qqline(residual_4)   
hist(residual_4)
df_res_4 <- data.frame(residual_4)
normaltest <- ggplot(df_res_4, aes(x = residual_4))
normaltest+geom_density()
#Model 5
residual_5 <- resid(Model_5)
qqnorm(residual_5)
qqline(residual_5)   
hist(residual_5)
df_res_5 <- data.frame(residual_5)
normaltest <- ggplot(df_res_5, aes(x = residual_5))
normaltest+geom_density()



changes_Sound %>% group_by(Gender, Decade) %>% summarise(Count = n())
