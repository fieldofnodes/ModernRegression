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

AgeGender <-  changes_Sound %>% group_by(Age) %>% summarise(Average = mean(F1), Variance = (sd(F1)^2))
write.csv(AgeDecadeGender, file = "data/Mean_Age_Gender_Decade.csv")

test <- changes_Sound %>% group_by(Following.POA, Preceding.POA) %>% summarise(Average = mean(F1), Variance = (sd(F1)^2))
ppppp <- ggplot(test, aes(x = Following.POA, y = Preceding.POA, fill = Average )) + geom_raster()
ggsave("fig/RasterFollowPrecedingPOA.png", plot = ppppp)



Model_1 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + (1|Speaker)+(1|Word))

Model_2 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Gender:Decade + (1|Speaker)+(1|Word))
Model_3 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Gender:Age + (1|Speaker)+(1|Word))
Model_4 <- lmer(F1~(Age+Decade+Gender)^3+Vowel8+Preceding.POA+Following.POA + (1|Speaker)+(1|Word))


Model_5 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Vowel8:Preceding.POA + (1|Speaker)+(1|Word))
Model_6 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Vowel8:Preceding.POA:Following.POA + (1|Speaker)+(1|Word))
Model_7 <- lmer(F1~(Age+Decade+Gender)^3+(Vowel8+Preceding.POA+Following.POA)^3 + (1|Speaker)+(1|Word))
Model_8 <- lmer(F1~(Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA)^6 + (1|Speaker)+(1|Word))

anova(Model_1,Model_2,Model_3,Model_4,Model_5,Model_6,Model_7,Model_8)

m1 <- data.frame(resid(Model_1),fitted(Model_1)) %>% mutate(Model = "Model 01") %>% rename(Residuals = resid.Model_1., Fitted = fitted.Model_1.)
m2 <- data.frame(resid(Model_2),fitted(Model_2)) %>% mutate(Model = "Model 02") %>% rename(Residuals = resid.Model_2., Fitted = fitted.Model_2.)
m3 <- data.frame(resid(Model_3),fitted(Model_3)) %>% mutate(Model = "Model 03") %>% rename(Residuals = resid.Model_3., Fitted = fitted.Model_3.)
m4 <- data.frame(resid(Model_4),fitted(Model_4)) %>% mutate(Model = "Model 04") %>% rename(Residuals = resid.Model_4., Fitted = fitted.Model_4.)
m5 <- data.frame(resid(Model_5),fitted(Model_5)) %>% mutate(Model = "Model 05") %>% rename(Residuals = resid.Model_5., Fitted = fitted.Model_5.)
m6 <- data.frame(resid(Model_6),fitted(Model_6)) %>% mutate(Model = "Model 06") %>% rename(Residuals = resid.Model_6., Fitted = fitted.Model_6.)
m7 <- data.frame(resid(Model_7),fitted(Model_7)) %>% mutate(Model = "Model 07") %>% rename(Residuals = resid.Model_7., Fitted = fitted.Model_7.)
m8 <- data.frame(resid(Model_8),fitted(Model_8)) %>% mutate(Model = "Model 08") %>% rename(Residuals = resid.Model_8., Fitted = fitted.Model_8.)
m <- dplyr::bind_rows(m1,m2,m3,m4,m5,m6,m7,m8)


density1 <- ggplot(m, aes(x = Residuals)) + geom_density(aes(color = Model)) + labs(title = "Density of residuals for all eight models") + theme(axis.title.y = element_blank(),axis.title.x = element_blank())
qqplots <- ggplot(m, aes(sample = Residuals, color = Model)) + stat_qq() + stat_qq_line() + labs(title = "Quantile Quanitile plots", x = "Theoretical", y = "Samples from Models") 
FitRes <- ggplot(m,aes(x = Fitted, y = Residuals)) + geom_point(aes(color = Model))+facet_wrap(~Model) +labs(title = "Fitted against Residuals")
ggsave("fig/ResidualPlots.pdf", plot = density1, dpi = "retina")
ggsave("fig/qqplots.pdf", plot = qqplots, dpi = "retina")
ggsave("fig/FitRes.pdf", plot = FitRes, dpi = "retina")






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
