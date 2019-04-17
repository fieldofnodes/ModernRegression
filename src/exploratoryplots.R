library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(lme4)
library(akima)

#Reading in the data
changes_Sound <- load(file = "data/sounds.rdata")
changes_Sound <- data.frame(data)
#This data is for the water quality of lakes in the UK
cyano_data <- read.table(file = "data/cyano.dat", header = TRUE)

str(changes_Sound)

#Plots to examine
names(changes_Sound)

changes_Sound %>% group_by(Decade,Age, Gender) %>% summarize(Count = n()) %>% arrange(desc(Count))

#Box plots for different vowels = 8 different vowles
p <- ggplot(changes_Sound, aes(x = Vowel8,y = F1, fill = Age)) + 
  geom_boxplot(aes(color = Decade),notch = FALSE, outlier.alpha = 0.1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(rows = vars(Preceding.POA), cols = vars(Gender)) + 
  labs(title = "First formant frequency means faceted by gender and Preceding PoA, grouped by decade and then age", x = "Eight different vowels", y = "First formant   frequency (mean)") + scale_fill_brewer(palette = "Blues")

q <- ggplot(changes_Sound, aes(x = Vowel8,y = F1, fill = Age)) + 
  geom_boxplot(aes(color = Decade),notch = FALSE, outlier.alpha = 0.1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(rows = vars(Following.POA), cols = vars(Gender)) + 
  labs(title = "First formant frequency means faceted by gender and Following PoA, grouped by decade and then age", x = "Eight different vowels", y = "First formant   frequency (mean)") + scale_fill_brewer(palette = "Blues")
ggsave("fig/PrecedingPOABoxPlotF1DecadeGender.pdf", plot = p, dpi = "retina")
ggsave("fig/FollowingPOABoxPlotF1DecadeGender.pdf", plot = q, dpi = "retina")


#Violin plots for frequencies, age, gender, decade
pp <- ggplot(changes_Sound,aes(x = Gender,y = F1, fill = Age))
pp + 
  geom_violin(scale = "area") +
  facet_grid(Vowel8 ~ Decade) +  
  theme(legend.position = c(0.11, -0.09), legend.direction = "horizontal")+
  scale_fill_discrete(name = "Age",labels = c("10-17","67-90")) + 
  labs(title ="Eight Vowels and their corresponding frequencies", y = "Formant First", caption = "Violin plots suggest changes in frequencies based on Gender, Decade and Age \nIt seems that there is a similar spread of frequencies accross the factors.")

#Jitter plot for Vowels and Preceding
ppq <- ggplot(changes_Sound,aes(x = Vowel8,y= Preceding.POA)) + geom_jitter(aes(color = F1))+facet_grid(Decade~.)
#Jitter plot for Vowels and Following
ppg <- ggplot(changes_Sound,aes(x = Vowel8,y= Following.POA))+ geom_jitter(aes(color = F1))+facet_grid(Decade~.)
POAplot1 <- ggarrange(ppq,ppg)
POAplot <- annotate_figure(POAplot1,
                top = text_grob("Place of Articulation: Preceding and Following", color = "Blue", face = "bold", size = 14),
                bottom = text_grob("Frequencies are \nscaled in blues", color = "blue", hjust = 1, x = 0.9, face = "italic", size = 10),
                fig.lab = "Jitter Figure", fig.lab.face = "bold")
ggsave("fig/POAVowelDecade.pdf", plot = POAplot, dpi = "retina")

summary(changes_Sound)


xy <-  changes_Sound %>% group_by(F1,Speaker,Decade,Age,Gender,Vowel8,Following.POA,Preceding.POA) %>% summarise(Ave = mean(F1), Var = (sd(F1)^2))
xx <- xy  %>% group_by(Vowel8)%>% summarise(VowelCount = n())
g <- ggplot(xy, aes(x = Vowel8, y = Ave))
g+stat_density()

tidyr::spread(xx, Vowel8, count)
class(xy)
names(xy)
plot(xy[,2])

a <- ggplot(xy)
a+geom_density(aes(x= Var))

attach(changes_Sound)
plot(F1~Speaker)

testi <- lmer(F1~ Decade+Age+Gender+Vowel8+Following.POA+Preceding.POA + (1+Word|Speaker), data = changes_Sound)
summary(testi)
plot(testi)
qqnorm(resid(testi))
qqline(resid(testi))   

test <- lm(F1~Decade)
summary(test)
plot(test)
qqnorm(resid(test))
qqline(resid(test))   

#No interaction terms
Model_1 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + (1|Speaker)+(1|Word))
# Interaction terms: Vowel8 and Preceding.POA and Following.POA
Model_2 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA +Vowel8*Preceding.POA*Following.POA + (1|Speaker)+(1|Word))

# Interaction terms: Vowel8 and Gender
Model_3 <- lmer(F1~Age+Gender+Vowel8+Preceding.POA+Following.POA +Vowel8*Gender + (1|Speaker)+(1|Word))

#Interaction Decade*Age + Decage*Gender + Age*Gender
Model_4 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Decade*Age + Decade*Gender + Age*Gender + (1|Speaker)+(1|Word))

Model_5 <- lmer(F1~Age+Decade+Gender+Vowel8+Preceding.POA+Following.POA + Vowel8*Preceding.POA  Vowel8*Following+  (1|Speaker)+(1|Word))

anova(Model_1,Model_2,Model_3,Model_4,Model_5)

plot(Model_6)
#Model 1
residual_1 <- resid(Model_1)
qqnorm(residual_1)
qqline(residual_1)   
hist(residual_1)
df_res_1 <- data.frame(residual_1)
normaltest <- ggplot(df_res_1, aes(x = residual_1))
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
