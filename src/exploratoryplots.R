library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(lme4)

#Reading in the data
changes_Sound <- load(file = "data/sounds.rdata")
changes_Sound <- data.frame(data)
#This data is for the water quality of lakes in the UK
cyano_data <- read.table(file = "data/cyano.dat", header = TRUE)

#Plots to examine
names(changes_Sound)

changes_Sound.mean <- changes_Sound %>% group_by(Vowel8, Gender, Decade, Preceding.POA, Following.POA) %>% summarize(y = mean(F1))

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

summary(changes_Sound)

changes_Sound %>% group_by(Decade,Preceding.POA, Vowel8, Following.POA,F1) %>% filter(Preceding.POA == Following.POA)
changes_Sound %>% summarise(Decade, Gender, Age, avg = mean(F1))


attach(changes_Sound)
boxplot(F1~Preceding.POA*Vowel8+Gender*Age)

testi <- lmer(F1~ Decade*Vowel8 + (1|Speaker), data = changes_Sound)
summary(testi)
plot(testi)
qqnorm(resid(testi))
qqline(resid(testi))   
plot(F1~ Gender*Age)

