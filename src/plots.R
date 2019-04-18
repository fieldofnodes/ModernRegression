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



#Jitter words and frequencuy
vowelPOA <- ggplot(changes_Sound, aes(x = Word, y = F1, color = F1)) + geom_jitter(width = .05, height = .05, stroke = TRUE)+theme(axis.text.x = element_blank())
ggsave("fig/FrequencyWord.pdf", plot = vowelPOA, dpi = "retina")


#Boxplot of each speaker
voweltest <- changes_Sound %>% select(Speaker, F1) 

plott <- ggplot(voweltest, aes(x = Speaker, y=F1)) + geom_jitter(aes(color = F1))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Frequency per Speaker")+scale_color_continuous(name = "Formant \nFirst \nMean ")
ggsave("fig/FrequencySpeaker.pdf", plot = plott, dpi = "retina")
plottt <- ggplot(changes_Sound, aes(x = Decade, y=F1)) + geom_jitter(aes(color = F1))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Frequency per Vowel")+scale_color_continuous(name = "Formant \nFirst \nMean ")+ facet_grid(.~Vowel8)
ggsave("fig/FrequencyVowel.pdf", plot = plottt, dpi = "retina")



