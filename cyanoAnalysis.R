library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(lme4)
library(akima)
library(GGally)
library(gam)
library(mgcv)
library(gamair)
library(mgcViz)
library(plotGAM)


cyano <- read.table(file = "data/cyano.dat")
attach(cyano)
cyano <- cyano %>% 
  rename(Altitude = lAltitude) %>%
  rename(Depth.mean = lMeanDepth) %>%
  rename(Alkalinity = lalkalinity) %>%
  rename(Color = lcolour) %>%
  rename(Phosphorous = logtp) %>%
  rename(Temperature.mean = meantemp)
cyano22 <- rep(cyano$PA, times = 6)
cyano11 <- cyano %>% select(2:7)
cyano4 <- gather(cyano11, "Type", "Value")
cyano1 <- cyano4 %>% mutate(PA = cyano22)
cyano1$PA <- as.factor(cyano1$PA)                      


#Question (a)
attach(cyano1)
boxes <- cyano1 %>% ggplot(aes(Type, Value,fill = PA))+geom_boxplot(notch = TRUE,outlier.colour="red", outlier.shape=8, outlier.size=3)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()+labs(title = "Box plots of all numerical variables per group", fill = "Presence or \nAbsence of \nCyanobacteria", caption = "*Red stars are outliers.")
ggsave("fig/boxplot.pdf", plot = boxes, dpi = "retina")

#GGpairs
pars <- list(geom_point(alpha=0.8, color="blue"),              
             geom_smooth(method="lm", color="red", lwd=1.1))
cyano_b <- cyano 
cyano_b$PA = as.factor(cyano_b$PA)  
p <- ggpairs(data=cyano_b,
        mapping = ggplot2::aes(color = PA),
        upper = list(continuous = wrap("density", alpha = 0.5), combo = "box"),
        lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot", alpha = 0.4), method = "lm")),
        diag = list(continuous = wrap("densityDiag")),
        title = "Pairs relationships"
)
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("red", "blue", "green", "yellow", "black")) +
      scale_color_manual(values=c("red", "blue", "green", "yellow", "black"))  
  }
}

p
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

p <- ggpairs(data=cyano_b,
        mapping = ggplot2::aes(color = PA),
        lower = list(continuous = my_fn),
        title = "Relationships between data variables per PA level")
p
ggsave("fig/pairsplots.pdf",plot = p, dpi = "retina")

#paret c

#Modelling GAM
b <- gam(PA~s(Altitude, bs = "cr")+s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr")+ s(Phosphorous, bs = "cr") + s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b <- getViz(b)
pl <- plot(b, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl, pages = 1)
qq(b, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)


summary(b)
anova(b)



b.d <- round(summary(b)$edf)+1 ## get edf per smooth
b.d <- pmax(b.d,3) # can't have basis dimension less than 3!


#Modelling GAM different modela
b1 <- gam(PA~s(Altitude, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr")+ s(Phosphorous, bs = "cr") + s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b1 <- getViz(b1)
pl1 <- plot(b1, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl1, pages = 1)
qq(b1, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b1, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b1, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)



b2 <- gam(PA~s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr")+ s(Phosphorous, bs = "cr") + s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b2 <- getViz(b2)
pl2 <- plot(b2, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl2, pages = 1)
qq(b2, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b2, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b2, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)


b3 <- gam(PA~s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr")+ s(Phosphorous, bs = "cr"), family = "binomial", data = cyano)
b3 <- getViz(b3)
pl3 <- plot(b3, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl3, pages = 1)
qq(b3, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b3, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b3, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)

b4 <- gam(PA~s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr"), family = "binomial", data = cyano)
b4 <- getViz(b4)
pl4 <- plot(b4, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl4, pages = 1)
qq(b4, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b4, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b4, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)


#Modelling GAM
b5 <- gam(PA~s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr")+ s(Phosphorous, bs = "cr") + s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b5 <- getViz(b5)
pl5 <- plot(b5, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl5, pages = 1)
qq(b5, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b5, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b5, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)



#Modelling GAM
b6 <- gam(PA~s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr")+ s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b6 <- getViz(b6)
pl6 <- plot(b6, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl6, pages = 1)
qq(b6, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b6, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b6, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)


#Modelling GAM
b7 <- gam(PA~s(Altitude, bs = "cr")+s(Phosphorous, bs = "cr") + s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b7 <- getViz(b7)
pl7 <- plot(b7, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl7, pages = 1)
qq(b7, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b7, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b7, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)



#Modelling GAM
b8 <- gam(PA~s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+s(Color, bs = "cr")+ s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b8 <- getViz(b8)
pl8 <- plot(b8, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl8, pages = 1)
qq(b8, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b8, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b8, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)


#Modelling GAM
b9 <- gam(PA~s(Depth.mean, bs = "cr")+s(Alkalinity, bs = "cr")+ s(Temperature.mean, bs = "cr"), family = "binomial", data = cyano)
b9 <- getViz(b9)
pl9 <- plot(b9, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl9, pages = 1)
qq(b9, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b9, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b9, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)

#Modelling GAM
b10 <- gam(PA~s(Alkalinity, bs = "cr")+s(Color, bs = "cr"), family = "binomial", data = cyano)
b10 <- getViz(b10)
pl10 <- plot(b10, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + labs(title = NULL) 
print(pl10, pages = 1)
qq(b10, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
qq(b10, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))
qq(b10, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, a.replin = list(alpha = 0.1), discrete = TRUE)


AIC(b,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)


summary(b10)
b.d <- round(summary(b8)$edf)+1 ## get edf per smooth
b.d <- pmax(b.d,3) # can't have basis dimension less than 3!


