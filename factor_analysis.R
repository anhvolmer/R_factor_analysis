install.packages("psych")
install.packages("car")
install.packages("reshape2")
install.packages("tidyverse")
library(psych)
library(car)
library(ggplot2)
library(reshape2)
library(tidyverse)

setwd("D:\Projects\TargetSmart\National Models\Models\SOURCE_DATA")
sharesvals_mp <- read.csv("civiqs_targetsmart_moral_pillars_2023_01_final.csv")
# sharesvals_b <- read.csv("NEPA_06292022_SHARESVALS_SB.csv")


full_form <- as.formula(paste("Authority ~ ",paste(variable.names(sharesvals_mp[,8:12]),collapse="+")))
full_r2 <- summary(lm(full_form,data=sharesvals_mp))$r.squared

xtable_mp_auth <- sharesvals_mp[,8:12]
xtable_mp_pur <- sharesvals_mp[,13:18]
xtable_mp_loy <- sharesvals_mp[,19:24]
xtable_mp_care <- sharesvals_mp[,25:31]
xtable_mp_equal <- sharesvals_mp[,32:34]
xtable_mp_equit <- sharesvals_mp[,35:42]


outtab <- c()
for (i in 1:5){
  reduced_form <- as.formula(paste("Authority ~ ",paste(variable.names(xtable_mp_auth[,-i]),collapse="+")))
  r2 <- summary(lm(reduced_form,data=sharesvals_mp))$r.squared
  outtab <- rbind(outtab,c(variable.names(xtable_mp[i]),round(full_r2 - r2,digits=6)))
}
outtab <- as.data.frame(outtab)
outtab[,2] <- as.numeric(outtab[,2])

auth_fa <- fa(r=xtable_mp_auth, nfactors = 2, rotate="varimax",fm="pa")
loadings <- loadings(auth_fa)[,1:2]
#loadings <- loadings[order(loadings[,1]),]
loadings.m <- melt(loadings[,1:2], id="Test",measure=c("PA1","PA2"),
                   variable.name="Factor",value.name="Loading")
loadings.m$Var2 <- as.character(loadings.m$Var2)
loadings.m$Var2[loadings.m$Var2=="PA1"] <- "Authority Factor 1"
loadings.m$Var2[loadings.m$Var2=="PA2"] <- "Authority Factor 2"



ggplot(loadings.m, aes(Var1, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Var2, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10)

purity_fa <- fa(r=xtable_mp_pur, nfactors = 2, rotate="varimax",fm="pa")
loadings <- loadings(purity_fa)[,1:2]
#loadings <- loadings[order(loadings[,1]),]
loadings.m <- melt(loadings[,1:2], id="Test",measure=c("PA1","PA2"),
                   variable.name="Factor",value.name="Loading")
loadings.m$Var2 <- as.character(loadings.m$Var2)
loadings.m$Var2[loadings.m$Var2=="PA1"] <- "Purity Factor 1"
loadings.m$Var2[loadings.m$Var2=="PA2"] <- "Purity Factor 2"


ggplot(loadings.m, aes(Var1, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Var2, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10)


care_fa <- fa(r=xtable_mp_care, nfactors = 2, rotate="varimax",fm="pa")
loadings <- loadings(care_fa)[,1:2]
#loadings <- loadings[order(loadings[,1]),]
loadings.m <- melt(loadings[,1:2], id="Test",measure=c("PA1","PA2"),
                   variable.name="Factor",value.name="Loading")
loadings.m$Var2 <- as.character(loadings.m$Var2)
loadings.m$Var2[loadings.m$Var2=="PA1"] <- "Care Factor 1"
loadings.m$Var2[loadings.m$Var2=="PA2"] <- "Care Factor 2"


ggplot(loadings.m, aes(Var1, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Var2, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10)

loyalty_fa <- fa(r=xtable_mp_loy, nfactors = 2, rotate="varimax",fm="pa")
loadings <- loadings(loyalty_fa)[,1:2]
#loadings <- loadings[order(loadings[,1]),]
loadings.m <- melt(loadings[,1:2], id="Test",measure=c("PA1","PA2"),
                   variable.name="Factor",value.name="Loading")
loadings.m$Var2 <- as.character(loadings.m$Var2)
loadings.m$Var2[loadings.m$Var2=="PA1"] <- "Loyalty Factor 1"
loadings.m$Var2[loadings.m$Var2=="PA2"] <- "Loyalty Factor 2"


ggplot(loadings.m, aes(Var1, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Var2, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10)

equal_fa <- fa(r=xtable_mp_equal, nfactors = 1, rotate="varimax",fm="pa")
loadings <- loadings(equal_fa)[,1]
#loadings <- loadings[order(loadings[,1]),]
loadings.m <- melt(loadings, id="Test",measure=c("PA1","PA2"),
                   variable.name="Factor",value.name="Loading")
loadings.m$Var1 <- rownames(loadings.m)
loadings.m$Var2 <- "Equality Factor"


ggplot(loadings.m, aes(Var1, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Var2, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10)

equit_fa <- fa(r=xtable_mp_equit, nfactors = 2, rotate="varimax",fm="pa")
loadings <- loadings(equit_fa)[,1:2]
#loadings <- loadings[order(loadings[,1]),]
loadings.m <- melt(loadings[,1:2], id="Test",measure=c("PA1","PA2"),
                   variable.name="Factor",value.name="Loading")
loadings.m$Var2 <- as.character(loadings.m$Var2)
loadings.m$Var2[loadings.m$Var2=="PA1"] <- "Equity Factor 1"
loadings.m$Var2[loadings.m$Var2=="PA2"] <- "Equity Factor 2"


ggplot(loadings.m, aes(Var1, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Var2, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10)
.libPaths()
