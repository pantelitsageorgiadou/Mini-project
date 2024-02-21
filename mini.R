rm(list=ls())
getwd()
setwd("C:/Users/admin/Downloads")
install.packages("emmeans")
install.packages("effects")
library(effects)
library(emmeans)
library(ggplot2)
hyenas<- read.csv("hyena.csv")
str(hyenas)
a<- hyenas$Season
b<- hyenas$Wild.boar
hyenas1 <-cbind(a, b)
hyenas1<-data.frame(hyenas1)
ggplot(hyenas1, aes( x=a, y=b ,fill=a))+
  geom_col()+
  labs(x= "Season", y="Wild boar", fill="Season")+
  theme_classic()
m1 <- glm( formula= as.numeric(b)  ~ a,  data=hyenas1, family="binomial") 
summary(m1)
par(mfrow=c(2,2)) #partitioning the plot window into a 2X2
plot(m1)
m1ph <- emmeans(m1, "a", data=hyenas1)
pairs(m1ph, adjust="holm")
plot(m1ph)
