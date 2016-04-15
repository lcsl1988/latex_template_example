##Analise da Base de Dados da Organização Mundial de Saude - Países da ONU##

rm(list=ls())
.libPaths("F:/Softwares/Programas/R/R-3.0.0/library")
.libPaths()
require(MASS)

##Análises 1995##

matriz1995<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ1995.csv",header=TRUE,sep=",")
matriz1995
df1995<-data.frame(matriz1995)
subset1995<-subset(df1995,select=COLESTEROL:DESPESA)
subset1995
plot(subset1995$DESPESA)
hist(subset1995$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset1995,use="complete.obs",method="pearson")

#Modelo Linear:
lm1995<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset1995)
summary(lm1995)
plot(subset1995$DESPESA)
abline(lm1995)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss1995<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset1995,family=poisson(link="log"))
summary(poiss1995)
anova(poiss1995)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb1995<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset1995)
summary(nb1995)
anova(nb1995)

##Fim 1995##

##Análises 1996##

matriz1996<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ1996.csv",header=TRUE,sep=",")
matriz1996
df1996<-data.frame(matriz1996)
subset1996<-subset(df1996,select=COLESTEROL:DESPESA)
subset1996
plot(subset1996$DESPESA)
hist(subset1996$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset1996,use="complete.obs",method="pearson")

#Modelo Linear:
lm1996<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset1996)
summary(lm1996)
plot(subset1996$DESPESA)
abline(lm1996)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss1996<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset1996,family=poisson(link="log"))
summary(poiss1996)
anova(poiss1996)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb1996<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset1996)
summary(nb1996)
anova(nb1996)

##Fim 1996##

##Análises 1997##

matriz1997<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ1997.csv",header=TRUE,sep=",")
matriz1997
df1997<-data.frame(matriz1997)
subset1997<-subset(df1997,select=COLESTEROL:DESPESA)
subset1997
plot(subset1997$DESPESA)
hist(subset1997$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset1997,use="complete.obs",method="pearson")

#Modelo Linear:
lm1997<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset1997)
summary(lm1997)
plot(subset1997$DESPESA)
abline(lm1997)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss1997<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset1997,family=poisson(link="log"))
summary(poiss1997)
anova(poiss1997)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb1997<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset1997)
summary(nb1997)
anova(nb1997)

##Fim 1997##

##Análises 1998##

matriz1998<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ1998.csv",header=TRUE,sep=",")
matriz1998
df1998<-data.frame(matriz1998)
subset1998<-subset(df1998,select=COLESTEROL:DESPESA)
subset1998
plot(subset1998$DESPESA)
hist(subset1998$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset1998,use="complete.obs",method="pearson")

#Modelo Linear:
lm1998<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset1998)
summary(lm1998)
plot(subset1998$DESPESA)
abline(lm1998)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss1998<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset1998,family=poisson(link="log"))
summary(poiss1998)
anova(poiss1998)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb1998<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset1998)
summary(nb1998)
anova(nb1998)

##Fim 1998##

##Análises 1999##

matriz1999<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ1999.csv",header=TRUE,sep=",")
matriz1999
df1999<-data.frame(matriz1999)
subset1999<-subset(df1999,select=COLESTEROL:DESPESA)
subset1999
plot(subset1999$DESPESA)
hist(subset1999$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset1999,use="complete.obs",method="pearson")

#Modelo Linear:
lm1999<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset1999)
summary(lm1999)
plot(subset1999$DESPESA)
abline(lm1999)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss1999<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset1999,family=poisson(link="log"))
summary(poiss1999)
anova(poiss1999)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb1999<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset1999)
summary(nb1999)
anova(nb1999)

##Fim 1999##

##Análises 2000##

matriz2000<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2000.csv",header=TRUE,sep=",")
matriz2000
df2000<-data.frame(matriz2000)
subset2000<-subset(df2000,select=COLESTEROL:DESPESA)
subset2000
plot(subset2000$DESPESA)
hist(subset2000$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2000,use="complete.obs",method="pearson")

#Modelo Linear:
lm2000<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2000)
summary(lm2000)
plot(subset2000$DESPESA)
abline(lm2000)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2000<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2000,family=poisson(link="log"))
summary(poiss2000)
anova(poiss2000)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2000<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2000)
summary(nb2000)
anova(nb2000)

##Fim 2000##

##Análises 2001##

matriz2001<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2001.csv",header=TRUE,sep=",")
matriz2001
df2001<-data.frame(matriz2001)
subset2001<-subset(df2001,select=COLESTEROL:DESPESA)
subset2001
plot(subset2001$DESPESA)
hist(subset2001$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2001,use="complete.obs",method="pearson")

#Modelo Linear:
lm2001<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2001)
summary(lm2001)
plot(subset2001$DESPESA)
abline(lm2001)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2001<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2001,family=poisson(link="log"))
summary(poiss2001)
anova(poiss2001)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2001<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2001)
summary(nb2001)
anova(nb2001)

##Fim 2001##

##Análises 2002##

matriz2002<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2002.csv",header=TRUE,sep=",")
matriz2002
df2002<-data.frame(matriz2002)
subset2002<-subset(df2002,select=COLESTEROL:DESPESA)
subset2002
plot(subset2002$DESPESA)
hist(subset2002$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2002,use="complete.obs",method="pearson")

#Modelo Linear:
lm2002<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2002)
summary(lm2002)
plot(subset2002$DESPESA)
abline(lm2002)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2002<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2002,family=poisson(link="log"))
summary(poiss2002)
anova(poiss2002)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2002<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2002)
summary(nb2002)
anova(nb2002)

##Fim 2002##

##Análises 2003##

matriz2003<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2003.csv",header=TRUE,sep=",")
matriz2003
df2003<-data.frame(matriz2003)
subset2003<-subset(df2003,select=COLESTEROL:DESPESA)
subset2003
plot(subset2003$DESPESA)
hist(subset2003$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2003,use="complete.obs",method="pearson")

#Modelo Linear:
lm2003<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2003)
summary(lm2003)
plot(subset2003$DESPESA)
abline(lm2003)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2003<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2003,family=poisson(link="log"))
summary(poiss2003)
anova(poiss2003)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2003<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2003)
summary(nb2003)
anova(nb2003)

##Fim 2003##

##Análises 2004##

matriz2004<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2004.csv",header=TRUE,sep=",")
matriz2004
df2004<-data.frame(matriz2004)
subset2004<-subset(df2004,select=COLESTEROL:DESPESA)
subset2004
plot(subset2004$DESPESA)
hist(subset2004$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2004,use="complete.obs",method="pearson")

#Modelo Linear:
lm2004<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2004)
summary(lm2004)
plot(subset2004$DESPESA)
abline(lm2004)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2004<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2004,family=poisson(link="log"))
summary(poiss2004)
anova(poiss2004)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2004<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2004)
summary(nb2004)
anova(nb2004)

##Fim 2004##

##Análises 2005##

matriz2005<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2005.csv",header=TRUE,sep=",")
matriz2005
df2005<-data.frame(matriz2005)
subset2005<-subset(df2005,select=COLESTEROL:DESPESA)
subset2005
plot(subset2005$DESPESA)
hist(subset2005$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2005,use="complete.obs",method="pearson")

#Modelo Linear:
lm2005<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2005)
summary(lm2005)
plot(subset2005$DESPESA)
abline(lm2005)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2005<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2005,family=poisson(link="log"))
summary(poiss2005)
anova(poiss2005)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2005<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2005)
summary(nb2005)
anova(nb2005)

##Fim 2005##

##Análises 2006##

matriz2006<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2006.csv",header=TRUE,sep=",")
matriz2006
df2006<-data.frame(matriz2006)
subset2006<-subset(df2006,select=COLESTEROL:DESPESA)
subset2006
plot(subset2006$DESPESA)
hist(subset2006$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2006,use="complete.obs",method="pearson")

#Modelo Linear:
lm2006<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2006)
summary(lm2006)
plot(subset2006$DESPESA)
abline(lm2006)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2006<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2006,family=poisson(link="log"))
summary(poiss2006)
anova(poiss2006)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2006<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2006)
summary(nb2006)
anova(nb2006)

##Fim 2006##

##Análises 2007##

matriz2007<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2007.csv",header=TRUE,sep=",")
matriz2007
df2007<-data.frame(matriz2007)
subset2007<-subset(df2007,select=COLESTEROL:DESPESA)
subset2007
plot(subset2007$DESPESA)
hist(subset2007$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2007,use="complete.obs",method="pearson")

#Modelo Linear:
lm2007<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2007)
summary(lm2007)
plot(subset2007$DESPESA)
abline(lm2007)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2007<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2007,family=poisson(link="log"))
summary(poiss2007)
anova(poiss2007)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2007<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2007)
summary(nb2007)
anova(nb2007)

##Fim 2007##

##Análises 2008##

matriz2008<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2008.csv",header=TRUE,sep=",")
matriz2008
df2008<-data.frame(matriz2008)
subset2008<-subset(df2008,select=COLESTEROL:DESPESA)
subset2008
plot(subset2008$DESPESA)
hist(subset2008$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2008,use="complete.obs",method="pearson")

#Modelo Linear:
lm2008<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2008)
summary(lm2008)
plot(subset2008$DESPESA)
abline(lm2008)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2008<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2008,family=poisson(link="log"))
summary(poiss2008)
anova(poiss2008)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2008<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2008)
summary(nb2008)
anova(nb2008)

##Fim 2008##

##Análises 2009##

matriz2009<-read.csv("F:/Graduacao/Estatística/8º Período/Projeto Final/Saude Mundial/Saude Mundial/Matrizes/MATRIZ2009.csv",header=TRUE,sep=",")
matriz2009
df2009<-data.frame(matriz2009)
subset2009<-subset(df2009,select=COLESTEROL:DESPESA)
subset2009
plot(subset2009$DESPESA)
hist(subset2009$DESPESA)

#Matriz de Correlação (Pearson)
cor(subset2009,use="complete.obs",method="pearson")

#Modelo Linear:
lm2009<-lm(DESPESA~COLESTEROL+GLICOSE+LN_PIB+PRESSAO+IMC,data=subset2009)
summary(lm2009)
plot(subset2009$DESPESA)
abline(lm2009)

#Modelo Linear Generalizado (Poisson, Link: Log):
poiss2009<-glm(DESPESA~LN_PIB+COLESTEROL,data=subset2009,family=poisson(link="log"))
summary(poiss2009)
anova(poiss2009)

#Modelo Linear Generalizado (Binomial Negativa, Link: Log):
nb2009<-glm.nb(DESPESA~LN_PIB+COLESTEROL,data=subset2009)
summary(nb2009)
anova(nb2009)

##Fim 2009##

