#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things                                      ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GSERM - St. Gallen (2024)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day Three: Panel Dynamics.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("RCurl","readr","haven","psych","sandwich","car","ggplot2",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "plm","tibble","pscl","naniar","ExPanDaR","stargazer","prais",
     "nlme","tseries","pcse","panelView","performance","pgmm",
     "dynpanel","pdynmc","OrthoPanels","xtsum","modelsummary",
     "marginaleffects","tidyverse","feisr")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that code 7-8 times to get everything installed and 
# loaded properly.
#
# The (useful) package "dotwhisker" is currently (June 2024) a bit broken,
# and so has to be loaded separately:
#
# url<-"https://cran.r-project.org/src/contrib/Archive/dotwhisker/dotwhisker_0.8.1.tar.gz"
# pkF<- "dotwhisker_0.8.1.tar.gz"
# download.file(url=url,destfile=pkF)
# 
# Then unzip the file (which will be saved to your local directory) and
# run:
# 
# install.packages(pkgs=pkF,type="source",repos=NULL)
# 
# Note that this may or may not actually work on any given day...
# 
# NOTE also:
# 
# library(panelAR) # This package is currently (June 2024) archived; see
                   # https://stackoverflow.com/questions/71664861/r-package-panelar-not-available-to-install
                   # for installation instructions. For example, this code
                   # will probably work:

download.file("https://cran.r-project.org/src/contrib/Archive/panelAR/panelAR_0.1.tar.gz",
              destfile="panelAR_0.1.tar.gz")
install.packages("panelAR_0.1.tar.gz", type= "source", repos= NULL)
library(panelAR)

# R options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Set working directory (uncomment and change as needed):
#
# setwd("~/Dropbox (Personal)/GSERM/Panel-2024/Notes and Slides")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some possibly-useful functions...

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_robust)

url_std <- "http://www.stat.columbia.edu/~gelman/standardize/standardize.R"
eval(parse(text = getURL(url_std, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_std)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Robust SEs mini-sim: ####

set.seed(3844469)
X <- rnorm(10)
Y <- 1 + X + rnorm(10)
df10 <- data.frame(ID=seq(1:10),X=X,Y=Y)
fit10 <- lm(Y~X,data=df10)
summary(fit10)
rob10 <- vcovHC(fit10,type="HC1")
sqrt(diag(rob10))

# "Clone" each observation 100 times:

df1K <- df10[rep(seq_len(nrow(df10)),each=100),]
df1K <- pdata.frame(df1K, index="ID")
fit1K <- lm(Y~X,data=df1K)
summary(fit1K)
summary(fit1K, cluster="ID")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GLS-ARMA models ####
#
# Pull the WDI data...

wdi<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2024/main/Data/WDI24a.csv")

# Add a "Cold War" variable:

wdi$PostColdWar <- with(wdi,ifelse(Year<1990,0,1))

# Add GDP Per Capita:

wdi$GDPPerCapita <- wdi$GDP / wdi$Population

# Keep a numeric year variable (for -panelAR-):

wdi$YearNumeric<-wdi$Year

# Summarize:

describe(wdi,fast=TRUE,ranges=FALSE,check=TRUE)

# summary(wdi)
#
# Make the data a panel dataframe:

WDI<-pdata.frame(wdi,index=c("ISO3","Year"))

xtsum(WDI,na.rm=TRUE)

# Summary statistics:

vars<-c("ISO3","Year","WomenBusLawIndex","PopGrowth","UrbanPopulation",
        "FertilityRate","GDPPerCapita","NaturalResourceRents","PostColdWar")
smol<-WDI[vars]
smol<-smol[complete.cases(smol),] # listwise deletion
smol$ISO3<-droplevels(smol$ISO3) # drop unused factor levels
smol$lnGDPPerCap<-log(smol$GDPPerCapita)
smol$GDPPerCapita<-NULL
describe(smol[,3:9],fast=TRUE)
# xtsum(smol,na.rm=TRUE)

# Make Year numeric:

smol$YearFactor<-smol$Year
smol$Year<-as.numeric(as.character(smol$Year))

# Standardize the variables, Gelman (2008)-style:

smol$PopGrowth<-smol$PopGrowth / sd(smol$PopGrowth,na.rm=TRUE)
smol$UrbanPopulation<-smol$UrbanPopulation / sd(smol$UrbanPopulation,na.rm=TRUE)
smol$FertilityRate<-smol$FertilityRate / sd(smol$FertilityRate,na.rm=TRUE)
smol$lnGDPPerCap<-smol$lnGDPPerCap / sd(smol$lnGDPPerCap,na.rm=TRUE)
smol$NaturalResourceRents<-smol$NaturalResourceRents / sd(smol$NaturalResourceRents,na.rm=TRUE)
smol$PostColdWar<-ifelse(smol$PostColdWar==0,-1,1)

describe(smol[,3:9],fast=TRUE)

# How much autocorrelation in those variables?
#
# First, our dependent variable, WBLI:

WI<-pdwtest(WomenBusLawIndex~1,data=smol)
WI
print(paste("Rho =",round(1 - (WI$statistic/2),3)))

PG<-pdwtest(PopGrowth~1,data=smol)
UP<-pdwtest(UrbanPopulation~1,data=smol)
FR<-pdwtest(FertilityRate~1,data=smol)
GDP<-pdwtest(lnGDPPerCap~1,data=smol)
NRR<-pdwtest(NaturalResourceRents~1,data=smol)
CW<-pdwtest(PostColdWar~1,data=smol)

rhos<-data.frame(Variable=c("Population Growth","Urban Population",
                            "Fertility Rate","GDP Per Capita",
                            "Natural Resource Rents","Post Cold War"),
                 Rho = c(1-(PG$statistic/2),1-(UP$statistic/2),
                         1-(FR$statistic/2),1-(GDP$statistic/2),
                         1-(NRR$statistic/2),1-(CW$statistic/2)))

stargazer(rhos,summary=FALSE,out="Rhos-24.tex",rownames=FALSE,
          title="WDI Data - Autocorrelation in the Predictors")


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Models:

# Pooled OLS:

OLS<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+NaturalResourceRents+
           PostColdWar,data=smol,model="pooling")
summary(OLS)

pdwtest(OLS)

# Residual plot:

OLSresids<-residuals(OLS)

pdf("OLSPanelResids.pdf",8,4)
par(mar=c(4,4,4,2))
par(mfrow=c(1,3))
plot(as.numeric(OLSresids), lag(as.numeric(OLSresids)),pch=20,
     main="Residuals vs. Lagged Residuals",ylab="OLS Residuals",
     xlab="Lagged OLS Residuals")
acf(OLSresids,main="ACF of OLS Residuals")
pacf(OLSresids,main="ACF of OLS Residuals")
dev.off()

# Unit-specific autocorrelation? Let's fit a regression to each
# country, then check for autocorrelation in each, and save the
# results:

Rhos<-NA
for(i in smol$ISO3) {
  foo<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                lnGDPPerCap+NaturalResourceRents+PostColdWar,
                model="pooling",data=smol[smol$ISO3==i,])
  r<-residuals(foo)
  R<-acf(r,plot=FALSE)
  Rhos<-c(Rhos,R$acf[2])
}

# Plot:

Rhos<-unique(Rhos)
medianRho<-round(median(Rhos,na.rm=TRUE),2)

pdf("RhosDensity.pdf",7,5)
par(mar=c(4,4,2,2))
plot(density(Rhos,na.rm=TRUE),t="l",main="",xlab="Estimated Unit-Specific Rho")
abline(v=medianRho,lwd=1,lty=2)
text(medianRho,0.25,pos=4,cex=0.7,
     labels=paste0("Median = ",medianRho))
dev.off()

# Prais-Winsten:

PraisWinsten<-panelAR(WomenBusLawIndex~PopGrowth+UrbanPopulation+
              FertilityRate+lnGDPPerCap+NaturalResourceRents+
              PostColdWar, data=smol,panelVar="ISO3",timeVar="Year",
              autoCorr="ar1",panelCorrMethod="none",
              rho.na.rm=TRUE)
summary(PraisWinsten)
PraisWinsten$panelStructure$rho

# Make a LaTeX table:
# 
# texreg(list(OLS,PraisWinsten),
#        custom.model.names=c("OLS","Prais-Winsten"),
#        custom.coef.names=c("Intercept","Population Growth","Urban Population",
#                            "Fertility Rate","ln(GDP Per Capita)",
#                            "Natural Resource Rents","Cold War"),
#        stars=0.05)
#
# Note: This ^^ doesn't work at the moment, because panelAR is
# currently (June 2024) a bit broken.
#
# GLS with homoscedasticity & unit-specific AR(1) autocorrelation:

GLS<-gls(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+NaturalResourceRents+
         PostColdWar,data=smol,correlation=corAR1(form=~1|ISO3),na.action="na.omit")

summary(GLS)


# PCSEs:

PCSE<-panelAR(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+NaturalResourceRents+
           PostColdWar,data=smol,panelVar="ISO3",timeVar="Year",autoCorr="ar1",
           panelCorrMethod="pcse",rho.na.rm=TRUE)

summary(PCSE)
PCSE$panelStructure$rho

# Make a nice comparison plot, the hard way:

hats<-data.frame(term=rep(c("(Intercept)","Pop. Growth",
                 "Urban Population","Fertility Rate",
                 "ln(GDP Per Capita)","Nat. Resource Rents",
                 "Post-Cold War"),4),
                 model=c(rep("OLS",7),rep("P-W",7),
                             rep("GLS",7),rep("PCSE",7)),
                 estimate=c(coef(OLS),coef(PraisWinsten),
                               coef(GLS),coef(PCSE)),
                 std.error=c(sqrt(diag(vcov(OLS))),
                                 sqrt(diag(vcov(PraisWinsten))),
                                 sqrt(diag(vcov(GLS))),
                                 sqrt(diag(vcov(PCSE))))
)


# This used to work, and look much prettier too, but the 
# current package dependencies prevent it...
# 
# pdf("DynModelLadderPlot-24.pdf",6,4)
# dwplot(hats,
#        vline=geom_vline(xintercept=0,linetype=2),
#        dot_args = list(aes(shape = model))) +
#        theme_classic() +
#        xlab("Coefficient Estimate") +
#        guides(shape = guide_legend("Model"),
#               colour = guide_legend("Model"))
# dev.off()
#
# Instead:

hats2<-hats[hats$term!="(Intercept)",]
hats2<-hats2[order(hats2$term,hats2$model),]

pdf("DynModelLadderPlot-24.pdf",7,5)
par(mar=c(4,4,2,2))
dotchart(hats2$estimate,labels=hats2$model,groups=as.factor(hats2$term),
         pch=19,xlim=c(-12,8),xlab="Estimate Values",cex=0.7,
         col=c("black","green","red","orange"))
abline(v=0,lty=4,lwd=1,col="grey")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dynamics!                                         ####
#
# # Simulating unit roots, etc.
#
# Leaving this commented out for now...
# 
# set.seed(7222009)
# 
# # T=250 N(0,1) errors:
# 
# T <- 250
# Time <- seq(1:T)
# u <- rnorm(T)
# 
# # I(1) process:
# 
# I1<-cumsum(u)
# 
# # AR(1) with rho = 0.9 (done "by hand):
# 
# rho9 <- numeric(T)
# rho9[1] <- u[1] # initialize T=1 to the first error
# for(i in 2:T) {
#   rho9[i] <- (0.9* rho9[i-1]) + u[i]
# }
# 
# # Plot:
# 
# pdf("TSIllustrated.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(Time,I1,t="l",lwd=2,lty=3,col="red",
#      xlab="Time",ylab="Y")
# lines(Time,rho9,lwd=2,lty=2,col="blue")
# lines(Time,u,lwd=2)
# legend("bottomleft",
#        legend=c("N(0,1) errors","Unit Root","AR(1) with rho=0.9"),
#        col=c("black","red","blue"),bty="n",
#        lwd=c(2,2,2),lty=c(1,3,2))
# dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Panel unit root tests... ####
#
# Data:

WBLI<-data.frame(ISO3=WDI$ISO3,Year=WDI$Year,
                 WBLI=WDI$WomenBusLawIndex)
WBLI<-na.omit(WBLI)  # remove missing
WBLI<-pdata.frame(WBLI,index=c("ISO3","Year")) # panel data
WBLI.W<-data.frame(split(WBLI$WBLI,WBLI$ISO3)) # "wide" data

purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
purtest(WBLI.W,exo="trend",test="ips",pmax=2)

# Gather the statistics:

ur1<-purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
ur2<-purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
ur3<-purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
ur4<-purtest(WBLI.W,exo="trend",test="ips",pmax=2)

urs<-matrix(nrow=4,ncol=5)
for(i in 1:4){
  nom<-get(paste0("ur",i))
  urs[i,1]<-nom$statistic$method
  urs[i,2]<-nom$statistic$alternative
  urs[i,3]<-names(nom$statistic$statistic)
  urs[i,4]<-round(nom$statistic$statistic,3)
  urs[i,5]<-round(nom$statistic$p.value,4)
}

urs<-data.frame(urs)
colnames(urs)<-c("Test","Alternative","Statistic",
                   "Estimate","P-Value")
URTests<-stargazer(urs,summary=FALSE,
                   title="Panel Unit Root Tests: WBRI",
                   out="URTests-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some regression models, with dynamics...               ####
#
# Lagged -dependent-variable model:

smol$WBLI.L <- plm::lag(smol$WomenBusLawIndex,n=1) # be sure to use the
# -plm- version of -lag-

LDV.fit <- lm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
                FertilityRate+lnGDPPerCap+NaturalResourceRents+
                PostColdWar,data=smol)

# First difference:

FD.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                lnGDPPerCap+NaturalResourceRents+PostColdWar,
              data=smol,effect="individual",model="fd")

# Plain fixed effects:
  
FE.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                lnGDPPerCap+NaturalResourceRents+PostColdWar,
              data=smol,effect="individual",model="within")

# LDV w/ FE:

LDV.FE.fit <- plm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+FertilityRate+
                  lnGDPPerCap+NaturalResourceRents+PostColdWar,data=smol,
                  effect="individual",model="within")

# Next: Anderson-Hsiao -- note that it's easier to first-difference
# the outcome and create a new data frame with lagged versions of 
# the Xs:

smol.d<-data.frame(ISO3=smol$ISO3,Year=smol$Year,
                   WomenBusLawIndex=diff(smol$WomenBusLawIndex),
                   WBLI.L=lag(diff(smol$WomenBusLawIndex)),
                   PopGrowth=lag(diff(smol$PopGrowth)),
                   UrbanPopulation=lag(diff(smol$UrbanPopulation)),
                   FertilityRate=lag(diff(smol$FertilityRate)),
                   lnGDPPerCap=lag(diff(smol$lnGDPPerCap)),
                   NaturalResourceRents=lag(diff(smol$NaturalResourceRents)),
                   PostColdWar=lag(diff(smol$PostColdWar)),
                   I.WBLI=lag(smol$WomenBusLawIndex,2),
                   I.PopGrowth=lag(smol$PopGrowth,2),
                   I.UrbanPopulation=lag(smol$UrbanPopulation,2),
                   I.FertilityRate=lag(smol$FertilityRate,2),
                   I.lnGDPPerCap=lag(smol$lnGDPPerCap,2),
                   I.NaturalResourceRents=lag(smol$NaturalResourceRents,2),
                   I.PostColdWar=lag(smol$PostColdWar,2))


AH.fit<-plm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+FertilityRate+lnGDPPerCap+
              NaturalResourceRents+PostColdWar |
              I.WBLI+I.PopGrowth+I.UrbanPopulation+I.FertilityRate+I.lnGDPPerCap+I.NaturalResourceRents+
              I.PostColdWar,data=smol.d,model="pooling")

# Old code:
# 
# AH.fit<-plm(diff(WomenBusLawIndex)~lag(diff(WomenBusLawIndex))+
#                     lag(diff(PopGrowth))+lag(diff(UrbanPopulation))+
#                     lag(diff(FertilityRate))+lag(diff(lnGDPPerCap))+
#                     lag(diff(NaturalResourceRents))+lag(diff(PostColdWar)) |
#                     lag(WomenBusLawIndex,2)+lag(PopGrowth,2)+
#                     lag(UrbanPopulation,2)+lag(FertilityRate,2)+
#                     lag(lnGDPPerCap,2)+lag(NaturalResourceRents,2)+
#                     lag(PostColdWar,2),data=smol,model="pooling")


#
# Finally: Arellano-Bond model. Do not run, unless
# you are young, and have lots of time on your
# hands...

AB.fit<-pgmm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
               FertilityRate+lnGDPPerCap+NaturalResourceRents+
               PostColdWar|lag(WomenBusLawIndex,2:5) |
               lag(PopGrowth)+lag(UrbanPopulation)+lag(FertilityRate)+
               lag(lnGDPPerCap)+lag(NaturalResourceRents)+
               lag(PostColdWar),data=smol,effect="individual",
               model="twosteps")

# Table:

models<-list("OLS"=OLS,"Lagged Y"=LDV.fit,"First Difference"=FD.fit,
             "Fixed Effects"=FE.fit,"FE + Lagged Y"=LDV.FE.fit,
             "Anderson-Hsaio"=AH.fit)

modelsummary(models,fmt=3,output="DynamicTable-24.tex",
             gof_omit="F|DF|Deviance|AIC|BIC|Log.lik.",
             coef_rename=c("WBLI.L"="Lagged WBLI",
                           "PopGrowth"="Population Growth",
                           "UrbanPopulation"="Urban Population",
                           "FertilityRate"="Fertility Rate",
                           "lnGDPPerCap"="ln(GDP Per Capita)",
                           "NaturalResourceRents"="Natural Resource Rents",
                           "PostColdWar"="Post-Cold War"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Trend things...                               ####
#
# Trend illustration simulation:

set.seed(2719)
Tobs<-40 
X<-cumsum(rnorm(Tobs))+5
u<-rnorm(T,0,2)
T<-1:Tobs
Y<-10+X+u
Yt<-5+X+0.5*T+u

pdf("TrendPlot-24.pdf",5,6)
par(mar=c(4,4,2,2))
plot(T,Yt,t="l",lwd=4,lty=2,ylim=c(0,35),
     ylab="Y",col="blue")
lines(T,Y,lty=4,lwd=4,col="orange")
lines(T,X,lwd=4,col="black")
legend("topleft",bty="n",lwd=4,lty=c(2,4,1),
       col=c("blue","orange","black"),
       legend=c("Y2","Y1","X"))
dev.off()

# Regressions:

f1<-lm(Y~X)
f2<-lm(Yt~X)
f3<-lm(Yt~X+T)

stargazer(f1,f2,f3,omit.stat="f")

# Make a better trend variable in the WDI data:

smol$Trend <- smol$Year-1950

# FE w/o trend:

FE  <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              lnGDPPerCap+NaturalResourceRents+PostColdWar,
              data=smol,effect="individual",model="within")

# FE with trend:

FE.trend <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+
              FertilityRate+lnGDPPerCap+NaturalResourceRents+
              PostColdWar+Trend,data=smol,effect="individual",
              model="within")

# FE with trend + interaction:

FE.intx <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                lnGDPPerCap+NaturalResourceRents+PostColdWar+Trend+PostColdWar*Trend,
                data=smol,effect="individual",model="within")

# A table:

TableTrend <- stargazer(FE,FE.trend,FE.intx,
                    title="FE Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War",
                                       "Trend (1950=0)","Post-Cold War x Trend"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    omit.stat=c("f"),out="Trendy-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Orthogonal Parameters Model:                            ####

set.seed(7222009)
OPM.fit <- opm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
               lnGDPPerCap+NaturalResourceRents+PostColdWar,
               data=smol,index=c("ISO3","Year"),n.samp=1000)

# Ladder plot of estimates & CIs:

pdf("OPM-Ladder-24.pdf",8,6)
par(mar=c(4,12,2,2))
caterplot(OPM.fit,parm=c("beta","rho"),
          main=c(""),xlab="Parameter Estimate",
          labels=c("Population Growth",
                   "Urban Population","Fertility Rate",
                   "ln(GDP Per Capita)","Natural Resource Rents",
                   "Cold War","Rho"))
abline(v=c(0),lty=2)
dev.off()

# Short- and long-run effects:

SREs<-numeric(6)
LREs<-numeric(6)
for(i in 1:6){
  SREs[i]<-quantile(OPM.fit$samples$beta[,i],probs=c(0.50))
  LREs[i]<-quantile(OPM.fit$samples$beta[,i]/(1-OPM.fit$samples$rho),
                    probs=c(0.50))
}

print(cbind(round(SREs,2),round(LREs,2)))



#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# FEIS...
#
# "Fixed-effects Individual Slopes" model:

smol$ID<-as.numeric(smol$ISO3) # needs numeric ID variable...

FEIS<-feis(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
           lnGDPPerCap+NaturalResourceRents | PostColdWar,
           data=(smol),id="ID",robust=FALSE)

summary(FEIS)

# Testing vs. FE, RE models:

FEIS.test<-feistest(FEIS)
summary(FEIS.test)

# Extract unit-specific slopes for Post-Cold War, and
# plot them:

PCWSlopes<-feisr::slopes(FEIS) # unit-specific slopes on Post-Cold War variable
meanSlope<-round(mean(PCWSlopes[,2]),2)

pdf("FEISSlopes-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(PCWSlopes[,2]),t="l",lwd=2,main="",
     xlab="Estimate of Post-Cold War Slopes")
abline(v=median(PCWSlopes[,2]),lwd=1,lty=2)
abline(v=mean(PCWSlopes[,2]),lwd=2,lty=3,col="orange")
legend("topright",bty="n",col=c("black","orange"),
       lwd=c(1,2),lty=c(2,3),legend=c("Median Slope = 0",
                paste0("Mean Slope = ",meanSlope)))
dev.off()

# Unit-specific variable trends:

FEIS2<-feis(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
           lnGDPPerCap+NaturalResourceRents+PostColdWar | Year,
           data=(smol),id="ID",robust=FALSE)

summary(FEIS2)

# Trends:

TrendSlopes<-feisr::slopes(FEIS2) # unit-specific slopes on Post-Cold War variable
medianTrend<-round(median(TrendSlopes[,2]),2)
meanTrend<-round(mean(TrendSlopes[,2]),2)

pdf("FEISTrends-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(TrendSlopes[,2]),t="l",lwd=2,main="",
     xlab="Estimate of Post-Cold War Slopes")
abline(v=0,col="grey78",lty=3)
abline(v=medianTrend,lwd=1,lty=2)
abline(v=meanTrend,lwd=2,lty=3,col="orange")
legend("topright",bty="n",col=c("black","orange"),
       lwd=c(1,2),lty=c(2,3),legend=c(paste0("Median Trend = ",medianTrend),
                                      paste0("Mean Trend = ",meanTrend)))
dev.off()

# fin