#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#
# GSERM - St. Gallen (2024)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day Two: "Unit Effects" models.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This code takes a list of packages ("P") and (a) checks for whether
# the package is installed or not, (b) installs it if it is not, and 
# then (c) loads each of them:

P<-c("RCurl","readr","haven","colorspace","foreign","psych","car",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "plm","tibble","pscl","naniar","ExPanDaR","stargazer","prais",
     "nlme","tseries","panelView","performance","xtsum",
     "modelsummary","marginaleffects")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run ^ this code a few times to make it work - that is, until
# you get all smiley faces :)
#
# Set R Options:

options(scipen = 8) # bias against scientific notation
options(digits = 2) # show fewer decimal places

setwd("~/Dropbox (Personal)/GSERM/Panel-2024/Notes and Slides")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# FE plot:

i<-1:4
t<-20
NT<-t*max(i)
set.seed(7222009)
df<-data.frame(i=rep(i,t),
               t=rep(1:t,max(i)),
               X=runif(NT))
df$Y=1+2*i+4*df$X+runif(NT)
df<-df[order(df$i,df$t),] # sort

pdf("FEIntuition.pdf",7,6)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=i+14,col=i,
              xlim=c(0,1),ylim=c(3,14)))
abline(a=3.5,b=4,lwd=2,lty=1,col=1)
abline(a=5.5,b=4,lwd=2,lty=2,col=2)
abline(a=7.5,b=4,lwd=2,lty=3,col=3)
abline(a=9.5,b=4,lwd=2,lty=4,col=4)
legend("bottomright",bty="n",col=1:4,lty=1:4,
       lwd=2,legend=c("i=1","i=2","i=3","i=4"))
dev.off()

# World Development Indicators (WDI) data ####
#
# The WDI data we'll be using for this class can
# be accessed by running the "WDI-MakeData.R"
# script found on the Github repository. You can 
# modify that script to add additional variables
# if you choose to. The data are also available
# in ready-to-use format in the "Data" folder
# on the Github repo.
#
# Get the data:

wdi<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2024/main/Data/WDI24a.csv")

# Add a "Cold War" variable:

wdi$PostColdWar <- with(wdi,ifelse(Year<1990,0,1))

# Add GDP Per Capita:

wdi$GDPPerCapita <- wdi$GDP / wdi$Population

# Summarize:

describe(wdi,fast=TRUE,ranges=FALSE,check=TRUE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Visualizing WDI data...                           ####

pdf("PanelWBLIViz24.pdf",7,5)
panelview(WomenBusLawIndex~1,data=wdi,theme.bw=TRUE,
          outcome.type="continuous",type="outcome",
          by.timing=TRUE,index=c("ISO3","Year"),
          main=" ",ylab="Women's Business Law Index",
          legendOff=TRUE)
dev.off()

pdf("PanelPLeaveViz24.pdf",7,5)
panelview(WomenBusLawIndex~PaidParentalLeave,data=wdi,theme.bw=TRUE,
          by.timing=FALSE,index=c("ISO3","Year"),
          color=c("orange","darkgreen"),
          legend.labs=c("No Paid Leave","Paid Leave"),
          main=" ",ylab="Country Code",axis.lab.gap=c(5,5),
          background="white")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Variation: Total, within, and between               ####
#
# Create "panel series"...

WDI<-pdata.frame(wdi,index=c("ISO3","Year"))
class(WDI)
WBLI<-WDI$WomenBusLawIndex
class(WBLI)

describe(WBLI,na.rm=TRUE) # all variation

pdf("WBLIAll24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WDI$WomenBusLawIndex,na.rm=TRUE),
     main="",xlab="WBL Index",lwd=2)
abline(v=mean(WDI$WomenBusLawIndex,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Between" variation:

describe(plm::between(WBLI,effect="individual",na.rm=TRUE)) # "between" variation

WBLIMeans<-plm::between(WBLI,effect="individual",na.rm=TRUE)

WBLIMeans<-ddply(WDI,.(ISO3),summarise,
                 WBLIMean=mean(WomenBusLawIndex,na.rm=TRUE))

pdf("WBLIBetween24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WBLIMeans$WBLIMean,na.rm=TRUE),
     main="",xlab="Mean WBLI",lwd=2)
abline(v=mean(WBLIMeans$WBLIMean,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Within" variation:

describe(Within(WBLI,na.rm=TRUE)) # "within" variation

pdf("WBLIWithin24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Within(WBLI,na.rm=TRUE),na.rm=TRUE),
     main="",xlab="WBLI: Within-Country Variation",
     lwd=2)
abline(v=0,lty=2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Regression! One-Way Unit Effects models               ####

# Subset (just for descriptives):

vars<-c("ISO3","Year","WomenBusLawIndex","PopGrowth","UrbanPopulation",
        "FertilityRate","GDPPerCapita","NaturalResourceRents","PostColdWar")
smol<-WDI[vars]
smol<-smol[complete.cases(smol),] # listwise deletion
smol$ISO3<-droplevels(smol$ISO3) # drop unused factor levels
smol$lnGDPPerCap<-log(smol$GDPPerCapita)
smol$GDPPerCapita<-NULL
describe(smol[,3:9],fast=TRUE)

# Summary: Between vs. within variation in the predictors:

Xs.df<-xtsum(smol,id="ISO3",t="Year",return.data.frame=TRUE)
stargazer(Xs.df,summary=FALSE)

# Pooled OLS:

OLS<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                 log(GDPPerCapita)+NaturalResourceRents+PostColdWar, 
        data=WDI,model="pooling")

summary(OLS)

# "Fixed" / within effects:

FE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+log(GDPPerCapita)+
        NaturalResourceRents+PostColdWar,data=WDI,effect="individual",model="within")

summary(FE)

# Make a table:

Table1 <- stargazer(OLS,FE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX11-24.tex")

# Time-period Fixed Effects:

FE.Time<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
         log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
         effect="time",model="within")

# summary(FET)

# A comparison table:

FE.Units <- FE

CompFETable <- stargazer(FE.Units,FE.Time,
                    title="FE Models of WBLI (Units vs. Periods)",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="FEComp1-24.tex")

# Test for \alpha_i = 0:

pFtest(FE,OLS)
plmtest(FE,effect=c("individual"),type=c("bp"))
plmtest(FE,effect=c("individual"),type=c("kw"))

pFtest(FE.Time,OLS)
plmtest(FE.Time,effect=c("time"),type=c("bp"))
plmtest(FE.Time,effect=c("time"),type=c("kw"))

#=-=-=-=-=-=-=-=-=-=-=-=   STOPPED HERE  
# Interpretation...

with(WDI, sd(UrbanPopulation,na.rm=TRUE)) # all variation

WDI<-ddply(WDI, .(ISO3), mutate,
               UPMean = mean(UrbanPopulation,na.rm=TRUE))
WDI$UPWithin<-with(WDI, UrbanPopulation-UPMean)

with(WDI, sd(UPWithin,na.rm=TRUE)) # "within" variation

#=-=-=-=-=-=-=-=-=-=-=-=-=
# Between effects:

BE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
        effect="individual",model="between")

summary(BE)

Table2 <- stargazer(OLS,FE,BE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX21-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=
# Random effects:

RE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
          PostColdWar,data=WDI,effect="individual",model="random")

summary(RE)

Table3 <- stargazer(OLS,FE,BE,RE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX31-24.tex")

# Hausman test:

phtest(FE, RE)  # ugh...

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# A bit of HLMs...                                  ####

AltRE<-lmer(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
            log(GDPPerCapita)+NaturalResourceRents+PostColdWar+(1|ISO3),
            data=WDI)

summary(AltRE)

# Are they the same?

TableHLM <- stargazer(RE,AltRE,
                    title="RE and HLM Models of WBLI",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="HLMTable1-24.tex")

# ... yes. More or less.
#
# More HLM fun... allow the coefficients on PostColdWar to vary
# randomly for each country...

HLM1<-lmer(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
            log(GDPPerCapita)+NaturalResourceRents+PostColdWar+(PostColdWar|ISO3),
            data=WDI,control=lmerControl(optimizer="bobyqa"))

summary(HLM1)


# Testing:

anova(AltRE,HLM1)
VarCorr(HLM1)

# Get some of those sweeeet random slopes:

Bs<-data.frame(coef(HLM1)[1])

head(Bs)
mean(Bs$ISO3..Intercept.)
mean(Bs$ISO3.PostColdWar)

# Plots:

pdf("WBLI-RandomIntercepts-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(density(ISO3..Intercept.),lwd=3,
              main="",xlab="Intercept Values"))
abline(v=mean(Bs$ISO3..Intercept.),lty=2)
dev.off()

pdf("WBLI-CWRandomSlopes-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(density(ISO3.PostColdWar),lwd=3,
              main="",xlab="Cold War Slopes"))
abline(v=mean(Bs$ISO3.PostColdWar),lty=2)
dev.off()

REcorr<-with(Bs,cor(ISO3..Intercept.,ISO3.PostColdWar))

pdf("WBLI-HLMScatter-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(ISO3..Intercept.,ISO3.PostColdWar,
              pch=19,main="",xlab="Intercept Values",
              ylab="Post-Cold War Slopes"))
text(-90,30,cex=1,
     labels=paste0("r = ",round(REcorr,3)))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Separating "within" and "between" effects           ####
# 
# Create "between" and "within" versions of the
# Natural Resource Rents variable:

WDI<-ddply(WDI,.(ISO3),mutate,
             NRR.Between=mean(NaturalResourceRents,na.rm=TRUE))
WDI$NRR.Within<- (WDI$NaturalResourceRents - WDI$NRR.Between) 

# Fit a (standard / OLS) regression model that includes both:

WEBE.OLS<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
           log(GDPPerCapita)+NRR.Within+NRR.Between+PostColdWar,
           data=WDI)

# summary(WEBE.OLS) # not reported

# Nice table:

Table4 <- stargazer(WEBE.OLS,
                    title="BE + WE Model of WBLI",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Within-Country Nat. Resource Rents",
                                       "Between-Country Nat. Resource Rents",
                                       "Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="WEBE1-24.tex")

# While it's pretty obvious that they are different, let's 
# formally test their equality, using a standard F-test (which
# we can do using the -linearHypothesis- command in the -car-
# package, among others):

linearHypothesis(WEBE.OLS,c("NRR.Within=NRR.Between"))


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Two-way effects...                                  ####
#
# First, "fixed" effects (that is, within-unit-and-time):

TwoWayFE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
              effect="twoway",model="within")

summary(TwoWayFE)

# Testing...
#
# 1. Are all two-way effects identical?:

pFtest(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
       log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
       effect="twoway",model="within")

plmtest(TwoWayFE,c("twoways"),type=("kw"))

# 2. Are the two kinds of one-way effects in the two-way model
# all equal?:

plmtest(TwoWayFE,c("individual"),type=("kw"))
plmtest(TwoWayFE,c("time"),type=("kw"))

# Equivalence to -lm- (note that we omit the Post-Cold War
# variable; in addition, the "-1" in the formula means that
# we're omitting the intercept in the model, so that the
# FE estimates are the unit- and period-specific means):

TwoWayFE.BF<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                factor(ISO3)+factor(Year)-1,data=WDI)

summary(TwoWayFE.BF)

# Two-way "random" effects:

TwoWayRE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+PostColdWar,data=WDI,
              effect="twoway",model="random")

summary(TwoWayRE)

# Here's a nicer table:

Table5 <- stargazer(OLS,FE,BE,RE,TwoWayFE,TwoWayRE,
                    title="Models of WBLI",
                    column.separate=c(1,1,1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Post-Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    column.sep.width="1pt",
                    omit.stat=c("f"),out="UFX51-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interpretation: modelsummary                              ####
#
# We'll use our OLS and one-way fixed and random (unit) effects 
# models as the example. Here's the no-frills version:

modelsummary(list(OLS,FE,RE),output="table.tex",stars=TRUE)

# And here's a better one that follows good practices for tables:

models<-list("OLS"=OLS,"Within"=FE,"Random"=RE)

modelsummary(models,output="MS-Table-24.tex",title="Models of WDBI",
             stars=TRUE,fmt=2,gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("PopGrowth"="Population Growth",
                           "UrbanPopulation"="Urban Population",
                           "FertilityRate"="Fertility Rate",
                           "log(GDPPerCapita)"="ln(GDP Per Capita)",
                           "NaturalResourceRents"="Natural Resource Rents",
                           "PostColdWar"="Post-Cold War"))

# Coefficient plot (with 99% CIs):

pdf("OLS-FE-RE-Coefplot-24.pdf",6,5)
modelplot(models,conf_level=0.99,coef_omit="(Intercept)",
          coef_rename=c("PopGrowth"="Population Growth",
                        "UrbanPopulation"="Urban Population",
                        "FertilityRate"="Fertility Rate",
                        "log(GDPPerCapita)"="ln(GDP Per Capita)",
                        "NaturalResourceRents"="Natural Resource Rents",
                        "PostColdWar"="Post-Cold War"))
dev.off()

# Coefficient plots:

plot_predictions(models,condition="PostColdWar")

# /fin