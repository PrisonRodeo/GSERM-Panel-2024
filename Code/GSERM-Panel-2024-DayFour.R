#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#
# GSERM - St. Gallen (2024)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day Four: Causal Inference.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","haven","psych","sandwich","countrycode","wbstats",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "plm","tibble","pscl","naniar","ExPanDaR","stargazer","prais",
     "nlme","tseries","pcse","panelView","performance","pgmm","dynpanel",
     "OrthoPanels","peacesciencer","corrplot","rgenoud",
     "MatchIt","Matching","did","optmatch","Synth","texreg",
     "cobalt","did","modelsummary","marginaleffects")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Set display options:

options(scipen = 99) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd yo'self, if you care to, or set up a project...
#
# setwd("~/This is a path")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# World Development Indicators data (yet again)...    ####

# Pull the "alternative" WDI data...

WDI<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2024/main/Data/WDI24b.csv")

# Add a "Post-Cold War" variable:

WDI$PostColdWar <- with(WDI,ifelse(Year<1990,0,1))

# log GDP Per Capita:

WDI$lnGDPPerCap<-log(WDI$GDPPerCapita)

# ... and log Net Aid Received:

WDI$lnNetAidReceived<-log(WDI$NetAidReceived)

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# summary(WDI)
#
# Make the data a panel dataframe:

WDI<-pdata.frame(WDI,index=c("ISO3","Year"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Descriptive statistics on the WDI data      ####

describe(WDI,fast=TRUE,ranges=FALSE,check=TRUE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some Regressions...                  ###########
#
# First, a simple preliminary investigation...
#
# Create logged Child Mortality variable:

WDI$lnCM <- log(WDI$ChildMortality)

# T-test (not reproduced):

t.test(lnCM~PaidParentalLeave,data=WDI)

# Bivariate regression:

BIV<-lm(lnCM~PaidParentalLeave,data=WDI)

# OLS:

OLS<-lm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures,data=WDI)

# Fixed Effects... One-way:

FE.1way<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures,
             data=WDI,effect="individual",model="within")

# Two-way:

FE.2way<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures,
             data=WDI,effect="twoway",model="within")


FE.LDV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures+
            lag(ChildMortality),data=WDI,effect="individual",model="within")

# A nice table:

MortTable1 <- stargazer(BIV,OLS,FE.1way,FE.2way,FE.LDV,
                    title="Models of log(Child Mortality)",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,p=c(0.05),
                    dep.var.caption="",omit.stat=c("f","ser"),
                    covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                       "ln(Net Aid Received)",
                                       "Government Expenditures",
                                       "Lagged Child Mortality"),
                    column.labels=c("Bivariate OLS","OLS","One-Way FE",
                                    "Two-Way FE","FE w.Lagged Y"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="ChildMortTable1-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# INSTRUMENTAL VARIABLES!                           ####
#
# We'll instrument Paid Parental Leave with the
# Women in Legislature variable:

with(WDI,t.test(WomenInLegislature~PaidParentalLeave))

pdf("IVBoxplot-24.pdf",7,5)
par(mar=c(4,4,2,2))
boxplot(WomenInLegislature~PaidParentalLeave,data=WDI,
        xlab="Paid Parental Leave",
        ylab="Percent of Legislative Seats Held By Women")
dev.off()

# IV regressions (one-way FE and RE):

FE.IV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
              lnNetAidReceived+GovtExpenditures |
              .-PaidParentalLeave+WomenInLegislature,
              data=WDI,effect="individual",model="within")


RE.IV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
             lnNetAidReceived+GovtExpenditures |
             .-PaidParentalLeave+WomenInLegislature,
             data=WDI,effect="individual",model="random")

# A nice table:

IVTable<-stargazer(OLS,FE.1way,FE.IV,RE.IV,
                   title="IV Models of log(Child Mortality)",
                   column.separate=c(1,1),align=TRUE,
                   dep.var.labels.include=FALSE,p=c(0.05),
                   dep.var.caption="",omit.stat=c("f","ser"),
                   covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                      "ln(Net Aid Received)",
                                      "Government Expenditures"),
                   column.labels=c("OLS","One-Way FE","FE w/IV","RE w/IV"),
                   header=FALSE,model.names=FALSE,
                   model.numbers=FALSE,multicolumn=FALSE,
                   object.names=TRUE,notes.label="",
                   out="ChildMortTable2-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Matching!                                         ####
#
# Subset and variables and listwise delete missing data, 
# to simplify things...

vars<-c("ISO3","Year","Region","country","UrbanPopulation",
        "FertilityRate","PrimarySchoolAge","ChildMortality",
        "lnGDPPerCap","lnNetAidReceived","NaturalResourceRents",
        "GovtExpenditures","PaidParentalLeave","PostColdWar",
        "lnCM")
wdi<-WDI[vars]
wdi<-na.omit(wdi)

# Create discrete-valued variables (i.e., coarsen) for
# matching on continuous predictors:

wdi$GDP.Decile<-as.factor(ntile(wdi$lnGDPPerCap,10))
wdi$Aid.Decile<-as.factor(ntile(wdi$lnNetAidReceived,10))
wdi$GSpend.Decile<-as.factor(ntile(wdi$GovtExpenditures,10))

# Pre-match balance statistics...

BeforeBal<-bal.tab(PaidParentalLeave~GDP.Decile+
                Aid.Decile+GSpend.Decile,data=wdi,
                stats=c("mean.diffs","ks.statistics"))

# Plot balance:

pdf("PreMatchBalance-24.pdf",5,6)
plot(BeforeBal)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Exact Matching:

M.exact <- matchit(PaidParentalLeave~GDP.Decile+Aid.Decile+
                  GSpend.Decile,data=wdi,method="exact")
summary(M.exact)

# Plot balance...

ExactBal<-bal.tab(M.exact,un=TRUE)

pdf("ExactMatchBal-24.pdf",5,6)
plot(ExactBal)
dev.off()

# Create matched data:

wdi.exact <- match.data(M.exact,group="all")
dim(wdi.exact)

# Model for propensity scores:

PS.fit<-glm(PaidParentalLeave~GDP.Decile+Aid.Decile+
              GSpend.Decile,data=wdi,
              family=binomial(link="logit"))

# Generate scores & check common support:

PS.df<-data.frame(PS = predict(PS.fit,type="response"),
                  PaidParentalLeave=PS.fit$model$PaidParentalLeave)

pdf("PS-Mort-Support-24.pdf",7,5)
par(mar=c(4,4,2,2))
with(PS.df[PS.df$PaidParentalLeave==0,],
     plot(density(PS),main="Propensity Score Balance",
          lwd=2,xlab="Propensity Score",xlim=c(0,1)))
with(PS.df[PS.df$PaidParentalLeave==1,],
     lines(density(PS),lwd=2,lty=2,col="red"))
legend("topright",bty="n",col=c("black","red"),
       lwd=2,lty=c(1,2),legend=c("No Paid Parental Leave",
                                 "Paid ParentalLeave"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Propensity score matching:

M.prop <- matchit(PaidParentalLeave~GDP.Decile+Aid.Decile+
                  GSpend.Decile,data=wdi,method="nearest",
                  ratio=3)
summary(M.prop)

# Balance check:

PSBal<-bal.tab(M.prop,un=TRUE)

pdf("PSMatchBal-24.pdf",5,6)
plot(PSBal,drop.distance=TRUE)
dev.off()

# Matched data:

wdi.ps <- match.data(M.prop,group="all")
dim(wdi.ps)


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Optimal matching:

M.opt <- matchit(PaidParentalLeave~GDP.Decile+Aid.Decile+
                  GSpend.Decile,data=wdi,method="optimal",
                  ratio=3)
summary(M.opt)

# Matched data:

wdi.opt <- match.data(M.opt,group="all")
dim(wdi.opt)

# Balance check:

OptBal<-bal.tab(M.opt,un=TRUE)

pdf("OptMatchBal-24.pdf",5,6)
plot(OptBal,drop.distance=TRUE)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Regressions (before and) after matching... ####

PreMatch.FE<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
             lnNetAidReceived+GovtExpenditures,data=wdi,
             effect="individual",model="within")

Exact.FE<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
                lnNetAidReceived+GovtExpenditures,data=wdi.exact,
              effect="individual",model="within")

PS.FE<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
                 lnNetAidReceived+GovtExpenditures,data=wdi.ps,
                 effect="individual",model="within")

Optimal.FE<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
                lnNetAidReceived+GovtExpenditures,data=wdi.opt,
                effect="individual",model="within")

# A table...

MatchMortTable <- stargazer(PreMatch.FE,Exact.FE,PS.FE,Optimal.FE,
                    title="FE Models of log(Child Mortality): Matched Data",
                    column.separate=c(1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                       "ln(Net Aid Received)",
                                       "Government Expenditures"),
                    column.labels=c("Pre-Matching","Exact","Prop. Score","Optimal"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    column.sep.width="1pt",
                    omit.stat=c("f","ser"),out="MatchMortRegs-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simple RDD...                                     ####
#
# Pull out *only* those countries that, at some
# point during the observed periods, instituted
# a paid parental leave policy:

PPLs<-WDI
PPLs<-PPLs %>% group_by(ISO3) %>%
  filter(any(PaidParentalLeave==1))
PPLs$YearNumeric<-as.numeric(as.character(PPLs$YearNumeric)) # fix

# Plot the trends by value of PaidParentalLeave: 

pdf("PPL-Plot-24.pdf",7,5)
par(mar=c(4,4,2,2))
with(PPLs[PPLs$PaidParentalLeave==0,],
               plot(YearNumeric,log(ChildMortality),
               pch=20,col="darkorange",xlab="Year",
               ylim=c(0.7,5.3)))
with(PPLs[PPLs$PaidParentalLeave==1,],
               points(YearNumeric,log(ChildMortality),
               pch=17,col="forestgreen"))
legend("topright",bty="n",pch=c(20,17),
       col=c("darkorange","forestgreen"),
       legend=c("No Paid Parental Leave",
                "Paid Parental Leave"))
dev.off()

# Create a better trend variable:

PPLs$Time<-PPLs$YearNumeric-1950

# Create interaction term:

PPLs$PPLxTime<-PPLs$PaidParentalLeave * PPLs$Time

# REGRESSION TIME 

RDD.OLS1<-lm(lnCM~PaidParentalLeave+Time+PPLxTime,data=PPLs)

RDD.OLS2<-lm(lnCM~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
             lnNetAidReceived+GovtExpenditures,data=PPLs)

# FE models...

# PPLs<-pdata.frame(PPLs,index=c("ISO3","Year")) # make panel data

RDD.1way.1<-plm(lnCM~PaidParentalLeave+Time+PPLxTime,data=PPLs,
                effect="individual",model="within")

RDD.1way.2<-plm(lnCM~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
                lnNetAidReceived+GovtExpenditures,
                data=PPLs,effect="individual",model="within")

RDD.2way.1<-plm(ChildMortality~PaidParentalLeave+Time+PPLxTime,data=PPLs,
                effect="twoway",model="within")

RDD.2way.2<-plm(ChildMortality~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
                  lnNetAidReceived+GovtExpenditures,
                  data=PPLs,effect="twoway",model="within")

# TABLE TIME...
#
# -stargazer- doesn't seem to be working for me right now
# (June 2024), but I'll put the code here anyway:
#
# RDDMortTable<-stargazer(RDD.OLS1,RDD.OLS2,RDD.1way.1,RDD.1way.2,
#                         RDD.2way.1,RDD.2way.2,
                        # title="RDD Models of log(Child Mortality)",
                        # column.separate=c(1,1,1),align=TRUE,
                        # dep.var.labels.include=FALSE,
                        # dep.var.caption="",
                        # covariate.labels=c("Paid Parental Leave","Time (1950=0)",
                        #                    "Paid Parental Leave x Time",
                        #                    "ln(GDP Per Capita)",
                        #                    "ln(Net Aid Received)",
                        #                    "Government Expenditures"),
                        # header=FALSE,model.names=FALSE,
                        # model.numbers=FALSE,multicolumn=FALSE,
                        # object.names=TRUE,notes.label="",
                        # column.sep.width="-15pt",order=c(1,2,6,3,4,5),
                        # omit.stat=c("f","ser"),out="RDDMortRegs-24.tex")

# Alternatives:

RDDs<-list("OLS #1"=RDD.OLS1,"OLS #2"=RDD.OLS2,
           "One-Way FE #1"=RDD.1way.1,"One-Way FE #2"=RDD.1way.2,
           "Two-Way FE #1"=RDD.2way.1,"Two-Way FE #2"=RDD.2way.2)

modelsummary(RDDs,output="RDDMortRegs-24.tex",stars=TRUE,
             title="RDD Models of log(Child Mortality)",
             fmt=4,gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("(Intercept)","Paid Parental Leave",
                           "Time (1950=0)","Paid Parental Leave x Time",
                           "ln(GDP Per Capita)","ln(Net Aid Received)",
                           "Government Expenditures"))

# Note that you could also use -texreg-:
# 
# texreg(list(RDD.OLS1,RDD.OLS2,RDD.1way.1,RDD.1way.2,
#             RDD.2way.1,RDD.2way.2),file="RDDMortRegs-24A.tex",
#             caption="",label="")
#
#=-=-=-=-=-=-=-=-=-=-=
# Finally, a differences-in-differences (DiD) example. This
# uses the -did- package, which deals with DiD models where
# there are more than two "periods" and/or staggered
# "treatment" timings.
#
# Ensure YEARNUMERIC is in fact numeric, and sort:

WDI$YearNumeric<-as.numeric(as.character(WDI$YearNumeric))
WDI<-WDI[order(WDI$ISO3,WDI$YearNumeric),] # sort the data

# Create a numeric unit ID variable:
  
WDI$ID<-as.numeric(WDI$ISO3)

# Now define a variable that equals the first year
# in which that country had paid parental leave:

WDI <- WDI %>%
  group_by(ID) %>%
  mutate(foo = ifelse(PaidParentalLeave==1,YearNumeric,NA)) %>%
  mutate(YearPPL = min(foo,na.rm=TRUE)) %>%
  ungroup()

WDI$foo<-NULL # clean up

# Recode those "INFs" to 2023:

WDI$YearPPL<-ifelse(WDI$YearPPL==Inf,2023,WDI$YearPPL)

# Now fit the simple / bivariate DiD model:

DiD.fit1<-att_gt(yname = "lnCM",gname = "YearPPL",idname = "ID",
                tname = "YearNumeric",allow_unbalanced_panel = TRUE,
                xformla = ~1,data = WDI,est_method = "reg")

# Event study object:

DiD.ev1 <- aggte(DiD.fit1,type="dynamic",na.rm=TRUE)

# Make an event study plot:

pdf("DiDEventPlot1-24.pdf",7,4)
par(mar=c(4,4,2,2))
ggdid(DiD.ev1,xgap=10)
dev.off()

# Total group effects:

DiD.grp1<-aggte(DiD.fit1,type="group",na.rm=TRUE)
summary(DiD.grp1)

# Plotted:

pdf("DiDGroupEffects1-24.pdf",5,6)
ggdid(DiD.grp1)
dev.off()

# Finally, add the three control variables:

DiD.fit2<-att_gt(yname = "lnCM",gname = "YearPPL",idname = "ID",
                 tname = "YearNumeric",allow_unbalanced_panel = TRUE,
                 xformla = ~lnGDPPerCap+lnNetAidReceived+GovtExpenditures,
                 data = WDI, est_method = "reg")

# Event study object:

DiD.ev2 <- aggte(DiD.fit2,type="dynamic",na.rm=TRUE)

# Make an event study plot:

pdf("DiDEventPlot2-24.pdf",7,4)
par(mar=c(4,4,2,2))
ggdid(DiD.ev1,xgap=10)
dev.off()

# Total group effects:

DiD.grp2<-aggte(DiD.fit2,type="group",na.rm=TRUE)
summary(DiD.grp2)

# Plotted:

pdf("DiDGroupEffects2-24.pdf",5,6)
ggdid(DiD.grp2)
dev.off()


# /fin