#=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introductory things...                             ####
#
# GSERM - St. Gallen (2024)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day One: Introduction to panel / TSCS data.
#
# The preliminary part of the code loads R packages and
# does some housekeeping. The actual code for the slides
# and things starts around line 50.
#
# This next bit of code takes a list of packages ("P") and (a)
# checks for whether the package is installed or not, (b) installs
# it if it is not, (c) loads each of them, and then (d) prints
# a smiley face if that package was successfully loaded:

P<-c("RCurl","readr","RColorBrewer","colorspace","foreign","psych",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "sf","maps","usmap","mapdata","countrycode","rworldmap",
     "xtsum","stargazer","pscl","naniar","panelView","ExPanDaR",
     "car","ggplot2")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Note: It's a good idea to run that ^ block of code 12-15
# times, until you see all "smiley faces" :)
#
# Next, set a few global options:

options(scipen = 9) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# Set the "working directory": modify / uncomment as necessary:
#
# setwd("~/Dropbox (Personal)/GSERM/Panel-2024/Notes and Slides")
#
# (Note: As a rule, I don't use R "projects" -- for complex 
# reasons -- but look into them if you want an alternative to
# using -setwd-.)
#
#=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Tiny TSCS data example:                            ####

tiny<-read_table("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2024/main/Data/tinyTSCSexample.txt")
tiny

aggXS <- ddply(tiny, .(ID), summarise,
               Year = mean(Year),
               Female = Female[1],
               GOP = mean(GOP),
               Approve = mean(Approve))
aggXS

aggT <- ddply(tiny, .(Year), summarise,
              Female = mean(Female),
              Pres=Pres[1],
              GOP = mean(GOP),
              Approve=mean(Approve))
aggT

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "Dimensions of variation" plots...

toy<-data.frame(ID=as.character(rep(LETTERS[1:4],4)),
                t=append(append(rep(1,4),rep(2,4)),
                         append(rep(3,4),rep(4,4))),
                X=c(1,3,5,7,2,4,6,8,3,5,7,9,4,6,8,10),
                Y=c(20,15,10,5,19,14,9,4,18,13,8,3,17,12,7,2))
toy$label<-paste0("i=",toy$ID,",t=",toy$t)

pdf("VariationScatter1.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
               ))
with(toy, text(X,Y,labels=label,pos=1))
dev.off()

# Means:

mY.ID<-tapply(toy$Y,toy$ID,mean)
mY.t<-tapply(toy$Y,toy$t,mean)
mX.ID<-tapply(toy$X,toy$ID,mean)
mX.t<-tapply(toy$X,toy$t,mean)

# #2

pdf("VariationScatter2.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(h=mY.ID,lty=2)
text(10,mY.ID,label=c("Mean of Y for unit A","Mean of Y for unit B",
                      "Mean of Y for unit C","Mean of Y for unit D"),
      pos=3,cex=0.8)
dev.off()

# #3

pdf("VariationScatter3.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(h=mY.t,lty=2,col=c(1,2,3,4))
text(10,mY.t,label=c("Mean of Y for Time 1","Mean of Y for Time 2",
                      "Mean of Y for Time 3","Mean of Y for Time 4"),
     pos=3,cex=0.8,col=c(1,2,3,4))
dev.off()

# #4

pdf("VariationScatter4.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(v=mX.ID,lty=2)
text(mX.ID,c(20,19,18,17),
     label=c("Mean of X for unit A","Mean of X for unit B",
                      "Mean of X for unit C","Mean of X for unit D"),
     cex=0.8)
dev.off()

# #5

pdf("VariationScatter5.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(v=mX.t,lty=2,col=c(1,2,3,4))
text(mX.t,c(20,19,18,17),
     label=c("Mean of X for Time 1","Mean of X for Time 2",
                     "Mean of X for Time 3","Mean of X for Time 4"),
     cex=0.8,col=c(1,2,3,4))
dev.off()

# Means, within- and between...

with(toy, describe(Y))
Ymeans <- ddply(toy,.(ID),summarise,
                Y=mean(Y))
with(Ymeans, describe(Y)) # between-unit variation

toy <- ddply(toy,.(ID), mutate,
             Ymean=mean(Y))
toy$within <- with(toy, Y-Ymean)
with(toy, describe(within)) # within-unit variation

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Our Running Example: The World Development Indicators   ####
# (a/k/a the "WDI")...
#
# The WDI data we'll be using for this class can
# be created by running the "WDI-MakeData.R"
# script found on the Github repository. You can 
# modify that script to add additional variables
# if you choose to. The data are also available
# in ready-to-use format in the "Data" folder
# on the Github repo; the current file (June 2024) is
# called "WDI24a" (we'll use WDI24b a bit later in
# the course).
#
# Get the data:

wdi<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2024/main/Data/WDI24a.csv")

# Add a "Post-Cold War" variable:

wdi$PostColdWar <- with(wdi,ifelse(Year<1990,0,1))

# Summarize:

describe(wdi,fast=TRUE,ranges=FALSE,check=TRUE)

# Visualize missing data (using the vis_miss routine in
# the -naniar- package; note that because vis_miss 
# creates a ggplot object, we have to modify it
# using those kinds of commands...):

pdf("WDI-Missing.pdf",7,5)
par(mar=c(4,4,2,9))
p <- vis_miss(wdi)
p <- p + theme(text = element_text(size=6),
               plot.margin = margin(t=20,r=50,b=20,l=20))
     
p
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dimensions of variation...                          ####
#
# Next, we'll look at dimensions of variation for
# different variables in the WDI data, using the
# -xtsum- package. (This is similar to a Stata 
# command with the same name.)
#
# First, we'll look at a familiar one: government
# expenditures. Here's a few over-time plots of
# a few different countries (note that this code
# is not super-efficient, in the interest of being
# more transparent):

pdf("GovSpendTS.pdf",7,5)
par(mar=c(4,5,2,2)) # plot margins...
lw<-3
with(wdi[wdi$country=="Switzerland",],
     plot(Year,GovtExpenditures,t="l",lwd=lw,
          xlim=c(1960,2025),ylim=c(0,60),
          ylab="Government Expenditures\n(Percent of GDP)"))
with(wdi[wdi$country=="Libya",],
     lines(Year,GovtExpenditures,lwd=lw,lty=2,
          col="grey32"))
with(wdi[wdi$country=="Norway",],
     lines(Year,GovtExpenditures,lwd=lw,lty=3,
           col="orange"))
with(wdi[wdi$country=="Pakistan",],
     lines(Year,GovtExpenditures,lwd=lw,lty=4,
           col="darkgreen"))
with(wdi[wdi$country=="Chad",],
     lines(Year,GovtExpenditures,lwd=lw,lty=5,
           col="blue"))
legend("topleft",bty="n",lty=c(1,2,3,4,5),lwd=lw,
       col=c("black","grey32","orange","darkgreen","blue"),
       legend=c("Switzerland","Libya","Norway","Pakistan",
                "Chad"))
dev.off()

# "Within," "Between," and overall summary stats, using
# the -xtsum- command (and then generate a LaTeX table
# using -stargazer-):

GE.df<-xtsum(wdi,variables=c("GovtExpenditures"),id="ISO3",t="Year",
      na.rm=TRUE,return.data.frame=TRUE)

stargazer(GE.df,summary=FALSE)

# Next, (logged) inflation...

wdi$lnInflation<-log(wdi$Inflation)

Inf.df<-xtsum(wdi,variables=c("lnInflation"),id="ISO3",t="Year",
             na.rm=TRUE,return.data.frame=TRUE)

stargazer(Inf.df,summary=FALSE)

# OK, now a variable that has **no** temporal variation:
# whether or not a country is in the Middle East:

wdi$MidEast<-ifelse(wdi$Region=="Middle East & North Africa",1,0)

ME.df<-xtsum(wdi,variables=c("MidEast"),id="ISO3",t="Year",
              na.rm=TRUE,return.data.frame=TRUE)

stargazer(ME.df,summary=FALSE)

# Now the same for a variable that has no **cross-sectional**
# variation: the Post-Cold War indicator...

PCW.df<-xtsum(wdi,variables=c("PostColdWar"),id="ISO3",t="Year",
             na.rm=TRUE,return.data.frame=TRUE)

stargazer(PCW.df,summary=FALSE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Regressions with panel data...                    ####
#
# OK regression time!
#
# Plot the relationship between (logged) inflation and
# (logged) government spending:

wdi$lnGovtExp<-log(wdi$GovtExpenditures)

pdf("InflationSpendingScatter.pdf",7,6)
par(mar=c(4,4,2,2))
with(wdi, plot(GovtExpenditures,Inflation,pch=20,cex=0.4,
               log="xy",ylab="Inflation (log scale)",
               xlab="Government Expenditures (logged)"))
dev.off()

# Regression (pooled data):

Inf.fit<-lm(lnInflation~lnGovtExp,data=wdi)
summary(Inf.fit)

# Cross-sectional regression: 1997 only...

wdi97<-wdi[wdi$Year==1997,]

pdf("InfReg1997.pdf",5,6)
par(mar=c(4,4,2,2))
with(wdi97,plot(GovtExpenditures,Inflation,pch="",cex=0.8,
               log="xy",ylab="Inflation (log scale)",
               xlab="Government Expenditures (logged)"))
with(wdi97,text(GovtExpenditures,Inflation,labels=wdi97$ISO3,cex=0.5))
dev.off()

Inf.fit97<-lm(lnInflation~lnGovtExp,data=wdi97)
summary(Inf.fit97)

# Now do the same for every year in the data, 
# 1960-2023:

# Now repeat for every term in the data:

intercepts<-numeric(length(unique(wdi$Year))) # place for B0s
slopes<-numeric(length(unique(wdi$Year)))     # place for B1s
sees<-numeric(length(unique(wdi$Year)))       # place for se(B1)s

start<-min(unique(wdi$Year))
end<-max(unique(wdi$Year))

for(i in start:end) {
  reg<-lm(lnInflation~lnGovtExp,data=wdi[wdi$Year==i,])
  intercepts[i-start+1]<-reg$coefficients[1]
  slopes[i-start+1]<-reg$coefficients[2]
  sees[i-start+1]<-sqrt(anova(reg)$`Mean Sq`[2])
}

# Plots:

Year<-seq(start,end)

pdf("Inf-Beta0s.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Year,intercepts,t="l",lwd=1.5,
     ylab="Estimated Intercept")
abline(h=Inf.fit$coefficients[1],lty=2)
dev.off()

pdf("Inf-Beta1s.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Year,slopes,t="l",lwd=1.5,
     ylab="Estimated Slope")
abline(h=Inf.fit$coefficients[2],lty=2)
dev.off()

pdf("Inf-SEEs.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Year,sees,t="l",lwd=1.5,
     ylab="Estimated SEE")
abline(h=sqrt(anova(Inf.fit)$`Mean Sq`[2]),lty=2)
dev.off()

# Next, country-specific regressions, e.g., for Switzerland:

CHE<-wdi[wdi$country=="Switzerland",]

pdf("InfRegCHE.pdf",5,6)
par(mar=c(4,4,2,2))
with(CHE,plot(GovtExpenditures,Inflation,pch="",cex=0.8,
                log="xy",ylab="Inflation (log scale)",
                xlab="Government Expenditures (logged)"))
with(CHE,text(GovtExpenditures,Inflation,labels=CHE$Year,cex=0.5))
dev.off()

Inf.fitCHE<-lm(lnInflation~lnGovtExp,data=CHE)
summary(Inf.fitCHE)

# Now let's do this for every different country in 
# the data (with some code to ensure that we throw out
# regressions that can't be fit -- e.g., because of 
# missing data):

index<-unique(wdi$ISO3)
results<-list()       # regression results

for(iso in index) {
  df<-subset(wdi,ISO3==iso)
  out<-tryCatch({          # tryCatch is useful here
       reg<-lm(lnInflation~lnGovtExp,data=df)
       list(group=iso,model=reg)
       },
       error=function(e) {cat("Skipping",iso," ")
                NULL})
  if (!is.null(out)) {
    results[[iso]] <- out
  }
}

# Gather results from list of regressions:

ISO<-character()
B0<-numeric()
B1<-numeric()
Rsq<-numeric()

for(i in 1:length(results)) {
  ISO<-c(ISO,results[[i]]$group)
  B0<-c(B0,results[[i]]$model$coefficients[1])
  B1<-c(B1,results[[i]]$model$coefficients[2])
  Rsq<-c(Rsq,summary(results[[i]]$model)$r.squared)
}

DFbyISO<-data.frame(ISO=ISO,B0=B0,B1=B1,Rsq=Rsq)

# Delete one outlier - San Marino (it had a very small
# N and wildly large estimates) - for plotting:

DFbyISO<-DFbyISO[DFbyISO$ISO!="SMR",]

# Plots:

pdf("CountryB0B1.pdf",7,6)
par(mar=c(4,4,2,2))
with(DFbyISO,plot(B0,B1,pch="",xlab="Estimated Intercepts",
                  ylab="Estimates Slopes"))
abline(h=0,lty=2,col="grey")
abline(v=0,lty=2,col="grey")
with(DFbyISO,text(B0,B1,labels=DFbyISO$ISO,cex=0.7))
dev.off()

pdf("CountryB1R2.pdf",7,6)
par(mar=c(4,4,2,2))
with(DFbyISO,plot(B1,Rsq,pch="",xlim=c(-10,10),
                  xlab="Estimated Slopes",
                  ylab="Estimates R-Squareds"))
abline(v=0,lty=2,col="grey")
with(DFbyISO,text(B1,Rsq,labels=DFbyISO$ISO,cex=0.7))
dev.off()

# Finally, a WORLD MAP of the slopes from the country-specific 
# regressions (because everyone likes maps!). This map is
# created using the -rworldmap- package:

mapData<-joinCountryData2Map(DFbyISO,joinCode="ISO3",
                             nameJoinColumn="ISO",
                             mapResolution="low")

pdf("SlopesMap.pdf",8,6)
par(mar=c(1,1,1,1))
MAP<-mapCountryData(mapData,nameColumnToPlot="B1",
               mapTitle="Estimated Slopes",
               addLegend="FALSE",
               colourPalette=c("darkblue","blue","lightblue","grey64",
                               "goldenrod1","orange","darkorange3"))
do.call(addMapLegend,c(MAP,legendLabels="all",
                       legendWidth=0.5,digits=2,
                       labelFontSize=0.8,legendMar=4))
dev.off()

pdf("R2Map.pdf",8,6)
par(mar=c(1,1,1,1))
MAP<-mapCountryData(mapData,nameColumnToPlot="Rsq",
                    mapTitle="Estimated R-Squareds",
                    addLegend="FALSE",catMethod="pretty",
                    colourPalette=c("grey84","grey64","grey48","grey36",
                                    "grey30","grey16","black"))
do.call(addMapLegend,c(MAP,legendLabels="all",digits=2,
                       legendWidth=0.5,sigFigs=2,
                       labelFontSize=0.8,legendMar=4))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Visualization: Using ExPanDaR                     ####

write.csv(wdi,"WDIPanelData.csv")
ExPanD()

# ... and then proceed interactively; it's pretty
# straightforward...
#
# fin
