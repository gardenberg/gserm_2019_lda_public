# GSERM Oslo 2019 - Exercise One (Marinov 2005)
#
# Code for data cleaning and model specification to
# begin to replicate Table 1, column 2...

# Get the data...

library(RCurl)

URL<-"https://raw.githubusercontent.com/PrisonRodeo/GSERM-Oslo-2019-git/master/Exercises/GSERM-Oslo-2019-ExOne.csv"
temp<-getURL(URL)
df<-read.csv(textConnection(temp))

summary(df)

# Remove missing data...

# install.packages("dplyr") # <-- uncomment if needed
library(dplyr)
df2 <- filter(df, is.na(fail)==FALSE, is.na(sanctionsl1)==FALSE,
              is.na(growthpc)==FALSE, is.na(lngdppc)==FALSE,
              is.na(democl1)==FALSE, is.na(democlnt)==FALSE,
              is.na(mixedl1)==FALSE, is.na(mixedlnt)==FALSE,
              is.na(age)==FALSE, is.na(ot3)==FALSE,
              is.na(X_spline1)==FALSE,is.na(ccode)==FALSE)

# The formula is:
# 
# fit1 <- glm(fail~sanctionsl1+forcel1+growthpc+lngdppc+democl1+
#              democlnt+mixedl1+mixedlnt+age+ot3+X_spline1+X_spline2+
#              X_spline3, data=df,family=binomial)

# GEE example:
#
# install.packages("geepack") # <-- uncomment if needed
library(geepack)
fitGEE <- geeglm(fail~sanctionsl1+forcel1+growthpc+lngdppc+democl1+
             democlnt+mixedl1+mixedlnt+age+ot3+X_spline1+X_spline2+
             X_spline3,data=df2,id=ccode,family=gaussian,
             corstr="exchangeable")
summary(fitGEE)


