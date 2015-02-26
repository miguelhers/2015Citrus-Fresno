# ------------------------------------------------------
# Modelling daily temperatures
# Date 25 FEB 
#-------------------------------------------------------


# Load libraries:

rm(list=ls(all=TRUE))

packages <- c("tools","xts","data.table","lubridate","zoo","TSA")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
for (p in packages){print(p)}

library(tools)
library(xts)
library(data.table)
library(lubridate)
library(z00)
library(TSA)

# 1. Import the Data.

file <- "C:/Users/Miguel Herschberg/Desktop/2015 Citrus/Daily Data/Daily1948_2015.csv"
data = read.table(file,sep=",",header=TRUE,na.strings = "-9999", stringsAsFactors = FALSE)
head(data)


#2. Change the date to actual date format
data$DATE = ymd(data$DATE)

# 3. Check for NA -- Here we don't have any outliers
anyNA(c(data$DATE,data$TMAX,data$TMIN))
which(is.na(data$TMIN))

setDT(data)

# 4. Add column of TAverage
data[,TAVG:={temp<-(TMAX+TMIN); temp/2}]
#5. Change it into a time series object
ts=xts(data,order.by=data$DATE)

#6. Remove unwanted columns
tsFresno=ts[,c(7,8,9)]
head(tsFresno) #tsFresno is our final time series


#7. plot the time series.

#7.1 This plots the TMIN for 2006-2008 where we can observe the 2007 frost
plot(as.zoo(tsFresno$TMIN[21001:21911]), plot.type='single',ylim = c(-30, 260),xlab="Time",ylab="TMIN")
title("TMIN FRESNO 2006-2008")
abline(h=-22)

#7.2 Plot all temperatures
plot(as.zoo(tsFresno[21490:21611,]), plot.type='single',ylim = c(-50, 250),xlab="Time",ylab="TMIN")
title("FRESNO WINTER 2007")
abline(h=-22)

#8. Periodograms:
#8.1 Using http://www.inside-r.org/packages/cran/TSA/docs/periodogram 
periodogram(as.numeric(tsFresno$TMIN))
periodogram(as.numeric(tsFresno$TMIN),log=YES)

#8.2 https://stat.ethz.ch/R-manual/R-patched/library/stats/html/spec.pgram.html
spec.pgram(as.numeric(tsFresno$TMIN)) #Raw Periodogram
spec.pgram(as.numeric(tsFresno$TMIN), taper=0, log="no") #No smoothing
spec.pgram(as.numeric(tsFresno$TMIN), taper=0, log="no",spans=4) #With smoothing
