##################################################################
###
### Alison Ketz
### 6/4/2019
###
### Animal social networks males
###


### preliminaries

rm(list=ls())


setwd("~/Documents/Animal_Social_Networks/asn_wtd_male")


library(Hmisc)
library(lubridate)
library(xlsx)
library(gridExtra)
library(xtable)
library(ggplot2)
library(nimble)
library(Matrix)
library(beepr)
library(coda)
library(RColorBrewer)
library(adehabitatLT)
library(asnipe)
library(spatstat)


###########################################################################################
###
### Reading in GPS data of bucks
###
###########################################################################################

###
### set working directory to where the data csv files are
###

setwd("~/Documents/Data/GPS_vectronics")

###
### Read data GPS
###

datalist_temp = list.files(pattern="*.csv")
myfiles = lapply(datalist_temp, read.csv,header=TRUE,stringsAsFactors=FALSE)
sizes=sapply(myfiles,function(x){dim(x)[1]})
myfiles=lapply(myfiles,function(x){
  x=x[,c(2:17,44,46:48)]
  x
})

#reset wd
setwd("~/Documents/Animal_Social_Networks/asn_wtd_male")


###
### downloading capture data
###

d.cap=mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Adult_Capture_2017_2018")
d.fawncap=mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Fawn Capture")
d.mort=mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Mortalities")
d.cens=mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Censor")
d.cwd=mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "RAMALT")
d.post.cwd=mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Postmortem_CWD")
d.tooth =mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Tooth")

#changing column names
names(d.cap)=tolower(gsub("[[:punct:]]","",names(d.cap)))
names(d.fawncap)=tolower(gsub("[[:punct:]]","",names(d.fawncap)))
names(d.mort)=tolower(gsub("[[:punct:]]","",names(d.mort)))
names(d.cens)=tolower(gsub("[[:punct:]]","",names(d.cens)))
names(d.cwd)=tolower(gsub('[[:punct:]]',"",names(d.cwd)))
names(d.post.cwd)=tolower(gsub('[[:punct:]]',"",names(d.post.cwd)))
names(d.tooth)=tolower(gsub('[[:punct:]]',"",names(d.tooth)))

### Double check loading correctly

head(d.cap)
head(d.fawncap)
head(d.mort)
head(d.cens)
head(d.cwd)
head(d.post.cwd)
head(d.tooth)

###
### Number individuals within each dataframe
###

n.cap = dim(d.cap)[1]
n.cens = dim(d.cens)[1]
n.fawncap=dim(d.fawncap)[1]
n.mort = dim(d.mort)[1]
n.cwdtest=dim(d.cwd)[1]
n.postcwd=dim(d.post.cwd)[1]
n.tooth = dim(d.tooth)[1]

#change class of CWD tests to integer for d.cwd
class(d.cwd$lowtag)="integer"


###
### combining all GPS data into 1 dataframe
###

df.gps = do.call("rbind",myfiles)

df.gps=myfiles[[1]]
df.gps=df.gps[!duplicated(df.gps),]
df.gps=df.gps[39:dim(df.gps)[1],]
df.gps=df.gps[which(df.gps$dop<10),]
df.gps=df.gps[!is.na(df.gps$longitude),]

df.gps$daytime <- as.POSIXct(strptime(paste(df.gps$lmt_date,df.gps$lmt_time),format="%m/%d/%Y %I:%M:%S %p"))

#Create time lag between successive locations to censor data if needed.
timediff <- diff(df.gps$daytime)
df.gps <- df.gps[-1,]
df.gps$timediff <-as.numeric(abs(timediff))


#Remove outlier locations or known outliers collected too far apart in time
# newmuleys <-subset(muleys, muleys$Long > -110.50 & muleys$Lat > 37.3 & muleys$Long < -107)
# muleys <- newmuleys
# str(muleys)



# setup coordinates
coords = cbind(df.gps$longitude, df.gps$latitude)
sp = SpatialPoints(coords)
head(sp)
# make spatial data frame
spdf = SpatialPointsDataFrame(sp,df.gps)

# EPSG strings
latlong = "+init=epsg:4326"
proj4string(spdf) = CRS(latlong)

df.gps.sp.proj = spTransform(spdf, CRS("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000+y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
df.gps=data.frame(df.gps.sp.proj,df.gps)

#load into adehabitat
df.gps.traj <- as.ltraj(cbind(df.gps$coords.x1,df.gps$coords.x2), date=df.gps$daytime,id=df.gps$collarid)

plot(df.gps.traj)
head(df.gps.traj)
#converts traj object to data frame
df.gps.traj.df = ld(df.gps.traj)