library(ggplot2)
library(dplyr)
setwd("~/Documents/Berkeley/Stat222/RedwoodProject/jcpalumbo3-work/sensor")
net=read.csv("./data/sonoma-data-net.csv")
log=read.csv("./data/sonoma-data-log.csv")
all=read.csv("./data/sonoma-data-all.csv")



###############################################################################

### NET ###

###############################################################################

#Look at sensor 122 with NAs since it suggests sensor isn't working properly 
unique(net[rowSums(is.na(net)) > 0,]$nodeid)

#Node 122 has only NA values; remove since node isn't working properly
node122=net[net$nodeid==122,]
ggplot(node122)+geom_line(aes(x=epoch,y=humidity)) #NA
ggplot(node122)+geom_line(aes(x=epoch,y=humid_temp)) #NA 
ggplot(node122)+geom_line(aes(x=epoch,y=hamatop)) #NA
ggplot(node122)+geom_line(aes(x=epoch,y=hamabot)) #NA
ggplot(node122)+geom_line(aes(x=epoch,y=voltage)) #incredibly variable
net=net[net$nodeid!=122,]

### 1. Humidity ###

#Let's first take a look at humidity vs time 
ggplot(net)+geom_line(aes(x=epoch,y=humidity,colour=nodeid))

#Several negative values and really large values so let's subset by these
negHumidity=net[net$humidity < 0,]
posHumidity=net[net$humidity > 105,]

#Nodes attributed to the negative humidity readings: 78, 123, 141
unique(negHumidity$nodeid)
#Nodes attributed to the very high humidity readings: 3, 118, 145
unique(posHumidity$nodeid)

#Strange readings; cleaner readings in log file so delete the node in net   
node78=net[net$nodeid==78,]
ggplot(node78)+geom_line(aes(x=epoch,y=humidity)) #strange behavior @ 3500 epoch
ggplot(node78)+geom_line(aes(x=epoch,y=humid_temp))  #increases rapidly @ 3250 epoch
ggplot(node78)+geom_line(aes(x=epoch,y=hamatop)) #strange behavior @ 3500 epoch
ggplot(node78)+geom_line(aes(x=epoch,y=hamabot)) #strange behavior @ 3500 epoch
ggplot(node78)+geom_line(aes(x=epoch,y=voltage)) #weird readings
net=net[net$nodeid!=78,]

#Strange readings; cleaner readings in log file so delete the node in net 
node123=net[net$nodeid==123,]
ggplot(node123)+geom_line(aes(x=epoch,y=humidity)) #negative values/big fluctuation 
ggplot(node123)+geom_line(aes(x=epoch,y=humid_temp)) #strange behavior @ 5000 epoch  
ggplot(node123)+geom_line(aes(x=epoch,y=hamatop)) #looks okay 
ggplot(node123)+geom_line(aes(x=epoch,y=hamabot)) #looks okay 
ggplot(node123)+geom_line(aes(x=epoch,y=voltage)) #looks okay
net=net[net$nodeid!=123,]

#Strange readings; cleaner readings in log file so delete the node in net   
node141=net[net$nodeid==141,]
ggplot(node141)+geom_line(aes(x=epoch,y=humidity)) #negative value @ 9000 epoch 
ggplot(node141)+geom_line(aes(x=epoch,y=humid_temp)) #strange behavior @ 9000 epoch  
ggplot(node141)+geom_line(aes(x=epoch,y=hamatop)) #looks okay
ggplot(node141)+geom_line(aes(x=epoch,y=hamabot)) #looks okay
ggplot(node141)+geom_line(aes(x=epoch,y=voltage)) #flatline
net=net[net$nodeid!=141,]

#Strange readings; cleaner readings in log file so delete the node in net  
node3=net[net$nodeid==3,]
ggplot(node3)+geom_line(aes(x=epoch,y=humidity)) #looks okay; just one outlier
ggplot(node3)+geom_line(aes(x=epoch,y=humid_temp)) #strange behavior @ 3700   
ggplot(node3)+geom_line(aes(x=epoch,y=hamatop)) #looks okay
ggplot(node3)+geom_line(aes(x=epoch,y=hamabot)) #looks okay
ggplot(node3)+geom_line(aes(x=epoch,y=voltage)) #looks okay
net=net[net$nodeid!=3,]

#Actually looks pretty good; maybe just one outlier
node118=net[net$nodeid==118,]
ggplot(node118)+geom_line(aes(x=epoch,y=humidity)) 
ggplot(node118)+geom_line(aes(x=epoch,y=humid_temp))  
ggplot(node118)+geom_line(aes(x=epoch,y=hamatop))
ggplot(node118)+geom_line(aes(x=epoch,y=hamabot)) 
ggplot(node118)+geom_line(aes(x=epoch,y=voltage)) 

#Strange readings; cleaner readings in log file so delete the node in net  
node145=net[net$nodeid==145,]
ggplot(node145)+geom_line(aes(x=epoch,y=humidity)) #strange behavior @ 3500 epoch
ggplot(node145)+geom_line(aes(x=epoch,y=humid_temp)) #strange behavior @ 3500 epoch  
ggplot(node145)+geom_line(aes(x=epoch,y=hamatop)) #looks okay
ggplot(node145)+geom_line(aes(x=epoch,y=hamabot)) #looks okay
ggplot(node145)+geom_line(aes(x=epoch,y=voltage)) #flatline
net=net[net$nodeid!=145,]

#Looks a lot smoother now 
net=net[net$humidity!=max(posHumidity$humidity),]
ggplot(net)+geom_line(aes(x=epoch,y=humidity,colour=nodeid))


### 2. Temperature ###

#Let's take a look at temperature vs time 
ggplot(net)+geom_line(aes(x=epoch,y=humid_temp,colour=nodeid))

#Couple high values so let's subset by these
posTemp=net[net$humid_temp > 30,]

#Two nodes attributed to the high temperature readings: 119, 127
unique(posTemp$nodeid)

#Actually looks pretty good; maybe just one weird point 
node119=net[net$nodeid==119,]
ggplot(node119)+geom_line(aes(x=epoch,y=humidity)) 
ggplot(node119)+geom_line(aes(x=epoch,y=humid_temp))  
ggplot(node119)+geom_line(aes(x=epoch,y=hamatop))
ggplot(node119)+geom_line(aes(x=epoch,y=hamabot)) 
ggplot(node119)+geom_line(aes(x=epoch,y=voltage)) 

#Actually looks pretty good; maybe just one weird point 
node127=net[net$nodeid==127,]
ggplot(node127)+geom_line(aes(x=epoch,y=humidity)) 
ggplot(node127)+geom_line(aes(x=epoch,y=humid_temp))  
ggplot(node127)+geom_line(aes(x=epoch,y=hamatop))
ggplot(node127)+geom_line(aes(x=epoch,y=hamabot)) 
ggplot(node127)+geom_line(aes(x=epoch,y=voltage)) 


### 3. Adjusted Humidity ###

#Nothing seems that unusual 
ggplot(net)+geom_line(aes(x=epoch,y=humid_adj,colour=nodeid))


### 4. Incident PAR ###

#Nothing seems that unusual 
ggplot(net)+geom_line(aes(x=epoch,y=hamatop,colour=nodeid))


### 5. Reflected PAR ###

#Nothing seems that unusual 
ggplot(net)+geom_line(aes(x=epoch,y=hamabot,colour=nodeid))


### 6. Voltage ###

#Let's take a look at voltage vs time 
ggplot(net)+geom_line(aes(x=epoch,y=voltage,colour=nodeid))

#High values so let's subset by these
posVolt=net[net$voltage > 1000,]

#Two nodes attributed to the high temperature readings: 134, 135
unique(posVolt$nodeid)

#We know that their values for other variables are okay; voltage must just be off
node134=net[net$nodeid==134,]
ggplot(node134)+geom_line(aes(x=epoch,y=voltage)) 

node135=net[net$nodeid==135,]
ggplot(node135)+geom_line(aes(x=epoch,y=voltage)) 

net[net$nodeid==134,]$voltage=NA
net[net$nodeid==135,]$voltage=NA

#Much better
ggplot(net)+geom_line(aes(x=epoch,y=voltage,colour=nodeid))



###############################################################################

### Log ###

###############################################################################

### 1. Humidity ###

#Let's first take a look at humidity vs time 
ggplot(log)+geom_line(aes(x=epoch,y=humidity,colour=nodeid))

#Several negative values so let's subset by these
negHumidity=log[log$humidity < 0,]

#Nodes attributed to the negative humidity readings: 29, 198, 65535
unique(negHumidity$nodeid)

#Remove humidity, humid_temp, humid_adj since flatline; other values look okay 
node29=log[log$nodeid==29,]
ggplot(node29)+geom_line(aes(x=epoch,y=humidity)) #flatline
ggplot(node29)+geom_line(aes(x=epoch,y=humid_temp))  #flatline
ggplot(node29)+geom_line(aes(x=epoch,y=hamatop)) #looks okay
ggplot(node29)+geom_line(aes(x=epoch,y=hamabot)) #looks okay
ggplot(node29)+geom_line(aes(x=epoch,y=voltage)) #low voltage readings
log[log$nodeid==29,]$humidity=NA
log[log$nodeid==29,]$humid_temp=NA
log[log$nodeid==29,]$humid_adj=NA

#Remove humidity, humid_temp, humid_adj outliers
node198=log[log$nodeid==198,]
ggplot(node198)+geom_line(aes(x=epoch,y=humidity)) #huge negative value @ 3500 epoch 
ggplot(node198)+geom_line(aes(x=epoch,y=humid_temp))  #huge negative val @ 3500 epoch
ggplot(node198)+geom_line(aes(x=epoch,y=hamatop)) #huge positive value @ 3500 epoch 
ggplot(node198)+geom_line(aes(x=epoch,y=hamabot)) #huge positive value @ 3500 epoch 
ggplot(node198)+geom_line(aes(x=epoch,y=voltage)) #voltage dives after 3000 epoch 
log[log$nodeid==198 & log$humidity<0,]$humid_temp=NA
log[log$nodeid==198 & log$humidity<0,]$humid_adj=NA
log[log$nodeid==198 & log$humidity<0,]$hamatop=NA
log[log$nodeid==198 & log$humidity<0,]$hamabot=NA
log[log$nodeid==198 & log$humidity<0,]$humidity=NA

#Only a single point but huge outliers
node65535=log[log$nodeid==65535,]
ggplot(node65535)+geom_point(aes(x=epoch,y=humidity)) #huge negative value 
ggplot(node65535)+geom_point(aes(x=epoch,y=humid_temp))  #huge positive val 
ggplot(node65535)+geom_point(aes(x=epoch,y=hamatop)) #huge positive value  
ggplot(node65535)+geom_point(aes(x=epoch,y=hamabot)) #huge positive value  
ggplot(node65535)+geom_point(aes(x=epoch,y=voltage)) #no voltage
log=log[log$nodeid!=65535,]


### 2. Temperature ###

#Nothing seems that unusual 
ggplot(log)+geom_line(aes(x=epoch,y=humid_temp,colour=nodeid))


### 3. Adjusted Humidity ###

#Nothing seems that unusual 
ggplot(log)+geom_line(aes(x=epoch,y=humid_adj,colour=nodeid))


### 4. Incident PAR ###

#Nothing seems that unusual 
ggplot(log)+geom_line(aes(x=epoch,y=hamatop,colour=nodeid))

#Several negative values so let's subset by these
posHamatop=log[log$hamatop > 150000,]

#Node 40 attributed to the highly positive hamatop readings
unique(posHamatop$nodeid)

#Remove node 40 hamatop and hamabot readings
node40=log[log$nodeid==40,]
ggplot(node40)+geom_line(aes(x=epoch,y=hamatop)) #strange behavior @ 50 epoch  
ggplot(node40)+geom_line(aes(x=epoch,y=hamabot)) #lots of 0 readings 
log[log$nodeid==40,]$hamatop=NA
log[log$nodeid==40,]$hamabot=NA


### 5. Reflected PAR ###

#Nothing seems that unusual 
ggplot(log)+geom_line(aes(x=epoch,y=hamabot,colour=nodeid))


### 6. Voltage ###

#Let's take a look at voltage vs time 
ggplot(log)+geom_line(aes(x=epoch,y=voltage,colour=nodeid))

#Low values so let's subset by these
lowVolt=log[log$voltage < 2,]

#Nodes attributed to the low voltage readings: 29, 128, 134, 135, 141, 142, 143, 145
unique(lowVolt$nodeid)

node29=log[log$nodeid==29,]
ggplot(node29)+geom_line(aes(x=epoch,y=voltage)) #low readings; variable 

node128=log[log$nodeid==128,]
ggplot(node128)+geom_line(aes(x=epoch,y=voltage)) #flatline

node134=log[log$nodeid==134,]
ggplot(node134)+geom_line(aes(x=epoch,y=voltage)) #flatline

node135=log[log$nodeid==135,]
ggplot(node135)+geom_line(aes(x=epoch,y=voltage)) #flatline

node141=log[log$nodeid==141,]
ggplot(node141)+geom_line(aes(x=epoch,y=voltage)) #flatline

node142=log[log$nodeid==142,]
ggplot(node142)+geom_line(aes(x=epoch,y=voltage)) #flatline

node143=log[log$nodeid==143,]
ggplot(node143)+geom_line(aes(x=epoch,y=voltage)) #flatline

node145=log[log$nodeid==145,]
ggplot(node145)+geom_line(aes(x=epoch,y=voltage)) #flatline

log[log$nodeid==29,]$voltage=NA
log[log$nodeid==128,]$voltage=NA
log[log$nodeid==134,]$voltage=NA
log[log$nodeid==135,]$voltage=NA
log[log$nodeid==141,]$voltage=NA
log[log$nodeid==142,]$voltage=NA
log[log$nodeid==143,]$voltage=NA
log[log$nodeid==145,]$voltage=NA



###############################################################################

### Delete Duplicate Values ###

###############################################################################

#Remove exact duplicate values 
allClean=bind_rows(net,log)
allClean=group_by(allClean,nodeid,epoch) %>% mutate(count=n())
max(allClean$count)
cleanDup=filter(allClean,count==2|count==3|count==4|count==5)
uniqCleanDup=cleanDup[!duplicated(cleanDup[,colnames(cleanDup)[2:12]]),]
allClean=setdiff(allClean,cleanDup) %>% bind_rows(uniqCleanDup)
allClean=allClean %>% arrange(epoch,nodeid)
allClean=allClean[,1:11]

#Remove doubles 
allClean=group_by(allClean,nodeid,epoch) %>% mutate(count=n())
max(allClean$count)
cleanDup2=filter(allClean,count==2)
allClean=setdiff(allClean,cleanDup2)
for(i in seq(1,nrow(cleanDup2),2)){
  for(j in 5:11){
    if(is.na(cleanDup2[i,j])&is.na(cleanDup2[(i+1),j])){
      cleanDup2[i,j]=NA
    }
    else if(is.na(cleanDup2[i,j])|is.na(cleanDup2[(i+1),j])){
      nonNA=c(cleanDup2[i,j],cleanDup2[(i+1),j])
      cleanDup2[i,j]=nonNA[which(!is.na(nonNA))]
    }
    else if(cleanDup2[i,j]!=cleanDup2[(i+1),j]){
      cleanDup2[i,j]=colMeans(cleanDup2[(i):(i+1),j],na.rm=TRUE)
    }
  }
  cleanDup2[(i+1),]=NA
}
cleanDup2=cleanDup2[!is.na(cleanDup2$epoch),]
allClean=bind_rows(allClean,cleanDup2)
allClean=allClean %>% arrange(epoch,nodeid)

#Remove triples 
cleanDup3=filter(allClean,count==3)
allClean=setdiff(allClean,cleanDup3)
for(i in seq(1,nrow(cleanDup3),3)){
  for(j in 5:11){
    if(cleanDup3[i,j]!=cleanDup3[(i+1),j]){
      cleanDup3[i,j]=colMeans(cleanDup3[(i):(i+2),j],na.rm=TRUE)
    }
  }
  cleanDup3[(i+1),]=NA
  cleanDup3[(i+2),]=NA
}
cleanDup3=cleanDup3[!is.na(cleanDup3$epoch),]
allClean=bind_rows(allClean,cleanDup3)
allClean=allClean %>% arrange(epoch,nodeid)

ggplot(allClean)+geom_line(aes(x=epoch,y=humidity,colour=nodeid))
ggplot(allClean)+geom_line(aes(x=epoch,y=humid_temp,colour=nodeid))
ggplot(allClean)+geom_line(aes(x=epoch,y=humid_adj,colour=nodeid))
ggplot(allClean)+geom_line(aes(x=epoch,y=hamatop,colour=nodeid))
ggplot(allClean)+geom_line(aes(x=epoch,y=hamabot,colour=nodeid))
ggplot(allClean)+geom_line(aes(x=epoch,y=voltage,colour=nodeid))

write.csv(allClean, file = "./data/sensor-data-all-cleaned.csv")
write.csv(net, file = "./data/sensor-data-net-cleaned.csv")
write.csv(log, file = "./data/sensor-data-log-cleaned.csv")
