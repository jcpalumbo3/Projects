###############################################################################

### Exploratory Data Analysis ###

###############################################################################
require(gridExtra)
library(ggplot2)
library(dplyr)
allClean=read.csv("./data/sensor-data-all-cleaned.csv")
log=read.csv("./data/sensor-data-log-cleaned.csv")
net=read.csv("./data/sensor-data-net-cleaned.csv")

#Join the cleaned dataset and the mote location data
loc=read.table("./data/mote-location-data.txt", 
               sep="\t", 
               col.names=c("nodeid","height","direc","dist","tree"), 
               fill=FALSE,
               header=TRUE,
               strip.white=TRUE)
loc=arrange(loc,nodeid)
allClean=left_join(allClean,loc,by="nodeid")
net=left_join(net,loc,by="nodeid")
log=left_join(log,loc,by="nodeid")



### Data Overview ###

#Summary statistics; Shows that edge has far fewer observations than interior so
#will focus analysis on the interior tree 
summary(allClean)
intTree=allClean[allClean$tree=="interior",]
may1=intTree[intTree$epoch>=947 & intTree$epoch<=1235,]

#Noticed while cleaning the datasets that a lot of plots weren't for entire range
#The histogram below shows that data becomes increasingly skewed towards nodes 
#higher in the tree. 
plot1=ggplot(na.exclude(intTree),aes(x=epoch,fill=as.factor(height),order=-as.numeric(height))) +
  geom_histogram(binwidth=100) + 
  scale_fill_discrete("Height") + guides(fill=guide_legend(ncol=1)) +
  theme(legend.key.size=unit(.25,"cm"),legend.key.height=unit(.25,"cm")) 
plot2=ggplot(na.exclude(intTree),aes(x=epoch,fill=as.factor(height),order=-as.numeric(height))) +
  geom_histogram(binwidth=100,position="fill") + 
  scale_fill_discrete("Height",guide=FALSE)
grid.arrange(plot1,plot2,ncol=2)

#Data retrieved over the network 
ggplot(na.exclude(net[net$tree=="interior",]),aes(x=epoch,fill=as.factor(height),order=-as.numeric(height))) +
  geom_histogram(binwidth=100) + 
  scale_fill_discrete("Height") + guides(fill=guide_legend(ncol=1)) +
  theme(legend.key.size=unit(.25,"cm"),legend.key.height=unit(.25,"cm")) 

#Data retrieved in the local log 
ggplot(na.exclude(log[log$tree=="interior",]),aes(x=epoch,fill=as.factor(height),order=-as.numeric(height))) +
  geom_histogram(binwidth=100) + 
  scale_fill_discrete("Height") + guides(fill=guide_legend(ncol=1)) +
  theme(legend.key.size=unit(.25,"cm"),legend.key.height=unit(.25,"cm")) 



### Plot each variable vs time, colored by height/direction ###

#Humidity
plot1=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=humidity,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=humidity,colour=height))
plot2=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=humidity,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=humidity,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Humid_temp
plot1=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=humid_temp,colour=height))
plot2=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=humid_temp,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=humid_temp,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Humid_adj
plot1=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=humid_adj,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=humid_adj,colour=height))
plot2=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=humid_adj,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=humid_adj,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Hamatop
plot1=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=hamatop,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=hamatop,colour=height))
plot2=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=hamatop,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=hamatop,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Hamabot
plot1=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=hamabot,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=hamabot,colour=height))
plot2=ggplot(na.exclude(intTree))+geom_point(aes(x=epoch,y=hamabot,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=hamabot,colour=direc))
grid.arrange(plot1,plot2,ncol=2)



### Plot each variable vs time for May 1st, colored by height/direction ###

#Humidity
plot1=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humidity,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=humidity,colour=height))
plot2=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humidity,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=humidity,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Humid_temp
plot1=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=humid_temp,colour=height))
plot2=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humid_temp,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=humid_temp,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Humid_adj
plot1=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humid_adj,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=humid_adj,colour=height))
plot2=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humid_adj,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=humid_adj,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Hamatop
plot1=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=hamatop,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=hamatop,colour=height))
plot2=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=hamatop,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=hamatop,colour=direc))
grid.arrange(plot1,plot2,ncol=2)

#Hamabot
plot1=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=hamabot,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=epoch,y=hamabot,colour=height))
plot2=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=hamabot,colour=direc))+
  scale_colour_brewer(palette="YlGn")+
  geom_jitter(aes(x=epoch,y=hamabot,colour=direc))
grid.arrange(plot1,plot2,ncol=2)



### Plot each variable vs time for May 1st/3 nodes, colored by height ###

intTreeNodes=sort(unique(na.exclude(intTree$height)))
# low=intTreeNodes[1:10]
# mid=intTreeNodes[11:21]
# high=intTreeNodes[22:31]
# sample(low,1); sample(mid,1); sample(high,1) #24.4, 54, 63.5

nodes=may1[may1$height==22.9 | may1$height==52.1 | may1$height==66.5,]
#nodes=may1[may1$height==24.4 | may1$height==54 | may1$height==63.5,]

#Humidity
ggplot(na.exclude(nodes))+geom_line(aes(x=epoch,y=humidity,colour=height))+
  geom_jitter(aes(x=epoch,y=humidity,colour=height))

#Humid_temp
ggplot(na.exclude(nodes))+geom_line(aes(x=epoch,y=humid_temp,colour=height))+
  geom_jitter(aes(x=epoch,y=humid_temp,colour=height))

#Humid_adj
ggplot(na.exclude(nodes))+geom_line(aes(x=epoch,y=humid_adj,colour=height))+
  geom_jitter(aes(x=epoch,y=humid_adj,colour=height))

#Hamatop
ggplot(na.exclude(nodes))+geom_point(aes(x=epoch,y=hamatop,colour=height))+
  geom_jitter(aes(x=epoch,y=hamatop,colour=height))

#Hamabot
ggplot(na.exclude(nodes))+geom_point(aes(x=epoch,y=hamabot,colour=height))+
  geom_jitter(aes(x=epoch,y=hamabot,colour=height))



### Temp vs other variables, colored by height ###

#Humidity
ggplot(na.exclude(intTree))+geom_point(aes(x=humidity,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=humidity,y=humid_temp,colour=height))

#Humid_adj
ggplot(na.exclude(intTree))+geom_point(aes(x=humid_adj,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=humid_adj,y=humid_temp,colour=height))

#Hamatop
ggplot(na.exclude(intTree))+geom_point(aes(x=hamatop,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=hamatop,y=humid_temp,colour=height))

#Hamabot
ggplot(na.exclude(intTree))+geom_point(aes(x=hamabot,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=hamabot,y=humid_temp,colour=height))



### Temp vs other variables for May 1st, colored by height ###

#Humidity
ggplot(na.exclude(may1))+geom_point(aes(x=humidity,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=humidity,y=humid_temp,colour=height))

#Humid_adj
ggplot(na.exclude(may1))+geom_point(aes(x=humid_adj,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=humid_adj,y=humid_temp,colour=height))

#Hamatop
ggplot(na.exclude(may1))+geom_point(aes(x=hamatop,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=hamatop,y=humid_temp,colour=height))

#Hamabot
ggplot(na.exclude(may1))+geom_point(aes(x=hamabot,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  geom_jitter(aes(x=hamabot,y=humid_temp,colour=height))



### Plots to be used in final report ###

# Finding 1
pdf(file="./graphs/finding1.pdf")
plot1=ggplot(na.exclude(intTree),aes(x=epoch,fill=as.factor(height),order=-as.numeric(height))) +
  geom_histogram(binwidth=100) + ggtitle("Observation Count")+
  scale_fill_discrete("Height") + guides(fill=guide_legend(ncol=1)) +
  theme(legend.key.size=unit(.25,"cm"),legend.key.height=unit(.25,"cm")) 
plot2=ggplot(na.exclude(intTree),aes(x=epoch,fill=as.factor(height),order=-as.numeric(height))) +
  geom_histogram(binwidth=100,position="fill") + ggtitle("Observation Percentage")+
  scale_fill_discrete("Height",guide=FALSE)
grid.arrange(plot1,plot2,ncol=2)
dev.off()

#Finding 2
pdf(file="./graphs/finding2.pdf")
plot1=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humidity,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+labs(x="Timestep",y="Humidity")+
  geom_jitter(aes(x=epoch,y=humidity,colour=height))
plot2=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+labs(x="Timestep",y="Temperature")+
  geom_jitter(aes(x=epoch,y=humid_temp,colour=height))
plot3=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=hamatop,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+labs(x="Timestep",y="Incident PAR")+
  geom_jitter(aes(x=epoch,y=hamatop,colour=height))
plot4=ggplot(na.exclude(may1))+geom_point(aes(x=epoch,y=hamabot,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+labs(x="Timestep",y="Reflected PAR")+
  geom_jitter(aes(x=epoch,y=hamabot,colour=height))
grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2,top="Climate Variables vs. Time on May 1st")
dev.off()

# Finding 3
png(file="./graphs/finding3.png",width=600,height=600)
ggplot(na.exclude(intTree))+geom_point(aes(x=humidity,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+ggtitle("Humidity vs. Temperature")+
  labs(x="Humidity",y="Temperature")+geom_jitter(aes(x=humidity,y=humid_temp,colour=height))
dev.off()

# Cleaned Dataset 
png(file="./graphs/figure1.png",width=600,height=600)
plot1=ggplot(na.exclude(allClean))+geom_point(aes(x=epoch,y=humidity,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Humidity")+geom_jitter(aes(x=epoch,y=humidity,colour=height))
plot2=ggplot(na.exclude(allClean))+geom_point(aes(x=epoch,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Temperature")+geom_jitter(aes(x=epoch,y=humid_temp,colour=height))
plot3=ggplot(na.exclude(allClean))+geom_point(aes(x=epoch,y=hamatop,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Incident PAR")+geom_jitter(aes(x=epoch,y=hamatop,colour=height))
plot4=ggplot(na.exclude(allClean))+geom_point(aes(x=epoch,y=hamabot,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Reflected PAR")+geom_jitter(aes(x=epoch,y=hamabot,colour=height))
grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2,top="Climate Variables vs. Time")
dev.off()

# Uncleaned Dataset
all=read.csv("./data/sonoma-data-all.csv")
all=left_join(all,loc,by="nodeid")
png(file="./graphs/figure2.png",width=600,height=600)
plot1=ggplot(na.exclude(all))+geom_point(aes(x=epoch,y=humidity,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Humidity")+geom_jitter(aes(x=epoch,y=humidity,colour=height))
plot2=ggplot(na.exclude(all))+geom_point(aes(x=epoch,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Temperature")+geom_jitter(aes(x=epoch,y=humid_temp,colour=height))
plot3=ggplot(na.exclude(all))+geom_point(aes(x=epoch,y=hamatop,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Incident PAR")+geom_jitter(aes(x=epoch,y=hamatop,colour=height))
plot4=ggplot(na.exclude(all))+geom_point(aes(x=epoch,y=hamabot,colour=height))+
  scale_colour_gradientn(colours=rev(terrain.colors(10)))+
  labs(x="Timestep",y="Reflected PAR")+geom_jitter(aes(x=epoch,y=hamabot,colour=height))
grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2,top="Climate Variables vs. Time")
dev.off()

# May 1st Trend 
pdf(file="./graphs/figure3.pdf")
plot1=ggplot(na.exclude(nodes))+geom_point(aes(x=epoch,y=humidity,colour=height))+
  scale_colour_gradientn(colours=rev(c("limegreen","yellow","goldenrod3")))+
  labs(x="Timestep",y="Humidity")+geom_jitter(aes(x=epoch,y=humidity,colour=height))
plot2=ggplot(na.exclude(nodes))+geom_point(aes(x=epoch,y=humid_temp,colour=height))+
  scale_colour_gradientn(colours=rev(c("limegreen","yellow","goldenrod3")))+
  labs(x="Timestep",y="Temperature")+geom_jitter(aes(x=epoch,y=humid_temp,colour=height))
grid.arrange(plot1,plot2,ncol=2,nrow=1,top="Climate Variables vs. Time on May 1st")
dev.off()
