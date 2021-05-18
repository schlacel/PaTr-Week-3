##### Task 1 - Segmentation ####################################################


library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

caro <-read_delim("caro60.csv",",")

#Measure the distance from every point to every other point within the temporal
#window v

caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),  # distance to pos +2 minutes
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  # distance to pos +3 minutes
  )

View(caro)

####Task 2 - Specify and apply threshold d #####################################

#calculate the mean distance of nMinus2, nMinus1, nPlus1, nPlus2 for each row

caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2,nPlus3))
  ) %>%
  ungroup() 

#exploring Values of stepMean to choose the treshold 
ggplot(data=caro, mapping = aes(x=stepMean))+
  geom_histogram(binwidth = 1)

ggplot(data=caro, mapping = aes(y=stepMean))+
  geom_boxplot()

summary(caro$stepMean)

#we use the mean of all stepMean values =  6.951

caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

caro_filter <- caro %>%
  filter(!static)

View(caro)

####Task 3 - Visualize segmented trajectories ##################################

ggplot(data = caro, mapping = aes(x= E, y=N) )+
  geom_path()+
  geom_point(aes(colour=static))+
  coord_equal()

####Task 4 - Segment-based analysis#############################################

#creating an unique ID for each segment:

rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

caro <- caro %>%
  mutate(segment_id = rle_id(static))

caro <- caro %>%
  filter(!static)

ggplot(data = caro, mapping = aes(x= E, y=N) )+
  geom_path(aes(colour=segment_id))+
  geom_point(aes(colour=segment_id))+
  coord_equal()+
  labs(title = "All segments (uncleaned)")+
  theme(title=element_text(size=8),legend.position = "none")


#remove all segments n<5: 

caro_filter_clean<- caro %>%
  group_by(segment_id)%>%
  mutate(n= n())%>%
  filter(n>=5)

ggplot(data = caro_filter_clean, mapping = aes(x= E, y=N) )+
  geom_path(aes(colour=segment_id))+
  geom_point(aes(colour=segment_id))+
  coord_equal()+
  labs(title = "Long segments (removed <5)")+
  theme(title=element_text(size=8),legend.position = "none")


####Task 5 - Similarity measures################################################

pedestrian<-read_delim("pedestrian.csv",",")

str(pedestrian)

pedestrian

pedestrian <- pedestrian%>%
  mutate(
    trajID_fac=as.factor(TrajID), 
    DateTime_integ=as.integer(DatetimeUTC)
    )

str(pedestrian)

ggplot(data=pedestrian, mapping = aes(x=E, y=N,))+
  geom_point(data=dplyr::select(pedestrian,-TrajID),color="grey")+
  geom_point(aes(colour=trajID_fac))+
  facet_wrap(~TrajID)+
  labs(title = "Visual comparison of the 6 trajectories", subtitle= "Each subplot highlights a trajectory")+
  theme_light()+
  theme(legend.position = "none")


  
####Task 6 - Calculate similarity###############################################

library(SimilarityMeasures)

help(package = "SimilarityMeasures")

#Create a matrix for each trajectory

T1 <- pedestrian[1:47,c(2:3,6)]
T2 <- pedestrian[48:95,c(2:3,6)]
T3 <- pedestrian[96:141,c(2:3,6)]
T4 <- pedestrian[142:190,c(2:3,6)]
T5 <- pedestrian[191:242,c(2:3,6)]
T6 <- pedestrian[243:289,c(2:3,6)]

T1 <- as.matrix(T1)
T2 <- as.matrix(T2)
T3 <- as.matrix(T3)
T4 <- as.matrix(T4)
T5 <- as.matrix(T5)
T6 <- as.matrix(T6)

##########Calculating different similarity Measures 

####DTW

D2<-DTW(T1,T2)
D3<-DTW(T1,T3)
D4<-DTW(T1,T4)
D5<-DTW(T1,T5)
D6<-DTW(T1,T6)

DTW<- c(NA,D2,D3,D4,D5,D6)

####Edit Distance 

ED2<-EditDist(T1,T2)
ED3<-EditDist(T1,T3)
ED4<-EditDist(T1,T4)
ED5<-EditDist(T1,T5)
ED6<-EditDist(T1,T6)

ED<- c(NA,ED2,ED3,ED4,ED5,ED6)

####Frechet

F2<-Frechet(T1,T2)
F3<-Frechet(T1,T3)
F4<-Frechet(T1,T4)
F5<-Frechet(T1,T5)
F6<-Frechet(T1,T6)

Frechet<-c(NA,F2,F3,F4,F5,F6)

####LCSS

LCSS2<-LCSS(T1,T2,2,2,0.5)
LCSS3<-LCSS(T1,T3,2,2,0.5)
LCSS4<-LCSS(T1,T4,2,2,0.5)
LCSS5<-LCSS(T1,T5,2,2,0.5)
LCSS6<-LCSS(T1,T6,2,2,0.5)

Lcss<-c(NA,LCSS2,LCSS3,LCSS4,LCSS5,LCSS6)

sim.measures<-data.frame(DTW,ED,Frechet,Lcss)
sim.measures$traj<-c(1,2,3,4,5,6)

View(sim.measures)

###### Visualization of the different Similarity Measures 

DTW.plot<-ggplot(data=sim.measures,aes(x=traj,y=DTW,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  labs(title = "DTW")+
  theme(legend.position = "none")
  
DTW.plot

ED.plot<-ggplot(data=sim.measures,aes(x=traj,y=ED,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  labs(title = "Edit Distance")+
  theme(legend.position = "none")

ED.plot

Frechet.plot<-ggplot(data=sim.measures,aes(x=traj,y=Frechet,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  labs(title = "Frechet")+
  theme(legend.position = "none")

Frechet.plot

Lcss.plot<-ggplot(data=sim.measures,aes(x=traj,y=Lcss,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  labs(title = "LCSS")+
  theme(legend.position = "none")

Lcss.plot

#Unite the plots:

library(cowplot)
library(patchwork)

all.plots<-plot_grid(DTW.plot,ED.plot,Frechet.plot,Lcss.plot, nrow=2, ncol=2)
all.plots+plot_annotation(title="Computed similarities using different measures between trajectory 1 to all other trajectories")
  