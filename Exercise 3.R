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

View(caro)

