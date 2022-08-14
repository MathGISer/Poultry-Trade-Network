##########################################################################
###### To estimate the trade flows among Chinese mainland cities, with modified radiation model. ######



# install.packages("stplanr")
library(stplanr)
library(sp)
library(tidyverse)



### load custom radiation function
source("Fun_Radiation_pd.R")


### load city data, including product and demand of poultry for each city
city <- read.csv("Poultry_Data_Suplement_V1.csv")
head(city)
str(city)



# Longitude and Latitude
IDcoord <- cbind(city$lon, city$lat)
row.names(IDcoord) <- city$city



# define coordinate reference system
library(rgeos)
library(rworldmap)
library(rworldxtra)



# get world map
wmap <- getMap(resolution = "high")
wmap@proj4string



# Build an object of SpatialPoints class
spts <- SpatialPointsDataFrame(IDcoord,
                               proj4string = wmap@proj4string,
                               data = city)
summary(spts)
head(spts)


# Estimate trade flows with modified radiation model
ptod <- radiation4tradeflows(
  spts,
  product_var = "product",
  demand_var = "consumption",
  proportion = 1
)



# check and save results
str(ptod)
summary(ptod)
head(ptod)

# save(ptod,file = "ptod.RData")


### Save results to a data frame
load("ptod.RData")
ptflow <- data.frame(ptod$O, ptod$D, ptod$flow)
names(ptflow) <- c("Origin", "Destination", "Flow")

### Delete NAs
ptflow <- ptflow[!is.na(ptflow$Flow), ]

##########################################################################





###############################################################################
####### Add province info for origination and destination for each flow #######

city<-read.csv("Data/Poultry_Data_Suplement_V1.csv")


city |>
  select(city, province) |>
  dplyr::rename(Origin=city, O.Prov=province) |>
  merge(ptflow,all.y = T) ->
  ptf2aOP


city |>
  select(city, province) |>
  dplyr::rename(Destination=city, D.Prov=province) |>
  merge(ptf2aOP,all.y = T) ->
  ptf2a

colnames(ptf2a)


ptf2a |>
  select(c(3,4,1,2,5))->ptf2a

###############################################################################

# Prop – The contribution rate of the poultry trade volume from the origin city to the total imports for the destination city.
# CumlProp – The cumulative contribution rate of the poultry trade volume from the origin city for the destination city, calculated from largest to smallest for each origin city.




############################################################
###### Contribution and cumulative contribution rates ######


### For each destination city,
# 1. Calculate the total amount of poultry trade imported to it;
# 2. Order the sources by trade volumes from largest to smallest;
# 3. Calculate the proportion of the trade volume from each source to the total volume;
# 4. Calculate the cumulative proportion of the trade volume from each source (from the largest to the smallest);
# Then, the main sources and the proxy variable for trade volume for each destination are obtained.



# proportion and cumulative proportion
ptf2a$Prop<-0
ptf2a$CumlProp<-0


# Create a data frame without any data but with identical variables to ptf2a, to save results with proportion and cumulative proportion
ptflows_withProp<-ptf2a[F,]

# Complete steps 1-4 above
for (i in unique(ptf2a$Destination)) {
  ptf2a |>
    filter(Destination==i)|>   # filter flows to destination i
    arrange(-Flow)->tempdt     # sort them by Flow, from largest to smallest

  totalflow<-sum(tempdt$Flow)

  tempdt$Prop<-tempdt$Flow/totalflow

  for (j in 1:nrow(tempdt)) {
    tempdt$CumlProp[j]<-sum(tempdt$Prop[1:j])
  }

  tempdt |>
    bind_rows(ptflows_withProp)->
    ptflows_withProp
}

# save(ptflows_withProp,file = "ptflows_withProp.RData")
write.csv(ptflows_withProp,
          file = "Suplement_V2_Poultry_Trade_Flows.csv",row.names = F)
######################################################



















### 按Flow从大到小排列
ptflow <- ptflow[order(ptflow$Flow, decreasing = T, na.last = NA), ]

summary(ptflow)
head(ptflow)




# 保存结果
write.csv(ptflow, file = "Ptflow_2022.csv", row.names = F)

save(ptflow,file = "ptflow.RData")

##################################################


