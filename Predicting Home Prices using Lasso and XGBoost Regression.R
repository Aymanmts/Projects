getwd()
setwd("C:/Users/AYMANS/Desktop/DataAnalytics4/Project")

install.packages("caret")
install.packages("plyr")
install.packages("pROC")
install.packages("corrplot")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("class")
install.packages("moments")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("data.table")
install.packages("randomForest")
install.packages("xgboost")
install.packages("Boruta")
install.packages("FeatureHashing")
install.packages("dummies")
install.packages("Metrics")
install.packages("factoextra")
install.packages("highcharter")
install.packages("edgebundleR")
install.packages("psych")
install.packages("DT")


library("DT")
library(psych)
require(psych)
library("edgebundleR")
library("factoextra")
library(highcharter)
library(caret)
library(Boruta)
library(plyr)
library(pROC)
library(corrplot)
library(dplyr)
library(gridExtra)
library(class)
library(moments)
library(RColorBrewer)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(randomForest)
library(xgboost)
require(ggplot2) 
require(stringr) 
require(Matrix) 
require(glmnet) 
require(xgboost) 
require(randomForest)
require(Metrics) 
require(dplyr) 
require(caret) 
require(scales) 
require(e1071) 
require(corrplot)
library(FeatureHashing)
library(Matrix)
library(xgboost)
require(randomForest)
require(caret)
require(dplyr)
require(ggplot2)
library(pROC)
library(stringr)
library(dummies)
library(Metrics)
library(kernlab)
library(mlbench)
library(e1071)

rm(list = ls())

#LOADING DATA#
housingtest<-read.csv("C:/Users/AYMANS/Desktop/DataAnalytics4/test.csv", stringsAsFactors = FALSE)
housingtrain<-read.csv("C:/Users/AYMANS/Desktop/DataAnalytics4/train.csv", stringsAsFactors = FALSE)

dim(housingtest)
str(housingtest)

dim(housingtrain)
str(housingtrain)

#JOINING BOTH DATASETS TO GET A FULL VIEW OF THE SET OF DATUA#
fulldataset<-rbind(within(housingtrain,rm('SalePrice','Id')),within(housingtest,rm('Id')))
housingtrain1<-bind_rows(housingtrain,housingtest)

main_test<-read.csv("C:/Users/AYMANS/Desktop/DataAnalytics4/sample_submission.csv", stringsAsFactors = FALSE)

dim(housingtrain1)

str(housingtrain1)

dim(housingtrain1)

dim(fulldataset)
str(fulldataset)
head(fulldataset)
tail(fulldataset)

#CLEANING DATA#

min(housingtrain$SalePrice)
max(housingtrain$SalePrice)

#1. Finding number of NAs per column in dataset#
NAs<-which(colSums(is.na(housingtrain1))>0)
NAs<-colSums(sapply(housingtrain1[NAs],is.na))
sort(NAs)

#1Ai. FINDING WHICH HOMES HAVE A POOL- so we can isolate the NAs in PoolQC for homes that dont have a pool#
Pools<-housingtrain1[is.na(housingtrain1$PoolQC) & (housingtrain1$PoolArea > 0),c('PoolQC','PoolArea')]
Pools


#1Aii. Finding Mean Values of Pool Areas of each Pool Quality Category#

PoolMeanArea <- summarize(group_by(fulldataset, PoolQC),
                          mean(PoolArea, na.rm=TRUE))

#1Aiii. Inputting the Mean Value of those NAs in PoolQC that dont have a pool (positive pool area).
#Replace the rest with character 'No Pool'
housingtrain1[2600,"PoolQC"]<-'Fa'
housingtrain1[2504,"PoolQC"]<-'Ex'
housingtrain1[2421,"PoolQC"]<-'Ex'
housingtrain1$PoolQC[is.na(housingtrain1$PoolQC)]<-'No'

#1B Investigating Alley

housingtrain1$Alley

#1Bi. Removing NA values and replacing it with No Alley (assuming these properties dont have these)
housingtrain1$Alley[is.na(housingtrain1$Alley)]<-'No'

#1C Investingating MiscFeature
housingtrain1$MiscFeature

#1Ci. Removing NA values and replacing it with No Feature (assuming these properties dont have them)
housingtrain1$MiscFeature[is.na(housingtrain1$MiscFeature)]<-'No'

#1C. Investingating Fence
housingtrain1$Fence

#1Ci. Removing NA values and replacing it with No Fences (assuming these properties dont have them)
housingtrain1$Fence[is.na(housingtrain1$Fence)]<-'No'

#1D. Investigating which homes have fireplaces but have missing values for fire place quality

which(is.na(housingtrain1$FireplaceQu) & (housingtrain1$Fireplaces>0))

#1Di. Removing NA values and replacing with No Fireplaces 
#(we checked above and there doesn't appear to be any NA values for homes with fireplaces)

housingtrain1$FireplaceQu[is.na(housingtrain1$FireplaceQu)]<-'No'

#1E. Investingating Lot Frontage.
housingtrain1$LotFrontage

#1Ei. Finding the neighborhoods of those houses that have a missing lot frontage value
#we are doing this because we will input the average amount of lot frontage for the NA values based 
#on the average area in the neighborhoods they are in.



NeighborhoodMeanLotArea <- housingtrain1[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(mean = mean(LotFrontage, na.rm = TRUE))
NeighborhoodMeanLotArea

NAsLotFrontage = which(is.na(housingtrain1$LotFrontage))

for (x in NAsLotFrontage){
  LotMeanArea <- NeighborhoodMeanLotArea[NeighborhoodMeanLotArea == housingtrain1$Neighborhood[x],'mean']
  housingtrain1[x,'LotFrontage'] <- LotMeanArea[[1]]
}


#1F. Investingating Garage Condition

housingtrain1$GarageCond

#1Fi FINDING WHICH HOMES HAVE A Garage- so we can isolate the NAs in GarageCond for homes that dont have a garage#

GarageCondition<-housingtrain1[(housingtrain1$GarageArea>0) & is.na(housingtrain1$GarageCond), c('GarageArea','GarageCond')]
GarageCondition
housingtrain1[2127,"GarageCond"]<-'TA' #Most Garages in this Neighborhood have this Garage Condition. So we set this as a condition.

housingtrain1$GarageCond[is.na(housingtrain1$GarageCond)]<-'No'

#1G FINDING WHICH HOMES HAVE A Garage- so we can isolate the NAs in GarageQual for homes that dont have a garage#

GarageQuality<-housingtrain1[(housingtrain1$GarageArea>0) & is.na(housingtrain1$GarageQual), c('GarageArea','GarageQual')]

GarageQuality

housingtrain1[2127,"GarageQual"]<-'TA' #All Garages in this Neighborhood and Build year and have this as their Garage Quality. So we set this as a condition as well.


housingtrain1$GarageQual[is.na(housingtrain1$GarageQual)]<-'No'


#1H FINDING WHICH HOMES HAVE A Garage- so we can isolate the NAs in Garage Finish for homes that dont have a garage#

GarageFin<-housingtrain1[(housingtrain1$GarageArea>0) & is.na(housingtrain1$GarageFinish), c('GarageArea','GarageFinish')]

GarageFin


housingtrain1[2127,"GarageFinish"]<-'Unf' #Similar Homes in this neighborhood have Unf Garages as well.

housingtrain1$GarageFinish[is.na(housingtrain1$GarageFinish)]<-'No'

#1I FINDING WHICH HOMES HAVE A Garage- so we can isolate the NAs in GarageYrBlt for homes that dont have a garage#

GarageBlt<-housingtrain1[(housingtrain1$GarageArea>0) & is.na(housingtrain1$GarageYrBlt), c('GarageArea','GarageYrBlt')]

GarageBlt

housingtrain1[2127,"GarageYrBlt"]<-1910 #Assuming garage built at the same time as home was built. This is the same as the rest of the similar homes in the neighborhood.

housingtrain1$GarageYrBlt[is.na(housingtrain1$GarageYrBlt)]<-"No"

#1J FINDING WHICH HOMES HAVE A Garage- so we can isolate the NAs in GarageType for homes that dont have a garage#

GarageTy<-housingtrain1[(housingtrain1$GarageArea>0) & is.na(housingtrain1$GarageType), c('GarageArea','GarageType')]

GarageTy

housingtrain1$GarageType[is.na(housingtrain1$GarageType)]<-'No' #these homes dont have a garage


#1K FINDING WHICH HOMES HAVE A BASEMENT- so we can isolate the NAs in Basement Exposure from homes that dont have a Basement#

BasmentExp<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtExposure), c('TotalBsmtSF','BsmtExposure')]


BasmentExp

housingtrain1[949,"BsmtExposure"]<-'No' #Homes in this neighborhood with this build date and bedrooms also dont have a Basement Exposure to Street Level. Assumiming this is the case here.
housingtrain1[1488,"BsmtExposure"]<-'Av'#The one home in this neighborhood with the same build date, zoning area and Basement Square Footage has Av as its value, assume its the same here.
housingtrain1[2349,"BsmtExposure"]<-'No'#The other homes in this neighborhood build date basement type and other similar features, all have NO as the Bsmt Exposure. We assume its the same for this home as well.
housingtrain1$BsmtExposure[is.na(housingtrain1$BsmtExposure)]<-'No' #The rest of the homes dont have any basement square footage so dont have a basment so there would be no basement exposure.

#1L FINDING WHICH HOMES HAVE A BASEMENT- so we can isolate the NAs in Basement Condition from homes that dont have a Basement#

BasmentCond<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtCond), c('TotalBsmtSF','BsmtCond')]


BasmentCond

housingtrain1[2041,"BsmtCond"]<-'TA'
housingtrain1[2186,"BsmtCond"]<-'Gd'
housingtrain1[2525,"BsmtCond"]<-'Gd'
housingtrain1$BsmtExposure[is.na(housingtrain1$BsmtCond)]<-'No'

#1M FINDING WHICH HOMES HAVE A BASEMENT- so we can isolate the NAs in Basement Quality from homes that dont have a Basement#

BasmentQual<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtQual), c('TotalBsmtSF','BsmtQual')]


BasmentQual

housingtrain1[2218,"BsmtQual"]<-'TA' #Similar to Basement Condition
housingtrain1[2219,"BsmtQual"]<-'TA' 

housingtrain1$BsmtQual[is.na(housingtrain1$BsmtQual)]<-'No'

#1N FINDING WHICH HOMES HAVE A BASEMENT- so we can isolate the NAs in Basement Finish2 from homes that dont have a Basement#

BasmentFin2<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtFinType2), c('TotalBsmtSF','BsmtFinType2')]


BasmentFin2

housingtrain1[333,"BsmtFinType2"]<-'ALQ' #Similar to others average quality

housingtrain1$BsmtFinType2[is.na(housingtrain1$BsmtFinType2)]<-'No'

#1O FINDING WHICH HOMES HAVE A BASEMENT- so we can isolate the NAs in Basement Finish1 from homes that dont have a Basement#

BasmentFin1<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtFinType1), c('TotalBsmtSF','BsmtFinType1')]


BasmentFin1


housingtrain1$BsmtFinType1[is.na(housingtrain1$BsmtFinType1)]<-'No'

#1P FINDING WHICH HOMES HAVE A BASEMENT- so we can isolate the NAs in Basement Condition from homes that dont have a Basement#

BasmentCond1<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtCond), c('TotalBsmtSF','BsmtCond')]


BasmentCond1


housingtrain1$BsmtCond[is.na(housingtrain1$BsmtCond)]<-'No'


#1Q FINDING WHICH HOMES HAVE A MasVnrType- so we can isolate the NAs in Masonry Veneer Type from homes that dont have a Masonary#

Mason<-housingtrain1[(housingtrain1$MasVnrArea>0) & is.na(housingtrain1$MasVnrType), c('MasVnrArea','MasVnrType')]


Mason


MasonVennerMean <- summarize(group_by(housingtrain1, MasVnrType),
                             mean(MasVnrArea, na.rm=TRUE))
MasonVennerMean

housingtrain1[2611,'MasVnrType']<-'BrkCmn' #THis is the type that has the closest Mean Value to the 198m2 area found in this row
housingtrain1$MasVnrType[is.na(housingtrain1$MasVnrType)]<-'None'

#1Q Removing NAs in the Masonary Areas that dont have a type since we know these are the same rows that didnt have a Mason Veneer we observed above so set their value to 0

housingtrain1$MasVnrArea[is.na(housingtrain1$MasVnrArea)]<-0

#1R Investigating NAs in MSZoning (Zoning Classification) by looking at the homes Subclass(building class) to determine which category its most likely in

housingtrain1[is.na(housingtrain1$MSZoning),c('MSZoning','MSSubClass')]

table(housingtrain1$MSZoning,housingtrain1$MSSubClass) 

#We see that row (home) 1916 has a subclass of 30. 67 of those are in the MS Zoning class of RM (highest amount). We will assign that value here as its most likely.
#We see that row (home) 2217 has a subclass of 20. 1016 of those are in the MS Zoning class of RL (highest amount). We will assign that value here as its most likely.
#We see that row (home) 2251 has a subclass of 70. 1016 of those are in the MS Zoning class of RM(highest amount). We will assign that value here as its most likely.
#We see that row (home) 2905 has a subclass of 20. 1016 of those are in the MS Zoning class of RL(highest amount). We will assign that value here as its most likely.

housingtrain1[1916,'MSZoning']<-'RM'
housingtrain1[2251,'MSZoning']<-'RM'
housingtrain1[2217,'MSZoning']<-'RL'
housingtrain1[2905,'MSZoning']<-'RL'

#1S. Investigating NAs in Functional

table(housingtrain1$Functional)# We see Typ is by far the greatest count of values in the Functional variable. We will assign NAs as this.

housingtrain1$Functional[is.na(housingtrain1$Functional)]<-'Typ'

#1T. Investigating Basement Fullbath and half bath. First check to see if there is infact a basement for these values, same as before.

BasmentBath<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtHalfBath), c('TotalBsmtSF','BsmtHalfBath')]
BasmentBathF<-housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtFullBath), c('TotalBsmtSF','BsmtFullBath')]

BasmentBath
BasmentBathF

#Both missing values indicate that those homes dont have Basments. We set them to 0

housingtrain1$BsmtFullBath[is.na(housingtrain1$BsmtFullBath)]<-0
housingtrain1$BsmtHalfBath[is.na(housingtrain1$BsmtHalfBath)]<-0

#1U. Investigating Utilities

table(housingtrain1$Utilities)

housingtrain1$Utilities[is.na(housingtrain1$Utilities)]<-"AllPub" #Because overwhelming majority have AllPub as their utility provider assume same her.

#1V.Investingting SaleType


housingtrain1[is.na(housingtrain1$SaleType),c('SaleType','SaleCondition')] #we see that the NA value corresponds to Sale Condition of Normal

table(housingtrain1$SaleType, housingtrain1$SaleCondition) #Overwhleming Majority of Normal Sale Conditions are of Sale Type WD. So we can assign that value here for the NA value.

housingtrain1$SaleType[is.na(housingtrain1$SaleType)]<-"WD"

#1W. Investigating GarageArea and Cars
housingtrain1[is.na(housingtrain1$GarageArea),c('GarageArea','GarageCond')] # No Garage here for this NA value. Set to 0 area
housingtrain1$GarageArea[is.na(housingtrain1$GarageArea)]<-0

housingtrain1[is.na(housingtrain1$GarageCars),c('GarageArea','GarageCars')] # No Garage here for this NA value. Set to 0 area
housingtrain1$GarageCars[is.na(housingtrain1$GarageCars)]<-0

#1X. Investigating KitchenQual
housingtrain1[is.na(housingtrain1$KitchenQual),c('KitchenQual','KitchenAbvGr')] # So there is a kitchen that exists in this home

table(housingtrain1$KitchenQual)
housingtrain1$KitchenQual[is.na(housingtrain1$KitchenQual)]<-'TA'# Most homes in this area have a Kitchen Quality of TA 

#1Y. Investigating Electrical

table(housingtrain1$Electrical)
housingtrain1$Electrical[is.na(housingtrain1$Electrical)]<-'SBrkr' #Overwhlming majority have this value for Electrical. So set it as most liely

#1Z. Investingating TotalBsmtSF, BsmtUnfSF, BsmtFinSF2, BsmtFinSF1

housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtCond), c('TotalBsmtSF','BsmtCond')]
housingtrain1$TotalBsmtSF[is.na(housingtrain1$TotalBsmtSF)]<-0

housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtFinSF1), c('TotalBsmtSF','BsmtFinSF1')]
housingtrain1$BsmtFinSF1[is.na(housingtrain1$BsmtFinSF1)]<-0

housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtFinSF2), c('TotalBsmtSF','BsmtFinSF2')]
housingtrain1$BsmtFinSF2[is.na(housingtrain1$BsmtFinSF2)]<-0

housingtrain1[(housingtrain1$TotalBsmtSF>0) & is.na(housingtrain1$BsmtUnfSF), c('TotalBsmtSF','BsmtUnfSF')]
housingtrain1$BsmtUnfSF[is.na(housingtrain1$BsmtUnfSF)]<-0

#1AA. Investingating Exterior1st and Exterior2nd

housingtrain1[is.na(housingtrain1$Exterior1st),c('Exterior1st','Exterior2nd')]
housingtrain1[is.na(housingtrain1$Exterior2nd),c('Exterior1st','Exterior2nd')] #Same home  has no exterior

table(housingtrain1$Exterior1st) 
table(housingtrain1$Exterior2nd) 

housingtrain1$Exterior1st[is.na(housingtrain1$Exterior1st)]<-"VinylSd"
housingtrain1$Exterior2nd[is.na(housingtrain1$Exterior2nd)]<-"VinylSd"

NAs<-which(colSums(is.na(housingtrain1))>0)
NAs<-colSums(sapply(housingtrain1[NAs],is.na))
sort(NAs)

#2. Now that the NAs are gone we have to add the sales price and change to numeric
# the character features of this dataset.

#2A. Spliting data into character and numeric

characters<-names(which(sapply(housingtrain1,is.character)))
numbers<-names(which(sapply(housingtrain1,is.numeric)))

characters.housingtrain1<-housingtrain1[characters]
numbers.housingtrain1<-housingtrain1[numbers]

alldata<-housingtrain1[1:1460,]

alldata$SalePrice<-housingtrain1$SalePrice[1:1460,]

names(which(sapply(housingtrain1,is.character)))


#2B. Converting MSZoning to numeric

price<-summarize(group_by(alldata, MSZoning),
                 mean(SalePrice, na.rm=T))

price

housingtrain1$MSZoning[housingtrain1$MSZoning %in% c("FV")] <- 4
housingtrain1$MSZoning[housingtrain1$MSZoning %in% c("RL")] <- 3
housingtrain1$MSZoning[housingtrain1$MSZoning %in% c("RH","RM")] <- 2
housingtrain1$MSZoning[housingtrain1$MSZoning %in% c("C (all)")] <- 1

alldata$MSZoning[alldata$MSZoning %in% c("FV")] <- 4
alldata$MSZoning[alldata$MSZoning %in% c("RL")] <- 3
alldata$MSZoning[alldata$MSZoning %in% c("RH","RM")] <- 2
alldata$MSZoning[alldata$MSZoning %in% c("C (all)")] <- 1

table(housingtrain1$MSZoning)

housingtrain1$MSZoning<-as.integer(housingtrain1$MSZoning)
alldata$MSZoning<-as.integer(alldata$MSZoning)

str(housingtrain1$MSZoning)

names(which(sapply(housingtrain1,is.character)))

#2C.Converting LandSlope to numeric

price<-summarize(group_by(alldata, LandSlope),
                 mean(SalePrice, na.rm=T))
price

housingtrain1$LandSlope[housingtrain1$LandSlope %in% c("Sev")] <- 3
housingtrain1$LandSlope[housingtrain1$LandSlope %in% c("Mod")] <- 2
housingtrain1$LandSlope[housingtrain1$LandSlope %in% c("Gtl")] <- 1

alldata$LandSlope[alldata$LandSlope %in% c("Sev")] <- 3
alldata$LandSlope[alldata$LandSlope %in% c("Mod")] <- 2
alldata$LandSlope[alldata$LandSlope %in% c("Gtl")] <- 1

table(housingtrain1$LandSlope)

housingtrain1$LandSlope<-as.integer(housingtrain1$LandSlope)
alldata$LandSlope<-as.integer(alldata$LandSlope)

str(housingtrain1$LandSlope)

characters

names(which(sapply(housingtrain1,is.character)))


#2D.Converting RoofMatl to numeric

price<-summarize(group_by(alldata, RoofMatl),
                 mean(SalePrice, na.rm=T))

housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("WdShngl")] <- 8
housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("Membran")] <- 7
housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("WdShake")] <- 6
housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("Tar&Grv")] <- 5
housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("Metal")] <- 4
housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("CompShg")] <- 3
housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("ClyTile")] <- 2
housingtrain1$RoofMatl[housingtrain1$RoofMatl %in% c("Roll")] <- 1

alldata$RoofMatl[alldata$RoofMatl %in% c("WdShngl")] <- 8
alldata$RoofMatl[alldata$RoofMatl %in% c("Membran")] <- 7
alldata$RoofMatl[alldata$RoofMatl %in% c("WdShake")] <- 6
alldata$RoofMatl[alldata$RoofMatl %in% c("Tar&Grv")] <- 5
alldata$RoofMatl[alldata$RoofMatl %in% c("Metal")] <- 4
alldata$RoofMatl[alldata$RoofMatl %in% c("CompShg")] <- 3
alldata$RoofMatl[alldata$RoofMatl %in% c("ClyTile")] <- 2
alldata$RoofMatl[alldata$RoofMatl %in% c("Roll")] <- 1

table(housingtrain1$RoofMatl)

housingtrain1$RoofMatl<-as.integer(housingtrain1$RoofMatl)
alldata$RoofMatl<-as.integer(alldata$RoofMatl)


str(housingtrain1$LandSlope)

names(which(sapply(housingtrain1,is.character)))

#2E.Converting BasmtQual to numeric

price<-summarize(group_by(alldata, BsmtQual),
                 mean(SalePrice, na.rm=T))

price



housingtrain1$BsmtQual[housingtrain1$BsmtQual %in% c("Ex")] <- 4
housingtrain1$BsmtQual[housingtrain1$BsmtQual %in% c("Gd")] <- 3
housingtrain1$BsmtQual[housingtrain1$BsmtQual %in% c("TA")] <- 2
housingtrain1$BsmtQual[housingtrain1$BsmtQual %in% c("Fa")] <- 1
housingtrain1$BsmtQual[housingtrain1$BsmtQual %in% c("No")] <- 0

alldata$BsmtQual[alldata$BsmtQual %in% c("Ex")] <- 4
alldata$BsmtQual[alldata$BsmtQual %in% c("Gd")] <- 3
alldata$BsmtQual[alldata$BsmtQual %in% c("TA")] <- 2
alldata$BsmtQual[alldata$BsmtQual %in% c("Fa")] <- 1
alldata$BsmtQual[alldata$BsmtQual %in% c("No")] <- 0

table(housingtrain1$BsmtQual)

housingtrain1$BsmtQual<-as.integer(housingtrain1$BsmtQual)
alldata$BsmtQual<-as.integer(alldata$BsmtQual)


str(housingtrain1$BsmtQual)

names(which(sapply(housingtrain1,is.character)))

#2F.Converting CentralAir to numeric

price<-summarize(group_by(alldata, CentralAir),
                 mean(SalePrice, na.rm=T))

price


housingtrain1$CentralAir[housingtrain1$CentralAir %in% c("Y")] <- 1
housingtrain1$CentralAir[housingtrain1$CentralAir %in% c("N")] <- 0

alldata$CentralAir[alldata$CentralAir %in% c("Y")] <- 1
alldata$CentralAir[alldata$CentralAir %in% c("N")] <- 0

table(housingtrain1$CentralAir)

housingtrain1$CentralAir<-as.integer(housingtrain1$CentralAir)
alldata$CentralAir<-as.integer(alldata$CentralAir)

str(housingtrain1$CentralAir)

names(which(sapply(housingtrain1,is.character)))

#2G.Converting GarageFinish to numeric

price<-summarize(group_by(alldata, GarageFinish),
                 mean(SalePrice, na.rm=T))

price


housingtrain1$GarageFinish[housingtrain1$GarageFinish %in% c("Fin")] <- 3
housingtrain1$GarageFinish[housingtrain1$GarageFinish %in% c("RFn")] <- 2
housingtrain1$GarageFinish[housingtrain1$GarageFinish %in% c("Unf")] <- 1
housingtrain1$GarageFinish[housingtrain1$GarageFinish %in% c("No")] <- 0


alldata$GarageFinish[alldata$GarageFinish %in% c("Fin")] <- 3
alldata$GarageFinish[alldata$GarageFinish %in% c("RFn")] <- 2
alldata$GarageFinish[alldata$GarageFinish %in% c("Unf")] <- 1
alldata$GarageFinish[alldata$GarageFinish %in% c("No")] <- 0


table(housingtrain1$GarageFinish)

housingtrain1$GarageFinish<-as.integer(housingtrain1$GarageFinish)
alldata$GarageFinish<-as.integer(alldata$GarageFinish)

str(housingtrain1$GarageFinish)

names(which(sapply(housingtrain1,is.character)))

#2H.Converting SaleType to numeric

price<-summarize(group_by(alldata, SaleType),
                 mean(SalePrice, na.rm=T))

price


housingtrain1$SaleType[housingtrain1$SaleType %in% c("New")] <- 9
housingtrain1$SaleType[housingtrain1$SaleType %in% c("Con")] <- 8
housingtrain1$SaleType[housingtrain1$SaleType %in% c("CWD")] <- 7
housingtrain1$SaleType[housingtrain1$SaleType %in% c("ConLI")] <- 6
housingtrain1$SaleType[housingtrain1$SaleType %in% c("WD")] <- 5
housingtrain1$SaleType[housingtrain1$SaleType %in% c("COD")] <- 4
housingtrain1$SaleType[housingtrain1$SaleType %in% c("ConLw")] <- 3
housingtrain1$SaleType[housingtrain1$SaleType %in% c("ConLD")] <- 2
housingtrain1$SaleType[housingtrain1$SaleType %in% c("Oth")] <- 1

alldata$SaleType[alldata$SaleType %in% c("New")] <- 9
alldata$SaleType[alldata$SaleType %in% c("Con")] <- 8
alldata$SaleType[alldata$SaleType %in% c("CWD")] <- 7
alldata$SaleType[alldata$SaleType %in% c("ConLI")] <- 6
alldata$SaleType[alldata$SaleType %in% c("WD")] <- 5
alldata$SaleType[alldata$SaleType %in% c("COD")] <- 4
alldata$SaleType[alldata$SaleType %in% c("ConLw")] <- 3
alldata$SaleType[alldata$SaleType %in% c("ConLD")] <- 2
alldata$SaleType[alldata$SaleType %in% c("Oth")] <- 1




table(housingtrain1$SaleType)

housingtrain1$SaleType<-as.integer(housingtrain1$SaleType)
alldata$SaleType<-as.integer(alldata$SaleType)

str(housingtrain1$SaleType)


table(housingtrain1$SaleType)

names(which(sapply(housingtrain1,is.character)))

#2I.Converting Street to numeric

price<-summarize(group_by(alldata, Street),
                 mean(SalePrice, na.rm=T))

price


housingtrain1$Street[housingtrain1$Street %in% c("Grvl")] <- 0
housingtrain1$Street[housingtrain1$Street %in% c("Pave")] <- 1

alldata$Street[alldata$Street %in% c("Grvl")] <- 0
alldata$Street[alldata$Street %in% c("Pave")] <- 1

table(housingtrain1$Street)

housingtrain1$Street<-as.integer(housingtrain1$Street)
alldata$Street<-as.integer(alldata$Street)

str(housingtrain1$Street)


table(housingtrain1$Street)

names(which(sapply(housingtrain1,is.character)))

#2J.Converting Neighborhood to numeric

price<-summarize(group_by(alldata, Neighborhood),
                 mean(SalePrice, na.rm=T))

price
summary(price$`mean(SalePrice, na.rm = T)`)

LowNP <- filter(price, price$`mean(SalePrice, na.rm = T)` < 136800)
MedNP <- filter(price, price$`mean(SalePrice, na.rm = T)` < 212600 &
                  price$`mean(SalePrice, na.rm = T)` >= 136800 )
HiNP <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 212600)

LowNP
MedNP
HiNP

housingtrain1$Neighborhood[housingtrain1$Neighborhood %in% LowNP$Neighborhood] <- 1
housingtrain1$Neighborhood[housingtrain1$Neighborhood %in% MedNP$Neighborhood] <- 2
housingtrain1$Neighborhood[housingtrain1$Neighborhood %in% HiNP$Neighborhood] <- 3

alldata$Neighborhood[alldata$Neighborhood %in% LowNP$Neighborhood] <- 1
alldata$Neighborhood[alldata$Neighborhood %in% MedNP$Neighborhood] <- 2
alldata$Neighborhood[alldata$Neighborhood %in% HiNP$Neighborhood] <- 3


table(housingtrain1$Neighborhood)

housingtrain1$Neighborhood<-as.integer(housingtrain1$Neighborhood)
alldata$Neighborhood<-as.integer(alldata$Neighborhood)

str(housingtrain1$Neighborhood)


table(housingtrain1$Neighborhood)

names(which(sapply(housingtrain1,is.character)))

#2K.Converting Exterior1st to numeric

price<-summarize(group_by(alldata, Exterior1st),
                 mean(SalePrice, na.rm=T))

price

summary(price$`mean(SalePrice, na.rm = T)`)

LowNP <- filter(price, price$`mean(SalePrice, na.rm = T)` < 128400)
MedNP <- filter(price, price$`mean(SalePrice, na.rm = T)` < 204200 &
                  price$`mean(SalePrice, na.rm = T)` >= 128400 )
HiNP <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 204200)

LowNP
MedNP
HiNP

housingtrain1$Exterior1st[housingtrain1$Exterior1st %in% LowNP$Exterior1st] <- 1
housingtrain1$Exterior1st[housingtrain1$Exterior1st %in% MedNP$Exterior1st] <- 2
housingtrain1$Exterior1st[housingtrain1$Exterior1st %in% HiNP$Exterior1st] <- 3

alldata$Exterior1st[alldata$Exterior1st %in% LowNP$Exterior1st] <- 1
alldata$Exterior1st[alldata$Exterior1st %in% MedNP$Exterior1st] <- 2
alldata$Exterior1st[alldata$Exterior1st %in% HiNP$Exterior1st] <- 3



table(housingtrain1$Exterior1st)

housingtrain1$Exterior1st<-as.integer(housingtrain1$Exterior1st)
alldata$Exterior1st<-as.integer(alldata$Exterior1st)

str(housingtrain1$Exterior1st)


table(housingtrain1$Exterior1st)

names(which(sapply(housingtrain1,is.character)))


#2K.Converting Exterior2nd to numeric

price<-summarize(group_by(alldata, Exterior2nd),
                 mean(SalePrice, na.rm=T))

price

summary(price$`mean(SalePrice, na.rm = T)`)

LowNP <- filter(price, price$`mean(SalePrice, na.rm = T)` < 145800)
MedNP <- filter(price, price$`mean(SalePrice, na.rm = T)` < 200500 &
                  price$`mean(SalePrice, na.rm = T)` >= 145800 )
HiNP <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200500)

LowNP
MedNP
HiNP

housingtrain1$Exterior2nd[housingtrain1$Exterior2nd %in% LowNP$Exterior2nd] <- 1
housingtrain1$Exterior2nd[housingtrain1$Exterior2nd %in% MedNP$Exterior2nd] <- 2
housingtrain1$Exterior2nd[housingtrain1$Exterior2nd %in% HiNP$Exterior2nd] <- 3

alldata$Exterior2nd[alldata$Exterior2nd %in% LowNP$Exterior2nd] <- 1
alldata$Exterior2nd[alldata$Exterior2nd %in% MedNP$Exterior2nd] <- 2
alldata$Exterior2nd[alldata$Exterior2nd %in% HiNP$Exterior2nd] <- 3



table(housingtrain1$Exterior2nd)

housingtrain1$Exterior2nd<-as.integer(housingtrain1$Exterior2nd)
alldata$Exterior2nd<-as.integer(alldata$Exterior2nd)

str(housingtrain1$Exterior2nd)


table(housingtrain1$Exterior2nd)

names(which(sapply(housingtrain1,is.character)))

#2L.Converting BsmtCond to numeric

price<-summarize(group_by(alldata, BsmtCond),
                 mean(SalePrice, na.rm=T))

price

housingtrain1$BsmtCond[housingtrain1$BsmtCond %in% c("Po")] <- 1
housingtrain1$BsmtCond[housingtrain1$BsmtCond %in% c("No")] <- 2
housingtrain1$BsmtCond[housingtrain1$BsmtCond %in% c("Fa")] <- 3
housingtrain1$BsmtCond[housingtrain1$BsmtCond %in% c("TA")] <- 4
housingtrain1$BsmtCond[housingtrain1$BsmtCond %in% c("Gd")] <- 5

alldata$BsmtCond[alldata$BsmtCond %in% c("Po")] <- 1
alldata$BsmtCond[alldata$BsmtCond %in% c("No")] <- 2
alldata$BsmtCond[alldata$BsmtCond %in% c("Fa")] <- 3
alldata$BsmtCond[alldata$BsmtCond %in% c("TA")] <- 4
alldata$BsmtCond[alldata$BsmtCond %in% c("Gd")] <- 5


table(housingtrain1$BsmtCond)

housingtrain1$BsmtCond<-as.integer(housingtrain1$BsmtCond)
alldata$BsmtCond<-as.integer(alldata$BsmtCond)

str(housingtrain1$BsmtCond)


table(housingtrain1$BsmtCond)
names(which(sapply(housingtrain1,is.character)))

#2M.Converting Electrical to numeric

price<-summarize(group_by(alldata, Electrical),
                 mean(SalePrice, na.rm=T))

price

housingtrain1$Electrical[housingtrain1$Electrical %in% c("Mix")] <- 1
housingtrain1$Electrical[housingtrain1$Electrical %in% c("FuseP")] <- 2
housingtrain1$Electrical[housingtrain1$Electrical %in% c("FuseF")] <- 3
housingtrain1$Electrical[housingtrain1$Electrical %in% c("FuseA")] <- 4
housingtrain1$Electrical[housingtrain1$Electrical %in% c("SBrkr")] <- 5

alldata$Electrical[alldata$Electrical %in% c("Mix")] <- 1
alldata$Electrical[alldata$Electrical %in% c("FuseP")] <- 2
alldata$Electrical[alldata$Electrical %in% c("FuseF")] <- 3
alldata$Electrical[alldata$Electrical %in% c("FuseA")] <- 4
alldata$Electrical[alldata$Electrical %in% c("SBrkr")] <- 5



table(housingtrain1$Electrical)

housingtrain1$Electrical<-as.integer(housingtrain1$Electrical)
alldata$Electrical<-as.integer(alldata$Electrical)

str(housingtrain1$Electrical)


table(housingtrain1$Electrical)
names(which(sapply(housingtrain1,is.character)))


#2N.Converting GarageQual to numeric

price<-summarize(group_by(alldata, GarageQual),
                 mean(SalePrice, na.rm=T))

price

housingtrain1$GarageQual[housingtrain1$GarageQual %in% c("Ex")] <- 6
housingtrain1$GarageQual[housingtrain1$GarageQual %in% c("Gd")] <- 5
housingtrain1$GarageQual[housingtrain1$GarageQual %in% c("TA")] <- 4
housingtrain1$GarageQual[housingtrain1$GarageQual %in% c("Fa")] <- 3
housingtrain1$GarageQual[housingtrain1$GarageQual %in% c("No")] <- 2
housingtrain1$GarageQual[housingtrain1$GarageQual %in% c("Po")] <- 1

alldata$GarageQual[alldata$GarageQual %in% c("Ex")] <- 6
alldata$GarageQual[alldata$GarageQual %in% c("Gd")] <- 5
alldata$GarageQual[alldata$GarageQual %in% c("TA")] <- 4
alldata$GarageQual[alldata$GarageQual %in% c("Fa")] <- 3
alldata$GarageQual[alldata$GarageQual %in% c("No")] <- 2
alldata$GarageQual[alldata$GarageQual %in% c("Po")] <- 1



table(housingtrain1$GarageQual)

housingtrain1$GarageQual<-as.integer(housingtrain1$GarageQual)
alldata$GarageQual<-as.integer(alldata$GarageQual)

str(housingtrain1$GarageQual)


table(housingtrain1$GarageQual)
names(which(sapply(housingtrain1,is.character)))

#2O.Converting SaleCondition to numeric

price<-summarize(group_by(alldata, SaleCondition),
                 mean(SalePrice, na.rm=T))

price

housingtrain1$SaleCondition[housingtrain1$SaleCondition %in% c("AdjLand")] <- 1
housingtrain1$SaleCondition[housingtrain1$SaleCondition %in% c("Abnorml")] <- 2
housingtrain1$SaleCondition[housingtrain1$SaleCondition %in% c("Family")] <- 3
housingtrain1$SaleCondition[housingtrain1$SaleCondition %in% c("Alloca")] <- 4
housingtrain1$SaleCondition[housingtrain1$SaleCondition %in% c("Normal")] <- 5
housingtrain1$SaleCondition[housingtrain1$SaleCondition %in% c("Partial")] <- 6

alldata$SaleCondition[alldata$SaleCondition %in% c("AdjLand")] <- 1
alldata$SaleCondition[alldata$SaleCondition %in% c("Abnorml")] <- 2
alldata$SaleCondition[alldata$SaleCondition %in% c("Family")] <- 3
alldata$SaleCondition[alldata$SaleCondition %in% c("Alloca")] <- 4
alldata$SaleCondition[alldata$SaleCondition %in% c("Normal")] <- 5
alldata$SaleCondition[alldata$SaleCondition %in% c("Partial")] <- 6


table(housingtrain1$SaleCondition)

alldata$SaleCondition<-as.integer(alldata$SaleCondition)
housingtrain1$SaleCondition<-as.integer(housingtrain1$SaleCondition)
str(housingtrain1$SaleCondition)


table(housingtrain1$SaleCondition)
names(which(sapply(housingtrain1,is.character)))

-------------------------------------------------------------------------------------------

#2P.Converting Alley to numeric

price<-summarize(group_by(alldata, Alley),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Alley[housingtrain1$Alley %in% c("Grvl")] <- 0
housingtrain1$Alley[housingtrain1$Alley %in% c("Pave")] <- 1
housingtrain1$Alley[housingtrain1$Alley %in% c("No")] <- 2

alldata$Alley[alldata$Alley %in% c("Grvl")] <- 0
alldata$Alley[alldata$Alley %in% c("Pave")] <- 1
alldata$Alley[alldata$Alley %in% c("No")] <- 2


table(housingtrain1$Alley)

alldata$Alley<-as.integer(alldata$Alley)
housingtrain1$Alley<-as.integer(housingtrain1$Alley)
str(housingtrain1$Alley)


table(housingtrain1$Alley)
names(which(sapply(housingtrain1,is.character)))

#2Q.Converting Condition1 to numeric

price <- summarize(group_by(alldata, Condition1),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Condition1[housingtrain1$Condition1 %in% c("Artery")] <- 1
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("RRAe")] <- 2
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("Feedr")] <- 3
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("RRAn")] <- 4
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("Norm")] <- 5
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("RRNe")] <- 6
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("RRNn")] <- 7
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("PosN")] <- 8
housingtrain1$Condition1[housingtrain1$Condition1 %in% c("PosA")] <- 9

alldata$Condition1[alldata$Condition1 %in% c("Artery")] <- 1
alldata$Condition1[alldata$Condition1 %in% c("RRAe")] <- 2
alldata$Condition1[alldata$Condition1 %in% c("Feedr")] <- 3
alldata$Condition1[alldata$Condition1 %in% c("RRAn")] <- 4
alldata$Condition1[alldata$Condition1 %in% c("Norm")] <- 5
alldata$Condition1[alldata$Condition1 %in% c("RRNe")] <- 6
alldata$Condition1[alldata$Condition1 %in% c("RRNn")] <- 7
alldata$Condition1[alldata$Condition1 %in% c("PosN")] <- 8
alldata$Condition1[alldata$Condition1 %in% c("PosA")] <- 9


table(housingtrain1$Condition1)

alldata$Condition1<-as.integer(alldata$Condition1)
housingtrain1$Condition1<-as.integer(housingtrain1$Condition1)
str(housingtrain1$Condition1)


table(housingtrain1$Condition1)
names(which(sapply(housingtrain1,is.character)))

#2R.Converting Condition2 to numeric

price <- summarize(group_by(alldata, Condition2),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Condition2[housingtrain1$Condition2 %in% c("RRNn")] <- 1
housingtrain1$Condition2[housingtrain1$Condition2 %in% c("Artery")] <- 2
housingtrain1$Condition2[housingtrain1$Condition2 %in% c("Feedr")] <- 3
housingtrain1$Condition2[housingtrain1$Condition2 %in% c("RRAn")] <- 4
housingtrain1$Condition2[housingtrain1$Condition2 %in% c("Norm")] <- 5
housingtrain1$Condition2[housingtrain1$Condition2 %in% c("RRAe")] <- 6
housingtrain1$Condition2[housingtrain1$Condition2 %in% c("PosN")] <- 7
housingtrain1$Condition2[housingtrain1$Condition2 %in% c("PosA")] <- 8


alldata$Condition2[alldata$Condition2 %in% c("RRNn")] <- 1
alldata$Condition2[alldata$Condition2 %in% c("Artery")] <- 2
alldata$Condition2[alldata$Condition2 %in% c("Feedr")] <- 3
alldata$Condition2[alldata$Condition2 %in% c("RRAn")] <- 4
alldata$Condition2[alldata$Condition2 %in% c("Norm")] <- 5
alldata$Condition2[alldata$Condition2 %in% c("RRAe")] <- 6
alldata$Condition2[alldata$Condition2 %in% c("PosN")] <- 7
alldata$Condition2[alldata$Condition2 %in% c("PosA")] <- 8


table(housingtrain1$Condition2)

alldata$Condition2<-as.integer(alldata$Condition2)
housingtrain1$Condition2<-as.integer(housingtrain1$Condition2)
str(housingtrain1$Condition2)


table(housingtrain1$Condition2)
names(which(sapply(housingtrain1,is.character)))


#2S.Converting BsmtExposure to numeric

price <- summarize(group_by(alldata, BsmtExposure),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$BsmtExposure[housingtrain1$BsmtExposure %in% c("No")] <- 0
housingtrain1$BsmtExposure[housingtrain1$BsmtExposure %in% c("Mn")] <- 1
housingtrain1$BsmtExposure[housingtrain1$BsmtExposure %in% c("Av")] <- 2
housingtrain1$BsmtExposure[housingtrain1$BsmtExposure %in% c("Gd")] <- 3

alldata$BsmtExposure[alldata$BsmtExposure %in% c("No")] <- 0
alldata$BsmtExposure[alldata$BsmtExposure %in% c("Mn")] <- 1
alldata$BsmtExposure[alldata$BsmtExposure %in% c("Av")] <- 2
alldata$BsmtExposure[alldata$BsmtExposure %in% c("Gd")] <- 3


table(housingtrain1$BsmtExposure)

alldata$BsmtExposure<-as.integer(alldata$BsmtExposure)
housingtrain1$BsmtExposure<-as.integer(housingtrain1$BsmtExposure)
str(housingtrain1$BsmtExposure)


table(housingtrain1$BsmtExposure)
names(which(sapply(housingtrain1,is.character)))

#2T.Converting KitchenQual to numeric

price <- summarize(group_by(alldata, KitchenQual),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$KitchenQual[housingtrain1$KitchenQual %in% c("Fa")] <- 1
housingtrain1$KitchenQual[housingtrain1$KitchenQual %in% c("TA")] <- 2
housingtrain1$KitchenQual[housingtrain1$KitchenQual %in% c("Gd")] <- 3
housingtrain1$KitchenQual[housingtrain1$KitchenQual %in% c("Ex")] <- 4

alldata$KitchenQual[alldata$KitchenQual %in% c("Fa")] <- 1
alldata$KitchenQual[alldata$KitchenQual %in% c("TA")] <- 2
alldata$KitchenQual[alldata$KitchenQual %in% c("Gd")] <- 3
alldata$KitchenQual[alldata$KitchenQual %in% c("Ex")] <- 4


table(housingtrain1$KitchenQual)

alldata$KitchenQual<-as.integer(alldata$KitchenQual)
housingtrain1$KitchenQual<-as.integer(housingtrain1$KitchenQual)
str(housingtrain1$KitchenQual)


table(housingtrain1$KitchenQual)
names(which(sapply(housingtrain1,is.character)))

#2U.Converting GarageCond to numeric

price <- summarize(group_by(alldata, GarageCond),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$GarageCond[housingtrain1$GarageCond %in% c("No")] <- 0
housingtrain1$GarageCond[housingtrain1$GarageCond %in% c("Po")] <- 1
housingtrain1$GarageCond[housingtrain1$GarageCond %in% c("Fa")] <- 2
housingtrain1$GarageCond[housingtrain1$GarageCond %in% c("Ex")] <- 3
housingtrain1$GarageCond[housingtrain1$GarageCond %in% c("Gd")] <- 4
housingtrain1$GarageCond[housingtrain1$GarageCond %in% c("TA")] <- 5

alldata$GarageCond[alldata$GarageCond %in% c("No")] <- 0
alldata$GarageCond[alldata$GarageCond %in% c("Po")] <- 1
alldata$GarageCond[alldata$GarageCond %in% c("Fa")] <- 2
alldata$GarageCond[alldata$GarageCond %in% c("Ex")] <- 3
alldata$GarageCond[alldata$GarageCond %in% c("Gd")] <- 4
alldata$GarageCond[alldata$GarageCond %in% c("TA")] <- 5


table(housingtrain1$GarageCond)


alldata$GarageCond<-as.integer(alldata$GarageCond)
housingtrain1$GarageCond<-as.integer(housingtrain1$GarageCond)
str(housingtrain1$GarageCond)


table(housingtrain1$GarageCond)


names(which(sapply(housingtrain1,is.character)))

#2V.Converting LotShape to numeric

price <- summarize(group_by(alldata, LotShape),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$LotShape[housingtrain1$LotShape%in% c("Reg")] <- 1
housingtrain1$LotShape[housingtrain1$LotShape%in% c("IR1")] <- 2
housingtrain1$LotShape[housingtrain1$LotShape%in% c("IR3")] <- 3
housingtrain1$LotShape[housingtrain1$LotShape%in% c("IR2")] <- 4


alldata$LotShape[alldata$LotShape%in% c("Reg")] <- 1
alldata$LotShape[alldata$LotShape%in% c("IR1")] <- 2
alldata$LotShape[alldata$LotShape%in% c("IR3")] <- 3
alldata$LotShape[alldata$LotShape%in% c("IR2")] <- 4

table(housingtrain1$LotShape)


alldata$LotShape<-as.integer(alldata$LotShape)
housingtrain1$LotShape<-as.integer(housingtrain1$LotShape)
str(housingtrain1$LotShape)


table(housingtrain1$LotShape)


names(which(sapply(housingtrain1,is.character)))

#2W.Converting LandContour to numeric

price <- summarize(group_by(alldata, LandContour),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$LandContour[housingtrain1$LandContour%in% c("Bnk")] <- 1
housingtrain1$LandContour[housingtrain1$LandContour%in% c("Lvl")] <- 2
housingtrain1$LandContour[housingtrain1$LandContour%in% c("Low")] <- 3
housingtrain1$LandContour[housingtrain1$LandContour%in% c("HLS")] <- 4

alldata$LandContour[alldata$LandContour%in% c("Bnk")] <- 1
alldata$LandContour[alldata$LandContour%in% c("Lvl")] <- 2
alldata$LandContour[alldata$LandContour%in% c("Low")] <- 3
alldata$LandContour[alldata$LandContour%in% c("HLS")] <- 4


table(housingtrain1$LandContour)

alldata$LandContour<-as.integer(alldata$LandContour)
housingtrain1$LandContour<-as.integer(housingtrain1$LandContour)
str(housingtrain1$LandContour)


table(housingtrain1$LandContour)


names(which(sapply(housingtrain1,is.character)))

#2X.Converting Utilities to numeric

price <- summarize(group_by(alldata, Utilities),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Utilities[housingtrain1$Utilities%in% c("NoSeWa")] <- 1
housingtrain1$Utilities[housingtrain1$Utilities%in% c("AllPub")] <- 2

alldata$Utilities[alldata$Utilities%in% c("NoSeWa")] <- 1
alldata$Utilities[alldata$Utilities%in% c("AllPub")] <- 2

table(housingtrain1$Utilities)

alldata$Utilities<-as.integer(alldata$Utilities)
housingtrain1$Utilities<-as.integer(housingtrain1$Utilities)
str(housingtrain1$Utilities)


table(housingtrain1$Utilities)


names(which(sapply(housingtrain1,is.character)))

#2Y.Converting LotConfig to numeric

price <- summarize(group_by(alldata, LotConfig),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$LotConfig[housingtrain1$LotConfig%in% c("Inside")] <- 1
housingtrain1$LotConfig[housingtrain1$LotConfig%in% c("FR2")] <- 2
housingtrain1$LotConfig[housingtrain1$LotConfig%in% c("Corner")] <- 3
housingtrain1$LotConfig[housingtrain1$LotConfig%in% c("FR3")] <- 4
housingtrain1$LotConfig[housingtrain1$LotConfig%in% c("CulDSac")] <- 5

alldata$LotConfig[alldata$LotConfig%in% c("Inside")] <- 1
alldata$LotConfig[alldata$LotConfig%in% c("FR2")] <- 2
alldata$LotConfig[alldata$LotConfig%in% c("Corner")] <- 3
alldata$LotConfig[alldata$LotConfig%in% c("FR3")] <- 4
alldata$LotConfig[alldata$LotConfig%in% c("CulDSac")] <- 5



table(housingtrain1$LotConfig)

alldata$LotConfig<-as.integer(alldata$LotConfig)
housingtrain1$LotConfig<-as.integer(housingtrain1$LotConfig)
str(housingtrain1$LotConfig)


table(housingtrain1$LotConfig)


names(which(sapply(housingtrain1,is.character)))

#2Z.Converting BldgType to numeric

price <- summarize(group_by(alldata, BldgType),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$BldgType[housingtrain1$BldgType %in% c("2fmCon")] <- 1
housingtrain1$BldgType[housingtrain1$BldgType %in% c("Duplex")] <- 2
housingtrain1$BldgType[housingtrain1$BldgType %in% c("Twnhs")] <- 3
housingtrain1$BldgType[housingtrain1$BldgType %in% c("TwnhsE")] <- 4
housingtrain1$BldgType[housingtrain1$BldgType %in% c("1Fam")] <- 5


alldata$BldgType[alldata$BldgType %in% c("2fmCon")] <- 1
alldata$BldgType[alldata$BldgType %in% c("Duplex")] <- 2
alldata$BldgType[alldata$BldgType %in% c("Twnhs")] <- 3
alldata$BldgType[alldata$BldgType %in% c("TwnhsE")] <- 4
alldata$BldgType[alldata$BldgType %in% c("1Fam")] <- 5

table(housingtrain1$BldgType)

alldata$BldgType<-as.integer(alldata$BldgType)
housingtrain1$BldgType<-as.integer(housingtrain1$BldgType)
str(housingtrain1$BldgType)


table(housingtrain1$BldgType)


names(which(sapply(housingtrain1,is.character)))


#2AA.Converting HouseStyle to numeric

price <- summarize(group_by(alldata, HouseStyle),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("1.5Unf")] <- 1
housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("SFoyer")] <- 2
housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("1.5Fin")] <- 3
housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("2.5Unf")] <- 4
housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("SLvl")] <- 5
housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("1Story")] <- 6
housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("2Story")] <- 7
housingtrain1$HouseStyle[housingtrain1$HouseStyle %in% c("2.5Fin")] <- 8


alldata$HouseStyle[alldata$HouseStyle %in% c("1.5Unf")] <- 1
alldata$HouseStyle[alldata$HouseStyle %in% c("SFoyer")] <- 2
alldata$HouseStyle[alldata$HouseStyle %in% c("1.5Fin")] <- 3
alldata$HouseStyle[alldata$HouseStyle %in% c("2.5Unf")] <- 4
alldata$HouseStyle[alldata$HouseStyle %in% c("SLvl")] <- 5
alldata$HouseStyle[alldata$HouseStyle %in% c("1Story")] <- 6
alldata$HouseStyle[alldata$HouseStyle %in% c("2Story")] <- 7
alldata$HouseStyle[alldata$HouseStyle %in% c("2.5Fin")] <- 8


table(housingtrain1$HouseStyle)

alldata$HouseStyle<-as.integer(alldata$HouseStyle)
housingtrain1$HouseStyle<-as.integer(housingtrain1$HouseStyle)
str(housingtrain1$HouseStyle)


table(housingtrain1$HouseStyle)


names(which(sapply(housingtrain1,is.character)))

#2BB.Converting RoofStyle to numeric

price <- summarize(group_by(alldata, RoofStyle),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$RoofStyle[housingtrain1$RoofStyle %in% c("Gambrel")] <- 1
housingtrain1$RoofStyle[housingtrain1$RoofStyle %in% c("Gable")] <- 2
housingtrain1$RoofStyle[housingtrain1$RoofStyle %in% c("Mansard")] <- 3
housingtrain1$RoofStyle[housingtrain1$RoofStyle %in% c("Flat")] <- 4
housingtrain1$RoofStyle[housingtrain1$RoofStyle %in% c("Hip")] <- 5
housingtrain1$RoofStyle[housingtrain1$RoofStyle %in% c("Shed")] <- 6

alldata$RoofStyle[alldata$RoofStyle %in% c("Gambrel")] <- 1
alldata$RoofStyle[alldata$RoofStyle %in% c("Gable")] <- 2
alldata$RoofStyle[alldata$RoofStyle %in% c("Mansard")] <- 3
alldata$RoofStyle[alldata$RoofStyle %in% c("Flat")] <- 4
alldata$RoofStyle[alldata$RoofStyle %in% c("Hip")] <- 5
alldata$RoofStyle[alldata$RoofStyle %in% c("Shed")] <- 6



table(housingtrain1$RoofStyle)

alldata$RoofStyle<-as.integer(alldata$RoofStyle)
housingtrain1$RoofStyle<-as.integer(housingtrain1$RoofStyle)
str(housingtrain1$RoofStyle)


table(housingtrain1$RoofStyle)


names(which(sapply(housingtrain1,is.character)))

#2CC.Converting MasVnrType to numeric

price <- summarize(group_by(alldata, MasVnrType),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$MasVnrType[housingtrain1$MasVnrType %in% c("BrkCmn")] <- 1
housingtrain1$MasVnrType[housingtrain1$MasVnrType %in% c("None")] <- 2
housingtrain1$MasVnrType[housingtrain1$MasVnrType %in% c("BrkFace")] <- 3
housingtrain1$MasVnrType[housingtrain1$MasVnrType %in% c("Stone")] <- 4


alldata$MasVnrType[alldata$MasVnrType %in% c("BrkCmn")] <- 1
alldata$MasVnrType[alldata$MasVnrType %in% c("None")] <- 2
alldata$MasVnrType[alldata$MasVnrType %in% c("BrkFace")] <- 3
alldata$MasVnrType[alldata$MasVnrType %in% c("Stone")] <- 4

table(housingtrain1$MasVnrType)

alldata$MasVnrType<-as.integer(alldata$MasVnrType)
housingtrain1$MasVnrType<-as.integer(housingtrain1$MasVnrType)
str(housingtrain1$MasVnrType)


table(housingtrain1$MasVnrType)


names(which(sapply(housingtrain1,is.character)))


#2DD.Converting ExterQual to numeric

price <- summarize(group_by(alldata, ExterQual),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$ExterQual[housingtrain1$ExterQual %in% c("Fa")] <- 1
housingtrain1$ExterQual[housingtrain1$ExterQual %in% c("TA")] <- 2
housingtrain1$ExterQual[housingtrain1$ExterQual %in% c("Gd")] <- 3
housingtrain1$ExterQual[housingtrain1$ExterQual %in% c("Ex")] <- 4

alldata$ExterQual[alldata$ExterQual %in% c("Fa")] <- 1
alldata$ExterQual[alldata$ExterQual %in% c("TA")] <- 2
alldata$ExterQual[alldata$ExterQual %in% c("Gd")] <- 3
alldata$ExterQual[alldata$ExterQual %in% c("Ex")] <- 4

table(housingtrain1$ExterQual)

alldata$ExterQual<-as.integer(alldata$ExterQual)
housingtrain1$ExterQual<-as.integer(housingtrain1$ExterQual)
str(housingtrain1$ExterQual)


table(housingtrain1$ExterQual)


names(which(sapply(housingtrain1,is.character)))

#2EE.Converting ExterCond to numeric

price <- summarize(group_by(alldata, ExterCond),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$ExterCond[housingtrain1$ExterCond %in% c("Po")] <- 1
housingtrain1$ExterCond[housingtrain1$ExterCond %in% c("Fa")] <- 2
housingtrain1$ExterCond[housingtrain1$ExterCond %in% c("Gd")] <- 3
housingtrain1$ExterCond[housingtrain1$ExterCond %in% c("TA")] <- 4
housingtrain1$ExterCond[housingtrain1$ExterCond %in% c("Ex")] <- 5


alldata$ExterCond[alldata$ExterCond %in% c("Po")] <- 1
alldata$ExterCond[alldata$ExterCond %in% c("Fa")] <- 2
alldata$ExterCond[alldata$ExterCond %in% c("Gd")] <- 3
alldata$ExterCond[alldata$ExterCond %in% c("TA")] <- 4
alldata$ExterCond[alldata$ExterCond %in% c("Ex")] <- 5


table(housingtrain1$ExterCond)

alldata$ExterCond<-as.integer(alldata$ExterCond)
housingtrain1$ExterCond<-as.integer(housingtrain1$ExterCond)
str(housingtrain1$ExterCond)


table(housingtrain1$ExterCond)


names(which(sapply(housingtrain1,is.character)))


#2FF.Converting ExterCond to numeric

price <- summarize(group_by(alldata, Foundation),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Foundation[housingtrain1$Foundation %in% c("Slab")] <- 1
housingtrain1$Foundation[housingtrain1$Foundation %in% c("BrkTil")] <- 2
housingtrain1$Foundation[housingtrain1$Foundation %in% c("CBlock")] <- 3
housingtrain1$Foundation[housingtrain1$Foundation %in% c("Stone")] <- 4
housingtrain1$Foundation[housingtrain1$Foundation %in% c("Wood")] <- 5
housingtrain1$Foundation[housingtrain1$Foundation %in% c("PConc")] <- 6


alldata$Foundation[alldata$Foundation %in% c("Slab")] <- 1
alldata$Foundation[alldata$Foundation %in% c("BrkTil")] <- 2
alldata$Foundation[alldata$Foundation %in% c("CBlock")] <- 3
alldata$Foundation[alldata$Foundation %in% c("Stone")] <- 4
alldata$Foundation[alldata$Foundation %in% c("Wood")] <- 5
alldata$Foundation[alldata$Foundation %in% c("PConc")] <- 6

table(housingtrain1$Foundation)

alldata$Foundation<-as.integer(alldata$Foundation)
housingtrain1$Foundation<-as.integer(housingtrain1$Foundation)
str(housingtrain1$Foundation)


table(housingtrain1$Foundation)


names(which(sapply(housingtrain1,is.character)))

#2GG.Converting BsmtFinType1 to numeric

price <- summarize(group_by(alldata, BsmtFinType1),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$BsmtFinType1[housingtrain1$BsmtFinType1 %in% c("No")] <- 0
housingtrain1$BsmtFinType1[housingtrain1$BsmtFinType1 %in% c("Rec")] <- 1
housingtrain1$BsmtFinType1[housingtrain1$BsmtFinType1 %in% c("BLQ")] <- 2
housingtrain1$BsmtFinType1[housingtrain1$BsmtFinType1 %in% c("LwQ")] <- 3
housingtrain1$BsmtFinType1[housingtrain1$BsmtFinType1 %in% c("ALQ")] <- 4
housingtrain1$BsmtFinType1[housingtrain1$BsmtFinType1 %in% c("Unf")] <- 5
housingtrain1$BsmtFinType1[housingtrain1$BsmtFinType1 %in% c("GLQ")] <- 6

alldata$BsmtFinType1[alldata$BsmtFinType1 %in% c("No")] <- 0
alldata$BsmtFinType1[alldata$BsmtFinType1 %in% c("Rec")] <- 1
alldata$BsmtFinType1[alldata$BsmtFinType1 %in% c("BLQ")] <- 2
alldata$BsmtFinType1[alldata$BsmtFinType1 %in% c("LwQ")] <- 3
alldata$BsmtFinType1[alldata$BsmtFinType1 %in% c("ALQ")] <- 4
alldata$BsmtFinType1[alldata$BsmtFinType1 %in% c("Unf")] <- 5
alldata$BsmtFinType1[alldata$BsmtFinType1 %in% c("GLQ")] <- 6


table(housingtrain1$BsmtFinType1)

alldata$BsmtFinType1<-as.integer(alldata$BsmtFinType1)
housingtrain1$BsmtFinType1<-as.integer(housingtrain1$BsmtFinType1)
str(housingtrain1$BsmtFinType1)


table(housingtrain1$BsmtFinType1)


names(which(sapply(housingtrain1,is.character)))

#2HH.Converting BsmtFinType2 to numeric

price <- summarize(group_by(alldata, BsmtFinType2),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$BsmtFinType2[housingtrain1$BsmtFinType2 %in% c("No")] <- 0
housingtrain1$BsmtFinType2[housingtrain1$BsmtFinType2 %in% c("BLQ")] <- 1
housingtrain1$BsmtFinType2[housingtrain1$BsmtFinType2 %in% c("LwQ")] <- 2
housingtrain1$BsmtFinType2[housingtrain1$BsmtFinType2 %in% c("Rec")] <- 3
housingtrain1$BsmtFinType2[housingtrain1$BsmtFinType2 %in% c("GLQ")] <- 4
housingtrain1$BsmtFinType2[housingtrain1$BsmtFinType2 %in% c("Unf")] <- 5
housingtrain1$BsmtFinType2[housingtrain1$BsmtFinType2 %in% c("ALQ")] <- 6


alldata$BsmtFinType2[alldata$BsmtFinType2 %in% c("No")] <- 0
alldata$BsmtFinType2[alldata$BsmtFinType2 %in% c("BLQ")] <- 1
alldata$BsmtFinType2[alldata$BsmtFinType2 %in% c("LwQ")] <- 2
alldata$BsmtFinType2[alldata$BsmtFinType2 %in% c("Rec")] <- 3
alldata$BsmtFinType2[alldata$BsmtFinType2 %in% c("GLQ")] <- 4
alldata$BsmtFinType2[alldata$BsmtFinType2 %in% c("Unf")] <- 5
alldata$BsmtFinType2[alldata$BsmtFinType2 %in% c("ALQ")] <- 6



table(housingtrain1$BsmtFinType2)

alldata$BsmtFinType2<-as.integer(alldata$BsmtFinType2)
housingtrain1$BsmtFinType2<-as.integer(housingtrain1$BsmtFinType2)
str(housingtrain1$BsmtFinType2)


table(housingtrain1$BsmtFinType2)


names(which(sapply(housingtrain1,is.character)))


#2II.Converting Heating to numeric

price <- summarize(group_by(alldata, Heating),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Heating[housingtrain1$Heating %in% c("Floor")] <- 1
housingtrain1$Heating[housingtrain1$Heating %in% c("Grav")] <- 2
housingtrain1$Heating[housingtrain1$Heating %in% c("Wall")] <- 3
housingtrain1$Heating[housingtrain1$Heating %in% c("OthW")] <- 4
housingtrain1$Heating[housingtrain1$Heating %in% c("GasW")] <- 5
housingtrain1$Heating[housingtrain1$Heating %in% c("GasA")] <- 6


alldata$Heating[alldata$Heating %in% c("Floor")] <- 1
alldata$Heating[alldata$Heating %in% c("Grav")] <- 2
alldata$Heating[alldata$Heating %in% c("Wall")] <- 3
alldata$Heating[alldata$Heating %in% c("OthW")] <- 4
alldata$Heating[alldata$Heating %in% c("GasW")] <- 5
alldata$Heating[alldata$Heating %in% c("GasA")] <- 6

table(housingtrain1$Heating)

alldata$Heating<-as.integer(alldata$Heating)
housingtrain1$Heating<-as.integer(housingtrain1$Heating)
str(housingtrain1$Heating)


table(housingtrain1$Heating)


names(which(sapply(housingtrain1,is.character)))

#2JJ.Converting HeatingQC to numeric

price <- summarize(group_by(alldata, HeatingQC),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$HeatingQC[housingtrain1$HeatingQC %in% c("Po")] <- 1
housingtrain1$HeatingQC[housingtrain1$HeatingQC %in% c("Fa")] <- 2
housingtrain1$HeatingQC[housingtrain1$HeatingQC %in% c("TA")] <- 3
housingtrain1$HeatingQC[housingtrain1$HeatingQC %in% c("Gd")] <- 4
housingtrain1$HeatingQC[housingtrain1$HeatingQC %in% c("Ex")] <- 5

alldata$HeatingQC[alldata$HeatingQC %in% c("Po")] <- 1
alldata$HeatingQC[alldata$HeatingQC %in% c("Fa")] <- 2
alldata$HeatingQC[alldata$HeatingQC %in% c("TA")] <- 3
alldata$HeatingQC[alldata$HeatingQC %in% c("Gd")] <- 4
alldata$HeatingQC[alldata$HeatingQC %in% c("Ex")] <- 5


table(housingtrain1$HeatingQC)

alldata$HeatingQC<-as.integer(alldata$HeatingQC)
housingtrain1$HeatingQC<-as.integer(housingtrain1$HeatingQC)
str(housingtrain1$HeatingQC)


table(housingtrain1$HeatingQC)


names(which(sapply(housingtrain1,is.character)))

#2KK.Converting Functional to numeric

price <- summarize(group_by(alldata, Functional),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Functional[housingtrain1$Functional %in% c("Maj2")] <- 1
housingtrain1$Functional[housingtrain1$Functional %in% c("Sev")] <- 2
housingtrain1$Functional[housingtrain1$Functional %in% c("Min2")] <- 3
housingtrain1$Functional[housingtrain1$Functional %in% c("Min1")] <- 4
housingtrain1$Functional[housingtrain1$Functional %in% c("Maj1")] <- 5
housingtrain1$Functional[housingtrain1$Functional %in% c("Mod")] <- 6
housingtrain1$Functional[housingtrain1$Functional %in% c("Typ")] <- 7


alldata$Functional[alldata$Functional %in% c("Maj2")] <- 1
alldata$Functional[alldata$Functional %in% c("Sev")] <- 2
alldata$Functional[alldata$Functional %in% c("Min2")] <- 3
alldata$Functional[alldata$Functional %in% c("Min1")] <- 4
alldata$Functional[alldata$Functional %in% c("Maj1")] <- 5
alldata$Functional[alldata$Functional %in% c("Mod")] <- 6
alldata$Functional[alldata$Functional %in% c("Typ")] <- 7

table(housingtrain1$Functional)

alldata$Functional<-as.integer(alldata$Functional)
housingtrain1$Functional<-as.integer(housingtrain1$Functional)
str(housingtrain1$Functional)


table(housingtrain1$Functional)


names(which(sapply(housingtrain1,is.character)))


#2LL.Converting FireplaceQu to numeric

price <- summarize(group_by(alldata, FireplaceQu),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$FireplaceQu[housingtrain1$FireplaceQu %in% c("Po")] <- 0
housingtrain1$FireplaceQu[housingtrain1$FireplaceQu %in% c("No")] <- 1
housingtrain1$FireplaceQu[housingtrain1$FireplaceQu %in% c("Fa")] <- 2
housingtrain1$FireplaceQu[housingtrain1$FireplaceQu %in% c("TA")] <- 3
housingtrain1$FireplaceQu[housingtrain1$FireplaceQu %in% c("Gd")] <- 4
housingtrain1$FireplaceQu[housingtrain1$FireplaceQu %in% c("Ex")] <- 5

alldata$FireplaceQu[alldata$FireplaceQu %in% c("Po")] <- 0
alldata$FireplaceQu[alldata$FireplaceQu %in% c("No")] <- 1
alldata$FireplaceQu[alldata$FireplaceQu %in% c("Fa")] <- 2
alldata$FireplaceQu[alldata$FireplaceQu %in% c("TA")] <- 3
alldata$FireplaceQu[alldata$FireplaceQu %in% c("Gd")] <- 4
alldata$FireplaceQu[alldata$FireplaceQu %in% c("Ex")] <- 5


table(housingtrain1$FireplaceQu)

alldata$FireplaceQu<-as.integer(alldata$FireplaceQu)
housingtrain1$FireplaceQu<-as.integer(housingtrain1$FireplaceQu)
str(housingtrain1$FireplaceQu)


table(housingtrain1$FireplaceQu)


names(which(sapply(housingtrain1,is.character)))

#2MM.Converting GarageType to numeric

price <- summarize(group_by(alldata, GarageType),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$GarageType[housingtrain1$GarageType %in% c("No")] <- 0
housingtrain1$GarageType[housingtrain1$GarageType %in% c("CarPort")] <- 1
housingtrain1$GarageType[housingtrain1$GarageType %in% c("Detchd")] <- 2
housingtrain1$GarageType[housingtrain1$GarageType %in% c("2Types")] <- 3
housingtrain1$GarageType[housingtrain1$GarageType %in% c("Basment")] <- 4
housingtrain1$GarageType[housingtrain1$GarageType %in% c("Attchd")] <- 5
housingtrain1$GarageType[housingtrain1$GarageType %in% c("BuiltIn")] <- 6


alldata$GarageType[alldata$GarageType %in% c("No")] <- 0
alldata$GarageType[alldata$GarageType %in% c("CarPort")] <- 1
alldata$GarageType[alldata$GarageType %in% c("Detchd")] <- 2
alldata$GarageType[alldata$GarageType %in% c("2Types")] <- 3
alldata$GarageType[alldata$GarageType %in% c("Basment")] <- 4
alldata$GarageType[alldata$GarageType %in% c("Attchd")] <- 5
alldata$GarageType[alldata$GarageType %in% c("BuiltIn")] <- 6


table(housingtrain1$GarageType)

alldata$GarageType<-as.integer(alldata$GarageType)
housingtrain1$GarageType<-as.integer(housingtrain1$GarageType)
str(housingtrain1$GarageType)


table(housingtrain1$GarageType)


names(which(sapply(housingtrain1,is.character)))

#2NN.Converting GarageYrBlt to numeric

price <- summarize(group_by(alldata, GarageYrBlt),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$GarageYrBlt<-year(as.Date(housingtrain1$GarageYrBlt, "%Y"))


housingtrain1$GarageYrBlt[is.na(housingtrain1$GarageYrBlt)]<-0

housingtrain1$GarageYrBlt<-as.numeric(housingtrain1$GarageYrBlt)

alldata$GarageYrBlt<-year(as.Date(alldata$GarageYrBlt, "%Y"))


alldata$GarageYrBlt[is.na(alldata$GarageYrBlt)]<-0

alldata$GarageYrBlt<-as.numeric(alldata$GarageYrBlt)


str(housingtrain1$GarageYrBlt)

names(which(sapply(housingtrain1,is.character)))

#2OO.Converting PavedDrive to numeric

price <- summarize(group_by(alldata, PavedDrive),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$PavedDrive[housingtrain1$PavedDrive %in% c("N")] <- 0
housingtrain1$PavedDrive[housingtrain1$PavedDrive %in% c("P")] <- 1
housingtrain1$PavedDrive[housingtrain1$PavedDrive %in% c("Y")] <- 2

alldata$PavedDrive[alldata$PavedDrive %in% c("N")] <- 0
alldata$PavedDrive[alldata$PavedDrive %in% c("P")] <- 1
alldata$PavedDrive[alldata$PavedDrive %in% c("Y")] <- 2

table(housingtrain1$PavedDrive)

alldata$PavedDrive<-as.integer(alldata$PavedDrive)
housingtrain1$PavedDrive<-as.integer(housingtrain1$PavedDrive)
str(housingtrain1$PavedDrive)


table(housingtrain1$PavedDrive)


names(which(sapply(housingtrain1,is.character)))

#2PP.Converting PoolQC to numeric

price <- summarize(group_by(alldata, PoolQC),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$PoolQC[housingtrain1$PoolQC %in% c("No")] <- 0
housingtrain1$PoolQC[housingtrain1$PoolQC %in% c("Gd")] <- 1
housingtrain1$PoolQC[housingtrain1$PoolQC %in% c("Fa")] <- 2
housingtrain1$PoolQC[housingtrain1$PoolQC %in% c("Ex")] <- 3

alldata$PoolQC[alldata$PoolQC %in% c("No")] <- 0
alldata$PoolQC[alldata$PoolQC %in% c("Gd")] <- 1
alldata$PoolQC[alldata$PoolQC %in% c("Fa")] <- 2
alldata$PoolQC[alldata$PoolQC %in% c("Ex")] <- 3


table(housingtrain1$PoolQC)

alldata$PoolQC<-as.integer(alldata$PoolQC)
housingtrain1$PoolQC<-as.integer(housingtrain1$PoolQC)
str(housingtrain1$PoolQC)


table(housingtrain1$PoolQC)


names(which(sapply(housingtrain1,is.character)))

#2QQ.Converting Fence to numeric

price <- summarize(group_by(alldata, Fence),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$Fence[housingtrain1$Fence %in% c("MnWw")] <- 0
housingtrain1$Fence[housingtrain1$Fence %in% c("GdWo")] <- 1
housingtrain1$Fence[housingtrain1$Fence %in% c("MnPrv")] <- 2
housingtrain1$Fence[housingtrain1$Fence %in% c("GdPrv")] <- 3
housingtrain1$Fence[housingtrain1$Fence %in% c("No")] <- 5

alldata$Fence[alldata$Fence %in% c("MnWw")] <- 0
alldata$Fence[alldata$Fence %in% c("GdWo")] <- 1
alldata$Fence[alldata$Fence %in% c("MnPrv")] <- 2
alldata$Fence[alldata$Fence %in% c("GdPrv")] <- 3
alldata$Fence[alldata$Fence %in% c("No")] <- 5


table(housingtrain1$Fence)


alldata$Fence<-as.integer(alldata$Fence)
housingtrain1$Fence<-as.integer(housingtrain1$Fence)
str(housingtrain1$Fence)


table(housingtrain1$Fence)


names(which(sapply(housingtrain1,is.character)))

#2RR.Converting MiscFeature to numeric

price <- summarize(group_by(alldata, MiscFeature),
                   mean(SalePrice, na.rm=T))

price

housingtrain1$MiscFeature[housingtrain1$MiscFeature %in% c("Othr")] <- 0
housingtrain1$MiscFeature[housingtrain1$MiscFeature %in% c("Shed")] <- 1
housingtrain1$MiscFeature[housingtrain1$MiscFeature %in% c("Gar2")] <- 2
housingtrain1$MiscFeature[housingtrain1$MiscFeature %in% c("No")] <- 3
housingtrain1$MiscFeature[housingtrain1$MiscFeature %in% c("TenC")] <- 4

alldata$MiscFeature[alldata$MiscFeature %in% c("Othr")] <- 0
alldata$MiscFeature[alldata$MiscFeature %in% c("Shed")] <- 1
alldata$MiscFeature[alldata$MiscFeature %in% c("Gar2")] <- 2
alldata$MiscFeature[alldata$MiscFeature %in% c("No")] <- 3
alldata$MiscFeature[alldata$MiscFeature %in% c("TenC")] <- 4


table(housingtrain1$MiscFeature)

alldata$MiscFeature<-as.integer(alldata$MiscFeature)
housingtrain1$MiscFeature<-as.integer(housingtrain1$MiscFeature)
str(housingtrain1$MiscFeature)


table(housingtrain1$MiscFeature)


names(which(sapply(housingtrain1,is.character)))
names(which(sapply(housingtrain1,is.numeric)))
names(which(sapply(alldata,is.character)))
names(which(sapply(alldata,is.numeric)))
#-----------------------------------------------------------------------------------------------


#3. Exploring Correlations

summary(housingtrain1)
str(housingtrain1)

dim(housingtrain1)

edgebundle(cor(alldata), cutoff = 0.64, tension = 0.5)
edgebundle(cor(housingtrain1), cutoff = 0.64, tension = 0.5)

#

#4. Outliers

#GrLivArea drop outliers
ggplot(housingtrain1, aes(x=GrLivArea)) +
  geom_histogram(fill='black',color='white') + 
  theme_minimal()
#
ggplot(housingtrain1, aes(x=OverallQual)) +
  geom_histogram(fill='black',color='white') +
  theme_minimal()

#
ggplot(housingtrain1, aes(x=KitchenQual)) +
  geom_histogram(fill='black',color='white') +
  theme_minimal()

#
ggplot(housingtrain1, aes(x=ExterQual)) +
  geom_histogram(fill='black',color='white') +
  theme_minimal()

#
ggplot(housingtrain1, aes(x=BsmtQual)) +
  geom_histogram(fill='black',color='white') +
  theme_minimal()

dim(alldata)
dim(housingtrain1)

#We see that out of the highly correlated variables with SalesPrice GrLivArea has two large outliers that are affecting the SalesPrice.
#We will remove those outlier values from GrLivArea.

outliers<-which(housingtrain1$GrLivArea>4100)
outliers
housingtrain1 <- housingtrain1[!1:nrow(housingtrain1) %in% outliers,]
alldata <- alldata[!1:nrow(alldata) %in% outliers,]
dim(alldata)
dim(housingtrain1)

str(alldata)
str(housingtrain1)




################################################################

#5 Normalize


columns <- sapply(names(alldata), function(i){class(alldata[[i]])})

numCols <- names(columns[columns == "integer" | columns == "numeric"])
skewed <- sapply(numCols, function(i){skewness(alldata[[i]], na.rm = T)})
skewed <- skewed[skewed > 0.75]

skewed

#Normalizing

alldata_normal <- alldata
for(i in names(skewed)) 
{
  alldata_normal[[i]] = log(alldata_normal[[i]] + 1)
  
}

# do the same for the rest of the housingdata1 (remember this is the entire dataset[train + test] transformed)
housingtrain1_normal <-housingtrain1
for(i in names(skewed)) 
{
  if(i != "SalePrice")
  {
    housingtrain1_normal[[i]] = log(housingtrain1_normal[[i]] + 1)
  }
}


#6. Removing low variance variables

zeroVar_alldata<-nearZeroVar(alldata_normal, saveMetrics = T)
zeroVar_housingtrain1<-nearZeroVar(housingtrain1_normal, saveMetrics = T)

dropCols_alldata <- rownames(zeroVar_alldata)[zeroVar_alldata$nzv == TRUE]
dropCols_housingtrain1 <- rownames(zeroVar_housingtrain1)[zeroVar_housingtrain1$nzv == TRUE]

alldata_normal_1<- alldata_normal[,!names(alldata_normal) %in% dropCols_alldata]
housingtrain1_normal_1 <- housingtrain1_normal[,!names(housingtrain1_normal) %in% dropCols_housingtrain1]

dim(housingtrain1_normal)
dim(alldata_normal)

dim(housingtrain1_normal_1)
dim(alldata_normal_1)

edgebundle(cor(alldata_normal_1), cutoff = 0.64, tension = 0.5)
edgebundle(cor(housingtrain1_normal_1), cutoff = 0.65, tension = 0.5)

corrplot(cor(alldata_normal_1), title = "Correlation among variables and between the target variable", method = "square")
corrplot(cor(alldata_normal_1), title = "Correlation among variables and between the target variable", method = "square")

#7. Conducting Linear Model


outcome <- alldata_normal_1$SalePrice

part <- createDataPartition(y=outcome,p=0.5,list=F)

train<-alldata_normal_1[part,]
test<-alldata_normal_1[-part,]

set.seed(100)

lm_model <- lm(SalePrice ~ ., data=train)


prediction <- predict(lm_model, test, type="response")

model_output <- cbind(test, prediction)

rmse(model_output$SalePrice,model_output$prediction)

sqrt(mean((model_output$SalePrice - model_output$prediction)^2, na.rm=T))

#RMSE 0.1169

#7A.Conducting Linear Model with all the data so that we can judge its true predictive power
  
##Creating Real Test Set from Supplied Data
main_test<-read.csv("C:/Users/AYMANS/Desktop/DataAnalytics4/sample_submission.csv", stringsAsFactors = FALSE) #Importing test Sale's Price
fulldatatrain<-housingtrain1_normal_1[1:1456,] #first half of the data set (this is what we had before)
fulldatatest<-housingtrain1_normal_1[1457:2914,1:59] #the rest of the data set minus the Sale's Price which is NA
fulldatatrain$SalePrice<-alldata_normal_1$SalePrice #Adding the Sale Price back into the training set

head(main_test)
head(fulldatatest)


fulldatatest1<-merge(x=fulldatatest,y=main_test,by="Id",all = TRUE)
dim(fulldatatest1)
fulldatatest2<-fulldatatest1[-1090,-59] #Removing NA or blank row and column values 
dim(fulldatatest2)

names(fulldatatest2)[names(fulldatatest2)=='SalePrice.y']<-'SalePrice'

fulldatatest2$SalePrice <- log(fulldatatest2$SalePrice + 1)

##Finished Creating
#Conducting Test Now

train<-fulldatatrain
test<-fulldatatest2

out<-fulldatatrain$SalePrice

part<-createDataPartition(y=out, p=0.5, list = FALSE)

set.seed(100)

lm_model <- lm(SalePrice ~ ., data=train)


prediction <- predict(lm_model, test, type="response")

model_output <- cbind(test, prediction)




rmse(model_output$SalePrice,model_output$prediction)
sqrt(mean((model_output$SalePrice - model_output$prediction)^2, na.rm=T))




#RMSE on full data 0.388
##############################################


#7B. Linear Model using 10 fold Cross Validation to see if we can reduce over fitting and reduce our RMSE score
train<-fulldatatrain
test<-fulldatatest2


CV <- trainControl(method = "cv", number = 10, verboseIter = FALSE)

model_lm <- train(SalePrice ~ ., data = train, method = "lm", trControl = CV)

prediction_lm <- predict(model_lm, test)

model_output <- cbind(test, prediction_lm)

rmse(model_output$SalePrice,model_output$prediction_lm)
sqrt(mean((model_output$SalePrice - model_output$prediction_lm)^2, na.rm=T))

#We found no difference in RMSE, so we know the chance that our data hasn't been overfitted is lower than anticipated since 
#CV helps against over fitting data


#8. Random Forest- Simple Random Forest

set.seed(100)
out<-fulldatatrain$SalePrice

part<-createDataPartition(y=out, p=0.5, list = FALSE)

train<-fulldatatrain[part,]
test<-fulldatatrain[-part,]

model_1 <- randomForest(SalePrice ~ ., data=train)

prediction <- predict(model_1, test)
model_output <- cbind(test, prediction)

rmse(model_output$SalePrice,model_output$prediction)
sqrt(mean((model_output$SalePrice - model_output$prediction)^2, na.rm=T))


#Random forest performed better than Linear model when we used it against the test set. Quite a low RMSE

#8A. Now with the real test set and using K fold model again to avoid overfitting just in case we had some.

train<-fulldatatrain
test<-fulldatatest2

set.seed(100)

model_1 <- train(SalePrice ~ ., data = train, tuneLength = 1, method = "ranger", importance = 'impurity', trControl = CV)

prediction_rf <- predict(model_1, test)
model_output <- cbind(test, prediction_rf)

mean((model_output$SalePrice - model_output$prediction_rf)^2)

rmse(model_output$SalePrice,model_output$prediction)
sqrt(mean((model_output$SalePrice - model_output$prediction)^2, na.rm=T))

#Again same RMSE so we know we have guarded against overfitting in the train data again. We will use cross validation method
#going forward for the sake of consistancy and brevity

#8C. We try now to deacrease our RMSE by tuning the number of predictors sampled for spliting at each node by changing it from 1 to 3.
train<-fulldatatrain[part,]
test<-fulldatatrain[-part,]

train<-fulldatatrain
test<-fulldatatest2

set.seed(100)

model_2 <- train(SalePrice ~ ., data = train, tuneLength = 3, method = "ranger", importance = 'impurity', trControl = CV)

prediction <- predict(model_2, test)
model_output <- cbind(test, prediction)

sqrt(mean((model_output$SalePrice - model_output$prediction)^2, na.rm=T))

#We get a worse score so we know 1 mtry is optimal for our data set

#8D.Random Forest with top 20 features found in the data set. This is similar to a PCA analysis 
#in that it finds the top 20 features that can explain close to 90% of the variance in our dataset.
train<-fulldatatrain[part,]
test<-fulldatatrain[-part,]

train<-fulldatatrain
test<-fulldatatest2

model_1 <- train(SalePrice ~ ., data = train, tuneLength = 1, method = "ranger", importance = 'impurity', trControl = CV)

top<-(varImp(model_1))
top
top<-c("OverallQual","GrLivArea","ExterQual","KitchenQual","TotalBsmtSF","YearBuilt","GarageArea","X1stFlrSF","GarageCars","Neighborhood","BsmtQual","GarageYrBlt","GarageFinish","FullBath","GarageType","FireplaceQu","LotArea","YearRemodAdd","BsmtFinSF1","TotRmsAbvGrd")
set.seed(100)
toptrain <- select(fulldatatrain, one_of(top, "SalePrice"))

model_3 <- train(SalePrice ~ ., data = toptrain, tuneLength = 1, method = "ranger", importance = 'impurity', trControl = CV)

prediction <- predict(model_3, test)
model_output <- cbind(test, prediction)


sqrt(mean((model_output$SalePrice - model_output$prediction)^2, na.rm=T))

#We see a slight decrease in RMSE using the top 20 features. This means that we are already using the data that best explains our full data set for prediction purposes.



#9 XGBOOST

train<-fulldatatrain[part,]
test<-fulldatatrain[-part,]

train<-fulldatatrain
test<-fulldatatest2

set.seed(100)


model_xgb <- train(SalePrice ~ ., data = train, tuneLength = 1, method = "xgbTree", trControl = CV)

prediction <- predict(model_xgb, test)
model_output <- cbind(test, prediction)


sqrt(mean((model_output$SalePrice - model_output$prediction)^2, na.rm=T))


#10. We are using combination of lasso regression**  #Modify wording#
#(penalising the number of non-zero coefficients) and **ridge regression** 
#(penalising the absolute magnitude of each coefficient) to prevent overfitting in 
#our test set (we only used CV to protect aainst overfitting in our train data) to build a more robust linear model. The advantage of net regularized
#is the model's ability to extrapolate information


##########################
train<-fulldatatrain[part,]
test<-fulldatatrain[-part,]

train <- fulldatatrain
test <- fulldatatest2

set.seed(100)
price<-train$SalePrice

training <- xgb.DMatrix(as.matrix(train), label = price)

testing <- xgb.DMatrix(as.matrix(test))

ridge <- cv.glmnet(as.matrix(train), price, alpha = 0)
lasso <- cv.glmnet(as.matrix(train), price, alpha = 1)
net <- cv.glmnet(data.matrix(train), price, alpha = 0.001)

# use the lamdba that minimizes the error
ridge_min_error <- ridge$lambda.min
lasso_min_error <- lasso$lambda.min
net_min_error <- net$lambda.min

ridge <- glmnet(x = as.matrix(train), y = price, alpha = 0, lambda = ridge_min_error )
lasso <- glmnet(x = as.matrix(train), y = price, alpha = 1, lambda = lasso_min_error)
net <- glmnet(x = as.matrix(train), y = price, alpha = 0.001, lambda = net_min_error)

prediction_ridge <- as.numeric(predict(ridge, as.matrix(test)))
prediction_lasso <- as.numeric(predict(lasso, as.matrix(test)))
prediction_net <- as.numeric(predict(net, as.matrix(test)))
prediction_ensemble<-(prediction_ridge + prediction_lasso + prediction_net)/3

model_output1 <- cbind(test, prediction_ridge)
model_output2<- cbind(test,prediction_lasso)
model_output3<- cbind(test,prediction_net)
model_output4<- cbind(test,prediction_ensemble)

rmse(model_output1$SalePrice,model_output1$prediction_ridge)
rmse(model_output2$SalePrice,model_output2$prediction_lasso)
rmse(model_output3$SalePrice,model_output3$prediction_net)
rmse(model_output4$SalePrice,model_output4$prediction_ensemble)


sqrt(mean((model_output1$SalePrice - model_output1$prediction_ridge)^2))
sqrt(mean((model_output2$SalePrice - model_output2$prediction_lasso)^2))
sqrt(mean((model_output3$SalePrice - model_output3$prediction_net)^2))
sqrt(mean((model_output4$SalePrice - model_output4$prediction_ensemble)^2))

