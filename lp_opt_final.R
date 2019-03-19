################################################################################
# Author: Tejas Bala, Data Science Co-op GAMMA S&S
# Date Created: 9/14/2017
# Last Modified: 12/6/2017
################################################################################
install.packages("lpSolveAPI")
install.package("xlsx")

library("lpSolveAPI")
library("xlsx")

origDir <- choose.dir()
setwd(origDir)

################################################################################
################################# LOAD DATA ####################################
################################################################################

#Read in data into data frames 
prices <- read.xlsx("OptimizationInput.xlsx", 1)
demand <- read.xlsx("OptimizationInput.xlsx", 2)
capacity <- read.xlsx("OptimizationInput.xlsx", 3)
#oldVolume <- read.xlsx("OptimizationInput.xlsx", 4)

#prices <- read.csv(priceDir, header = TRUE)
#demand <- read.csv(demandDir, header = TRUE)
#capacity <- read.csv(capacityDir, header = TRUE)
#oldVolume <- read.csv("VolumeMatrix.csv", header = TRUE)

################################################################################
######################### CLEAN / FORMAT DATA ##################################
################################################################################

#Ensure dataframes are in the same order as oldVolume
#ORDER prices & demand by Supplier and then Location
# @ Edit column indices for dataframe @
prices<- prices[order(prices[,1],prices[,2],decreasing=FALSE),]
demand<- demand[order(demand[,1],demand[,2],decreasing=FALSE),]


################################################################################
## Constraint matrix will track which variables each constrain applies to.
## 'Variables' refers to all unique Supplier-Location-Product items
################################################################################


################################################################################
##list for the Supplier + Location + Product name THAT ARE NOT 0 for constraint 
## matrix X-axis
##comboNames: list of all combinations of supplier, product and location where 
## price does not equal 0
##flatPrice: flat list of all non-zero prices
################################################################################

comboNames <- NULL
flatPrice <- NULL
#iterate through prices df
#starts at 3 since 1st 2 columns are labels
# @ Change start index if necessary @ 

for(i in 3:ncol(prices)) {
  for(k in 1:nrow(prices)) { 
    #check if price > 0
    if(prices[k,i] > 0) {
      #create combination name of supplier name + product name + location name
      name <- paste(prices[k,1],colnames(prices[i]),prices[k,2], sep="_")
      comboNames <- append(comboNames, name)
      flatPrice <- append(flatPrice, prices[k,i])
    }
  }
}

################################################################################
##create list for the Location + Product name for  for constraint matrix Y-axis
##demandNames: list of all location-product supply demands
##mRowNames: list of demandNames appended to supplier names 
## (since that is the other constraint)
################################################################################

#list for Location + Product name
demandNames <- NULL
#iterate through all demands
for(i in 1:nrow(demand)) { 
  name <- paste(demand[i,1], demand[i,2], sep="_")
  demandNames <- append(demandNames, name)
}

#stack supplier and location/prod constraints for matrix row names
mRowNames <- append(as.character(capacity$Supplier),demandNames)


################################################################################
########################### MAKE CONSTRAINT MATRIX #############################
################################################################################

################################################################################
# valueMatrix: rows: constraints, columns: location, prod, sup combinations
################################################################################

#create DF with rownames
valueMatrix <- data.frame(row.names = mRowNames)

#set column names and fill matrix w 0's fo default value
for(i in 1 : length(comboNames)) {
  colN <- comboNames[i]
  valueMatrix[,colN] <- 0
}
   
##fill with 1 for cells that constraint applies to

  #aggregated index to avoid restarting loop from beginning
  agg <- 1
  #iterate 1 to length of capacity
  for(i in 1:nrow(capacity)) {
    for(j in agg:ncol(valueMatrix)) {
      #if supplier names match
      # @ Edit substr index @
      if(substr(rownames(valueMatrix[i,]),9,10) == 
         substr(colnames(valueMatrix)[j],9,10)) {
        valueMatrix[i,j] <- 1
        agg <- j
      }
      else {
        agg <- j
        break
      }
    }
  }
  
#if location+prod matches
for(k in (nrow(capacity)+1):nrow(valueMatrix)) {
  valueMatrix[k,which((rownames(valueMatrix[k,])) == 
                        # @ Edit substr index @
                        substr(colnames(valueMatrix), 12,28))] <- 1
}
  
#Clean up iterator leftovers
rm(i)
rm(j)
rm(k)
rm(agg)
rm(colN)
rm(name)


################################################################################
################################ CREATE MODEL ##################################
################################################################################

#CREATE Model for lpSolver
##SKIP BELOW TO READ IN FROM FILE!!

#initializes model with 0 constraints and variables for each comboName
model <- make.lp(0,length(comboNames))
#set sense to min to minimize objective function
# @ Edit to "max" if desired @
lp.control(model, sense="min")

#Objective function: minimize cost, since price is fixed we minimize volume 
# while satisfying all constraints
set.objfn(model, flatPrice)

#Supplier Capacity constraints
# @ Edit indices  and inequality as necessary @
add.constraint(model, valueMatrix[1,], "<" , capacity[1,2])
add.constraint(model, valueMatrix[2,], "<" , capacity[2,2])
add.constraint(model, valueMatrix[3,], "<" , capacity[3,2])
add.constraint(model, valueMatrix[4,], "<" , capacity[4,2])
add.constraint(model, valueMatrix[5,], "<" , capacity[5,2])
add.constraint(model, valueMatrix[6,], "<" , capacity[6,2])
add.constraint(model, valueMatrix[7,], "<" , capacity[7,2])
add.constraint(model, valueMatrix[8,], "<" , capacity[8,2])
add.constraint(model, valueMatrix[9,], "<" , capacity[9,2])
add.constraint(model, valueMatrix[10,], "<" , capacity[10,2])
add.constraint(model, valueMatrix[11,], "<" , capacity[11,2])
add.constraint(model, valueMatrix[12,], "<" , capacity[12,2])
add.constraint(model, valueMatrix[13,], "<" , capacity[13,2])
add.constraint(model, valueMatrix[14,], "<" , capacity[14,2])
add.constraint(model, valueMatrix[15,], "<" , capacity[15,2])
add.constraint(model, valueMatrix[16,], "<" , capacity[16,2])
add.constraint(model, valueMatrix[17,], "<" , capacity[17,2])


#Location Demand constraints
#WARNING: Takes ~2 minutes to complete this step
for(i in 18:nrow(valueMatrix)) {
  add.constraint(model, valueMatrix[i,], ">", demand[i-17,3]) 
}

presolve(model)

solve(model)

################################################################################
######################### WRITE/READ MODEL [OPTIONAL] ##########################
################################################################################

#WRITE model out to file
#write.lp(model, "lpfilename.lp", "lp")

#READ in model from file
#model = read.lp("lpfilename.lp", "lp")

#READ in results from model
#volumeNew <- read.csv("volumeNew.csv", header = TRUE)
#volumeNew <- volumeNew[,-1]

################################################################################
################################ GET SOLUTIONS #################################
################################################################################

#save new Volumes
newVol <- get.variables(model)
#volumeNew <- oldVolume[,]
volumeNew <- prices[,]


#ORDERED the same as prices and demand were above
# @Edit indices if necessary @
volumeNew<- volumeNew[order(volumeNew[,1],volumeNew[,2],decreasing=FALSE),]

#create df of variable names and new volumes
x <- data.frame(comboNames, newVol)
#remove any variables that have volume = 0
x <- x[which(x$newVol != 0),]

#fill volumeNew with 0s as default
for(i in 1:195) {
  for (k in 3:54) {
    volumeNew[i,k] <- 0
  }
}

#fill volumeNew with new volumes
#iterate through new non-zero volumes
for(k in 1:nrow(x)) {
  thisCombo <- as.character(x[k,1])
  #separate string
  thisCombo <- unlist(strsplit(thisCombo, "_"))
  #supplier
  sup <- thisCombo[1]
  #location
  location <- thisCombo[2]
  #product
  prod <- thisCombo[3] 
  
  #iterate through volumeNew
  for(i in 3:ncol(volumeNew)) {
    #check if location equals
    if(location == colnames(volumeNew[i])) {
      for(j in 1:nrow(volumeNew)) {
        #check if supplier/prod equals
        if(volumeNew[j,1] == sup) {
          if(volumeNew[j,2] == prod) {
            volumeNew[j,i] <- as.numeric(x[k,2])
          }
        }
      }
    }
  }
}


################################################################################
################################ CONSTRAINT CHECKS #############################
################################################################################


#CHECK if Supplier Capacity constraints are satisfied
for(i in 1:nrow(capacity)) {
  supName = capacity[i,1]
  supSum = sum(volumeNew[which(volumeNew[,1] == supName),-c(1,2)])
  capacity[i,3] <- supSum <= capacity[i,2]
  capacity[i,4] <- supSum
}


#CHECK if location demands are satisfied

new_df <- cbind.data.frame(comboNames, flatPrice, newVol)
#take supplier out of names
newNames <- substr(comboNames,12,28)

#create df with demandNames and new volumes
new_df <- cbind.data.frame(newNames, newVol)
new_df[,1] <- as.character(new_df[,1])
#new_df[,2] <-as.numeric(new_df[,2])

demand[,3] <- round(demand[,3], digits = 1)

#for each location prod demand
for(i in 1:nrow(demand)) {
  #get location_prod name
  name <- paste(demand[i,1], demand[i,2], sep="_")
  #sum for each location_prod
  psum <- sum(new_df[which(new_df[,1] == name), 2])
  psum <- round(psum, digits = 1)
  demand[i,4] <- psum >= demand[i,3]
  demand[i,5] <- psum
}


################################################################################
################################ RESULT OUTPUTS ################################
################################################################################

#Compute TOTAL COST

labelCol <- prices[,c(1,2)]

#create totalCostNew with same shape as prices
totalCostNew <- prices[,-c(1,2)] * volumeNew[,-c(1,2)]
totalCostNew <- cbind.data.frame(labelCol, totalCostNew)

#create totalCostOld for comparison
#totalCostOld <- prices[,-c(1,2)] * oldVolume[,-c(1,2)]  #DONT HAVE oldVolume
#totalCostOld <- cbind.data.frame(labelCol, totalCostOld)

#Write DF out to file

write.xlsx(totalCostNew, file = "OptimizationOutput.xlsx", sheetName = "totalCostNew")
write.xlsx(volumeNew, file = "OptimizationOutput.xlsx", sheetName = "volumeNew", append = TRUE)
write.xlsx(demand, file = "OptimizationOutput.xlsx", sheetName = "demandCheck", append = TRUE)
write.xlsx(capacity, file = "OptimizationOutput.xlsx", sheetName = "capacityCheck", append = TRUE)


#write.csv(totalCostNew, file = "totalCostNew.csv")
#write.csv(volumeNew, file = "volumeNew.csv")
#write.csv(demand, file = "demandCheck.csv")
#write.csv(capacity, file = "capacityCheck.csv")

sum(totalCostNew[,-c(1,2)])
