install.packages("rlang", repos = "http://cran.us.r-project.org")
install.packages("MASS", repos = "http://cran.us.r-project.org")
install.packages("fitdistrplus", repos = "http://cran.us.r-project.org")
install.packages("magrittr", repos = "http://cran.us.r-project.org")
install.packages("simmer", repos = "http://cran.us.r-project.org")
install.packages("simmer.plot", repos = "http://cran.us.r-project.org")
install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("lazyeval", repos = "http://cran.us.r-project.org")
install.packages("parallel", repos = "http://cran.us.r-project.org")
install.packages("e1071", repos = "http://cran.us.r-project.org")
install.packages("plotly", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("triangle", repos = "http://cran.us.r-project.org")
install.packages("sqldf", repos = "http://cran.us.r-project.org")
install.packages("knitr", repos = "http://cran.us.r-project.org")
install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
install.packages("readxl", repos = "http://cran.us.r-project.org")
installed.packages("vctrs", repos="https://CRAN.R-project.org/package=vctrs")
devtools::install_github("tidyverse/ggplot2")
install.packages("tidyverse")
library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(readxl)
library(knitr)
library(rmarkdown)
library(xlsx)
library(vctrs)


# connection to Tableau and 
install.packages("Rserve")
library(Rserve) 
Rserve()

# -----------------------------------------------------------tables-----------------------------------------------------------------

defected <- read.csv("C:/Users/talco/Desktop/iotExe1/defected.csv")
production <- read.csv("C:/Users/talco/Desktop/iotExe1/originalData/production.csv")
customers <- read.csv("C:/Users/talco/Desktop/iotExe1/customers.csv")
employees <- read.csv("C:/Users/talco/Desktop/iotExe1/employees.csv")
failures <- read.csv("C:/Users/talco/Desktop/iotExe1/failures.csv")
sales <- read.csv("C:/Users/talco/Desktop/iotExe1/sales.csv")
shifts <- read.csv("C:/Users/talco/Desktop/iotExe1/shifts.csv")


#--------------------------------------------------------------queries--------------------------------------------------------------------

# counts defected parts fro defected table
newDefected <- sqldf("select BoxRFID, PartType, numberOfPartsinBox,count(*) as numOfDefectedParts 
              from defected 
              group by BoxRFID, PartType")


defectedRatio <- (newDefected$numOfDefectedParts)/(newDefected$NumberofPartsinBox)
newDefected <- cbind(newDefected, defectedRatio)

#-------------------------------------------------------------newTables------------------------------------------------------------------

failureTime <- failures$FailureEndTime-failures$FailureStartTime
failureCrossShift <- array()

#checks if failure was accross shifts
for(i in 1:length(failures$FailureStartTime)){ 
  if (failures$FailureEndTime[i] > 480*(failures$Shift[i])){
    failureCrossShift[i] =1
  }
  else
  {
    failureCrossShift[i] = 0
  }
}

failures <- cbind(failures, failureTime, failureCrossShift)


#--------------------------------------------------defected parts affects box returns ------------------------------------------------------------

product_defected <- sqldf("select p.ProductID, f.BoxRFID,f.PartType, f.NumberofPartsinBox, f.numOdDefectedParts, f.defectedRatio, s.Returned
                          from (production as p join newDefected as f on p.BoxRFID=f.BoxRFID) as pf join sales as s on pf.ProductID=s.ProductID
    
                                                ")

names(product_defected)[names(product_defected) == "BoxRFID:1"] <- "BoxRFID"

#-------------------------------------------------------------------------returns from customers----------------------------------------------------------

numOfPurchases <- sqldf("select Customer, count(*) as total
                      from sales
                      group by Customer")

badCustomers <- sqldf("select sa.Customer, c.Type, count(*) as returnedCount
                      from sales as sa join customers as c on sa.Customer = c.CustID
                      where Returned='YES'
                      group by Customer")

numOfPurchases <- sqldf("select nop.Customer, bc.Type, nop.total, bc.returnedCount
                        from numOfPurchases as nop join badCustomers as bc on nop.Customer = bc.Customer")

returnedRatio <- numOfPurchases$returnedCount/numOfPurchases$total
numOfPurchases <- cbind(numOfPurchases, returnedRatio)
  
names(numOfPurchases)[names(numOfPurchases) == "badCustomers$`count(*)`"] <- "ReturnedCount"
names(numOfPurchases)[names(numOfPurchases) == "count(*)"] <- "TotalCount"

boxplot(numOfPurchases$returnedRatio~numOfPurchases$Type, main = "Returned Ratio and Type of Customer", xlab = "Type", ylab = "Returned Ratio", col = "deepskyblue4",
        ,cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)

ratioAverageCustomers <- sqldf("select Type, avg(returnedRatio)
                   from numOfPurchases
                   group by Type")

#--------------------------------------------production with sales------------------------------------------------------------------------------------------

newProduction <- sqldf("select *
                        from production as p join sales as sa on p.ProductID= sa.ProductID")

newProduction <- newProduction[,-13]

#newProductionShiftTotal <- sqldf("select sh.shiftNum, count(*) as Total
#                            from newProduction as np join shifts as sh on np.Shift = sh.Shift
#                            group by shiftNum")

#newProductionShiftYes <- sqldf("select sh.shiftNum, count(*) as Total
#                            from newProduction as np join shifts as sh on np.Shift = sh.Shift
#                            where np.Returned = 'YES'
#                            group by shiftNum")

#-------------------------------------------------------------------------count by boxes-------------------------------------------------------------------------

boxProductCount <- sqldf("select BoxRFID, count(*) as ProcutsAmount
                          from production
                          group by BoxRFID")

returnedFromBox <- sqldf("select BoxRFID, count(*) as Returned
                          from newProduction
                          where Returned='YES'
                          group by BoxRFID")

boxProductCount <- sqldf("select *
                     from boxProductCount as bpc left join returnedFromBox as rfm on bpc.BoxRFID=rfm.BoxRFID")

boxProductCount[is.na(boxProductCount)] <- 0
boxProductCount <- boxProductCount[,-5]
boxReturnedRatio <- boxProductCount$Returned/boxProductCount$ProcutsAmount
boxProductCount <- cbind(boxProductCount, boxReturnedRatio)

#---------------------------------------------------------------------shift Failures--------------------------------------------------------------------------------

shiftFailures <- sqldf("select sh.Shift, sh.EmpID, f.FailureStartTime
                       from shifts as sh left join failures as f on sh.Shift = f.Shift")
wasFailure <- array()
for (i in 1:length(shiftFailures$Shift)){
  if (is.na(shiftFailures$FailureStartTime[i])){
    wasFailure[i] = 0
  }
    else
    {
      wasFailure[i] = 1
    }
} 

shiftFailures <- cbind(shiftFailures, wasFailure)
shiftFailures <- shiftFailures[,-3]

shiftFailures <- sqldf("select sf.Shift, sf.EmpID, sf.wasFailure, e.Seniority
                        from shiftFailures as sf join employees as e on sf.EmpID=e.EmpID")

#---------------------------------------------------------------------------shift returns------------------------------------------------------------------------------

shiftReturns <- sqldf("select p.Shift, count(*) as RetunedFromShift
                       from production as p join sales as sa on p.ProductID = sa.ProductID
                       where sa.Returned='YES'
                       group by p.Shift")

shiftReturns <- sqldf("select *
                       from shiftFailures as sf join shiftReturns as sr on sf.Shift = sr.Shift
                       where wasFailure=1")  
  
shiftReturns <- shiftReturns[,-5]

producedInShift <- sqldf("select shift, count(*) as total
                          from production
                          group by shift")
shiftReturns <- sqldf("select sr.Shift, sr.EmpID, sr.wasFailure, sr.Seniority, sr.RetunedFromShift, pis.total
                       from shiftReturns as sr join producedInShift as pis on sr.shift=pis.shift")
ratio <- shiftReturns$RetunedFromShift/shiftReturns$total
shiftReturns <- cbind(shiftReturns, ratio)

years <- ceiling(shiftReturns$Seniority)
shiftReturns <-  cbind(shiftReturns, years)
boxplot(shiftReturns$ratio~shiftReturns$years, main = "Returned Ratio and Seniority - With Failures", xlab = "Seniority", ylab = "Returned Ratio", col = "deepskyblue4",
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

ratioAverage <- sqldf("select years, avg(ratio)
                   from shiftReturns
                   group by years")

#-----------------------------------------------------------------------shifts---------------------------------------------------------------------------------------------------------

Day <- array()
k=1
counter = 0
for(i in 1:(length(shifts$Shift))){
  if (counter != 3){
    Day[i] = k
  }
  else{
    k=k+1
    counter=0
    Day[i] = k
  }
  counter= counter+1
}

shiftNum <- array()
k=1
counter = 0
for(i in 1:(length(shifts$Shift))){
  if (counter != 3){
      shiftNum[i] = k
  }
  else{
    k=1
    counter=0
    shiftNum[i] = k
  }
  k=k+1
  counter= counter+1
}


shifts <- cbind(shifts,Day, shiftNum)
boxplot(shiftReturns$RetunedFromShift~shiftReturns$wasFailure, data = shiftReturns)



#-----------------------------------------------------------------------------------------failures by shift num---------------------------------------------------------------------------------

countShiftNumFailures <- sqldf("select sh.shiftNum, count(*)
                     from failures as f join shifts as sh on f.shift=sh.shift
                     group by shiftNum")

#------------------------------------------------------------------------------------------------------exponential----------------------------------------------------------
x <- sales$TimeToSell
hist(x,prob=TRUE, col="mistyrose3", main="Time To Sell", xlab = "Time To Sell", ,cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
lines(density(sales$TimeToSell), col="deepskyblue4", lwd=2)
summary(x)
plot(ecdf(x))
expFit1<-fitdist(data = x,"pnorm", discrete=FALSE) # fitting exponential distrbution 
print(expFit1)
ks.test(x/1000,"pexp",0.02493355)
hist()

#---------------------------------------------------------------------------------------------------random-------------------------------------------------

plot(production$WaitForPackage, type = 'l', col="deepskyblue4",main="Wait For Package", xlab = "Index",ylab='Wait For Package' ,cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
summary(production$WaitForPackage)
sd0 <- sd(production$WaitForPackage, na.rm = TRUE)
summary(production$WaitForPackage)
print(sd0)

#---------------------------------------------------------------------------------------------------linear-------------------------------------------------------------------



print(glueTimeCombined)
newGlueTime <- as.array(newGlueTime)
glueTimeCombined <- array()
for(i in 1:300)
{
  if ((i%%36 == 1) | (i%%36==2) | (i%%36==3) | (i%%36==4))
  {
    glueTimeCombined[i] <- rnorm(1,3*meanGlueTime, sdGlueTime)
  }
  else{
    glueTimeCombined[i] <- rnorm(1, meanGlueTime, sdGlueTime)
  }
  
}

i=0
for(j in 1:length(glueTimeCombined))
{
  glueTimeCombined[j] = glueTimeCombined[j] + i
  i=i+1
}
print(mean(production$GlueTime, na.rm = TRUE))
print(sd(production$GlueTime, na.rm = TRUE))


print(glueTimeCombined)
plot(glueTimeCombined, type='l',main = "Linear Trend", xlab="Index", ylab = "Glue Time + Shift Time",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1, lwd=2)

tempData <- production$Production_Time..h.[seq(from = 1,to = length(production$timeBetweenProduct)/30, by = 5)]
helpData <- production$Shift[seq(from = 1,to = length(production$Shift)/20, by = 1)]
plot(tempData, type ='l' ,main = "Linear Trend", xlab="Index", ylab = "Production Time (H)",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1, lwd=2, col="deepskyblue4")
abline(a=0, b=1.4, col = 'purple')
abline(lm(tempData2~production$ProductID[which(production$timeBetweenProduct<6)]), col='red')

tempData2 <- production$Production_Time..h.[seq(from = 1,to = length(production$timeBetweenProduct)/30, by = 5)]
helpData2 <- seq(from = 1,to = 133, by=1)
plot(tempData2, type ='l' ,main = "Linear Trend", xlab="Index", ylab = "Production Time (H)",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1, lwd=2,col="deepskyblue4" )
lm(tempData2~helpData2)
abline(lm(tempData2~helpData2), col='red', lwd=2)
summary(lm(tempData2~helpData2))

tempData3 <- array()
for (i in 0:132){
  j <- 5*i+1
  tempData3[i] <- production$Production_Time..h.[j]+1.223-1.392*(i+1)
}
plot(tempData3, type ='l' ,main = "Sesonal", xlab="Index", ylab = "Production Time (H)",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1, lwd=2,col="deepskyblue4" )


#-------------------------------------------------------------------------------concept drift - glue amount--------------------------------------------------------------

summary(production$GlueAmount)
sd1 <- sd(production$GlueAmount, na.rm = TRUE)
mean1 <- mean(production$GlueAmount, na.rm = TRUE)
sd2 <- mean1*0.1
oldIndex <- rnorm(300, mean1, sd1)
newIndex <- rnorm(300, mean1, sd2)
newSamples <- c(oldIndex, newIndex)

plot(newSamples, type = 'l', col="deepskyblue4", main = "Concept Drift - Glue Amount", xlab="Index", ylab = "Glue Amount",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1, lwd=2)
summary(newSamples)
sd(newSamples)

plot(production$AssemblyTime, type = 'l', col="deepskyblue4", main = "Concept Drift - Glue Amount", xlab="Index", ylab = "Glue Amount",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1, lwd=2)

helpDataCD <- production$WaitForPackage[seq(from = 100,to = 300 , by = 1)]
plot(helpDataCD,type='l')

shiftReturns <- sqldf("select *
                       from shiftReturns as sr join shifts as s on sr.shift=s.shift
                       ")

tempData2 <- shiftReturns$RetunedFromShift[seq(from = 20,to = length(shiftReturns$RetunedFromShift), by = 5)]
plot(tempData2, type ='l' ,main = "Concept Drift", xlab="Index", ylab = "Returned From Shift",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1, lwd=2, col = "deepskyblue4")

#-------------------------------------------------------------------------------time between products------------------------------------------------------------------------------------

timeBetweenProduct <- array()
timeBetweenProduct[1] = 0
for (i in 2:length(production$ProductID)){
    timeBetweenProduct[i]=production$Production_Time..h.[i]-production$Production_Time..h.[i-1]
}
production <- cbind(production,timeBetweenProduct)
production <- production[,-13]
productionHour <- ceiling(production$Production_Time..h.)
production <- cbind(production, productionHour)

productionHour <- sqldf("select sh.Day, count(*)
                         from production as p join shifts as sh on p.Shift = sh.Shift
                         group by Day")


shiftSales <- sqldf("select p.shift, count(*) as totalSales
                     from production as p join sales as s on p.ProductID = s.ProductID
                     group by p.shift")


plot(shiftSales$totalSales, type = 'l')
summary(productionHour$`count(*)`)

#------------------------------------------------------------------boxplots----------------------------------------------

boxplot(newProduction$Energy_Consumption~newProduction$Returned, col="deepskyblue4", main = "Energy Consumption and Returned", xlab="Returned", ylab = "Energy Consumption",
        ,cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
boxplot(sales$TimeToSell~sales$Returned, col="deepskyblue4", main = "Time to Sell and Returned", xlab="Returned", ylab = "Time to Sell",
        ,cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)


#-------------------------------------------------------------------energy consumption per shift-----------------------------------------------

energyConsumptionShift <- sqldf("select *
                                 from energyConsumptionShift as e join failures as f on e.Shift = f.Shift
                                 where f.Robot = 'Assembly'")

energyConsumptionShift <- energyConsumptionShift[,-c(6,7,8,9,10)]

print(mean(energyConsumptionShift$`avg(Energy_Consumption)`))
print(mean(production$Energy_Consumption))

#--------------------------------------------------------------------energy consumption with shift---------------------------------------------------------

energyConsumption <- sqldf("select *
                            from energyConsumptionShift as e join shiftFailures as s on e.Shift = s.Shift")


newProduction2 <- newProduction[-6010,]

#---------------------------------------------------------------time between products that cross shifts----------------------------------------------------

names(production)[names(production) == "Production_Time..h."] <- "ProductionTimeH"

maxProductionTime <- sqldf("select p.shift, max(ProductionTimeH) as lastProduct, p.ProductID, s.Returned, p.timeBetweenProduct
                            from production as p join sales as s on p.ProductID = s.ProductID
                            group by shift")

minProductionTime <- sqldf("select p.shift, min(ProductionTimeH) as firstProduct, p.ProductID, s.Returned, p.timeBetweenProduct 
                            from production as p join sales as s on p.ProductID = s.ProductID
                            group by shift")

countOfReturnedMax <- sqldf("select count(*)
                  from maxProductionTime
                  where Returned='YES'")

countOfReturnedMin <- sqldf("select count(*)
                  from minProductionTime
                  where Returned='YES'")

avgTimeBetweenProduct <- sqldf("select avg(timeBetweenProduct)
            from production")

sum=0
counter=1
timeDiffShifts <- array()
for(i in 2:length(production$ProductID))
{
  if (production$Shift[i] != production$Shift[i-1])
  {
    timeDiffShifts[counter] = production$timeBetweenProduct[i] 
    counter=counter+1
  }
}

print(mean(timeDiffShifts))
print(length(timeDiffShifts))

hist(timeDiffShifts, col = "deepskyblue4", main = "Histogram of Time Between Shifts", xlab="Time(H)", ylab = "Frequency",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.)
hist(production$timeBetweenProduct, col = "mistyrose3", main = "Histogram of Time Between Products", xlab="Time(H)", ylab = "Frequency",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.)

 #------------------------------------------------------------------------------------csv files----------------------------------------------------------------------------------------

write.csv(newProduction, file ="C:/Users/talco/Desktop/newProduction.csv")
write.csv(shiftReturns, file ="C:/Users/talco/Desktop/shiftReturns.csv")
write.csv(production, file ="C:/Users/talco/Desktop/production.csv")
write.csv(numOfPurchases, file ="C:/Users/talco/Desktop/numOfPurchases.csv")
write.csv(failures2, file ="C:/Users/talco/Desktop/failures2.csv")
write.csv(minProductionTime, file ="C:/Users/talco/Desktop/minProductionTime.csv")
write.csv(maxProductionTime, file ="C:/Users/talco/Desktop/maxProductionTime.csv")
write.csv(newDefected, file ="C:/Users/talco/Desktop/newDefected.csv")
write.csv(shifts, file ="C:/Users/talco/Desktop/shifts.csv")
hist(production$GlueTime, col = "deepskyblue4", main = "Histogram of Glue Time", xlab="Glue Time", ylab = "Frequency",cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.)

energyAVGRet <- sqldf("select avg(Energy_Consumption)
                       from newProduction
                       ")
print(summary(newProduction$Energy_Consumption))
shiftReturns <- sqldf("select *
                       from shiftReturns as sr left join failures as f on sr.Shift = f.Shift
                       ")
shiftReturns <- shiftReturns[,-c(9,10,11,12,13,15)]
shiftReturns$failureTime[is.na(shiftReturns$failureTime)] <- 0
plot(x=shiftReturns$failureTime, y=shiftReturns$RetunedFromShift)

oneYear <- sqldf("select ratio
                  from shiftReturns
                  where years=1")

industryReturns <- sqldf("select returnedRatio
                          from numOfPurchases
                          where Type='industry'")

rest <- shiftReturns[which(shiftReturns$years!=1),]
restCustomers <- numOfPurchases[which(numOfPurchases$Type!='industry'),]

energyConRet <- sqldf("select energy_consumption
                       from newProduction
                       where Returned='YES'")
energyConNo<- sqldf("select energy_consumption
                       from newProduction
                       where Returned='NO'")

y <- rexp(13000, rate = 0.1539163)
length(y)
t.test(x=energyConRet$Energy_Consumption, y=energyConNo$Energy_Consumption, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
t.test(x=sales$TimeToSell[which(sales$Returned=='YES')], y=sales$TimeToSell[which(sales$Returned=='NO')], alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
print(chisq.test(x=sales$TimeToSell[1:13000], y))


install.packages("FinCal",dependencies=TRUE) # from CRAN
install.packages("raster")
library(FinCal)
library(raster)
cv(production$WaitForPackage, aszero = FALSE)
