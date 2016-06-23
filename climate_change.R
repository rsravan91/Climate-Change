setwd("C:/Users/DELL/Desktop/ISSMTECHASSIGNMENTS/Kaggle/Climate Change/")
climate=read.csv("climate_change.csv")
str(climate)
cor(climate)

# split the dataset
trainClimate=subset(climate,subset = Year<=2006)
testClimate=subset(climate,subset = Year>2006)

# Predict temp
tempReg1=lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,trainClimate)
summary(tempReg1)

with(trainClimate,cor(cbind(MEI,CO2,CH4,N2O,CFC.11,CFC.12,TSI,Aerosols)))

tempReg2=lm(Temp~MEI+N2O+TSI+Aerosols,trainClimate)
summary(tempReg2)

# Buid model using AIc and step function
steppedTempReg=step(tempReg1)
summary(steppedTempReg)

# Test the model on test set
temp=predict(steppedTempReg,testClimate)
SSE=sum((temp-testClimate$Temp)^2)
SST=sum((testClimate$Temp-mean(trainClimate$Temp))^2)
R2=1-(SSE/SST)