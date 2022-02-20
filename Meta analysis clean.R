library(tidyverse)
library(meta)
library(metafor)
library(dmetar)
library(xlsx)
library(ggplot2)
library(vegan)

EstData <- read.csv("Data.csv")

as.factor(EstData$CompetitorINDEX) -> EstData$CompetitorINDEX

#Calucate the Sahnnon's index
het <- EstData[(27:37)]

head(het)

d.HET <-diversity(het, index = "shannon")


#standadise d.HET
zHET <- (d.HET - mean(d.HET,  na.rm = TRUE)) / sd(d.HET, na.rm = TRUE)

EstData<-cbind(EstData, zHET)
hist(d.HET)

estdata <- filter(EstData, 
                         Sex %in% c("Female","Male")
                         )

str(estdata)
estdata$DataCollection

#Filter out studies that don't comply with inclusion criteria
estdataC <-dplyr::filter(EstData, 
                        Sex %in% c("Female","Male"),
                        DataCollection %in% c("VHF", "GPS"),
                        Country %in% c("New Zealand", "Australia"),
                        !Study %in% c("Johnston et al. 2012b", "Jones & Coman 1982", "Langham & Porter 1991
", "Langham & Charleston 1990"))




estdata$HomeRangeEstimator
v <-log(estdata$Value+1)
s <-log(estdata$SE+1)



#PCA####


het <- estdata[(27:37)]

het <-na.omit(het)

head(het)

landusepca <- prcomp(het, center = TRUE, scale.=TRUE)

pcs <- as.data.frame(landusepca$x)

estdata <-cbind(estdata,pcs$PC1, pcs$PC2, pcs$PC3, pcs$PC4)

contvariables <- cbind(pcs$PC1, pcs$PC2, pcs$PC3, pcs$PC4, estdata$zHET)

#Assess correlations
correlationlanduse <- round(cor(contvariables), digits =2)

loadings <- landusepca$rotation
write.xlsx(loadings, "C:/Users/cnot147/OneDrive - The University of Auckland/PhD/Literature Review/Data/landusepca.xlsx") 

write.xlsx(correlationlanduse, "C:/Users/cnot147/OneDrive - The University of Auckland/PhD/Literature Review/Data/Cor cont variables.xlsx") 

sumpcs <-summary(landusepca)

screeplot(landusepca, type = 'lines')

ggbiplot(landusepca, choices = c(1,2),labels = estdata$Country)
ggbiplot(landusepca, choices = c(3,4),labels = estdata$Country)



#Meta-analysis models#####


estdata$HomeRangeEstimator <- relevel(estdata$HomeRangeEstimator,
                                      ref = 2)

he <-estdata %>%
  count(HomeRangeEstimator)

estdata$v <-log(estdata$Value+1)
estdata$s <-log(estdata$SE+1)

rma.mv(yi = v, 
       V = s, 
       data = estdata, 
       method = "ML", random = ~1|Study,
       mods = ~ HomeRangeEstimator)

rma.mv(yi = v, 
       V = s, 
       data = estdata, 
       method = "ML", random = ~1|Study,
       mods = ~ HomeRangeEstimator) 

model1 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study) 



model2 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ zHET+HomeRangeEstimator) 


model3 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ pcs$PC1 +HomeRangeEstimator) 

model4 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ pcs$PC2 +HomeRangeEstimator) 

model5 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ pcs$PC3 +HomeRangeEstimator) 
model6 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ pcs$PC4 +HomeRangeEstimator) 

model7 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ CompetitorINDEX+HomeRangeEstimator) 

model8 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ DataCollection+HomeRangeEstimator) 




estdata$Season <- relevel(estdata$Season,
                                      ref = 2)

model9 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdata, 
                 method = "ML", random = ~1|Study, 
                 mods = ~ Sex+HomeRangeEstimator) 


model10 <- rma.mv(yi = v, 
                  V = s, 
                  data = estdata, 
                  method = "ML", random = ~1|Study, 
                  mods = ~ Land+HomeRangeEstimator)


model11 <- rma.mv(yi = v, 
                 V = s, 
                 data = estdatasea, 
                 method = "ML", random = ~1|Study,
                 mods = ~ Season+HomeRangeEstimator)

model12 <- rma.mv(yi = v, 
                  V = s, 
                  data = estdata, 
                  method = "ML", random = ~1|Study,
                  mods = ~ zHET+ pcs$PC1 + pcs$PC2 + pcs$PC3+ pcs$PC4 + CompetitorINDEX +Sex +Season+HomeRangeEstimator) 



newfitstats10 <- fitstats(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12)
