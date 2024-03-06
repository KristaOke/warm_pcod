#==================================================================================
#check existing ESP temperature metrics

#by Krista, Mar 2024
#==================================================================================
#Notes:
#==================================================================================

library(tidyverse)
library(AKesp)
library(corrplot)


wd <- getwd()

#sablefish----
AKesp::esp_stock_options() #look at stocks and pick one
sbltbl <- AKesp::get_esp_data(stock="Alaska Sablefish")

AKesp::check_data(sbltbl)

#drop socioecon indicators
sbltbl <- sbltbl[which(sbltbl$INDICATOR_TYPE=="Ecosystem"),]

sblwide <- pivot_wider(sbltbl[,c(1:3)], names_from = INDICATOR_NAME, values_from = "DATA_VALUE")

colnames(sblwide)


#GOA pollock----
AKesp::esp_stock_options() #look at stocks and pick one
goapolltbl <- AKesp::get_esp_data(stock="GOA Pollock")

AKesp::check_data(goapolltbl)

#drop socioecon indicators
goapolltbl <- goapolltbl[which(goapolltbl$INDICATOR_TYPE=="Ecosystem"),]

goapollwide <- pivot_wider(goapolltbl[,c(1:3)], names_from = INDICATOR_NAME, values_from = "DATA_VALUE")

colnames(goapollwide)

plot(goapollwide$YEAR, goapollwide$Summer_Temperature_Bottom_GOA_Survey)

goa_bot_temp <- goapollwide[c("YEAR", "Summer_Temperature_Bottom_GOA_Survey")]
write.csv(goa_bot_temp,file=paste0(wd, "/data/", "ESP_goa_bot_temp.csv", sep=""),row.names=F)

#GOA p cod----
AKesp::esp_stock_options() #look at stocks and pick one
goapcodtbl <- AKesp::get_esp_data(stock="GOA Pacific Cod")

AKesp::check_data(goapcodtbl)

#drop socioecon indicators
goapcodtbl <- goapcodtbl[which(goapcodtbl$INDICATOR_TYPE=="Ecosystem"),]

goapcodwide <- pivot_wider(goapcodtbl[,c(1:3)], names_from = INDICATOR_NAME, values_from = "DATA_VALUE")

colnames(goapcodwide)









