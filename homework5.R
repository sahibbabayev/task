library(dplyr)
library(writexl)
library(readxl)
library(readr)
library(xgboost)
library(parsnip)
library(tidyverse)
library(caTools)
library(highcharter)
library(glue)
library(plotly)
library(DataExplorer)
library(htmltools)
library(rlang)
library(skimr)
library(imputeTS)
library(recipes)
library(inspectdf)
#1. Dataný R-a daxil edin.
price.data<-read.csv('C:/Users/Sahib Babayev/Desktop/qss/House prices in Ontario.csv')
#2. Str funksiyasý vasit??sil?? datanýn strukturuna baxýn v?? factor tipind?? olan sütunlarý character il?? ??v??z edin.
str(price.data)
skim(price.data)
#3. Adress sütunundaký boþluqlarý “unavailable” il?? ??v??z edin.
price.data <- price.data %>% mutate(Address = replace(Address, Address == "", "Unavailable"))
#4. Evl??rin yerl??þdikl??ri ??razil??ri mode funksiyasý yaradaraq boþ xanalarý ??n çox t??krarlanan ??razi il?? doldurun.
mode <- function(v) {
  unique <- unique(price.data$AreaName)
  unique[which.max(tabulate(match(v, unique)))]}
mode(price.data$AreaName)
price.data <- price.data %>% mutate(AreaName= replace(AreaName,AreaName=="",
                                                      mode(AreaName)))
#5.Evl??rin qiym??tl??rind?? olan boþluqlarý is?? median vasit??sil?? doldurun.
price.data <- price.data %>% mutate(Price= replace(Price,is.na(Price),
                                                   median(Price, na.rm = TRUE)))
#6. Neç?? unique ??razi var?
length(unique(price.data$AreaName) )
#7. ??n yüks??k qiym??t?? sahib olan 5 evin hansý ??razil??rd?? yerl??þdiyini mü??yy??n edin.
arrangeprice<-price.data %>% arrange(desc(Price))
arrangeprice$AreaName[1:5]
#8. Neç?? ev 700000 d??n yüks??k qiym??t?? malikdir.
price.data %>% filter(Price>700000) %>% count()
