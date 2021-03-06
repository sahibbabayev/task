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
#1. Datan� R-a daxil edin.
price.data<-read.csv('C:/Users/Sahib Babayev/Desktop/qss/House prices in Ontario.csv')
#2. Str funksiyas� vasit??sil?? datan�n strukturuna bax�n v?? factor tipind?? olan s�tunlar� character il?? ??v??z edin.
str(price.data)
skim(price.data)
#3. Adress s�tunundak� bo�luqlar� �unavailable� il?? ??v??z edin.
price.data <- price.data %>% mutate(Address = replace(Address, Address == "", "Unavailable"))
#4. Evl??rin yerl??�dikl??ri ??razil??ri mode funksiyas� yaradaraq bo� xanalar� ??n �ox t??krarlanan ??razi il?? doldurun.
mode <- function(v) {
  unique <- unique(price.data$AreaName)
  unique[which.max(tabulate(match(v, unique)))]}
mode(price.data$AreaName)
price.data <- price.data %>% mutate(AreaName= replace(AreaName,AreaName=="",
                                                      mode(AreaName)))
#5.Evl??rin qiym??tl??rind?? olan bo�luqlar� is?? median vasit??sil?? doldurun.
price.data <- price.data %>% mutate(Price= replace(Price,is.na(Price),
                                                   median(Price, na.rm = TRUE)))
#6. Ne�?? unique ??razi var?
length(unique(price.data$AreaName) )
#7. ??n y�ks??k qiym??t?? sahib olan 5 evin hans� ??razil??rd?? yerl??�diyini m�??yy??n edin.
arrangeprice<-price.data %>% arrange(desc(Price))
arrangeprice$AreaName[1:5]
#8. Ne�?? ev 700000 d??n y�ks??k qiym??t?? malikdir.
price.data %>% filter(Price>700000) %>% count()
