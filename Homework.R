install.packages(c("dplyr","writexl","readxl","readr","xgboost","parsnip","tidyverse","caTools",
                   "highcharter","glue","plotly","DataExplorer","htmltools","rlang","skimr",
                   "imputeTS","recipes","inspectdf"))
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
#1. Data DAAG kitabxanasýnda «nassCDS» funksiyasý daxilind?? yerl??þdirilmiþ. Dataný R-a daxil edin.
install.packages("DAAG")
my_data<- DAAG:: nassCDS
#2. Dvcat sütunun adýný d??yiþdir??r??k Speed, Abcat sütunun is?? Airbag_deploy adý il?? d??yiþdirin.
my_data<- my_data %>% rename(Speed=dvcat, Aitbag_deploy=abcat)
#3. Neç?? q??zada sürücünün qoruyucu k??m??ri taxmadýðý s??b??bind??n h??yatýný itirdiyini mü??yy??n edin.
my_data %>% filter(occRole=='driver', seatbelt=='none', dead=='dead') %>% count()
#528
#4. Qadýnlarýn h??yatýný itirdiyi q??zalarda maþýnlarýn ??n çox ön, yoxsa arxa hiss??d??n z??d?? aldýqlarýný mü??yy??n edin.
my_data %>% filter(sex=='f', dead=='dead', frontal=='0') %>% count()
#257
my_data %>% filter(sex=='f', dead=='dead', frontal=='1') %>% count()
#207
#5. 2001-ci ild??n baþlayaraq q??zalarýn neç??sind?? ölüm s??b??bi hava yastýqlarýnýn olmamasý olub.
my_data %>% filter(yearacc>='2001',dead=='dead', airbag=='none') %>% count()
#154
#6. General datasý üzr?? dataexplorerd??n istifad?? ed??r??k online report hazýrlayýn.
general_data<-read.csv('C:/Users/Sahib Babayev/Desktop/qss/general data.csv')
create_report(general_data)
#7. General v?? manager datalarýný Generalý baza götür??r??k EmployeeID gör?? birl??þdirin
manager_data<-read.csv('C:/Users/Sahib Babayev/Desktop/qss/manager survey data.csv')
join<-left_join(general_data,manager_data, by=c("EmployeeID"="EmployeeID"))
#8. Facet_grid funksiyasindan istifad?? ed??r??k h??r bir T??hsil sah??si üçün aylýq g??lirl??rin paylanmasýný göst??r??n histogram qurun.
library(ggplot2)
ggplot(join, aes(x=MonthlyIncome)) +
  geom_histogram(binwidth=0.5, aes(fill=EducationField), colour="Black") + 
  facet_grid(EducationField~.,scales="free") +
  xlab("MonthlyIncome") + 
  ylab("") +
  ggtitle("Monthly Income by Education Field") +
  theme(plot.title = element_text(hjust = 0.5))
