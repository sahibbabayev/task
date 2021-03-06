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
#1. Data DAAG kitabxanas�nda �nassCDS� funksiyas� daxilind?? yerl??�dirilmi�. Datan� R-a daxil edin.
install.packages("DAAG")
my_data<- DAAG:: nassCDS
#2. Dvcat s�tunun ad�n� d??yi�dir??r??k Speed, Abcat s�tunun is?? Airbag_deploy ad� il?? d??yi�dirin.
my_data<- my_data %>% rename(Speed=dvcat, Aitbag_deploy=abcat)
#3. Ne�?? q??zada s�r�c�n�n qoruyucu k??m??ri taxmad��� s??b??bind??n h??yat�n� itirdiyini m�??yy??n edin.
my_data %>% filter(occRole=='driver', seatbelt=='none', dead=='dead') %>% count()
#528
#4. Qad�nlar�n h??yat�n� itirdiyi q??zalarda ma��nlar�n ??n �ox �n, yoxsa arxa hiss??d??n z??d?? ald�qlar�n� m�??yy??n edin.
my_data %>% filter(sex=='f', dead=='dead', frontal=='0') %>% count()
#257
my_data %>% filter(sex=='f', dead=='dead', frontal=='1') %>% count()
#207
#5. 2001-ci ild??n ba�layaraq q??zalar�n ne�??sind?? �l�m s??b??bi hava yast�qlar�n�n olmamas� olub.
my_data %>% filter(yearacc>='2001',dead=='dead', airbag=='none') %>% count()
#154
#6. General datas� �zr?? dataexplorerd??n istifad?? ed??r??k online report haz�rlay�n.
general_data<-read.csv('C:/Users/Sahib Babayev/Desktop/qss/general data.csv')
create_report(general_data)
#7. General v?? manager datalar�n� General� baza g�t�r??r??k EmployeeID g�r?? birl??�dirin
manager_data<-read.csv('C:/Users/Sahib Babayev/Desktop/qss/manager survey data.csv')
join<-left_join(general_data,manager_data, by=c("EmployeeID"="EmployeeID"))
#8. Facet_grid funksiyasindan istifad?? ed??r??k h??r bir T??hsil sah??si ���n ayl�q g??lirl??rin paylanmas�n� g�st??r??n histogram qurun.
library(ggplot2)
ggplot(join, aes(x=MonthlyIncome)) +
  geom_histogram(binwidth=0.5, aes(fill=EducationField), colour="Black") + 
  facet_grid(EducationField~.,scales="free") +
  xlab("MonthlyIncome") + 
  ylab("") +
  ggtitle("Monthly Income by Education Field") +
  theme(plot.title = element_text(hjust = 0.5))
