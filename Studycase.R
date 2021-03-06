#1. Data ISLR kitabxanas�nda �Carseats� funksiyas� daxilind?? yerl??�dirilmi�. Datan� R-a daxil edin.
install.packages("ISLR")
Carseats<-ISLR::Carseats
#2. Shelveloc s�tunun ad�n� d??yi�dir??r??k Quality il?? ??v??z edin.
Carseats<-Carseats %>% rename(Quality=ShelveLoc)
#3. Datan�n ilkin statistik g�st??ril??rini ��xard�n.
skim(Carseats)
#4. Hans� ma�azalar�n 10 mind??n �ox sat��lar� var? (eyni zamanda azdan �oxa do�ru d�z�n)
Carseats %>% filter(Sales>10) %>% arrange(Sales) %>% View()
#5. US-da yerl??�m??y??n ??n y�ks??k g??lir?? malik ma�azan�n sat�� qiym??ti v?? r??qib �irk??tin t??klif etdiyi qiym??t ne�??dir?
With_filter<-Carseats %>% filter(US=='No') %>% arrange(desc(Income))
With_filter$Price[1]; With_filter$CompPrice[1]
#5(2) Dataya Sat�� g??lirini g�st??r??n s�tun ??lav?? edin. (Tips: h??r m??hsul ���n sat��� sat�� qiym??tin?? vurmaqla)                                           
Carseats<-Carseats %>% mutate(SaleIncome=Sales*Price)
#6. Dataya X??rcl??ri g�st??r??n s�tun ??lav?? edin.
Carseats<-Carseats %>% mutate(Cost=SaleIncome-Income)
#7. Reklama he� bir x??rci olmayan ma�azalar�n ??n �ox �??h??r, yoxsa k??nd ??razisind?? yerl??�diyini m�??yy??nl??�dirin.
Carseats %>%  filter(Advertising==0, Urban=='Yes') %>% count
Carseats %>%  filter(Advertising==0, Urban=='No') %>% count
#8. Scatter Plot vasit??sil?? Ma�azalar�n �??h??r ??traf�nda yerl??�ib yerl??�m??diyin?? ??sas??n G??lir v?? X??rc aras�ndak� qrafiki qurun
ggplot(Carseats, aes(x=Income, y=Cost, colour=Urban)) + 
  geom_point(size= 2) 
#9. D??yi�??nl??r aras�nda hcharterd??n istifad?? ed??r??k correlation qurun v?? onu �??rh edin. 
hchart(cor(Carseats %>% 
             mutate_if(is.character,as.factor) %>% 
             mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)
#10. Data Explorer vasit??sil?? online report haz�rlay�n.   
   create_report(Carseats)
   