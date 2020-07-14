#1. Data ISLR kitabxanasýnda «Carseats» funksiyasý daxilind?? yerl??þdirilmiþ. Dataný R-a daxil edin.
install.packages("ISLR")
Carseats<-ISLR::Carseats
#2. Shelveloc sütunun adýný d??yiþdir??r??k Quality il?? ??v??z edin.
Carseats<-Carseats %>% rename(Quality=ShelveLoc)
#3. Datanýn ilkin statistik göst??ril??rini çýxardýn.
skim(Carseats)
#4. Hansý maðazalarýn 10 mind??n çox satýþlarý var? (eyni zamanda azdan çoxa doðru düzün)
Carseats %>% filter(Sales>10) %>% arrange(Sales) %>% View()
#5. US-da yerl??þm??y??n ??n yüks??k g??lir?? malik maðazanýn satýþ qiym??ti v?? r??qib þirk??tin t??klif etdiyi qiym??t neç??dir?
With_filter<-Carseats %>% filter(US=='No') %>% arrange(desc(Income))
With_filter$Price[1]; With_filter$CompPrice[1]
#5(2) Dataya Satýþ g??lirini göst??r??n sütun ??lav?? edin. (Tips: h??r m??hsul üçün satýþý satýþ qiym??tin?? vurmaqla)                                           
Carseats<-Carseats %>% mutate(SaleIncome=Sales*Price)
#6. Dataya X??rcl??ri göst??r??n sütun ??lav?? edin.
Carseats<-Carseats %>% mutate(Cost=SaleIncome-Income)
#7. Reklama heç bir x??rci olmayan maðazalarýn ??n çox þ??h??r, yoxsa k??nd ??razisind?? yerl??þdiyini mü??yy??nl??þdirin.
Carseats %>%  filter(Advertising==0, Urban=='Yes') %>% count
Carseats %>%  filter(Advertising==0, Urban=='No') %>% count
#8. Scatter Plot vasit??sil?? Maðazalarýn þ??h??r ??trafýnda yerl??þib yerl??þm??diyin?? ??sas??n G??lir v?? X??rc arasýndaký qrafiki qurun
ggplot(Carseats, aes(x=Income, y=Cost, colour=Urban)) + 
  geom_point(size= 2) 
#9. D??yiþ??nl??r arasýnda hcharterd??n istifad?? ed??r??k correlation qurun v?? onu þ??rh edin. 
hchart(cor(Carseats %>% 
             mutate_if(is.character,as.factor) %>% 
             mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)
#10. Data Explorer vasit??sil?? online report hazýrlayýn.   
   create_report(Carseats)
   