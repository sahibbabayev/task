startup.data<-read.csv("C:/Users/Sahib Babayev/Desktop/R/50_startups.csv")
skim(startup.data)
create_report(startup.data)
hchart(cor(startup.data %>% 
  mutate_if(is.character,as.factor) %>% mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)
split <- startup.data$Profit %>% sample.split(SplitRatio = 0.80)
train <- startup.data  %>% subset (split == TRUE)                          
test <- startup.data  %>% subset (split == FALSE)                          
regression<- boost_tree(mode = "regression",
mtry = 30,
learn_rate = 0.357,
tree_depth = 2) %>% 
  set_engine(engine = "xgboost") %>% 
  fit(Profit~., data  = train)
y_pred <- regression %>% predict(new_data = test %>%  select(-Profit))
residuals = test$Profit - y_pred$.pred 
y_test_mean = mean(test$Profit)
tss = sum((test$Profit -y_test_mean)^2)
rss = sum(residuals^2)
R2 = 1-(rss/tss)
n <- 50
q <-2
Adjusted_R2 = 1-(1-R2)*((n-2)/(n-q-1))
Adjusted_R2 <- paste0(round(Adjusted_R2*100, 1), "%")
my_data <- as.data.frame(cbind(predicted = y_pred$.pred,
                               observed = test$Profit))
my_data %>% ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  ggtitle(glue('Linear Modelling - {enexpr(Adjusted_R2)}')) +
  xlab("Predecited") + 
  ylab("Observed") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly()






