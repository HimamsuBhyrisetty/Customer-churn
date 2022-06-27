#packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
install.packages("MLmetrics")
install.packages("factoextra")
library(factoextra)
library(GGally)
library(gtools)
library(caret)
library(car)
library(scales)
library(lmtest)
library(ggplot2)
library(plotly)
library(ggthemes)
library(MLmetrics)
library(performance)
library(ggpubr)

Churn <- read_excel("/Users/himamsu//Churn_Forecast.xlsx")
view(Churn)
#removing non impacting variables
summary(Churn %>% mutate_if(is.character, as.factor)%>% select(-customerID)) 
# checking missing values
colSums(is.na(Churn))
Churn %>% 
  select(-customerID) %>% 
  filter(is.na(TotalCharges))
cust_churn <- Churn %>%
  na.omit() %>% 
  select(-c(customerID))
dim(cust_churn)
#after removing nulls values and customer id
View(cust_churn)
#turing yes and no to 0 and 1
clus.lables = cust_churn_tidy$Churn

clus <- cust_churn_tidy[1:19]
clus
c <- scale(clus)
c_data <- dist(c)
# elbow plot to determine no of clusters
fviz_nbclust(c, kmeans, method = "wss")+
  labs(subtitle = "elbow")
km.out <- kmeans(c, centers = 3, nstart = 100)

print(km.out)
#visualizing the clusters
km.cluster <- km.out$cluster
fviz_cluster(list(data=c, cluster=km.cluster))
table(km.cluster, cust_churn_tidy$Churn)

#Churn against categorical features
cust_churn %>% select_if(is.character) %>% names(.)

# churn rate based on customer profile
cprofile_p1 <- cust_churn %>% 
  select(Churn, gender) %>% count(Churn, gender) %>% 
  ggplot(aes(gender, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

cprofile_p2 <- cust_churn %>% 
  select(Churn, Partner) %>% count(Churn, Partner) %>% 
  ggplot(aes(Partner, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

cprofile_p3 <- cust_churn %>% 
  select(Churn, Dependents) %>% count(Churn, Dependents) %>%
  ggplot(aes(Dependents, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

cprofile_p4 <- cust_churn %>% 
  mutate(SeniorCitizen = ifelse(SeniorCitizen == "1", "Yes", "No")) %>% 
  select(Churn, SeniorCitizen) %>% count(Churn, SeniorCitizen) %>% 
  ggplot(aes(SeniorCitizen, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

subplot1 <- ggarrange(cprofile_p1, cprofile_p2, cprofile_p3, cprofile_p4,  ncol = 2, nrow = 2, 
                      common.legend = TRUE, 
                      legend = "bottom")
subplot1


# churn rate based on service subscriptions
services_p8 <- cust_churn %>% 
  select(Churn, InternetService) %>% count(Churn, InternetService) %>% 
  ggplot(aes(InternetService, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

services_p9 <- cust_churn %>% 
  select(Churn, PhoneService) %>% count(Churn, PhoneService) %>% 
  ggplot(aes(PhoneService, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

subplot2 <- ggarrange(services_p8, services_p9, 
                      ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
subplot2
# 2nd churn rate based on service subscriptions
services_p1 <- cust_churn %>% 
  select(Churn, MultipleLines) %>% count(Churn, MultipleLines) %>% 
  ggplot(aes(MultipleLines, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

services_p2 <- cust_churn %>% 
  select(Churn, OnlineSecurity) %>% count(Churn, OnlineSecurity) %>% 
  ggplot(aes(OnlineSecurity, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

services_p3 <- cust_churn %>% 
  select(Churn, OnlineBackup) %>% count(Churn, OnlineBackup) %>% 
  ggplot(aes(OnlineBackup, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

services_p4 <- cust_churn %>% 
  select(Churn, DeviceProtection) %>% count(Churn, DeviceProtection) %>% 
  ggplot(aes(DeviceProtection, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

services_p5 <- cust_churn %>% 
  select(Churn, TechSupport) %>% count(Churn, TechSupport) %>% 
  ggplot(aes(TechSupport, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

services_p6 <- cust_churn %>% 
  select(Churn, StreamingTV) %>% count(Churn, StreamingTV) %>% 
  ggplot(aes(StreamingTV, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

services_p7 <- cust_churn %>% 
  select(Churn, StreamingMovies) %>% count(Churn, StreamingMovies) %>% 
  ggplot(aes(StreamingMovies, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))


subplot2B <- ggarrange(services_p1, services_p2, services_p3, services_p4,
                      services_p5, services_p6, services_p7, 
                      #labels = c("MultipleLines", "OnlineSecurity",  "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies"),
                      ncol = 2, nrow = 4, common.legend = TRUE, legend = "bottom")
subplot2B

# churn rate based on contract+billing type
subsinfo_p1 <- cust_churn %>% 
  select(Churn, Contract) %>% count(Churn, Contract) %>% 
  ggplot(aes(Contract, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

subsinfo_p2 <- cust_churn %>% 
  select(Churn, PaymentMethod) %>% count(Churn, PaymentMethod) %>% 
  ggplot(aes(PaymentMethod, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

subsinfo_p3 <- cust_churn %>% 
  select(Churn, PaperlessBilling) %>% count(Churn, PaperlessBilling) %>% 
  ggplot(aes(PaperlessBilling, n, fill = Churn)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  geom_text(aes(label=n),position = position_fill(vjust = 0.5))

subplot3 <- ggarrange(subsinfo_p1, subsinfo_p2, subsinfo_p3, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom")
subplot3

#Churn against numerical features
cust_churn %>% select_if(is.numeric) %>% names(.)
cust_churn %>% select_if(is.numeric) %>% summary(.)
#box plot on numerical data
bp1 <- cust_churn %>% 
  select(tenure) %>% 
  ggplot(aes(tenure)) + geom_boxplot()

bp2 <- cust_churn %>% 
  select(MonthlyCharges) %>% 
  ggplot(aes(MonthlyCharges)) + geom_boxplot()

bp3 <- cust_churn %>% 
  select(TotalCharges) %>% 
  ggplot(aes(TotalCharges)) + geom_boxplot()

subplot4 <- ggarrange(bp1, bp2, bp3, 
                      ncol = 1, nrow = 3,
                      common.legend = TRUE, legend = "bottom")
subplot4
# plot on all the numerical data
agg_tenure_churn <- cust_churn %>% 
  filter(Churn == "Yes") %>% 
  select(tenure, Churn) %>% 
  group_by(tenure) %>% 
  summarize(n = n()) %>% mutate(ChurnRate = round((n/sum(n)),2)*100)

numplot1 <- agg_tenure_churn %>% 
  ggplot(aes(x=tenure, y=ChurnRate, col=tenure)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(
    x = "tenure (in months)", y = "Churn Rate (%)"
  ) + theme(legend.position = "none") +
  annotate("text", label = paste("Corr = ", round(cor(agg_tenure_churn %>% select(-n))[2],3)), 
           x = 65, y = 20, size = 3, colour = "red")

agg_monthlycharges_churn <- cust_churn %>% 
  filter(Churn == "Yes") %>%
  select(MonthlyCharges, Churn) %>% 
  group_by(MonthlyCharges) %>% 
  summarize(n = n()) %>% mutate(ChurnRate = round((n/sum(n)),2)*100)

numplot2 <- agg_monthlycharges_churn %>% 
  ggplot(aes(x=MonthlyCharges, y=ChurnRate, color=MonthlyCharges)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(
    x = "Monthly Charges", y = "Churn Rate (%)"
  ) + theme(legend.position = "none") +
  annotate("text", label = paste("Corr = ", round(cor(agg_monthlycharges_churn %>% select(-n))[2],3)), 
           x = 100, y = 20, size = 3, colour = "red")

agg_totalcharges_churn <- cust_churn %>% 
  filter(Churn == "Yes") %>% 
  select(TotalCharges, Churn) %>% 
  group_by(TotalCharges) %>% 
  summarize(n = n()) %>% mutate(ChurnRate = round((n/sum(n)),2)*100)

numplot3 <- agg_totalcharges_churn %>% 
  ggplot(aes(x=TotalCharges, y=ChurnRate, color=TotalCharges)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(
    x = "Total Charges", y = "Churn Rate (%)"
  ) + theme(legend.position = "none") +
  annotate("text", label = paste("Corr = ", round(cor(agg_totalcharges_churn %>% select(-n))[2],6)), 
           x = 7500, y = 20, size = 3, colour = "red")

subplot5 <- ggarrange(numplot1, numplot2, numplot3, ncol = 1, nrow = 3)
subplot5


#Cleaning Categorical Feature
cust_churn_num <- cust_churn %>% select_if(is.numeric) # 4 columns
cust_churn_char <- cust_churn %>% 
  select(-Churn) %>% 
  select_if(is.character) # 15 columns

Churn <- as.factor(cust_churn$Churn) 
cust_churn_char <- lapply(cust_churn_char %>% select_if(is.character), 
                          function(x) {
                            gsub("No phone service", "No", x)
                          }) %>% as_tibble()
unique(cust_churn_char$MultipleLines)
cust_churn_char <- lapply(cust_churn_char %>% select_if(is.character), 
                          function(x) {
                            gsub("No internet service", "No", x)
                          }) %>% as_tibble()


unique(cust_churn_char$StreamingTV)
unique(cust_churn_char$OnlineSecurity)
unique(cust_churn_char$StreamingMovies)

#Feature Selection
cust_churn_char <- cust_churn_char %>% 
  select(-c(gender, PhoneService))

cust_churn_num <- cust_churn_num %>% 
  select(-c(MonthlyCharges, TotalCharges))
#Create Dummy Variable
cust_churn_char <- data.frame(sapply(cust_churn_char,function(x) data.frame(model.matrix(~x-1,data =cust_churn_char))[,-1]))

head(cust_churn_char)
# concat cust_churn_char + cust_churn_num
cust_churn_tidy <- cbind(cust_churn_char, cust_churn_num, Churn)
str(cust_churn_tidy)

# cross validation
RNGkind(sample.kind = "Rounding")
set.seed(123)
row_data <- nrow(cust_churn_tidy)
# sampel dan ambil 80% data secara acak
index <- sample(row_data, row_data*0.8)
data_train <- cust_churn_tidy[ index, ]
data_test <- cust_churn_tidy[ -index, ] 

data_train %>% 
  group_by(Churn) %>% 
  summarise(total = n()) %>% 
  mutate(percentage = round((total/sum(total))*100,2))

library(caret)
set.seed(123)
data_train_up <- upSample(x = data_train %>% select(-Churn),
                          y = data_train$Churn,
                          list = F,
                          yname = "Churn")

table(data_train_up$Churn)

#fitting R model
# model - only intercept
model_null <- glm(Churn~1,data_train_up, family = "binomial")

# model -full predictor
model_full <- glm(Churn~.,data_train_up, family = "binomial")

# model stepwise backward
model_bw <- step(object = model_full, direction = "backward", trace = F)
summary(model_bw)



# fitting knn model
#initialize k neighbour
k_neighbour <- sqrt(nrow(data_train_up)) %>% round()

# Model Fitting K-NN + prediction
model_knn <- class::knn(train = data_train_up %>% select(-Churn),
                        cl = data_train_up$Churn,
                        test = data_test %>% select(-Churn),
                        k = k_neighbour)

#evaluation of test data
data_test %>% 
  group_by(Churn) %>% 
  summarise(total = n()) %>% 
  mutate(percentage = round((total/sum(total))*100,2))

#LR model
churn_modelbw <- predict(model_bw, data_test, type = "response")
pred_churn_modelbw <- ifelse(churn_modelbw > 0.5, "Yes", "No") %>% 
  as.factor()

library(caret)
cm_model_lr <- confusionMatrix(pred_churn_modelbw,
                               data_test$Churn,
                               positive = "Yes")
cm_model_lr

#knn model
cm_model_knn <- confusionMatrix(model_knn, data_test$Churn, positive = "Yes")
cm_model_knn


#evalution of both models
library(tibble)

summary_eval_lr <- data_frame(Model = "Logistic Regression", Accuracy = round(cm_model_lr$overall[1]*100,2),
                              Recall = round(cm_model_lr$byClass[1]*100,2),
                              Specificity = round(cm_model_lr$byClass[2]*100,2),
                              Precision = round(cm_model_lr$byClass[3]*100,2),
                              Kappa = round(cm_model_lr$overall[2]*100,2))

summary_eval_knn <- data_frame(Model = "K-NN", Accuracy = round(cm_model_knn$overall[1]*100,2),
                               Recall = round(cm_model_knn$byClass[1]*100,2),
                               Specificity = round(cm_model_knn$byClass[2]*100,2),
                               Precision = round(cm_model_knn$byClass[3]*100,2), 
                               Kappa = round(cm_model_knn$overall[2]*100,2))

summary_eval_model <-  rbind(summary_eval_lr, summary_eval_knn)
summary_eval_model