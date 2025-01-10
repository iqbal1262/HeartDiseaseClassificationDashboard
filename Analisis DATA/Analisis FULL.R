####DATA FRAME####
setwd('D:/Sekolah/Kuliah/Semester 6/Data Mining dan Visualisasi/Final Project') ; getwd()
heart_dataset<-read.csv('heart.csv');heart_dataset
head(heart_dataset)

####PREPROCESSING####
#TYPE DATA
str(heart_dataset)

#COVERT DATA KATEGORIK KE FAKTOR
for (i in c("sex","cp","fbs","restecg","exng","slp","thall","output")){
  heart_dataset[[i]]<-as.factor(heart_dataset[[i]])}
str(heart_dataset)

#DUPLIKAT & PENANGANANNYA
heart_dataset[duplicated(heart_dataset), ]
heart_dataset<-unique(heart_dataset)
rownames(heart_dataset) <- NULL

#UNIQUE VALUE
unique_value<-list()
for(i in names(heart_dataset)){
  unique_value[[i]]<-unique(heart_dataset[[i]])}
unique_value

#CEK MISSING VALUE
colSums(is.na(heart_dataset))

#VARIABEL CAA NILAINYA 0-3
heart_dataset[heart_dataset$caa>3,]
heart_dataset<-heart_dataset[!heart_dataset$caa==4,]
rownames(heart_dataset) <- NULL

#BOXPLOT
library(dplyr)
df_num <- heart_dataset %>% select_if(is.numeric)
boxplot(df_num, main = "Boxplot Kolom Numerik Heart Attack Dataset")

library(ggplot2)
ggplot(df_num,aes(y = age)) +
  geom_boxplot(fill="skyblue") +
  labs(title = "Boxplot Variabel Age")

ggplot(df_num,aes(y = trtbps)) +
  geom_boxplot(fill="skyblue") +
  labs(title = "Boxplot Variabel Resting Blood Pressure")

ggplot(df_num,aes(y = chol)) +
  geom_boxplot(fill="skyblue") +
  labs(title = "Boxplot Variabel Cholestoral")

ggplot(df_num,aes(y = thalachh)) +
  geom_boxplot(fill="skyblue") +
  labs(title = "Boxplot Variabel Maximum Heart Rate Achieved")

ggplot(df_num,aes(y = oldpeak)) +
  geom_boxplot(fill="skyblue") +
  labs(title = "Boxplot Variabel Oldpeak")

#PENANGANAN OUTLIER
for (i in c("trtbps","chol","thalachh","oldpeak")) {
  Q1 <- quantile(heart_dataset[[i]], 0.25)
  Q3 <- quantile(heart_dataset[[i]], 0.75)
  IQR <- Q3 - Q1
  upper_limit <- Q3 + 1.5 * IQR
  lower_limit <- Q1 - 1.5 * IQR
  heart_dataset[[i]] <- ifelse(heart_dataset[[i]] > upper_limit, upper_limit,
                            ifelse(heart_dataset[[i]] < lower_limit, lower_limit, 
                                   heart_dataset[[i]]))}

#BOXPLOT SETELAH PENANGANAN
df_num <- heart_dataset %>% select_if(is.numeric)
boxplot(df_num, main = "Boxplot Kolom Numerik Heart Attack Dataset")

library(ggplot2)
ggplot(df_num,aes(y = trtbps)) +
  geom_boxplot() +
  labs(title = "Boxplot Variabel Resting Blood Pressure")

ggplot(df_num,aes(y = chol)) +
  geom_boxplot() +
  labs(title = "Boxplot Variabel Cholestoral")

ggplot(df_num,aes(y = thalachh)) +
  geom_boxplot() +
  labs(title = "Boxplot Variabel Maximum Heart Rate Achieved")

ggplot(df_num,aes(y = oldpeak)) +
  geom_boxplot() +
  labs(title = "Boxplot Variabel Oldpeak")

#PROPORSI
kategorik <- c("sex","cp","fbs","restecg","exng","slp","thall","output")
for (col in kategorik) {
  cat(paste("Jumlah dan Proporsi setiap level dalam kolom", col, ":\n"))
  freq_table <- table(heart_dataset[[col]])
  prop_table <- prop.table(freq_table) * 100
  result_table <- data.frame(
    Level = names(freq_table),
    Frekuensi = as.vector(freq_table),
    Proporsi = as.vector(prop_table))
  print(result_table)
  cat("\n")}

heart_dataset<-heart_dataset[!heart_dataset$restecg==2,]
heart_dataset[["restecg"]]<-as.factor(as.vector(heart_dataset[["restecg"]]))
heart_dataset<-heart_dataset[!heart_dataset$thall==0,]
heart_dataset[["thall"]]<-as.factor(as.vector(heart_dataset[["thall"]]))
rownames(heart_dataset) <- NULL

#DESKRIPSI DATASET
library(psych)
heart_dataset %>% select_if(is.numeric) %>% describe()
summary(heart_dataset)

write.csv(heart_dataset,"D:/KULIAH/DATMIN/DATA SETELAH PREPROCESING.csv")

####VISUALIASI####
#HISTOGRAM
df_num <- heart_dataset %>% select_if(is.numeric) 
library(ggplot2)
ggplot(df_num,aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram Variabel Age")
  
ggplot(df_num,aes(x = trtbps)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Histogram Variabel Resting Blood Pressure")

ggplot(df_num,aes(x = chol)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram Variabel Cholestoral")

ggplot(df_num,aes(x = thalachh)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram Variabel Maximum Heart Rate Achieved")

ggplot(df_num,aes(x = oldpeak)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram Variabel Oldpeak")

ggplot(df_num,aes(x = caa)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram Variabel Caa")

#BAR CHART
ggplot(heart_dataset,aes(x = output)) + 
  geom_bar() + labs(title = "Bar Chart", x = "Output", y = "Jumlah") + theme_minimal()

library(reshape2)
df_kat <- heart_dataset %>% select_if(is.factor) 
heart_melted <- melt(df_kat, id.vars = "output")
ggplot(heart_melted, aes(x = value, fill = factor(output))) +
  geom_bar(position = "dodge", stat = "count") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Bar Chart Variabel vs Output",
       x = "Nilai",
       y = "Jumlah",
       fill = "0 = Tidak Heart Attack, 1 = Heart Attack") +
  theme_minimal()

df_num <- heart_dataset %>% select_if(is.numeric)
df_num$output <- heart_dataset$output
heart_melted <- melt(df_num, id.vars = "output")
ggplot(heart_melted, aes(x = value, fill = factor(output))) +
  geom_histogram(position = "dodge", bins = 20) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogram Variabel Numerik vs Output",
       x = "Nilai",
       y = "Jumlah",
       fill = "0 = Tidak Heart Attack, 1 = Heart Attack") +
  theme_minimal()

library(dplyr)
library(faux)
library(DataExplorer)
library(randomForest)

####FEATURE SELECTION####
library(caret)
heart_scale <- heart_dataset
for (i in names(heart_scale)) {
  if (is.numeric(heart_scale[[i]])) {
    heart_scale[[i]] <- scale(heart_scale[[i]])}
  else {heart_scale[[i]] <- heart_scale[[i]]}}
idx_0 <- as.numeric(row.names(heart_scale[heart_scale$output==0,]))
idx_1 <- as.numeric(row.names(heart_scale[heart_scale$output==1,]))
set.seed(11)
sample_idx_0 <- sample(idx_0, size = 2 * length(idx_0)/3, replace = FALSE)
set.seed(22)
sample_idx_1 <- sample(idx_1, size = 2 * length(idx_1)/3, replace = FALSE)
train <- heart_scale[c(sample_idx_0, sample_idx_1), ]
test <- heart_scale[-c(sample_idx_0,sample_idx_1), ]
y_train <- train$output
y_test <- test$output
x_train <- train[, -which(names(heart_scale) == "output")]
x_test <- test[, -which(names(heart_scale) == "output")]

#METODE RFE (Recursive Feature Elimination)
set.seed(33)
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 10, 
                      number = 10) 
hasil_rfe <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:(ncol(heart_scale)-1)),
                   rfeControl = control)

hasil_rfe
predictors(hasil_rfe)

#ACCURACY DAN KAPPA
postResample(predict(hasil_rfe, x_test), y_test)

#VARIABEL PENTING
variabel_penting <- data.frame(feature = row.names(varImp(hasil_rfe))[1:11],
                          importance = varImp(hasil_rfe)[1:11, 1]);variabel_penting
ggplot(data = variabel_penting, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Variabel", y = "Nilai Kontribusi", main="Kontribusi Variabel") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6) + theme_minimal()
heart_imp <- data.frame(heart_scale[,predictors(hasil_rfe)[1:6]],output=heart_scale$output)

heart_important <- data.frame(heart_dataset[,predictors(hasil_rfe)[1:6]],output=heart_dataset$output)
write.csv(heart_important,"D:/KULIAH/DATMIN/DATA ANALISIS.csv")

####TRAINING-TESTING REPEATED HOLT OUT####
library(kernlab)
library(caret)
library(ROCR)
library(dplyr)
library(e1071)
library(partykit)
library(randomForest)

n_repeats <- 10
kernels <- c("rbfdot", "polydot", "vanilladot")
C_values <- seq(0.01, 0.1, 0.01)

svm_results <- data.frame(Kernel = character(), Akurasi = double(), C = double(), Sensitivitas = double(), Spesifisitas = double(), AUC = double(), Repetition = integer())
nb_results <- data.frame(Akurasi = double(), Sensitivitas = double(), Spesifisitas = double(), AUC = double(), Repetition = integer())
rf_results <- data.frame(Akurasi = double(), Sensitivitas = double(), Spesifisitas = double(), AUC = double(), Repetition = integer())
dt_results <- data.frame(Akurasi = double(), Sensitivitas = double(), Spesifisitas = double(), AUC = double(), Repetition = integer())

roc_data <- list()
#REPEATED HOLD OUT
for (rep in 1:n_repeats) {
  idx_0 <- as.numeric(row.names(heart_imp[heart_imp$output == 0, ]))
  idx_1 <- as.numeric(row.names(heart_imp[heart_imp$output == 1, ]))
  set.seed(rep * 10)
  sample_idx_0 <- sample(idx_0, size = 2 * length(idx_0)/3, replace = FALSE)
  sample_idx_1 <- sample(idx_1, size = 2 * length(idx_1)/3, replace = FALSE)
  train <- heart_imp[c(sample_idx_0, sample_idx_1), ]
  test <- heart_imp[-c(sample_idx_0, sample_idx_1), ]
  y_train <- train$output
  y_test <- test$output
  x_train <- train[, -which(names(heart_imp) == "output")]
  x_test <- test[, -which(names(heart_imp) == "output")]
  
  #DATA SVM
  xtrain <- model.matrix(~ ., data = x_train)[, -1]
  xtest <- model.matrix(~ ., data = x_test)[, -1]
  
  #SVM
  for (i in 1:length(kernels)) {
    set.seed(44)
    best_accuracy <- 0
    best_model <- NULL
    best_C <- 0
    
    for (C_val in C_values) {
      model <- ksvm(x = as.matrix(xtrain), y = as.factor(y_train), kernel = kernels[i], C = C_val)
      ypred <- predict(model, as.matrix(xtest))
      cm <- confusionMatrix(as.factor(y_test), ypred)
      accuracy <- cm$overall["Accuracy"]
      sensitivity <- cm$byClass["Sensitivity"]
      specificity <- cm$byClass["Specificity"]
      pred_test_svm <- predict(model, xtest, type = "decision")
      pred_roc_svm <- prediction(predictions = pred_test_svm, labels = y_test)
      auc <- performance(prediction.obj = pred_roc_svm, measure = "auc")@y.values[[1]]
      
      if (accuracy > best_accuracy) {
        best_accuracy <- accuracy
        best_model <- model
        best_C <- C_val
        best_cm <- cm
        best_auc <- auc
        best_pred_roc <- pred_roc_svm}}
    
    svm_results <- rbind(svm_results, data.frame(Kernel = kernels[i],C=best_C, Akurasi = best_accuracy, Sensitivitas = best_cm$byClass["Sensitivity"], Spesifisitas = best_cm$byClass["Specificity"], AUC = best_auc, Repetition = rep))}
  
  #NAIVE BAYES
  model_naive <- naiveBayes(y_train ~ ., x_train)
  pred_naive <- predict(model_naive, x_test, type = "class")
  cm <- confusionMatrix(pred_naive, y_test, positive = "1")
  pred_test_nb <- predict(model_naive, x_test, type = "raw")
  pred_roc_nb <- prediction(predictions = pred_test_nb[, "1"], labels = y_test)
  auc <- performance(prediction.obj = pred_roc_nb, measure = "auc")@y.values[[1]]
  nb_results <- rbind(nb_results, data.frame(Akurasi = cm$overall["Accuracy"], Sensitivitas = cm$byClass["Sensitivity"], Spesifisitas = cm$byClass["Specificity"], AUC = auc, Repetition = rep))
  
  #RANDOM FOREST
  set.seed(55)
  model_rf <- randomForest(y_train ~ ., x_train)
  pred_rf <- predict(model_rf, x_test, type = "class")
  cm <- confusionMatrix(pred_rf, y_test, positive = "1")
  pred_test_rf <- predict(model_rf, x_test, type = "prob")
  pred_roc_rf <- prediction(predictions = pred_test_rf[, "1"], labels = y_test)
  auc <- performance(prediction.obj = pred_roc_rf, measure = "auc")@y.values[[1]]
  rf_results <- rbind(rf_results, data.frame(Akurasi = cm$overall["Accuracy"], Sensitivitas = cm$byClass["Sensitivity"], Spesifisitas = cm$byClass["Specificity"], AUC = auc, Repetition = rep))
  
  #DECISION TREE
  set.seed(66)
  model_dt <- ctree(y_train ~ ., x_train)
  pred_dt <- predict(model_dt, x_test, type = "response")
  cm <- confusionMatrix(pred_dt, y_test, positive = "1")
  pred_test_dt <- predict(model_dt, x_test, type = "prob")
  pred_roc_dt <- prediction(predictions = pred_test_dt[, "1"], labels = y_test)
  auc <- performance(prediction.obj = pred_roc_dt, measure = "auc")@y.values[[1]]
  dt_results <- rbind(dt_results, data.frame(Akurasi = cm$overall["Accuracy"], Sensitivitas = cm$byClass["Sensitivity"], Spesifisitas = cm$byClass["Specificity"], AUC = auc, Repetition = rep))
  
  #ROC
  roc_data[[paste0("SVM_", rep)]] <- best_pred_roc
  roc_data[[paste0("NB_", rep)]] <- pred_roc_nb
  roc_data[[paste0("RF_", rep)]] <- pred_roc_rf
  roc_data[[paste0("DT_", rep)]] <- pred_roc_dt}

#AGGREGAT
svm_summary <- svm_results %>% group_by(Kernel) %>% summarize(Mean_Akurasi = mean(Akurasi), Mean_Sensitivitas = mean(Sensitivitas), Mean_Spesifisitas = mean(Spesifisitas), Mean_AUC = mean(AUC),Mean_C = mean(C))
nb_summary <- nb_results %>% summarize(Mean_Akurasi = mean(Akurasi), Mean_Sensitivitas = mean(Sensitivitas), Mean_Spesifisitas = mean(Spesifisitas), Mean_AUC = mean(AUC))
rf_summary <- rf_results %>% summarize(Mean_Akurasi = mean(Akurasi), Mean_Sensitivitas = mean(Sensitivitas), Mean_Spesifisitas = mean(Spesifisitas), Mean_AUC = mean(AUC))
dt_summary <- dt_results %>% summarize(Mean_Akurasi = mean(Akurasi), Mean_Sensitivitas = mean(Sensitivitas), Mean_Spesifisitas = mean(Spesifisitas), Mean_AUC = mean(AUC))

#SUMMARY
print(svm_summary)
print(nb_summary)
print(rf_summary)
print(dt_summary)

# SVM KERNEL TERBAIK
best_svm <- svm_summary %>% filter(Mean_Akurasi == max(Mean_Akurasi))
print(best_svm)
best_kernel <- "vanilladot"
best_C <- best_svm$Mean_C
svm_results_terbaik <- data.frame(Akurasi = double(), C = double(), Sensitivitas = double(), Spesifisitas = double(), AUC = double(), Repetition = integer())
roc_data_svm <- list()

for (rep in 1:n_repeats) {
  idx_0 <- as.numeric(row.names(heart_imp[heart_imp$output == 0, ]))
  idx_1 <- as.numeric(row.names(heart_imp[heart_imp$output == 1, ]))
  set.seed(rep * 100)
  sample_idx_0 <- sample(idx_0, size = 2 * length(idx_0)/3, replace = FALSE)
  sample_idx_1 <- sample(idx_1, size = 2 * length(idx_1)/3, replace = FALSE)
  train <- heart_imp[c(sample_idx_0, sample_idx_1), ]
  test <- heart_imp[-c(sample_idx_0, sample_idx_1), ]
  y_train <- train$output
  y_test <- test$output
  x_train <- train[, -which(names(heart_imp) == "output")]
  x_test <- test[, -which(names(heart_imp) == "output")]
  
  xtrain <- model.matrix(~ ., data = x_train)[, -1]
  xtest <- model.matrix(~ ., data = x_test)[, -1]
  
  #SVM KERNEL TERBAIK
  set.seed(77)
  model <- ksvm(x = as.matrix(xtrain), y = as.factor(y_train), kernel = best_kernel, C = best_C)
  ypred <- predict(model, as.matrix(xtest))
  cm <- confusionMatrix(as.factor(y_test), ypred)
  accuracy <- cm$overall["Accuracy"]
  sensitivity <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  pred_test_svm <- predict(model, xtest, type = "decision")
  pred_roc_svm <- prediction(predictions = pred_test_svm, labels = y_test)
  auc <- performance(prediction.obj = pred_roc_svm, measure = "auc")@y.values[[1]]
  
  svm_results_terbaik <- rbind(svm_results_terbaik, data.frame(C=best_C, Akurasi = accuracy, Sensitivitas = sensitivity, Spesifisitas = specificity, AUC = auc, Repetition = rep))
  
  roc_data_svm[[paste0("SVM_", rep)]] <- best_pred_roc}

svm_terbaik_summary <- svm_results_terbaik %>% summarize(Mean_Akurasi = mean(Akurasi), Mean_Sensitivitas = mean(Sensitivitas), Mean_Spesifisitas = mean(Spesifisitas), Mean_AUC = mean(AUC),Mean_C = mean(C))
print(svm_terbaik_summary)

#KURVA ROC
mean_roc_curve <- function(pred_roc_list) {
  tpr_list <- list()
  fpr_list <- list()
  
  for (i in 1:length(pred_roc_list)) {
    perf <- performance(pred_roc_list[[i]], measure = "tpr", x.measure = "fpr")
    tpr_list[[i]] <- perf@y.values[[1]]
    fpr_list[[i]] <- perf@x.values[[1]]}
  
  max_length <- max(sapply(tpr_list, length))
  mean_tpr <- sapply(1:max_length, function(j) mean(sapply(tpr_list, function(x) ifelse(j <= length(x), x[j], NA)), na.rm = TRUE))
  mean_fpr <- sapply(1:max_length, function(j) mean(sapply(fpr_list, function(x) ifelse(j <= length(x), x[j], NA)), na.rm = TRUE))
  
  list(tpr = mean_tpr, fpr = mean_fpr)}

#MEAN KURVA ROC
nb_mean_roc <- mean_roc_curve(roc_data[grep("NB_", names(roc_data))])
rf_mean_roc <- mean_roc_curve(roc_data[grep("RF_", names(roc_data))])
dt_mean_roc <- mean_roc_curve(roc_data[grep("DT_", names(roc_data))])
svm_mean_roc_terbaik <- mean_roc_curve(roc_data_svm[grep("SVM_", names(roc_data_svm))])

#PLOT MEAN KURVA ROC
plot(svm_mean_roc_terbaik$fpr, svm_mean_roc_terbaik$tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Mean Kurva ROC SVM")
abline(0, 1, lty = 2)

plot(nb_mean_roc$fpr, nb_mean_roc$tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Mean Kurva ROC Naive Bayes")
abline(0, 1, lty = 2)

plot(rf_mean_roc$fpr, rf_mean_roc$tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Mean Kurva ROC Random Forest")
abline(0, 1, lty = 2)

plot(dt_mean_roc$fpr, dt_mean_roc$tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Mean Kurva ROC Decision Tree")
abline(0, 1, lty = 2)


####TRAINING TESTING 10-FOLD CROSS VALIDATION####
ctrl <- trainControl(method = "cv", number = 10)
#NAIVE BAYES CV
set.seed(88)
model_nb_cv <- train(output ~., data = train, method = "naive_bayes", trControl = ctrl)
pred_nb_cv <- predict(model_nb_cv, test[, !names(test) %in% "output"])
confusionMatrix(pred_nb_cv, test[, names(test) %in% "output"])
pred_test_nb_cv <- predict(model_nb_cv, test[, !names(test) %in% "output"], type = "prob")
pred_roc_nb_cv <- prediction(predictions = pred_test_nb_cv[, 2], labels = test[, names(test) %in% "output"])
perf <- performance(pred_roc_nb_cv, "tpr", "fpr")
plot(perf, main = "Kurva ROC Untuk Naive Bayes")
abline(0, 1, lty = 2)
auc_pred_nb_cv <- performance(pred_roc_nb_cv, "auc")
auc_pred_nb_cv@y.values

#RANDOM FOREST CV
set.seed(99)
model_rf_cv <- train(output ~., data = train, method = "rf", trControl = ctrl)
pred_rf_cv <- predict(model_rf_cv, test[, !names(test) %in% "output"])
confusionMatrix(pred_rf_cv, test[, names(test) %in% "output"])
pred_test_rf_cv <- predict(model_rf_cv, test[, !names(test) %in% "output"], type = "prob")
pred_roc_rf_cv <- prediction(predictions = pred_test_rf_cv[, 2], labels = test[, names(test) %in% "output"])
perf <- performance(pred_roc_rf_cv, "tpr", "fpr")
plot(perf,main = "Kurva ROC Untuk Random Forest")
abline(0, 1, lty = 2)
auc_pred_rf_cv <- performance(pred_roc_rf_cv, "auc")
auc_pred_rf_cv@y.values

#DECISION TREE CV
set.seed(111)
model_dt_cv <- train(output ~., data = train, method = "ctree", trControl = ctrl)
pred_dt_cv <- predict(model_dt_cv, test[, !names(test) %in% "output"])
confusionMatrix(pred_dt_cv, test[, names(test) %in% "output"])
pred_test_dt_cv <- predict(model_dt_cv, test[, !names(test) %in% "output"], type = "prob")
pred_roc_dt_cv <- prediction(predictions = pred_test_dt_cv[, 2], labels = test[, names(test) %in% "output"])
perf <- performance(pred_roc_dt_cv, "tpr", "fpr")
plot(perf,main = "Kurva ROC Untuk Decision Tree")
abline(0, 1, lty = 2)
auc_pred_dt_cv <- performance(pred_roc_dt_cv, "auc")
auc_pred_dt_cv@y.values

#SVM CV
library(kernlab)
library(caret)
library(ROCR)
library(pROC)
kernels <- c("svmLinear", "svmPoly", "svmRadial")
model_svm <- list()
akurasi <- rep(0, length(kernels))
C_terbaik <- rep(0, length(kernels))
degree_terbaik <- rep(0, length(kernels))
scale_terbaik <- rep(0, length(kernels))
sigma_terbaik <- rep(0, length(kernels))

xtrain <- model.matrix(~ ., data = x_train)[, -1]
xtest <- model.matrix(~ ., data = x_test)[, -1]
C_values <- seq(0.01, 0.1, 0.01)

for (i in 1:length(kernels)) {
  set.seed(222)
  best_accuracy <- 0
  best_model <- NULL
  best_C <- 0
  best_degree <- 0
  best_scale <- 0
  best_sigma <- 0
  
  if (kernels[i] == "svmPoly") {
    degree_values <- c(2, 3, 4)
    scale_values <- c(0.001, 0.01, 0.1)
    for (C_val in C_values) {
      for (degree_val in degree_values) {
        for (scale_val in scale_values) {
          train_control <- trainControl(method = "cv", number = 10)
          tune_grid <- expand.grid(C = C_val, degree = degree_val, scale = scale_val)
          
          model <- train(x = xtrain, y = as.factor(y_train), method = kernels[i], 
                         trControl = train_control, tuneGrid = tune_grid)
          
          ypred <- predict(model, xtest)
          accuracy <- confusionMatrix(as.factor(y_test), ypred)$overall["Accuracy"]
          
          if (accuracy > best_accuracy) {
            best_accuracy <- accuracy
            best_model <- model
            best_C <- C_val
            best_degree <- degree_val
            best_scale <- scale_val}}}}
  } else if (kernels[i] == "svmRadial") {
    sigma_values <- c(0.001, 0.01, 0.1)
    for (C_val in C_values) {
      for (sigma_val in sigma_values) {
        train_control <- trainControl(method = "cv", number = 10)
        tune_grid <- expand.grid(C = C_val, sigma = sigma_val)
        
        model <- train(x = xtrain, y = as.factor(y_train), method = kernels[i], 
                       trControl = train_control, tuneGrid = tune_grid)
        
        ypred <- predict(model, xtest)
        accuracy <- confusionMatrix(as.factor(y_test), ypred)$overall["Accuracy"]
        
        if (accuracy > best_accuracy) {
          best_accuracy <- accuracy
          best_model <- model
          best_C <- C_val
          best_sigma <- sigma_val
        }}}
  } else {
    for (C_val in C_values) {
      train_control <- trainControl(method = "cv", number = 10)
      tune_grid <- expand.grid(C = C_val)
      
      model <- train(x = xtrain, y = as.factor(y_train), method = kernels[i], 
                     trControl = train_control, tuneGrid = tune_grid)
      
      ypred <- predict(model, xtest)
      accuracy <- confusionMatrix(as.factor(y_test), ypred)$overall["Accuracy"]
      
      if (accuracy > best_accuracy) {
        best_accuracy <- accuracy
        best_model <- model
        best_C <- C_val}}}
  
  model_svm[[i]] <- best_model
  akurasi[i] <- best_accuracy
  C_terbaik[i] <- best_C
  if (kernels[i] == "svmPoly") {
    degree_terbaik[i] <- best_degree
    scale_terbaik[i] <- best_scale
  } else if (kernels[i] == "svmRadial") {
    sigma_terbaik[i] <- best_sigma}}

#HASIL TUNING SVM
results <- data.frame(Kernel = kernels, Akurasi = akurasi, Best_C = C_terbaik, Best_Degree = degree_terbaik, Best_Scale = scale_terbaik, Best_Sigma = sigma_terbaik)
print(results)

model_terbaik_svm <- train(x = xtrain, y = as.factor(y_train), method = "svmPoly", 
                           trControl = trainControl(method = "cv", number = 10), 
                           tuneGrid = expand.grid(C = 0.09,degree = 2, scale = 0.1))

decision_values <- predict(model_terbaik_svm, xtest, type = "raw")
confusionMatrix(y_test,decision_values)
roc_curve <- roc(as.numeric(y_test), as.numeric(decision_values));roc_curve
auc_value <- auc(roc_curve);auc_value
plot(roc_curve, main = paste("Kurva ROC Untuk SVM Kernel Polynomial"))



