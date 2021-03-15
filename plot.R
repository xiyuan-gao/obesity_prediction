BMImean <- BMIsd <- agemean <- agesd <- NULL

for (i in 1:26) {
  # preprocessing ####
  analysisdata <- data[[i]]  %>% 
    subset(data.ha54 == "no/don't know") %>%
    subset(data.hv106 != "don't know") %>%
    subset(data.hv106 != "missing") %>% 
    subset(data.hv106 != "dk") %>%
    subset(data.ha1 < 60) %>%
    mutate_at(cols1, funs(factor(.))) %>% 
    mutate_at(cols2, funs(as.numeric(.))) %>% 
    rename(weight=data.ha2, hight=data.ha3, area=data.hv025, wealth=data.hv270, 
           age=data.ha1, edu=data.hv106, pregnancy=data.ha54, 
           marriage=data.hv115, kids=data.hv014, members=data.hv009) %>% 
    na.omit() 
  # describe(analysisdata)
  analysisdata$BMI <- (analysisdata$weight/10)/(analysisdata$hight/1000)^2
  analysisdata$biclass <- ifelse(analysisdata$BMI > 25, 1, 0)
  analysisdata$biclass <- as.factor(analysisdata$biclass)
  analysisdata$wt <- analysisdata$data.hv005 / 1000000
  # describe(analysisdata)
  #%%% result_prevalence[[i]] <- rbind(data[[i]]$country[1], prevalence(analysisdata))
  BMImean[i] <- format(round(mean(analysisdata$BMI), 2), 
                       nsmall = 2)
  BMIsd[i] <- format(round(sqrt(var(analysisdata$BMI)), 2), 
                     nsmall = 2)
  
  agemean[i] <- format(round(mean(analysisdata$age), 2), 
                       nsmall = 2)
  
  agesd[i] <- format(round(sqrt(var(analysisdata$age)), 2), 
                     nsmall = 2)
}

sink("C:/Users/z1045/Desktop/description.txt")
cat("BMI \n")
print(cbind(BMImean , BMIsd))

cat("\n AGE \n")
print(cbind(agemean , agesd))
sink()


auc_tree <- auc_bagging <- auc_knn <- auc_nn <- auc_rf <- 
  auc_svm_linear <- auc_svm_radial <- auc_svm_polynomial <- NULL
for (i in 1:26) {
  auc_tree <- c(auc_tree, as.numeric(result_tree[[i]]$AUC))
  auc_bagging <- c(auc_bagging, as.numeric(result_bagging[[i]]$AUC))
  auc_knn <- c(auc_knn, as.numeric(result_knn[[i]]$AUC))
  auc_nn <- c(auc_nn, as.numeric(result_nn[[i]]$AUC))
  auc_rf <- c(auc_rf, as.numeric(result_rf[[i]]$AUC))
  auc_svm_linear <- c(auc_svm_linear, as.numeric(result_svm_linear[[i]]$AUC))
  auc_svm_radial <- c(auc_svm_radial, as.numeric(result_svm_radial[[i]]$AUC))
  auc_svm_polynomial <- c(auc_svm_polynomial, as.numeric(result_svm_polynomial[[i]]$AUC))
}

auc_tree <- auc_tree[auc_tree < 0.99 & auc_tree > 0.01]
auc_bagging <- auc_bagging[auc_bagging < 0.99 & auc_bagging > 0.01]
auc_knn <- auc_knn[auc_knn < 0.99 & auc_knn > 0.01]
auc_nn <- auc_nn[auc_nn < 0.99 & auc_nn > 0.01]
auc_rf <- auc_rf[auc_rf < 0.99 & auc_rf > 0.01]
auc_svm_linear <- auc_svm_linear[auc_svm_linear < 0.99 & auc_svm_linear > 0.01]
auc_svm_radial <- auc_svm_radial[auc_svm_radial < 0.99 & auc_svm_radial > 0.01]
auc_svm_polynomial <- auc_svm_polynomial[auc_svm_polynomial < 0.99 & auc_svm_polynomial > 0.01]


boxplot(auc_tree , auc_bagging , auc_knn , auc_nn , auc_rf , 
          auc_svm_linear , auc_svm_radial , auc_svm_polynomial,
        xaxt = "n", xlab="Algorithms",
        ylim = c(0.5, 1.1), ylab="AUC")
axis(side = 1, at = x,labels = c("CARt", "BAG", "KNN", "NN",
                                 "RF", "SVMlinr", "SVMradl", "SVMpoly"))
