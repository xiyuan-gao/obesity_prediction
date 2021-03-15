library(rdhs)

library(rpart) # decision tree
library(nnet) # nerual network
library(caret)

library(BBmisc)
library(dplyr)
library(ROSE)
set_rdhs_config(email = "wolduh@health.missouri.edu",
                project = "Socioeconomic and Geographical Factors’ Contribution to 
                Variation in Anemia in Low-income and Mid",
                config_path = "rdhs.json",
                global=FALSE)
################## set up ####
# lets find all the surveys that fit our search criteria 32 COUNTRIES
survs <- dhs_surveys(countryIds = c("AO", "BJ", "BF", "BU", "CM", "TD", "KM",
                                    "EG", "GA", "GM", "GH", "GN", "KE", "LS", 
                                    "LB", "MD", "MW", "ML", "MZ", "NM", "NI", 
                                    "NG", "RW", "ST", "SN", "SL", "ZA", "TZ", 
                                    "TG", "UG", "ZM", "ZW"),
                     surveyType = "DHS",
                     surveyYearStart = 2007)
datasets <- dhs_datasets(surveyIds = survs$SurveyId, 
                         fileFormat = "flat",
                         fileType = "PR")

# the first time we call this function, rdhs will make the API request
# microbenchmark::microbenchmark(dhs_surveys(surveyYear = 1992),times = 1)

vars <- search_variables(datasets$FileName, 
                         variables = c("ha2", "ha3", "hv025", "hv270", "ha1", "hv106", 
                                       "hv005","ha54","hv115", "hv014", "hv009"), 
                         reformat = TRUE)
extract <- extract_dhs(vars, add_geo = FALSE)
cols1 <- c("data.hv025", "data.hv270", "data.hv106", "data.ha54", "data.hv115")
cols2 <- c("data.hv005", "data.ha1", "data.ha2", "data.ha3")

################## get all datasets ####
data <- list()
data[[	1 	]] = data.frame(index =	1	, country =	"Benin" , data = extract$BJPR71FL)
data[[	2 	]] = data.frame(index =	2	, country =	"BurkinaFaso", data = extract$BFPR62FL)
data[[	3 	]] = data.frame(index =	3	, country =	"Burundi", data = extract$BUPR71FL)
data[[	4 	]] = data.frame(index =	4	, country =	"Cameroon", data = extract$CMPR71FL)
data[[	5 	]] = data.frame(index =	5	, country =	"Comoros", data = extract$KMPR61FL)
data[[	6 	]] = data.frame(index =	6	, country =	"Ethiopia", data = extract$EGPR61FL)
data[[	7 	]] = data.frame(index =	7	, country =	"Gabon", data = extract$GAPR61FL)
data[[	8 	]] = data.frame(index =	8	, country =	"Gambia", data = extract$GMPR61FL)
data[[	9 	]] = data.frame(index =	9	, country =	"Ghana" , data = extract$GHPR72FL)
data[[	10 	]] = data.frame(index =	10	, country =	"Guinea", data = extract$GNPR71FL)
data[[	11 	]] = data.frame(index =	11	, country =	"Lesotho", data = extract$LSPR71FL)
data[[	12 	]] = data.frame(index =	12	, country =	"Madagascar", data = extract$MDPR51FL)
data[[	13 	]] = data.frame(index =	13	, country =	"Malawi", data = extract$MWPR7AFL)
data[[	14 	]] = data.frame(index =	14	, country =	"Mali"  , data = extract$MLPR7AFL)
data[[	15 	]] = data.frame(index =	15	, country =	"Mozambique", data = extract$MZPR62FL)
data[[	16 	]] = data.frame(index =	16	, country =	"Namibia", data = extract$NMPR61FL)
data[[	17 	]] = data.frame(index =	17	, country =	"Niger" , data = extract$NIPR61FL)
data[[	18 	]] = data.frame(index =	18	, country =	"Nigeria", data = extract$NGPR7AFL)
data[[	19 	]] = data.frame(index =	19	, country =	"Rwanda", data = extract$RWPR70FL)
data[[	20 	]] = data.frame(index =	20	, country =	"Sao Tome and Principe", data = extract$STPR51FL)
data[[	21 	]] = data.frame(index =	21	, country =	"Sierra Leone", data = extract$SLPR61FL)
data[[	22 	]] = data.frame(index =	22	, country =	"South Africa", data = extract$ZAPR71FL)
data[[	23 	]] = data.frame(index =	23	, country =	"Tanzania", data = extract$TZPR7BFL)
data[[	24 	]] = data.frame(index =	24	, country =	"Togo", data = extract$TGPR61FL)
data[[	25 	]] = data.frame(index =	25	, country =	"Uganda", data = extract$UGPR7BFL)
data[[	26 	]] = data.frame(index =	26	, country =	"Zimbabwe", data = extract$ZWPR72FL)
data[[	27 	]] = rbind(data[[	1 	]],data[[	2 	]],data[[	3 	]],data[[	4 	]],
                      data[[	5 	]],data[[	6 	]],data[[	7 	]],data[[	8 	]],
                      data[[	9 	]],data[[	10 	]],data[[	11 	]],data[[	12 	]],
                      data[[	13 	]],data[[	14 	]],data[[	15 	]],data[[	16 	]],
                      data[[	17 	]],data[[	18 	]],data[[	19 	]],data[[	20 	]],
                      data[[	21 	]],data[[	22 	]],data[[	23 	]],data[[	24 	]],
                      data[[	25 	]],data[[	26 	]])








################## metric ####
prevalence <- function(x){
  a <- data.frame("prevalence_Overall"=0, "percentage_Urban"=0, "prevalence_Urban"=0,
                  "percentage_HighEdu"=0, "prevalence_HighEdu"=0)
  a$percentage_Urban = format(round(100 * sum(x$area=="urban") / nrow(x), 2), 
                              nsmall = 2)
  a$percentage_HighEdu = format(round(100 * sum(x$edu=="higher") / nrow(x), 2), 
                                nsmall = 2)
  a$prevalence_Overall = format(round(sum(x$biclass==1)/length(x$biclass), 4), 
                                nsmall = 4)
  a$prevalence_Urban = format(round(sum(subset(x, area=="urban")$biclass==1)/length(subset(x, area=="urban")$biclass), 4),
                              nsmall = 4)
  a$prevalence_HighEdu = format(round(sum(subset(x, edu=="higher")$biclass==1)/length(subset(x, edu=="higher")$biclass), 4),
                                nsmall = 4)
  return(a)
}
f_score <- function(true, pred, beta){
  if (sum(pred==0) == 0){
    a <- data.frame("precision"="ALL 1", "recall"="ALL 1", 
                    "F_score"="ALL 1", "F_beta_score"="ALL 1", 
                    "AUC"="ALL 1")
  } else if ( sum(pred==1) == 0) {
    a <- data.frame("precision"="ALL 0", "recall"="ALL 0", 
                    "F_score"="ALL 0", "F_beta_score"="ALL 0", 
                    "AUC"="ALL 0")
  } else if ( is.error(accuracy.meas(true, pred)) == TRUE) {
    a <- data.frame("precision"="FAIL", "recall"="FAIL", 
                    "F_score"="FAIL", "F_beta_score"="FAIL", 
                    "AUC"="FAIL")
  } else{
    a <- data.frame("precision"=0, "recall"=0, "F_score"=0, "F_beta_score"=0, "AUC"=0)
    acc <- accuracy.meas(true, pred)
    a$precision <- as.numeric(acc$precision)
    a$recall <- as.numeric(acc$recall)
    a$F_score <- as.numeric(acc$F)
    a$F_beta_score <- as.numeric((1+beta^2) * (a$precision*a$recall)/(beta^2*a$precision + a$recall))
    a$AUC <- as.numeric(roc.curve(true, pred, plotit = F)$auc)
    a$precision <- as.numeric(format(round(a$precision, 4), nsmall = 4))
    a$recall <- as.numeric(format(round(a$recall, 4), nsmall = 4))
    a$F_score <- as.numeric(format(round(a$F_score, 4), nsmall = 4))
    a$F_beta_score <- as.numeric(format(round(a$F_beta_score, 4), nsmall = 4))
    a$AUC <- as.numeric(format(round(a$AUC, 4), nsmall = 4))
  }
  return(a)
}
f_score_2 <- function(true, pred, beta){
  if (sum(pred==0) == 0){
    a <- data.frame("precision"="ALL 1", "recall"="ALL 1", 
                    "F_score"="ALL 1", "F_beta_score"="ALL 1", 
                    "AUC"="ALL 1")
  } else if ( sum(pred==1) == 0) {
    a <- data.frame("precision"="ALL 0", "recall"="ALL 0", 
                    "F_score"="ALL 0", "F_beta_score"="ALL 0", 
                    "AUC"="ALL 0")
  } else if ( is.error(accuracy.meas(true, pred)) == TRUE) {
    a <- data.frame("precision"="FAIL", "recall"="FAIL", 
                    "F_score"="FAIL", "F_beta_score"="FAIL", 
                    "AUC"="FAIL")
  } else{
    a <- data.frame("precision"=0, "recall"=0, "F_score"=0, "F_beta_score"=0, "AUC"=0)
    acc <- accuracy.meas(true, pred)
    a$precision <- as.numeric(acc$precision)
    a$recall <- as.numeric(acc$recall)
    a$F_score <- as.numeric(acc$F)
    a$F_beta_score <- as.numeric((1+beta^2) * (a$precision*a$recall)/(beta^2*a$precision + a$recall))
    a$AUC <- as.numeric(roc.curve(true, pred, plotit = F)$auc)
    a$precision <- format(round(a$precision, 4), nsmall = 4)
    a$recall <- format(round(a$recall, 4), nsmall = 4)
    a$F_score <- format(round(a$F_score, 4), nsmall = 4)
    a$F_beta_score <- format(round(a$F_beta_score, 4), nsmall = 4)
    a$AUC <- format(round(a$AUC, 4), nsmall = 4)
  }
  return(a)
}

################## seperate modelling ####
result_prevalence <- result_tree <- result_logit <- result_nn  <- list()
for (i in 1:26) {
  print(i)
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
  result_prevalence[[i]] <- rbind(prevalence(analysisdata))
  
  ##  split and resample                                  ####
  set.seed(10+5*i)
  smp_size <- floor(0.75 * nrow(analysisdata))
  index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
  train <- analysisdata[index, ]
  test <- analysisdata[-index, ]
  
  while (sum(test$biclass==1) / length(test$biclass) < 0.05 | sum(test$biclass==1) / length(test$biclass) > 0.95) {
    index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
    train <- analysisdata[index, ]
    test <- analysisdata[-index, ]
  }
  
  
  set.seed(132+5*i)
  data_balanced_over <- ovun.sample(biclass ~ area + wealth + age + edu + marriage + kids + wt, 
                                    data = train, method = "over", seed = 1)$data
  set.seed(133+5*i)
  data_balanced_under <- ovun.sample(biclass ~ area + wealth + age + edu + marriage + kids + wt, 
                                     data = train, method = "under", seed = 1)$data
  set.seed(134+5*i)
  data_balanced_both <- ovun.sample(biclass ~ area + wealth + age + edu + marriage + kids + wt, 
                                    data = train, method = "both", seed = 1)$data
  set.seed(135+5*i)
  data_balanced_rose <- ROSE(biclass ~ area + wealth + age + edu + marriage + kids + wt, 
                             data = train, seed = 1)$data
  
  ## decision tree (CART)                                 ####
  print("tree")
  treeObesity <- train(biclass ~ area + wealth + age + edu + marriage + kids, 
                       data = train, method = "rpart",
                       trControl = trainControl("cv", number = 10),
                       tuneLength = 10)
  pred.treeObesity <- predict(treeObesity, newdata = test)
  
  tree.over <- train(biclass ~ area + wealth + age + edu + marriage + kids, 
                     data = data_balanced_over, method = "rpart",
                     trControl = trainControl("cv", number = 10),
                     tuneLength = 10)
  tree.under <- train(biclass ~ area + wealth + age + edu + marriage + kids, 
                      data = data_balanced_under, method = "rpart",
                      trControl = trainControl("cv", number = 10),
                      tuneLength = 10)
  tree.both <- train(biclass ~ area + wealth + age + edu + marriage + kids, 
                     data = data_balanced_both, method = "rpart",
                     trControl = trainControl("cv", number = 10),
                     tuneLength = 10)
  tree.rose <- train(biclass ~ area + wealth + age + edu + marriage + kids, 
                     data = data_balanced_rose, method = "rpart",
                     trControl = trainControl("cv", number = 10),
                     tuneLength = 10)
  
  pred.tree.over <- predict(tree.over, newdata = test)
  pred.tree.under <- predict(tree.under, newdata = test)
  pred.tree.both <- predict(tree.both, newdata = test)
  pred.tree.rose <- predict(tree.rose, newdata = test)
  
  result_tree[[i]] <- rbind(f_score(test$biclass, pred.treeObesity, 2),
                            f_score(test$biclass, pred.tree.over, 2),
                            f_score(test$biclass, pred.tree.under, 2),
                            f_score(test$biclass, pred.tree.both, 2),
                            f_score(test$biclass, pred.tree.rose, 2))
}



################## 26 together countries ####
i <- 27
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
result_prevalence[[i]] <- rbind(prevalence(analysisdata))

# cross validation ####
n <- nrow(analysisdata)
K <- 5
taille <- n %/% K

set.seed(678)
alea <- runif(n)
rang <- rank(alea)
bloc <- (rang-1) %/% taille + 1
bloc <- as.factor(bloc)
print(summary(bloc))

score.tree <- score.tree.over <- score.tree.under <- score.tree.both <- score.tree.rose <- data.frame()
score.logit <- score.logit.over <- score.logit.under <- score.logit.both <- score.logit.rose <- data.frame()
score.nn <- score.nn.over <- score.nn.under <- score.nn.both <- score.nn.rose <- data.frame()

for (k in 1:K) {
  print(k)
  train <- analysisdata[bloc!=k,]
  test <- analysisdata[bloc==k,]
  set.seed(567+12*k)
  data_balanced_over <- ovun.sample(biclass ~ country + area + wealth + age + edu + marriage + kids + wt, 
                                    data = train, method = "over", seed = 1)$data
  data_balanced_under <- ovun.sample(biclass ~ country + area + wealth + age + edu + marriage + kids + wt, 
                                     data = train, method = "under", seed = 1)$data
  data_balanced_both <- ovun.sample(biclass ~ country + area + wealth + age + edu + marriage + kids + wt, 
                                    data = train, method = "both", seed = 1)$data
  data_balanced_rose <- ROSE(biclass ~ country + area + wealth + age + edu + marriage + kids + wt, 
                             data = train, seed = 1)$data
  
  ## decision tree (CART)                                 ####
  print("tree")
  treeObesity <- train(biclass ~ area + wealth + age + edu + marriage + kids, 
                       data = train, method = "rpart",
                       trControl = trainControl("cv", number = 10),
                       tuneLength = 10)
  pred.treeObesity <- predict(treeObesity, newdata = test)
  
  tree.over <- train(biclass ~ country + area + wealth + age + edu + marriage + kids, 
                     data = data_balanced_over, method = "rpart",
                     trControl = trainControl("cv", number = 10),
                     tuneLength = 10)
  tree.under <- train(biclass ~ country + area + wealth + age + edu + marriage + kids, 
                      data = data_balanced_under, method = "rpart",
                      trControl = trainControl("cv", number = 10),
                      tuneLength = 10)
  tree.both <- train(biclass ~ country + area + wealth + age + edu + marriage + kids, 
                     data = data_balanced_both, method = "rpart",
                     trControl = trainControl("cv", number = 10),
                     tuneLength = 10)
  tree.rose <- train(biclass ~ country + area + wealth + age + edu + marriage + kids, 
                     data = data_balanced_rose, method = "rpart",
                     trControl = trainControl("cv", number = 10),
                     tuneLength = 10)
  
  pred.tree.over <- predict(tree.over, newdata = test)
  pred.tree.under <- predict(tree.under, newdata = test)
  pred.tree.both <- predict(tree.both, newdata = test)
  pred.tree.rose <- predict(tree.rose, newdata = test)
  
  score.tree <- rbind(score.tree, f_score(test$biclass, pred.treeObesity, 2))
  score.tree.over <- rbind(score.tree.over, f_score(test$biclass, pred.tree.over, 2))
  score.tree.under <- rbind(score.tree.under, f_score(test$biclass, pred.tree.under, 2))
  score.tree.both <- rbind(score.tree.both, f_score(test$biclass, pred.tree.both, 2))
  score.tree.rose <- rbind(score.tree.rose, f_score(test$biclass, pred.tree.rose, 2))
}

result_tree[[i]] <- rbind(colMeans(score.tree), colMeans(score.tree.over), colMeans(score.tree.under),
                          colMeans(score.tree.both), colMeans(score.tree.rose))




################## pretty output set up ####
country_name <- c("Benin" , "BurkinaFaso", "Burundi","Cameroon", "Comoros", "Ethiopia", "Gabon",
                  "Gambia", "Ghana" , "Guinea", "Lesotho", "Madagascar", "Malawi", "Mali"  ,
                  "Mozambique", "Namibia", "Niger" , "Nigeria", "Rwanda", 
                  "Sao Tome and Principe","Sierra Leone", "South Africa",
                  "Tanzania", "Togo", "Uganda", "Zimbabwe", "ALL 26 COUNTRIES (5-cross-validation)")
names(result_prevalence) <- country_name
method_name <- c("DECISION TREE", "SIMPLE LOGISTIC", "NEURAL NETWORK")
model_result <- rbind(result_tree, result_logit, result_nn)

sink("C:/Users/z1045/Desktop/method_treeGini_cv.txt")
print("Angola / Senegal / Zambia: no ha2(weight), no ha3(height), no ha40(BMI)")
print("Chad: no ha54 pregnancy")
print("Kenya: no hv115 marriage")
print("Liberia: all ha54 pregnancy are NA")
print("After excluding the above countries, 32 countries are reduced to 26")
print("formula: biclass ~ country + area + wealth + age + edu + marriage status + number of kids under 5")
print("working hours are missing in some countries")
print("For each method, different rows means: ")
print("no resampling --- over resampling --- under resampling --- over and under resampling (--- rose resampling)")
cat("\n","\n", "\n","\n")

for(j in 1:26){
  cat("=========================================================================================================================", "\n",
      country_name[j], country_name[j], country_name[j], country_name[j], country_name[j],
      country_name[j], country_name[j], country_name[j], country_name[j], country_name[j], "\n",
      "========================================================================================================================","\n")
  
  cat("\n", "PERCENTAGE AND PREVALENCE", "\n")
  print(result_prevalence[[j]])
  
  cat("\n", method_name[1], "\n")
  print(result_tree[[j]])
  
  cat("\n","\n", "\n","\n")
  
}

j <- 27
cat("=========================================================================================================================", "\n",
    country_name[j], country_name[j], country_name[j], country_name[j], country_name[j],
    country_name[j], country_name[j], country_name[j], country_name[j], country_name[j], "\n",
    "========================================================================================================================","\n")

cat("\n", "PERCENTAGE AND PREVALENCE", "\n")
print(result_prevalence[[j]])

cat("\n", method_name[1], "\n")
print(result_tree[[j]])

sink()




















################## end ####