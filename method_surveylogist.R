varss <- search_variables(datasets$FileName, 
                          variables = c("ha2", "ha3", "hv025", "hv270", "ha1", "hv106", 
                                        "hv005","ha54","hv115", "hv014", "hv009", "hv021", "hv023"), 
                          reformat = TRUE)
extracts <- extract_dhs(varss, add_geo = FALSE)
cols1 <- c("data.hv025", "data.hv270", "data.hv106", "data.ha54", "data.hv115")
cols2 <- c("data.hv005", "data.ha1", "data.ha2", "data.ha3")
################## get all datasets ####
datas <- list()
datas[[	1 	]] = data.frame(index =	1	, country =	"Benin" , data = extracts$BJPR71FL)
datas[[	2 	]] = data.frame(index =	2	, country =	"BurkinaFaso", data = extracts$BFPR62FL)
datas[[	3 	]] = data.frame(index =	3	, country =	"Burundi", data = extracts$BUPR71FL)
datas[[ 4   ]] = data.frame(index =	4	, country =	"Cameroon", data = extracts$CMPR71FL)
datas[[	5 	]] = data.frame(index =	5	, country =	"Comoros", data = extracts$KMPR61FL)
datas[[	6 	]] = data.frame(index =	6	, country =	"Ethiopia", data = extracts$EGPR61FL)
datas[[	7	  ]] = data.frame(index =	7	, country =	"Ghana" , data = extracts$GHPR72FL)
datas[[	8 	]] = data.frame(index =	8	, country =	"Guinea", data = extracts$GNPR71FL)
datas[[	9 	]] = data.frame(index =	9	, country =	"Lesotho", data = extracts$LSPR71FL)
datas[[	10 	]] = data.frame(index =	10	, country =	"Madagascar", data = extracts$MDPR51FL)
datas[[	11 	]] = data.frame(index =	11	, country =	"Malawi", data = extracts$MWPR7AFL)
datas[[	12 	]] = data.frame(index =	12	, country =	"Mali"  , data = extracts$MLPR7AFL)
datas[[	13 	]] = data.frame(index =	13	, country =	"Mozambique", data = extracts$MZPR62FL)
datas[[	14 	]] = data.frame(index =	14	, country =	"Namibia", data = extracts$NMPR61FL)
datas[[	15 	]] = data.frame(index =	15	, country =	"Niger" , data = extracts$NIPR61FL)
datas[[	16 	]] = data.frame(index =	16	, country =	"Nigeria", data = extracts$NGPR7AFL)
datas[[	17 	]] = data.frame(index =	17	, country =	"Rwanda", data = extracts$RWPR70FL)
datas[[	18 	]] = data.frame(index =	18	, country =	"Sao Tome and Principe", data = extracts$STPR51FL)
datas[[	19 	]] = data.frame(index =	19	, country =	"Sierra Leone", data = extracts$SLPR61FL)
datas[[	20 	]] = data.frame(index =	20	, country =	"South Africa", data = extracts$ZAPR71FL)
datas[[	21 	]] = data.frame(index =	21	, country =	"Tanzania", data = extracts$TZPR7BFL)
datas[[	22 	]] = data.frame(index =	22	, country =	"Togo", data = extracts$TGPR61FL)
datas[[	23 	]] = data.frame(index =	23	, country =	"Uganda", data = extracts$UGPR7BFL)
datas[[	24 	]] = data.frame(index =	24	, country =	"Zimbabwe", data = extracts$ZWPR72FL)






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
    a <- data.frame("precision"=1, "recall"=1, 
                    "F_score"=1, "F_beta_score"=1, 
                    "AUC"=1)
  } else if ( sum(pred==1) == 0) {
    a <- data.frame("precision"=0, "recall"=0, 
                    "F_score"=0, "F_beta_score"=0, 
                    "AUC"=0)
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
    a <- data.frame("precision"=1, "recall"=1, 
                    "F_score"=1, "F_beta_score"=1, 
                    "AUC"=1)
  } else if ( sum(pred==1) == 0) {
    a <- data.frame("precision"=0, "recall"=0, 
                    "F_score"=0, "F_beta_score"=0, 
                    "AUC"=0)
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

################## modelling ####
result_prevalence <- result_survlogit <-   list()

for (i in 1:24) {
  print(i)
  # preprocessing ####
  analysisdata <- datas[[i]]  %>% 
    subset(data.ha54 == "no/don't know") %>%
    subset(data.hv106 != "don't know") %>%
    subset(data.hv106 != "missing") %>% 
    subset(data.hv106 != "dk") %>%
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
  #%%% result_prevalence[[i]] <- rbind(datas[[i]]$country[1], prevalence(analysisdata))
  result_prevalence[[i]] <- rbind(prevalence(analysisdata))
  
  ##  split and resample                                  ####
  set.seed(10+i)
  smp_size <- floor(0.75 * nrow(analysisdata))
  index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
  training <- analysisdata[index, ]
  testing <- analysisdata[-index, ]
  
  while (sum(testing$biclass==1) / length(testing$biclass) < 0.05 | sum(testing$biclass==1) / length(testing$biclass) > 0.95) {
    index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
    training <- analysisdata[index, ]
    testing <- analysisdata[-index, ]
  }
  
  # over sampling
  set.seed(112+i)
  data_balanced_over <- ovun.sample(biclass ~ area + wealth + age + edu
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "over",
                                    seed = 1)$data
  # under sampling
  set.seed(113+i)
  data_balanced_under <- ovun.sample(biclass ~ area + wealth + age + edu 
                                     + marriage + kids +  data.hv021 + data.hv023 + wt 
                                     + pregnancy + BMI, 
                                     data = training, 
                                     method = "under",
                                     seed = 1)$data
  
  # under and over sampling 
  set.seed(114+i)
  data_balanced_both <- ovun.sample(biclass ~ area + wealth + age + edu 
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "both",
                                    seed = 1)$data
  # rose sampling
  set.seed(135+5*i)
  training$data.hv023 <- as.factor(training$data.hv023)
  data_balanced_rose <- ROSE(biclass ~ area + wealth + age + edu 
                             + marriage + kids + data.hv021 + data.hv023 + wt, 
                             data = training, seed = 1)$data
  
  ##  survey logistic biclass                             ####
  print("survey logistic")
  set.seed(120+i)
  design1 <- svydesign(id = ~training$data.hv021, 
                       strata = ~training$data.hv023, 
                       weights = ~training$wt, 
                       data = training)
  model_surv1 <- svyglm(biclass ~ area + wealth + age + edu + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = training,
                        design = design1)
  
  fitted_prob1 <- predict(model_surv1, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class1 <- ifelse(fitted_prob1 < 0.5, 0, 1)
  
  set.seed(121+i)
  design2 <- svydesign(id = ~data_balanced_over$data.hv021, 
                       strata = ~data_balanced_over$data.hv023, 
                       weights = ~data_balanced_over$wt, 
                       data = data_balanced_over)
  model_surv2 <- svyglm(biclass ~ area + wealth + age + edu
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_over,
                        design = design2)
  
  fitted_prob2 <- predict(model_surv2, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class2 <- ifelse(fitted_prob2 < 0.5, 0, 1)
  
  set.seed(122+i)
  design3 <- svydesign(id = ~data_balanced_under$data.hv021, 
                       strata = ~data_balanced_under$data.hv023, 
                       weights = ~data_balanced_under$wt, 
                       data = data_balanced_under)
  model_surv3 <- svyglm(biclass ~ area + wealth + age + edu  
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_under,
                        design = design3)
  
  fitted_prob3 <- predict(model_surv3, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class3 <- ifelse(fitted_prob3 < 0.5, 0, 1)
  
  set.seed(123+i)
  design4 <- svydesign(id = ~data_balanced_both$data.hv021, 
                       strata = ~data_balanced_both$data.hv023, 
                       weights = ~data_balanced_both$wt, 
                       data = data_balanced_both)
  model_surv4 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_both,
                        design = design4)
  
  fitted_prob4 <- predict(model_surv4, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class4 <- ifelse(fitted_prob4 < 0.5, 0, 1)
  
  set.seed(124+i)
  design5 <- svydesign(id = ~data_balanced_rose$data.hv021, 
                       strata = ~data_balanced_rose$data.hv023, 
                       weights = ~data_balanced_rose$wt, 
                       data = data_balanced_rose)
  model_surv5 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_rose,
                        design = design5)
  
  fitted_prob5 <- predict(model_surv5, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class5 <- ifelse(fitted_prob5 < 0.5, 0, 1)
  
  # results
  result_survlogit[[i]] <- rbind(f_score(testing$biclass, fitted_class1, 2), 
                                 f_score(testing$biclass, fitted_class2, 2), 
                                 f_score(testing$biclass, fitted_class3, 2), 
                                 f_score(testing$biclass, fitted_class4, 2),
                                 f_score(testing$biclass, fitted_class5, 2))
}

# preprocessing ####
analysisdata <- datas[[i]]  %>% 
  subset(data.ha54 == "no/don't know") %>%
  subset(data.hv106 != "don't know") %>%
  subset(data.hv106 != "missing") %>% 
  subset(data.hv106 != "dk") %>%
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
#%%% result_prevalence[[i]] <- rbind(datas[[i]]$country[1], prevalence(analysisdata))
result_prevalence[[i]] <- rbind(prevalence(analysisdata))

##  split and resample                                  ####
set.seed(10+i)
smp_size <- floor(0.75 * nrow(analysisdata))
index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
training <- analysisdata[index, ]
testing <- analysisdata[-index, ]

while (sum(testing$biclass==1) / length(testing$biclass) < 0.05 | sum(testing$biclass==1) / length(testing$biclass) > 0.95) {
  index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
  training <- analysisdata[index, ]
  testing <- analysisdata[-index, ]
}

# over sampling
set.seed(112+i)
data_balanced_over <- ovun.sample(biclass ~ area + wealth + age + edu
                                  + marriage + kids + data.hv021 + data.hv023 + wt 
                                  + pregnancy + BMI, 
                                  data = training, 
                                  method = "over",
                                  seed = 1)$data
# under sampling
set.seed(113+i)
data_balanced_under <- ovun.sample(biclass ~ area + wealth + age + edu 
                                   + marriage + kids +  data.hv021 + data.hv023 + wt 
                                   + pregnancy + BMI, 
                                   data = training, 
                                   method = "under",
                                   seed = 1)$data

# under and over sampling 
set.seed(114+i)
data_balanced_both <- ovun.sample(biclass ~ area + wealth + age + edu 
                                  + marriage + kids + data.hv021 + data.hv023 + wt 
                                  + pregnancy + BMI, 
                                  data = training, 
                                  method = "both",
                                  seed = 1)$data
# rose sampling
set.seed(135+5*i)
training$data.hv023 <- as.factor(training$data.hv023)
data_balanced_rose <- ROSE(biclass ~ area + wealth + age + edu 
                           + marriage + kids + data.hv021 + data.hv023 + wt, 
                           data = training, seed = 1)$data

##  survey logistic biclass                             ####
print("survey logistic")
set.seed(120+i)
design1 <- svydesign(id = ~training$data.hv021, 
                     strata = ~training$data.hv023, 
                     weights = ~training$wt, 
                     data = training)
model_surv1 <- svyglm(biclass ~ area + wealth + age + edu + marriage + kids,
                      family = quasibinomial(),
                      weights = wt,
                      data = training,
                      design = design1)

fitted_prob1 <- predict(model_surv1, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
fitted_class1 <- ifelse(fitted_prob1 < 0.5, 0, 1)

set.seed(121+i)
design2 <- svydesign(id = ~data_balanced_over$data.hv021, 
                     strata = ~data_balanced_over$data.hv023, 
                     weights = ~data_balanced_over$wt, 
                     data = data_balanced_over)
model_surv2 <- svyglm(biclass ~ area + wealth + age + edu
                      + marriage + kids,
                      family = quasibinomial(),
                      weights = wt,
                      data = data_balanced_over,
                      design = design2)

fitted_prob2 <- predict(model_surv2, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
fitted_class2 <- ifelse(fitted_prob2 < 0.5, 0, 1)


# results
result_survlogit[[i]] <- rbind(f_score(testing$biclass, fitted_class1, 2), 
                               f_score(testing$biclass, fitted_class2, 2))



# new loop ####
for (i in c(10,20)) {
  print(i)
  # preprocessing ####
  analysisdata <- datas[[i]]  %>% 
    subset(data.ha54 == "no/don't know") %>%
    subset(data.hv106 != "don't know") %>%
    subset(data.hv106 != "missing") %>% 
    subset(data.hv106 != "dk") %>%
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
  #%%% result_prevalence[[i]] <- rbind(datas[[i]]$country[1], prevalence(analysisdata))
  result_prevalence[[i]] <- rbind(prevalence(analysisdata))
  
  ##  split and resample                                  ####
  set.seed(10+i)
  smp_size <- floor(0.75 * nrow(analysisdata))
  index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
  training <- analysisdata[index, ]
  testing <- analysisdata[-index, ]
  
  while (sum(testing$biclass==1) / length(testing$biclass) < 0.05 | sum(testing$biclass==1) / length(testing$biclass) > 0.95) {
    index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
    training <- analysisdata[index, ]
    testing <- analysisdata[-index, ]
  }
  
  # over sampling
  set.seed(112+i)
  data_balanced_over <- ovun.sample(biclass ~ area + wealth + age + edu
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "over",
                                    seed = 1)$data

  
  # under and over sampling 
  set.seed(114+i)
  data_balanced_both <- ovun.sample(biclass ~ area + wealth + age + edu 
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "both",
                                    seed = 1)$data
  # rose sampling
  set.seed(135+5*i)
  training$data.hv023 <- as.factor(training$data.hv023)
  data_balanced_rose <- ROSE(biclass ~ area + wealth + age + edu 
                             + marriage + kids + data.hv021 + data.hv023 + wt, 
                             data = training, seed = 1)$data
  
  ##  survey logistic biclass                             ####
  print("survey logistic")
  set.seed(120+i)
  design1 <- svydesign(id = ~training$data.hv021, 
                       strata = ~training$data.hv023, 
                       weights = ~training$wt, 
                       data = training)
  model_surv1 <- svyglm(biclass ~ area + wealth + age + edu + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = training,
                        design = design1)
  
  fitted_prob1 <- predict(model_surv1, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class1 <- ifelse(fitted_prob1 < 0.5, 0, 1)
  
  set.seed(121+i)
  design2 <- svydesign(id = ~data_balanced_over$data.hv021, 
                       strata = ~data_balanced_over$data.hv023, 
                       weights = ~data_balanced_over$wt, 
                       data = data_balanced_over)
  model_surv2 <- svyglm(biclass ~ area + wealth + age + edu
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_over,
                        design = design2)
  
  fitted_prob2 <- predict(model_surv2, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class2 <- ifelse(fitted_prob2 < 0.5, 0, 1)
  
  set.seed(123+i)
  design4 <- svydesign(id = ~data_balanced_both$data.hv021, 
                       strata = ~data_balanced_both$data.hv023, 
                       weights = ~data_balanced_both$wt, 
                       data = data_balanced_both)
  model_surv4 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_both,
                        design = design4)
  
  fitted_prob4 <- predict(model_surv4, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class4 <- ifelse(fitted_prob4 < 0.5, 0, 1)
  
  set.seed(124+i)
  design5 <- svydesign(id = ~data_balanced_rose$data.hv021, 
                       strata = ~data_balanced_rose$data.hv023, 
                       weights = ~data_balanced_rose$wt, 
                       data = data_balanced_rose)
  model_surv5 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_rose,
                        design = design5)
  
  fitted_prob5 <- predict(model_surv5, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class5 <- ifelse(fitted_prob5 < 0.5, 0, 1)
  
  # results
  result_survlogit[[i]] <- rbind(f_score(testing$biclass, fitted_class1, 2), 
                                 f_score(testing$biclass, fitted_class2, 2), 
                                 f_score(testing$biclass, fitted_class4, 2),
                                 f_score(testing$biclass, fitted_class5, 2))
  
  
  
}
for (i in 11:19) {
  print(i)
  # preprocessing ####
  analysisdata <- datas[[i]]  %>% 
    subset(data.ha54 == "no/don't know") %>%
    subset(data.hv106 != "don't know") %>%
    subset(data.hv106 != "missing") %>% 
    subset(data.hv106 != "dk") %>%
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
  #%%% result_prevalence[[i]] <- rbind(datas[[i]]$country[1], prevalence(analysisdata))
  result_prevalence[[i]] <- rbind(prevalence(analysisdata))
  
  ##  split and resample                                  ####
  set.seed(10+i)
  smp_size <- floor(0.75 * nrow(analysisdata))
  index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
  training <- analysisdata[index, ]
  testing <- analysisdata[-index, ]
  
  while (sum(testing$biclass==1) / length(testing$biclass) < 0.05 | sum(testing$biclass==1) / length(testing$biclass) > 0.95) {
    index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
    training <- analysisdata[index, ]
    testing <- analysisdata[-index, ]
  }
  
  # over sampling
  set.seed(112+i)
  data_balanced_over <- ovun.sample(biclass ~ area + wealth + age + edu
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "over",
                                    seed = 1)$data
  # under sampling
  set.seed(113+i)
  data_balanced_under <- ovun.sample(biclass ~ area + wealth + age + edu 
                                     + marriage + kids +  data.hv021 + data.hv023 + wt 
                                     + pregnancy + BMI, 
                                     data = training, 
                                     method = "under",
                                     seed = 1)$data
  
  # under and over sampling 
  set.seed(114+i)
  data_balanced_both <- ovun.sample(biclass ~ area + wealth + age + edu 
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "both",
                                    seed = 1)$data
  # rose sampling
  set.seed(135+5*i)
  training$data.hv023 <- as.factor(training$data.hv023)
  data_balanced_rose <- ROSE(biclass ~ area + wealth + age + edu 
                             + marriage + kids + data.hv021 + data.hv023 + wt, 
                             data = training, seed = 1)$data
  
  ##  survey logistic biclass                             ####
  print("survey logistic")
  set.seed(120+i)
  design1 <- svydesign(id = ~training$data.hv021, 
                       strata = ~training$data.hv023, 
                       weights = ~training$wt, 
                       data = training)
  model_surv1 <- svyglm(biclass ~ area + wealth + age + edu + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = training,
                        design = design1)
  
  fitted_prob1 <- predict(model_surv1, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class1 <- ifelse(fitted_prob1 < 0.5, 0, 1)
  
  set.seed(121+i)
  design2 <- svydesign(id = ~data_balanced_over$data.hv021, 
                       strata = ~data_balanced_over$data.hv023, 
                       weights = ~data_balanced_over$wt, 
                       data = data_balanced_over)
  model_surv2 <- svyglm(biclass ~ area + wealth + age + edu
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_over,
                        design = design2)
  
  fitted_prob2 <- predict(model_surv2, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class2 <- ifelse(fitted_prob2 < 0.5, 0, 1)
  
  set.seed(122+i)
  design3 <- svydesign(id = ~data_balanced_under$data.hv021, 
                       strata = ~data_balanced_under$data.hv023, 
                       weights = ~data_balanced_under$wt, 
                       data = data_balanced_under)
  model_surv3 <- svyglm(biclass ~ area + wealth + age + edu  
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_under,
                        design = design3)
  
  fitted_prob3 <- predict(model_surv3, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class3 <- ifelse(fitted_prob3 < 0.5, 0, 1)
  
  set.seed(123+i)
  design4 <- svydesign(id = ~data_balanced_both$data.hv021, 
                       strata = ~data_balanced_both$data.hv023, 
                       weights = ~data_balanced_both$wt, 
                       data = data_balanced_both)
  model_surv4 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_both,
                        design = design4)
  
  fitted_prob4 <- predict(model_surv4, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class4 <- ifelse(fitted_prob4 < 0.5, 0, 1)
  
  set.seed(124+i)
  design5 <- svydesign(id = ~data_balanced_rose$data.hv021, 
                       strata = ~data_balanced_rose$data.hv023, 
                       weights = ~data_balanced_rose$wt, 
                       data = data_balanced_rose)
  model_surv5 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_rose,
                        design = design5)
  
  fitted_prob5 <- predict(model_surv5, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class5 <- ifelse(fitted_prob5 < 0.5, 0, 1)
  
  # results
  result_survlogit[[i]] <- rbind(f_score(testing$biclass, fitted_class1, 2), 
                                 f_score(testing$biclass, fitted_class2, 2), 
                                 f_score(testing$biclass, fitted_class3, 2), 
                                 f_score(testing$biclass, fitted_class4, 2),
                                 f_score(testing$biclass, fitted_class5, 2))
}
for (i in 22:24) {
  print(i)
  # preprocessing ####
  analysisdata <- datas[[i]]  %>% 
    subset(data.ha54 == "no/don't know") %>%
    subset(data.hv106 != "don't know") %>%
    subset(data.hv106 != "missing") %>% 
    subset(data.hv106 != "dk") %>%
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
  #%%% result_prevalence[[i]] <- rbind(datas[[i]]$country[1], prevalence(analysisdata))
  result_prevalence[[i]] <- rbind(prevalence(analysisdata))
  
  ##  split and resample                                  ####
  set.seed(10+i)
  smp_size <- floor(0.75 * nrow(analysisdata))
  index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
  training <- analysisdata[index, ]
  testing <- analysisdata[-index, ]
  
  while (sum(testing$biclass==1) / length(testing$biclass) < 0.05 | sum(testing$biclass==1) / length(testing$biclass) > 0.95) {
    index <- sample(seq_len(nrow(analysisdata)), size = smp_size)
    training <- analysisdata[index, ]
    testing <- analysisdata[-index, ]
  }
  
  # over sampling
  set.seed(112+i)
  data_balanced_over <- ovun.sample(biclass ~ area + wealth + age + edu
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "over",
                                    seed = 1)$data
  # under sampling
  set.seed(113+i)
  data_balanced_under <- ovun.sample(biclass ~ area + wealth + age + edu 
                                     + marriage + kids +  data.hv021 + data.hv023 + wt 
                                     + pregnancy + BMI, 
                                     data = training, 
                                     method = "under",
                                     seed = 1)$data
  
  # under and over sampling 
  set.seed(114+i)
  data_balanced_both <- ovun.sample(biclass ~ area + wealth + age + edu 
                                    + marriage + kids + data.hv021 + data.hv023 + wt 
                                    + pregnancy + BMI, 
                                    data = training, 
                                    method = "both",
                                    seed = 1)$data
  # rose sampling
  set.seed(135+5*i)
  training$data.hv023 <- as.factor(training$data.hv023)
  data_balanced_rose <- ROSE(biclass ~ area + wealth + age + edu 
                             + marriage + kids + data.hv021 + data.hv023 + wt, 
                             data = training, seed = 1)$data
  
  ##  survey logistic biclass                             ####
  print("survey logistic")
  set.seed(120+i)
  design1 <- svydesign(id = ~training$data.hv021, 
                       strata = ~training$data.hv023, 
                       weights = ~training$wt, 
                       data = training)
  model_surv1 <- svyglm(biclass ~ area + wealth + age + edu + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = training,
                        design = design1)
  
  fitted_prob1 <- predict(model_surv1, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class1 <- ifelse(fitted_prob1 < 0.5, 0, 1)
  
  set.seed(121+i)
  design2 <- svydesign(id = ~data_balanced_over$data.hv021, 
                       strata = ~data_balanced_over$data.hv023, 
                       weights = ~data_balanced_over$wt, 
                       data = data_balanced_over)
  model_surv2 <- svyglm(biclass ~ area + wealth + age + edu
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_over,
                        design = design2)
  
  fitted_prob2 <- predict(model_surv2, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class2 <- ifelse(fitted_prob2 < 0.5, 0, 1)
  
  set.seed(122+i)
  design3 <- svydesign(id = ~data_balanced_under$data.hv021, 
                       strata = ~data_balanced_under$data.hv023, 
                       weights = ~data_balanced_under$wt, 
                       data = data_balanced_under)
  model_surv3 <- svyglm(biclass ~ area + wealth + age + edu  
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_under,
                        design = design3)
  
  fitted_prob3 <- predict(model_surv3, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class3 <- ifelse(fitted_prob3 < 0.5, 0, 1)
  
  set.seed(123+i)
  design4 <- svydesign(id = ~data_balanced_both$data.hv021, 
                       strata = ~data_balanced_both$data.hv023, 
                       weights = ~data_balanced_both$wt, 
                       data = data_balanced_both)
  model_surv4 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_both,
                        design = design4)
  
  fitted_prob4 <- predict(model_surv4, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class4 <- ifelse(fitted_prob4 < 0.5, 0, 1)
  
  set.seed(124+i)
  design5 <- svydesign(id = ~data_balanced_rose$data.hv021, 
                       strata = ~data_balanced_rose$data.hv023, 
                       weights = ~data_balanced_rose$wt, 
                       data = data_balanced_rose)
  model_surv5 <- svyglm(biclass ~ area + wealth + age + edu 
                        + marriage + kids,
                        family = quasibinomial(),
                        weights = wt,
                        data = data_balanced_rose,
                        design = design5)
  
  fitted_prob5 <- predict(model_surv5, newdata = subset(testing, select = c(5:8,11:12), type = 'response'))
  fitted_class5 <- ifelse(fitted_prob5 < 0.5, 0, 1)
  
  # results
  result_survlogit[[i]] <- rbind(f_score(testing$biclass, fitted_class1, 2), 
                                 f_score(testing$biclass, fitted_class2, 2), 
                                 f_score(testing$biclass, fitted_class3, 2), 
                                 f_score(testing$biclass, fitted_class4, 2),
                                 f_score(testing$biclass, fitted_class5, 2))
}

################## pretty output set up ####
country_name <- c("Benin" , "BurkinaFaso", "Burundi","Cameroon", "Comoros", "Ethiopia", 
                  "Ghana" , "Guinea", "Lesotho", "Madagascar original and over", "Malawi", "Mali"  ,
                  "Mozambique", "Namibia", "Niger" , "Nigeria", "Rwanda", 
                  "Sao Tome and Principe","Sierra Leone", "South Africa no under",
                  "Tanzania no under", "Togo", "Uganda", "Zimbabwe")
names(result_prevalence) <- country_name


sink("C:/Users/z1045/Desktop/method_survlogistic.txt")
print("Total 32 countries")
print("25 after excluding those with missing columns")
print("19 after excluding stage I problem in survey logistic")
print("formula: biclass ~ area + wealth + age + edu + marriage status + number of kids under 5")
print("working hours are missing in some countries")
print("For each method, different rows means: ")
print("no resampling --- over resampling --- under resampling --- over and under resampling (--- rose resampling)")
cat("\n","\n", "\n","\n")
for (i in 1:length(result_prevalence)) {
  cat("=========================================================================================================================", "\n",
      country_name[i], country_name[i], country_name[i], country_name[i], country_name[i],
      country_name[i], country_name[i], country_name[i], country_name[i], country_name[i], 
      country_name[i], country_name[i], country_name[i], country_name[i], country_name[i], "\n",
      "========================================================================================================================","\n")
  
  cat("\n", "PERCENTAGE AND PREVALENCE", "\n")
  print(result_prevalence[[i]])
  
  cat("\n", "survey logistic", "\n")
  print(result_survlogit[[i]])
  
  
  cat("\n","\n", "\n","\n")
}
sink()


