levels(as.factor(data[[1]]$data.hv025)); table(as.factor(data[[1]]$data.hv025))
levels(as.factor(data[[1]]$data.hv270)); table(as.factor(data[[1]]$data.hv270))
levels(as.factor(data[[1]]$data.hv106)); table(as.factor(data[[1]]$data.hv106))
levels(as.factor(data[[1]]$data.hv115)); table(as.factor(data[[1]]$data.hv115))
str(data[[1]]$data.hv014); range(data[[1]]$data.hv014)
str(data[[1]]$data.hv009); range(data[[1]]$data.hv009)

result_prevalence_paper <- prevalence_Overall_paper <- NULL
for (i in 1:25) {
  analysisdata <- data[[i]]  %>% 
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
  
  result_prevalence_paper[[i]] <- rbind(prevalence(analysisdata))
  prevalence_Overall_paper[i] <- rbind(result_prevalence_paper[[i]]$prevalence_Overall)
  
  print(i)
  cat(" age", range(analysisdata$age))
  cat("\n kids", range(analysisdata$kids))
  cat("\n members", range(analysisdata$members))
  cat("\n")
}

range(as.numeric(result_prevalence_paper[[1]]$prevalence_Overall))
prevalence_Overall_paper; range(prevalence_Overall_paper)
