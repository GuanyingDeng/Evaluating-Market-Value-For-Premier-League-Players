################## Defenders ########################
library(readxl)
library(dplyr)
library(randomForest)
library(rlist)
Defenders <- read_excel("Data_ML_Final_Project.xlsx", sheet = "Defenders")
#View(Defenders)

Defenders<-subset(Defenders,select = -c(player_name))
Defenders$age_squared=(Defenders$age)^2
Defenders$age_factor<-as.factor(Defenders$age)
Defenders$clean_sheet_per_100<-Defenders$perc_clean_Sheets*100
Defenders$age_flag<-ifelse(Defenders$age>23,0,1)
Defenders$age_mod<-Defenders$age-23

colnames(Defenders)
View(cor(Defenders))

Defenders%>%group_by(England_flag)%>%summarise(mean_mv=mean(market_value_in_millions))
Defenders%>%group_by(top_club_flag)%>%summarise(mean_mv=mean(market_value_in_millions))

model_df_explanability<-lm(market_value_in_millions~age_flag*age_mod+top_club_flag+appearances+England_flag+team_passes_per_match+clean_sheet_per_100+recoveries_per_game,data=Defenders)
summary(model_df_explanability)

############## Prediction ###########################

########### Regression ##################
regrsq <-c()
i=1

while(i<2000 )  {
set.seed(i)
train <- sample(nrow(Defenders), 0.65*nrow(Defenders), replace = FALSE)
TrainSet <- Defenders[train,]
ValidSet <- Defenders[-train,]

model_df_prediction<-lm(market_value_in_millions~age+age_squared+appearances+England_flag+team_passes_per_match+clean_sheet_per_100+recoveries_per_game,data=TrainSet)
summary(model_df_prediction)

# Predicting on Validation set
test.pred<- predict(model_df_prediction, ValidSet)
# Checking classification accuracy
test.y<-ValidSet$market_value_in_millions

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
test.rsq <- as.vector(1 - (SS.residual/SS.total))
regrsq<-c(regrsq,test.rsq)
i=i+1
}
mean(regrsq)
sd(regrsq)

########## Random Forests ######################
# train the model
rfrsq <-c()
i=1

while(i<50 )  {
  set.seed(i)
  train <- sample(nrow(Defenders), 0.65*nrow(Defenders), replace = FALSE)
  TrainSet <- Defenders[train,]
  ValidSet <- Defenders[-train,]
rf_defender <- randomForest(market_value_in_millions ~age+age_squared+appearances+England_flag+team_passes_per_match+clean_sheet_per_100+recoveries_per_game, data = TrainSet,ntree=500,importance = TRUE)
# Predicting on Validation set
test.pred<- predict(rf_defender, ValidSet)
# Checking classification accuracy
test.y<-ValidSet$market_value_in_millions

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
test.rsq <- 1 - (SS.residual/SS.total)  
rfrsq<-c(rfrsq,test.rsq)
i=i+1

}
rf_defender
mean(rf_defender$rsq)
mean(rfrsq)
sd(rfrsq)

############### Midfielders ######################

Midfielders <- read_excel("Data_ML_Final_Project.xlsx", sheet = "Midfielders")
#View(Midfielders)
Midfielders<-subset(Midfielders,select = -c(player_name))
Midfielders$age_squared=(Midfielders$age)^2
Midfielders$age_flag<-ifelse(Midfielders$age>23,0,1)
Midfielders$age_mod<-Midfielders$age-23
Midfielders$goals_per_100<-Midfielders$atk_goals_per_match*100
Midfielders$wins_per_100<-Midfielders$win_perc*100
Midfielders$assists_per_100<-Midfielders$assists_per_game*100

Midfielders%>%group_by(England_flag)%>%summarise(mean_mv=mean(market_value_in_millions))
Midfielders%>%group_by(top_club_flag)%>%summarise(mean_mv=mean(market_value_in_millions))

model_mf_explanability<-lm(market_value_in_millions~age_mod*age_flag+assists_per_100+passes_per_game+wins_per_100+England_flag+top_club_flag+goals_per_100+dfc_successful_per_game,data=Midfielders)
summary(model_mf_explanability)

############## Prediction ###########################

########### Regression ##################
regrsq <-c()
i=1

while(i<2000 )  {
set.seed(i)
train <- sample(nrow(Midfielders), 0.65*nrow(Midfielders), replace = FALSE)
TrainSet <- Midfielders[train,]
ValidSet <- Midfielders[-train,]

model_mf_prediction<-lm(market_value_in_millions~age+age_squared+assists_per_100+passes_per_game+wins_per_100+England_flag+top_club_flag+goals_per_100+dfc_successful_per_game,data=TrainSet)
summary(model_mf_prediction)

# Predicting on Validation set
test.pred<- predict(model_mf_prediction, ValidSet)
# Checking classification accuracy
test.y<-ValidSet$market_value_in_millions

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
test.rsq <- as.vector(1 - (SS.residual/SS.total))
regrsq<-c(regrsq,test.rsq)
i=i+1
}
mean(regrsq)
sd(regrsq)

########## Random Forests ######################
# train the model
rfrsq <-c()
i=1

while(i<50 )  {
  set.seed(i)
  train <- sample(nrow(Midfielders), 0.65*nrow(Midfielders), replace = FALSE)
  TrainSet <- Midfielders[train,]
  ValidSet <- Midfielders[-train,]
  rf_midfielder <- randomForest(market_value_in_millions~age+age_squared+assists_per_100+passes_per_game+wins_per_100+England_flag+top_club_flag+goals_per_100+dfc_successful_per_game,data=TrainSet,ntree=500,importance = TRUE)
  
  # Predicting on Validation set
  test.pred<- predict(rf_midfielder, ValidSet)
  # Checking classification accuracy
  test.y<-ValidSet$market_value_in_millions
  
  SS.total      <- sum((test.y - mean(test.y))^2)
  SS.residual   <- sum((test.y - test.pred)^2)
  test.rsq <- 1 - (SS.residual/SS.total)  
  rfrsq<-c(rfrsq,test.rsq)
  i=i+1
  
}

rf_midfielder
mean(rf_midfielder$rsq)
mean(rfrsq)
sd(rfrsq)


############## Forwards #########################

Forwards <- read_excel("Data_ML_Final_Project.xlsx", sheet = "Forwards")
#View(Forwards)
Forwards<-subset(Forwards,select = -c(player_name))
Forwards$age_squared=(Forwards$age)^2
Forwards$age_mod<-Forwards$age-25
Forwards$age_flag<-ifelse(Forwards$age>25,0,1)
Forwards$goals_per_100<-Forwards$atk_goals_per_match*100
Forwards$wins_per_100<-Forwards$win_perc*100
Forwards$passes_per_game<-Forwards$team_passes_per_match
Forwards$assists_per_100<-Forwards$assists_per_game*100

Forwards%>%group_by(England_flag)%>%summarise(mean_mv=mean(market_value_in_millions))
Forwards%>%group_by(top_club_flag)%>%summarise(mean_mv=mean(market_value_in_millions))

model_f_explanability<-lm(market_value_in_millions~age_mod*age_flag+assists_per_100+passes_per_game+England_flag+top_club_flag+goals_per_100,data=Forwards)
summary(model_f_explanability)

############## Prediction ###########################

########### Regression ##################

regrsq <-c()
i=1

while(i<2000 )  {
  set.seed(i)
  train <- sample(nrow(Forwards), 0.65*nrow(Forwards), replace = FALSE)
  TrainSet <- Forwards[train,]
  ValidSet <- Forwards[-train,]
  
  model_f_prediction<-lm(market_value_in_millions~age+age_squared+assists_per_100+passes_per_game+England_flag+top_club_flag+goals_per_100,data=TrainSet)
  summary(model_f_prediction)
  
  # Predicting on Validation set
  test.pred<- predict(model_f_prediction, ValidSet)
  # Checking classification accuracy
  test.y<-ValidSet$market_value_in_millions
  
  SS.total      <- sum((test.y - mean(test.y))^2)
  SS.residual   <- sum((test.y - test.pred)^2)
  test.rsq <- as.vector(1 - (SS.residual/SS.total))
  regrsq<-c(regrsq,test.rsq)
  i=i+1
}
mean(regrsq)
sd(regrsq)

########## Random Forests ######################
# train the model
rfrsq <-c()
i=1

while(i<50 )  {
  set.seed(i)
  train <- sample(nrow(Forwards), 0.65*nrow(Forwards), replace = FALSE)
  TrainSet <- Forwards[train,]
  ValidSet <- Forwards[-train,]
  rf_forward <- randomForest(market_value_in_millions~age+age_squared+assists_per_100+passes_per_game+England_flag+top_club_flag+goals_per_100,data=TrainSet,ntree=500,importance = TRUE)
  
  # Predicting on Validation set
  test.pred<- predict(rf_forward, ValidSet)
  # Checking classification accuracy
  test.y<-ValidSet$market_value_in_millions
  
  SS.total      <- sum((test.y - mean(test.y))^2)
  SS.residual   <- sum((test.y - test.pred)^2)
  test.rsq <- 1 - (SS.residual/SS.total)  
  rfrsq<-c(rfrsq,test.rsq)
  i=i+1
  
}
mean(rf_forward$rsq)
mean(rfrsq)
sd(rfrsq)



########## Keepers ############################

Keepers<- read_excel("Data_ML_Final_Project.xlsx", sheet = "Keepers")
Keepers<-subset(Keepers,select = -c(player_name))
Keepers$age_squared=(Keepers$age)^2
Keepers$age_mod<-Keepers$age-26
Keepers$age_flag<-ifelse(Keepers$age>26,0,1)
Keepers%>%group_by(England_flag)%>%summarise(mean_mv=mean(market_value_in_millions))
Keepers%>%group_by(top_club_flag)%>%summarise(mean_mv=mean(market_value_in_millions))
Keepers$clean_sheet_per_100<-Keepers$perc_clean_sheets*100
Keepers$wins_per_100<-Keepers$win_perc*100

model_k_explanability<-lm(market_value_in_millions~age+wins_per_100+clean_sheet_per_100+England_flag+top_club_flag,data=Keepers)
summary(model_k_explanability)

############## Prediction ###########################

########### Regression ##################

regrsq <-c()
i=1

while(i<2000 )  {
  set.seed(i)
  train <- sample(nrow(Keepers), 0.65*nrow(Keepers), replace = FALSE)
  TrainSet <- Keepers[train,]
  ValidSet <- Keepers[-train,]
  
  model_k_prediction<-lm(market_value_in_millions~age+age_squared+wins_per_100+clean_sheet_per_100+England_flag+top_club_flag,data=TrainSet)
  summary(model_k_prediction)
  
  # Predicting on Validation set
  test.pred<- predict(model_k_prediction, ValidSet)
  # Checking classification accuracy
  test.y<-ValidSet$market_value_in_millions
  
  SS.total      <- sum((test.y - mean(test.y))^2)
  SS.residual   <- sum((test.y - test.pred)^2)
  test.rsq <- as.vector(1 - (SS.residual/SS.total))
  regrsq<-c(regrsq,test.rsq)
  i=i+1
}
mean(regrsq)
sd(regrsq)

########## Random Forests ######################
# train the model
rfrsq <-c()
i=1

while(i<50 )  {
  set.seed(i)
  train <- sample(nrow(Keepers), 0.65*nrow(Keepers), replace = FALSE)
  TrainSet <- Keepers[train,]
  ValidSet <- Keepers[-train,]
  rf_keeper <- randomForest(market_value_in_millions~age+age_squared+wins_per_100+clean_sheet_per_100+England_flag+top_club_flag,data=TrainSet,ntree=500,importance = TRUE)
  
  # Predicting on Validation set
  test.pred<- predict(rf_keeper, ValidSet)
  # Checking classification accuracy
  test.y<-ValidSet$market_value_in_millions
  
  SS.total      <- sum((test.y - mean(test.y))^2)
  SS.residual   <- sum((test.y - test.pred)^2)
  test.rsq <- 1 - (SS.residual/SS.total)  
  rfrsq<-c(rfrsq,test.rsq)
  i=i+1
  
}
mean(rf_keeper$rsq)
mean(rfrsq)
sd(rfrsq)
