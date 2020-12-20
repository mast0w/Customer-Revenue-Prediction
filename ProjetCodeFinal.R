#---------------Lecture des données-------------
setwd("~/Desktop/Cours/S9/Apprentissage/Projet")
require(jsonlite) 
library(data.table)
#lecture des donnees
train<-read.csv("train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 

#creation d'une colonne indicatrice train test avant assemblage des deux tables
train$datasplit<-"train"

# suppression d'une colonne visiblement inutile
train$campaignCode<-NULL

# identification des 4 colonnes au format json
json<-c("trafficSource","totals","geoNetwork","device")
tables<-c("train")

glob<-data.table() #table vide qui va reunir les tables transformes

# lecture et transformation successive train et test (suppression au passage de colonnes inutiles) 
for (t in tables) {
  partiel<-get(t)[,setdiff(names(get(t)),json)] # colonnes non json
  for (j in json) partiel<-cbind(partiel,fromJSON(paste("[", paste(get(t)[[j]], collapse = ","), "]")))
  temp<-partiel$adwordsClickInfo
  partiel$adwordsClickInfo<-NULL
  temp$targetingCriteria<-NULL
  result<-as.data.table(cbind(partiel,temp))
  if(t=="train") result$campaignCode<-NULL else result$transactionRevenue<-NA
  glob<-rbind(glob,result)
}
rm(partiel, train) ; gc()

#---------------Exploration et Pré-Processing-------------

#Premiere exploration
summary(glob)
head(glob)

#Separation du dataset en 2
x = split(glob, sample(rep(1:2, 500000)))
data = x$`1`
#data = glob #si l'on veut travailler sur tout le dataset

#Visualisation valeurs manquantes
library(visdat)
vis_miss(data[1:100000], warn_large_data = FALSE)

#On regarde chaque colonne en détail
z <- data$country
print(unique(z))

#On supprime les colonnes avec valeurs constantes ou des valeurs "not available"
data$criteriaParameters <- NULL
data$screenResolution <- NULL
data$screenColors <- NULL
data$language <- NULL
data$flashVersion <- NULL
data$mobileDeviceMarketingName <- NULL
data$mobileDeviceInfo <- NULL
data$mobileInputSelector <- NULL
data$mobileDeviceModel <- NULL
data$mobileDeviceBranding <- NULL
data$operatingSystemVersion <- NULL
data$browserSize <- NULL
data$browserVersion <- NULL
data$networkLocation <- NULL
data$longitude <- NULL
data$latitude <- NULL
data$cityId <- NULL
data$socialEngagementType <- NULL
data$visits <- NULL
data$datasplit <- NULL

#Conversion en date
data$date <- as.Date(as.character(data$date), format='%Y%m%d', tz="UTC")

#Conversion en POSIXct
library(lubridate)
data$visitStartTime <- as_datetime(data$visitStartTime)

#On decompose la date de visite
data[,visitStartYear:=format(visitStartTime,"%Y",tz="UTC")]
data[,visitStartMonth:=format(visitStartTime,"%m",tz="UTC")]
data[,visitStartDay:=format(visitStartTime,"%d",tz="UTC")]
data[,visitStartWeekDay:=format(visitStartTime,"%A",tz="UTC")]
data[,visitStartHour:=as.integer(format(visitStartTime,"%H",tz="UTC"))]

#Conversion de certaines colonnes en entier
data[, "hits"] <- lapply(data[, "hits"], as.integer)
data[, "pageviews"] <- lapply(data[, "pageviews"], as.integer)
data[, "page"] <- lapply(data[, "page"], as.integer)
data[, "bounces"] <- lapply(data[, "bounces"], as.integer)
data[, "newVisits"] <- lapply(data[, "newVisits"], as.integer)

#On transforme toutes les variables NA appelées autrement en vraies NA
is_na_vals <- c('unknown.unknown', '(not set)', 'not available in demo dataset', 
                '(not provided)', '(none)', '<NA>')
for(col in names(data)) {
  
  set(data, i=which(data[[col]] %in% is_na_vals), j=col, value=NA)
  
}

#On transforme les NA en 0 puis on converti newVisits en logical
data$newVisits[is.na(data$newVisits)] <- 0
data$bounces[is.na(data$bounces)] <- 0
data$newVisits = as.logical(data$newVisits)

#Transformation des NA en 1 (au moins 1 page vue)
data$pageviews[is.na(data$pageviews)] <- 1

#Transformation NA en 0
data$transactionRevenue[is.na(data$transactionRevenue)] <- 0
#Puis conversion en numeric
data$transactionRevenue <- as.numeric(data$transactionRevenue)
#Enfin on applique le log
data$transactionRevenue <- log(data$transactionRevenue + 1)

#Transforme les variables bas niveau en "Other"
to_other <- c("browser", "operatingSystem")
for (c in to_other) for (v in names(which(table(data[[c]])<1000))) data[get(c)==v,(c):="Other"]
to_other <- c("country")
for (c in to_other) for (v in names(which(table(data[[c]])<1500))) data[get(c)==v,(c):="Other"]

#Transforme les NA de certaines colonnes utiles en "ex.na" 
data$continent[is.na(data$continent)] <- "ex.na"
data$subContinent[is.na(data$subContinent)] <- "ex.na"
data$country[is.na(data$country)] <- "ex.na"
data$operatingSystem[is.na(data$operatingSystem)] <- "ex.na"

#Transformation en factor
data[, "channelGrouping"] <- lapply(data[, "channelGrouping"], as.factor)
data[, "browser"] <- lapply(data[, "browser"], as.factor)
data[, "operatingSystem"] <- lapply(data[, "operatingSystem"], as.factor)
data[, "deviceCategory"] <- lapply(data[, "deviceCategory"], as.factor)
data[, "continent"] <- lapply(data[, "continent"], as.factor)
data[, "subContinent"] <- lapply(data[, "subContinent"], as.factor)
data[, "country"] <- lapply(data[, "country"], as.factor)
data[, "region"] <- lapply(data[, "region"], as.factor)
data[, "metro"] <- lapply(data[, "metro"], as.factor)
data[, "city"] <- lapply(data[, "city"], as.factor)
data[, "campaign"] <- lapply(data[, "campaign"], as.factor)
data[, "visitStartYear"] <- lapply(data[, "visitStartYear"], as.factor)
data[, "visitStartMonth"] <- lapply(data[, "visitStartMonth"], as.factor)
data[, "visitStartDay"] <- lapply(data[, "visitStartDay"], as.factor)
data[, "visitStartWeekDay"] <- lapply(data[, "visitStartWeekDay"], as.factor)

#Visualisation des valeurs manquantes apres tous ces traitements
library(dplyr)
glimpse(data)
vis_miss(data[1:100000], warn_large_data = FALSE)


#---------------Visualisation--------------
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
vizDF = glob
vizDF$transactionRevenue[is.na(vizDF$transactionRevenue)] <- 0
vizDF$transactionRevenue <- as.numeric(vizDF$transactionRevenue)
vizDF$date <- as.Date(as.character(vizDF$date), format='%Y%m%d', tz="UTC")
vizDF$visitStartTime <- as_datetime(vizDF$visitStartTime)

#Pie charts
pie(table(data$continent), main = 'Pie Chart des continents')
pie(table(data$operatingSystem), main = 'Pie Chart des OS')

#affichage des premieres et dernieres dates.
time_range <- range(vizDF$date)
print(time_range)

#affichage des valeurs manquantes par colonnes.
ggplot(data.table(
  pmiss = sapply(glob, function(x) { (sum(is.na(x)) / length(x)) }),
  column = names(glob)
) ,aes(x = reorder(column, -pmiss), y = pmiss)) +
  geom_bar(stat = 'identity', fill = 'blue') + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    title='Valeurs manquantes par colonne',
    x='Colonne',
    y='% de valeurs manquantes')

#affichage des plus petites et plus grandes valeurs de la variable transactionRevenue
#sur laquelle porte notre etude.
rev_range <- round(range(vizDF$transactionRevenue, na.rm=TRUE), 2)
print(rev_range)#la variable transactionRevenue prend des valeurs de 10000 a 23129500000

rev_range_log <- round(range(log(vizDF$transactionRevenue + 1), na.rm=TRUE), 2)
print(rev_range_log)#le log de la variable transactionRevenue prend des valeurs de 0 a 23.86

#Distribution de la cible transaction revenue
ggplot(vizDF,aes(x=log(transactionRevenue), y=..density..)) +
  geom_histogram(fill='blue', na.rm=TRUE, bins=40) +
  geom_density(aes(x=log(transactionRevenue)), fill='red', color='red', alpha=0.3, na.rm=TRUE) + 
  labs(
    title = 'Distribution de la cible transaction revenue',
    x = 'Logarithme de transaction revenue'
  )
#d'apres cette courbe la moyenne du log de transactionRevenue est environ a 18 et la distribution est normale.

#affichage des visites, puis des montants des transactions, par jour
#Visites par jour
ggplot(vizDF[, .(n = .N), by=date],aes(x=date, y=n)) +
  geom_line(color='steelblue') +
  geom_smooth(color='orange') +
  labs(
    x='',
    y='Nombre de visites',
    title='Visites par jour'
  )
#Revenu par jour
ggplot(vizDF[, .(revenue = sum(transactionRevenue/10^6, na.rm=TRUE)), by=date] ,aes(x=date, y=revenue)) + 
  geom_line(color='steelblue') +
  geom_smooth(color='orange') +
  labs(
    x='',
    y='Revenu (dollars)',
    title='Revenu par jour'
  )
#Nous pouvons voir que les revenus sont stables au long de l'annee
#contrairement au nombre de visites qui a augmente en fin d'annee 2016 
#et qui semble a premiere vue ne pas avoir d'impact significatif sur les revenus

#affichage des visites, puis des montants des transactions, par heure
grid.arrange(
  ggplot(vizDF[, .(visitHour = hour(visitStartTime))][
    , .(visits = .N), by = visitHour], aes(x = visitHour, y = visits / 1000)) +
    geom_line(color = 'steelblue', size = 1) +
    geom_point(color = 'steelblue', size = 2) +
    labs(
      x = 'Heure de la journée',
      y = 'Nombre de visites)',
      title = "Visites moyennes par rapport à l'heure de la journée (UTC)"
    ),
  ggplot(vizDF[, .(transactionRevenue, visitHour = hour(visitStartTime))][
    , .(revenue = sum(transactionRevenue/10^6, na.rm = T)), by = visitHour] ,aes(x = visitHour, y = revenue / 1000))  +
    geom_line(color = 'steelblue', size = 1) +
    geom_point(color = 'steelblue', size = 2) +
    labs(
      x = 'Hour of day',
      y = 'Revenu (dollars)',
      title = "Revenu moyen par rapport à l'heure de la journée (UTC)"
    )
  , nrow = 2)

#Affichage des moyens par lesquels les utilisateurs sont arrives sur le site
#Ainsi que les revenus et visites pour chaque moyen
ggplot(vizDF[, .(n = .N), by=channelGrouping],aes(x=reorder(channelGrouping, -n), y=n)) +
  geom_bar(stat='identity', fill='blue') +
  labs(x='Channel Grouping',
       y='Visites',
       title="Nombre de visites par moyen d'acces")

ggplot(vizDF[, .(revenue = sum(transactionRevenue, na.rm=TRUE)), by=channelGrouping] ,aes(x=reorder(channelGrouping, revenue), y=revenue/1000)) +
  geom_bar(stat='identity', fill='blue') +
  coord_flip() + 
  labs(x='Channel Grouping',
       y='Revenu',
       title='Revenus totaux par channel grouping')

ggplot(vizDF[, .(n=.N), by=deviceCategory],aes(x=reorder(deviceCategory, -n), y=n)) + 
  geom_bar(stat='identity', fill='blue') +
  labs(x='Device Category', 
       y='Nombre de visites',
       title="Distribution des visites par categorie d'appareil") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vizDF[, .(revenue = sum(transactionRevenue, na.rm=TRUE)), by=deviceCategory],aes(x=reorder(deviceCategory, -revenue), y=revenue)) +
  geom_bar(stat='identity', fill='blue') +
  labs(x='Device category',
       y='Revenus',
       title="Distribution des revenus par categorie d'appareil") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affichage de la difference des revenus entre utilisateurs de smartphones et les autres
ggplot(vizDF,aes(x=log(transactionRevenue), y=..density.., fill=isMobile)) +
  geom_density(alpha=0.5) + 
  scale_fill_manual(values = c('blue', 'pink')) + 
  labs(title="Distribution des revenus entre les utilisateurs de smartphones et d'ordinateurs")
#il y a legerement plus de faibles transactions par mobile
#mais cela n'est pas forcement significatif


#---------------Modeles--------------
library(dplyr)

#METHODES EVALUATION
RMSE = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2, na.rm=TRUE))
}
RSS = function(actual,predicted) {
  sum((predicted-actual)^2, na.rm = TRUE)/length(actual)
}
R2 = function(actual,predicted) {
  1-sum((predicted-actual)^2, na.rm = TRUE)/sum((actual-mean(actual))^2)
} 

#MODELE NAIF
#Train et Test set
data_idx = sample(1:nrow(data), nrow(data) * (3/4)) 
data_train = data[data_idx,]
data_test  = data[-data_idx,]

#Prediction
meanRevenue <- mean(data_test$transactionRevenue)
naive <- rep(meanRevenue,length(data_test))

#Evaluation
print(RMSE(data_test$transactionRevenue, meanRevenue))
print(RSS(data_test$transactionRevenue, meanRevenue))
print(R2(data_test$transactionRevenue, meanRevenue))

#Evaluation apres group by
data_test$prediction <- meanRevenue
predictionDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(predictionSum = sum(prediction))
actualDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(transactionSum = sum(transactionRevenue))
print(RMSE(actualDF$transactionSum, predictionDF$predictionSum))
print(RSS(actualDF$transactionSum, predictionDF$predictionSum))
print(R2(actualDF$transactionSum, predictionDF$predictionSum))


#REGRESSION LINEAIRE
#Train et Test set
data_idx = sample(1:nrow(data), nrow(data) * (3/4)) 
data_train = data[data_idx,]
data_test  = data[-data_idx,]

#Prediction
T1<-Sys.time()
model_lm <- lm(formula=transactionRevenue ~ channelGrouping + visitNumber + hits + pageviews + newVisits + continent + subContinent + country + browser + operatingSystem + isMobile + deviceCategory + visitStartYear + visitStartMonth + visitStartDay + visitStartWeekDay + visitStartHour , data = data_train)
summary(model_lm)
lm_pred = predict(model_lm, newdata = data_test)
T2<-Sys.time()
Tdiff= difftime(T2, T1)
print(Tdiff)

#Evaluation
print(RMSE(data_test$transactionRevenue, lm_pred))
print(RSS(data_test$transactionRevenue, lm_pred))
print(R2(data_test$transactionRevenue, lm_pred))

#Visualisation
plot(lm_pred, data_test$transactionRevenue,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "#cd0050", pch = 20)
grid()
abline(0, 1, col = "dodgerblue", lwd = 2)

#Evaluation apres group by
data_test$prediction <- lm_pred
predictionDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(predictionSum = sum(prediction))
actualDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(transactionSum = sum(transactionRevenue))
print(RMSE(actualDF$transactionSum, predictionDF$predictionSum))
print(RSS(actualDF$transactionSum, predictionDF$predictionSum))
print(R2(actualDF$transactionSum, predictionDF$predictionSum))


#ARBRE SIMPLE
library(rpart)
library(rpart.plot)
#Train et Test set
data_idx = sample(1:nrow(data), nrow(data) * (3/4)) 
data_train = data[data_idx,]
data_test  = data[-data_idx,]

#Prediction
T1<-Sys.time()
tree = rpart(transactionRevenue ~ channelGrouping + visitNumber + hits + pageviews + newVisits + continent + subContinent + country + browser + operatingSystem + isMobile + deviceCategory + visitStartYear + visitStartMonth + visitStartDay + visitStartWeekDay + visitStartHour, data = data_train)
tree_pred = predict(tree, data_test)
T2<-Sys.time()
Tdiff= difftime(T2, T1)
print(Tdiff)

#Evaluation
print(RMSE(data_test$transactionRevenue, tree_pred))
print(RSS(data_test$transactionRevenue, tree_pred))
print(R2(data_test$transactionRevenue, tree_pred))

#Visualisation
rpart.plot(tree)

#Evaluation apres group by
data_test$prediction <- tree_pred
predictionDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(predictionSum = sum(prediction))
actualDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(transactionSum = sum(transactionRevenue))
print(RMSE(actualDF$transactionSum, predictionDF$predictionSum))
print(RSS(actualDF$transactionSum, predictionDF$predictionSum))
print(R2(actualDF$transactionSum, predictionDF$predictionSum))


#RANDOM FOREST
library(randomForest)
#Train et Test set
newx = split(data, sample(rep(1:4, 100000)))
newdata = newx$`1`
data_idx = sample(1:nrow(newdata), nrow(newdata) * (3/4)) 
data_train = newdata[data_idx,]
data_test  = newdata[-data_idx,]

#Grid Search pour faire le meilleur RandomForest
#adapted from https://www.janbasktraining.com/blog/random-forest-in-r/
require(caret)
require(e1071)
#On cherche mtry
#best mtry=8
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(transactionRevenue~.,
                 data = data_train,
                 method = "rf",
                 metric = "RMSE",
                 tuneGrid = tuneGrid,
                 trControl = trainControl(),
                 importance = TRUE,
                 nodesize = 500,
                 ntree = 2)
print(rf_mtry)
best_mtry=8

#On cherche n_tree
#best n_tree = 600
x = split(data_train, sample(rep(1:10, 300)))
data = x$`1`
store_maxtrees <- list()
tuneGrid <- expand.grid(.mtry = c(1: 10))
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(transactionRevenue~.,
                       data = data,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trainControl(),
                       importance = TRUE,
                       nodesize = 100,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#On cherche nodesize
#best nodesize = 25
store_nodesize <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (nodesize in c(0: 20)) {
  set.seed(1234)
  rf_nodesize <- train(survived~.,
                       data = data_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = nodesize,
                       ntree = 300)
  current_iteration <- toString(nodesize)
  store_nodesize[[current_iteration]] <- rf_nodesize
}
results_nodesize <- resamples(store_nodesize)
summary(results_nodesize)

#Prediction
T1 <- Sys.time()
RF<- randomForest(transactionRevenue ~ channelGrouping + visitNumber + hits + pageviews + newVisits + continent + subContinent + country + browser + operatingSystem + isMobile + deviceCategory + visitStartYear + visitStartMonth + visitStartDay + visitStartWeekDay + visitStartHour,data=data_train, mtry=8, ntree=500, nodesize=25,na.action = "na.exclude")
predRF<-predict(RF,data_test)
RF$importance[order(RF$importance[, 1], decreasing = TRUE), ]
T2<-Sys.time()
Tdiff= difftime(T2, T1)
print(Tdiff)

#Evaluation
print(RMSE(data_test$transactionRevenue, predRF))
print(RSS(data_test$transactionRevenue, predRF))
print(R2(data_test$transactionRevenue, predRF))

#Evaluation apres group by
data_test$prediction <- predRF
predictionDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(predictionSum = sum(prediction))
actualDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(transactionSum = sum(transactionRevenue))
print(RMSE(actualDF$transactionSum, predictionDF$predictionSum))
print(RSS(actualDF$transactionSum, predictionDF$predictionSum))
print(R2(actualDF$transactionSum, predictionDF$predictionSum))


#GRADIENT BOOSTING MODEL
library(gbm)
#Train et Test set
data_idx = sample(1:nrow(data), nrow(data) * (3/4)) 
data_train = data[data_idx,]
data_test  = data[-data_idx,]
data_train[, "newVisits"] <- lapply(data_train[, "newVisits"], as.factor)
data_test[, "isMobile"] <- lapply(data_test[, "isMobile"], as.factor)
data_train[, "isMobile"] <- lapply(data_train[, "isMobile"], as.factor)
data_test[, "newVisits"] <- lapply(data_test[, "newVisits"], as.factor)

#Prediction
T1 <- Sys.time()
gbm_model <- gbm(transactionRevenue ~ channelGrouping + visitNumber + hits + pageviews + newVisits + continent + subContinent + country + browser + operatingSystem + isMobile + deviceCategory + visitStartYear + visitStartMonth + visitStartDay + visitStartWeekDay + visitStartHour
                 , distribution = "gaussian"
                 , data = data_train
                 , n.trees = 700 # nombre d'arbres, si grand meilleurs perfs, mais plus long ? compiler
                 , interaction.depth = 3
                 , n.minobsinnode = 100 # nombre mini d'observations dans chaque noeud final
                 , shrinkage = 0.1
                 , bag.fraction = 0.5
                 , train.fraction = 0.5
                 , cv.folds = 10     
                 , verbose = FALSE  
                 , n.cores = 1
)
best.iter = gbm.perf(gbm_model, method = "cv")
gbm_pred = predict(gbm_model, newdata = data_test, n.trees = best.iter)
T2<-Sys.time()
Tdiff= difftime(T2, T1)
print(Tdiff)

#Evaluation
print(RMSE(data_test$transactionRevenue, gbm_pred))
print(RSS(data_test$transactionRevenue, gbm_pred))
print(R2(data_test$transactionRevenue, gbm_pred))

#Visualisation
plot(gbm_model,i.var="newVisits", ylab = "Prediction",
     main = "Prediction en fonction de newVisits - GBM",)
plot(gbm_model,i.var="visitStartMonth", ylab = "Prediction",
     main = "Prediction en fonction de visitStartMonth - GBM",)
plot(gbm_model,i.var="visitStartHour", ylab = "Prediction",
     main = "Prediction en fonction de visitStartHour - GBM",)

#Evaluation apres group by
data_test$prediction <- gbm_pred
predictionDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(predictionSum = sum(prediction))
actualDF <- data_test%>%
  group_by(fullVisitorId)%>%
  summarise(transactionSum = sum(transactionRevenue))
print(RMSE(actualDF$transactionSum, predictionDF$predictionSum))
print(RSS(actualDF$transactionSum, predictionDF$predictionSum))
print(R2(actualDF$transactionSum, predictionDF$predictionSum))