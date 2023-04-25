#Alyssa Venn
#Started 1/20/2023
#The goal of this file is to build a model that can detect email addresses
#within free text fields, where they are not meant to be.

#Libraries ------
library(tidyverse)
library(plyr)
library(tm)
library(tidytext)
library(text2vec)
library(xgboost)
library(pdp)
library(randomForest)
library(ggplot2)
library(caret)
library(Rnssp)
library(corpus)

#Data ------
#Emails, downloaded from https://www.briandunning.com/sample-data/
us_email <- read_csv("PII Project/us-500.csv")
uk_email <- read_csv("PII Project/uk-500.csv")
ca_email <- read_csv("PII Project/ca-500.csv")
au_email <- read_csv("PII Project/au-500.csv")

emails <- rbind.fill(us_email, uk_email, ca_email, au_email)
emails <- as.data.frame(emails$email)
names(emails) <- "emails"

#Creating 10 new emails with non-standard urls
#I used https://www.name-generator.org.uk/quick/ to come up with first and
#last names
email2 <- as.data.frame(c("alyssa@venns.com", "saul.adkins@adkins.net", "ediaz@diaz.org",
            "veronicaryan@oryan.org", "mckenziem@moons.com", "da78@allenterprises.net",
            "kittykatpeters@ilovecats.net", "georgelawson@george.org", "ronan.silva@silvar.com",
            "ssharp@sharps.com"))
names(email2) <- "emails"

emails <- rbind(emails, email2)

#Sample data from Cache_ER_Base DB in NSSP. 6000 random rows from between
#3-30-2022 through 8-03-2022
load(file="~/PII Project/sample_dataset.rda")

#Grab chief complaints and merge it with emails
chief_complaints <- as.data.frame(sample_dataset$ChiefComplaintOrig)

set.seed(120)
samp <- sample(nrow(chief_complaints), 2010)

sample <- as.data.frame(chief_complaints[samp,])
emailless <- as.data.frame(chief_complaints[-samp,])

emails <- as.data.frame(emails[sample(1:nrow(emails)),])

df <- cbind(sample, emails)
names(df) <- c("cc", "emails")

#Combining the two columns into one
df$ChiefComplaintOrig <- paste(df$cc, df$emails)

#Marking all rows with emails as PII = 1
df$PII <- 1

names(emailless) <- "ChiefComplaintOrig"

emailless$PII <- 0

df <- rbind.fill(df, emailless)

#Pre-processing ------
#Split into train and test (70/30)
df <- df %>% 
  dplyr::mutate(id = row_number())

d_train <- df %>%
  group_by(PII) %>%
  sample_frac(size=.7)

d_test  <- anti_join(df, d_train, by = 'id')

#Make sure that the proportions of PII are the same in test and train
prop.table(table(d_train$PII))

prop.table(table(d_test$PII))

#Tokenize data

#d_train$tokenized_cc <- str_to_lower(d_train$ChiefComplaintOrig)
#d_train$tokenized_cc <- str_extract_all(d_train$tokenized_cc, "[[:alpha:]']+-?[[:alpha:]']+|[:alpha:]{1}|@")

#Since the DTM doesn't like the @, and is deleting it (I guess because it isn't
#a term), I'm replacing it with the word "atsymbol"
#d_train$tokenized_cc <- str_replace_all(d_train$tokenized_cc, "@", "atsymbol")
#d_train$tokenized_cc <- str_remove_all(d_train$tokenized_cc, '["()),]')
#d_train$tokenized_cc <- str_remove(d_train$tokenized_cc, "c")

#Creating the document term matrix
create_dictionary <- function(data){
  corpus <- data$ChiefComplaintOrig
  
  corpus <- str_to_lower(corpus)
  corpus <- str_extract_all(corpus, "[[:alpha:]']+-?[[:alpha:]']+|[:alpha:]{1}|@")
  
  #Tokenization
  corpus <- str_replace_all(corpus, "@", "atsymbol")
  corpus <- str_remove_all(corpus, '["()),]')
  corpus <- str_remove(corpus, "c")
  corpus <- purrr::map(corpus, function(x) str_split(tolower(x),"\\s+") %>% unlist) 
  
  #Remove stop-words ("the", "in", etc.)
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))])
  
  #stem
  corpus <- purrr::map(corpus, function(x) text_tokens(x, stemmer="en") %>% unlist)
  
  #Count word frequency and remove rare words
  words <- as.data.frame(sort(table(unlist(corpus)), decreasing=T), stringsAsFactors = F)
  words <- words$Var1[which(words$Freq >=20)]
  
  return(words)  
}

dict <- create_dictionary(d_train)

create_dtm <- function(data, dict){
  corpus <- data$ChiefComplaintOrig
  
  #Repeat pre-processing from above
  
  corpus <- str_to_lower(corpus)
  corpus <- str_extract_all(corpus, "[[:alpha:]']+-?[[:alpha:]']+|[:alpha:]{1}|@")
  
  #Tokenization
  corpus <- str_replace_all(corpus, "@", "atsymbol")
  corpus <- str_remove_all(corpus, '["()),]')
  corpus <- str_remove(corpus, "c")
  corpus <- purrr::map(corpus, function(x) str_split(tolower(x),"\\s+") %>% unlist) 
  
  #Remove stop-words ("the", "in", etc.)
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))])
  
  #stem
  corpus <- purrr::map(corpus, function(x) text_tokens(x, stemmer="en") %>% unlist)
  
  #Keep only words from the dictionary
  corpus <- purrr::map(corpus, function(x) x[x %in% dict])
  
  #Make dtm
  dtm <- as.data.frame(matrix(0L, nrow=nrow(data), ncol=length(dict)))
  names(dtm) <- dict
  
  freq <- purrr::map(corpus, table)
  for (i in 1:nrow(dtm)){
    dtm[i, names(freq[[i]])] <- unname(freq[[i]])
  }
  
  return(dtm)
}

dtm_train <- create_dtm(d_train, dict)

# #dtm_train = DocumentTermMatrix(unlist(d_train$tokenized_cc))
# dtm_train = DocumentTermMatrix(d_train$tokenized_cc)
# 
# dtm_train = removeSparseTerms(dtm_train, 0.999)
# 
# dataset_train = as.data.frame(as.matrix(dtm_train))

#xgbTree ------

train_model <- function(data, dtm, target_topic){
  t <- factor(unlist(data[,target_topic]), levels=c(1,0))
  
  caret::train(dtm, t, method="xgbTree",
               trControl = trainControl(method="cv", number=5, 
                                        search = "random", 
                                        verboseIter=T))
}

mod <- train_model(d_train, dtm_train, "PII")
mod

#Tuning hyperparameters using https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret

train_model <- function(data, dtm, target_topic, nrounds){
  
  tune_grid <- expand.grid(
    nrounds = seq(from = 200, to = nrounds, by = 50),
    eta = c(0.025, 0.05, 0.1, 0.3),
    max_depth = c(2, 3, 4, 5, 6),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  t <- factor(unlist(data[,target_topic]), levels=c(1,0))
  
  caret::train(dtm, t, method="xgbTree",
               trControl = trainControl(method="cv", number=5, 
                                        search = "random", 
                                        verboseIter=T),
               tuneGrid = tune_grid)
}

xgb_tune <- train_model(d_train, dtm_train, "PII", 1000)
# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
    theme_bw()
}

tuneplot(xgb_tune)

#Evaluate model
#100% accuracy

dtm_test <- create_dtm(d_test, dict)

evaluate_model <- function(model, data_test, target_topic){
  t <- factor(unlist(data_test[,target_topic]), levels=c(1,0))
  
  predictions <- predict(mod, newdata = dtm_test)
  confusionMatrix(predictions, t)
  
}

evaluate_model(mod, d_test, "PII")

#Creating a dataset of predictions for Jeremy
t <- factor(unlist(d_test[, "PII"]), levels=c(1,0))
dtm_test <- create_dtm(d_test, dict)

predictions <- predict(mod, newdata = dtm_test)

predictions_xgb <- cbind(d_test, predictions)


#Random Forest ------

dtm_train_rf <- dtm_train
dtm_train_rf$PII <- d_train$PII

classifier = randomForest(x = dtm_train_rf[-length(dtm_train_rf)],
                          y = dtm_train_rf$PII,
                          ntree = 10)

dtm_test_rf <- dtm_test
dtm_test_rf$PII  <- d_test$PII

y_pred = predict(classifier, newdata = dtm_test_rf[-length(dtm_train_rf)])


#The predictions are looking really wild, ranging from -8.11E-15 to 1, so I'm
#going to say that they probably are not rounding properly. So first I'll
#round

y_pred_round <- round(y_pred)

#99.94% accuracy on original data, missed a PII and marked it as clean
#Add 10 examples of emails with non-standard urls (like alyssa@venns.com)
#1/25/2023 - With 10 examples of non-standard urls, the accuracy is 
#99.78%. This is slightly worst accuracy, as it got 4 wrong. However,
#this time it marked non-PII as having PII, and didn't miss any with PII.
#This may potentially be better, as it is more important to catch all the PII.
#A false negative is more dangerous than a false positive in this instance.
cm=table(y_pred_round,dtm_test_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy

#Getting df of predictions
predictions_rf <- cbind(d_test, y_pred_round)

#Finding which ones were wrong
predictions_rf$combo <- predictions_rf$PII + predictions_rf$y_pred_round
wrong <- subset(predictions_rf, combo == 1)


#Testing with QA set from Jeremy ------

qa <- readxl::read_xlsx(
  "~/PII Project/QA Test Data (Emails).xlsx"
)

names(qa) <- c("ChiefComplaintOrig", "PII")

#xgbTree
#Accuracy: 100%
dtm_qa <- create_dtm(qa, dict)

evaluate_model <- function(model, data_test, target_topic){
  t <- factor(unlist(data_test[,target_topic]), levels=c(1,0))
  
  predictions <- predict(mod, newdata = dtm_qa)
  confusionMatrix(predictions, t)
  
}

evaluate_model(mod, qa, "PII")

#Random forest
#Accuracy: 100%
dtm_qa_rf <- dtm_qa
dtm_qa_rf$PII  <- qa$PII

y_pred = predict(classifier, newdata = dtm_qa_rf[-length(dtm_qa_rf)])

y_pred_round <- round(y_pred)

cm=table(y_pred_round,dtm_qa_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy


#CCQV testing ----
myProfile <- readRDS("~/myProfile.rds")
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?endDate=28Feb2022&percentParam=noPercent&datasource=va_erccdd&detector=nodetectordetector&startDate=1Jan2022&timeResolution=monthly&userId=4877&aqtTarget=DataDetails"
#api_data <- get_api_data(url, fromCSV = TRUE)

#data <- api_data %>% 
#  select(ChiefComplaintOrig,DischargeDiagnosis) %>% 
#  unite("CCDD.text", ChiefComplaintOrig, DischargeDiagnosis)
#save(ccqv_sample, file="~/PII Project/ccqv_sample.rda")
#save(ccqv, file="~/PII Project/ccqv.rda")
load(file="~/PII Project/ccqv_sample.rda")

# ccqv <- ccqv_sample %>%
#   sample_n(size=3000000)
names(ccqv_sample) <- "ChiefComplaint"
test <- ccqv_sample[1:100,]

create_dtm_ccqv <- function(data, dict){
  corpus <- data$ChiefComplaint
  
  #Repeat pre-processing from above
  
  corpus <- str_to_lower(corpus)
  corpus <- str_extract_all(corpus, "[[:alpha:]']+-?[[:alpha:]']+|[:alpha:]{1}|@")
  
  #Tokenization
  corpus <- str_replace_all(corpus, "@", "atsymbol")
  corpus <- str_remove_all(corpus, '["()),]')
  corpus <- str_remove(corpus, "c")
  corpus <- purrr::map(corpus, function(x) str_split(tolower(x),"\\s+") %>% unlist) 
  
  #Remove stop-words ("the", "in", etc.)
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))])
  
  #stem
  corpus <- purrr::map(corpus, function(x) text_tokens(x, stemmer="en") %>% unlist)
  
  #Keep only words from the dictionary
  corpus <- purrr::map(corpus, function(x) x[x %in% dict])
  
  #Make dtm
  dtm <- as.data.frame(matrix(0L, nrow=nrow(data), ncol=length(dict)))
  names(dtm) <- dict
  
  freq <- purrr::map(corpus, table)
  for (i in 1:nrow(dtm)){
    dtm[i, names(freq[[i]])] <- unname(freq[[i]])
  }
  
  return(dtm)
}

tictoc::tic()
dtm_ccqv_v2 <- create_dtm_ccqv(ccqv_sample, dict)
tictoc::toc()

#Save the dtm since it takes 18 hours to run
save(dtm_ccqv, file = "~/PII Project/ccqv_dtm.rda")
save(dtm_ccqv_v2, file = "~/PII Project/ccqv_dtm_v2.rda")

#xgbTree

predictions <- predict(mod, newdata = dtm_ccqv_v2)

ccqv_sample_xgb <- ccqv_sample

ccqv_sample_xgb$prediction <- predictions

#The model marked 3393 observations as having emails. I scrolled through the first
#250, and it guessed 209 correctly, but all the others did not have emails, 
#they simply had @ symbols used for other reasons
xgb_predictions <- subset(ccqv_sample_xgb, predictions == 1)

#Random forest

y_pred = predict(classifier, newdata = dtm_ccqv_v2)

y_pred_round <- round(y_pred)

ccqv_sample_rf <- ccqv_sample

ccqv_sample_rf$prediction <- y_pred_round

#The random forest ALSO had 3393 predictions, which is interesting.
rf_predictions <- subset(ccqv_sample_rf, predictions == 1)

#Look if there's any differences in the predictions
#There were 44 that differed, none of which contained an email. 
xgb_predictions$prediction <- as.numeric(xgb_predictions$prediction)
diff <- anti_join(xgb_predictions, rf_predictions)

#I'm going to try and manually look for emails in the prediction set
#I found quite a few emails like this! Not all of the 308 are emails still,
#but it's a decent portion. Of course, I have no idea how many emails it's missing. 
test <- str_detect(xgb_predictions$ChiefComplaint, ".com|.edu|.net|.gov|.org")
xgb_predictions$com_string <- test
coms <- subset(xgb_predictions, com_string == TRUE)

#Total there are 3648 @ signs in the ccqv_sample data, out of 3 million. This is pretty close
#to the 3393 positive predictions, which makes me think that perhaps the model is
#overestimating the importance of an @ on email vs. no emails. Perhaps re-training
#the model on data that includes more @ signs is important.
#In the sample data, 13 of 6000 chief complaints contain @ signs. In the CCQV
#data, 3648 of the 3 million contain @ signs. It's appropriately small, but still.

#Add 1% @ symbols into training set. I pulled 100 rows with @ symbols from the database
load("~/PII Project/atsymbol_dataset.rda")

sample_dataset <- rbind(sample_dataset, atsymbol_dataset)
sample_dataset <- sample_dataset[101:6100,]

#Then go back to line 47 and run it again

############################################################
# The best model seems to be the last version of the xgbTree.
# I will save the model permanently.
############################################################
mod_final <- mod
save(mod_final, file="~/PII Project/mod_final.rda")
save(xgb_predictions, file="~/PII Project/xgb_predictions.rda")
