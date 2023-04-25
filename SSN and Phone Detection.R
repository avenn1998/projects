#Alyssa Venn
#Started 3/2/2023
#The goal of this file is to build a model that can detect Social Security Numbers
#OR phone numbers within free text fields, where they are not meant to be. The idea is
#to build a model that can detect multiple weird numbers, as any long string of
#numbers is likely to be an issue.

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
library(ngram)

#Notes from code review 3/23
#Diagnosis codes

#Data ------
ssn <- read.csv("~/PII Project/SSN DB.csv")

#Phone numbers, downloaded from https://www.briandunning.com/sample-data/
us_phone <- read_csv("PII Project/us-500.csv")
ca_phone <- read_csv("PII Project/ca-500.csv")

#Potential issue: All of these phone #s are in the same format (delineated
#by -), when is completely likely that in the real world they would not be
#(could be delineated by (), or nothing, or ., etc.)

#Tokenization idea, replace ALL punctuation/special characters with a space?
#That way the model is searching for either 123 123 1234 or 1231231234. 

#Replace us_500 - with . or deleting it
us_phone$phone1 <- str_replace_all(us_phone$phone1, "-", ".")
us_phone$phone2 <- str_replace_all(us_phone$phone2, "-", "")


#Replace ca_500 - with space
ca_phone$phone1 <- str_replace_all(ca_phone$phone1, "-", " ")


#Combine
phones <- rbind.fill(us_phone, ca_phone)
phone1 <- as.data.frame(phones$phone1)
phone2 <- as.data.frame(phones$phone2)
names(phone1) <- "phone"
names(phone2) <- "phone"

phones <- rbind(phone1, phone2)

#Sample data from Cache_ER_Base DB in NSSP. 6000 random rows from between
#3-30-2022 through 8-03-2022
load(file="~/PII Project/sample_dataset.rda")

#Adding an ID
sample_dataset$ID <- 1:nrow(sample_dataset)

#Grab chief complaints and merge it with SSN
chief_complaints <- subset(sample_dataset, select = c(ChiefComplaintOrig, ID))

set.seed(120)
samp <- sample(nrow(chief_complaints), 1001)

sample <- as.data.frame(chief_complaints[samp,])
ssn_free <- as.data.frame(chief_complaints[-samp,])

ssn <- as.data.frame(ssn[sample(1:nrow(ssn)),])

df <- cbind(sample, ssn)
names(df) <- c("cc", "ID", "ssn")

#Combining the two columns into one
df$ChiefComplaintOrig <- paste(df$cc, df$ssn)

#Marking all rows with emails as PII = 1
df$PII_ssn <- 1

names(ssn_free) <- c("ChiefComplaintOrig", "ID")

ssn_free$PII_ssn <- 0

df <- rbind.fill(df, ssn_free)

#Now taking ANOTHER random sample for phone numbers. They may overlap, they may
#not.
#Grab chief complaints and merge it with SSN
chief_complaints <- subset(df, select = c(ChiefComplaintOrig, ID))

samp <- sample(nrow(chief_complaints), 2000)

sample <- as.data.frame(chief_complaints[samp,])
phone_free <- as.data.frame(chief_complaints[-samp,])

phones <- as.data.frame(phones[sample(1:nrow(phones)),])

df2 <- cbind(sample, phones)
names(df2) <- c("cc", "ID", "phones")

#Combining the two columns into one
df2$ChiefComplaintOrig <- paste(df2$cc, df2$phones)

#Marking all rows with emails as PII = 1
df2$PII_phone <- 1

names(phone_free) <- c("ChiefComplaintOrig", "ID")

phone_free$PII_phone <- 0

df2 <- rbind.fill(df2, phone_free)

#Get marker for PII_ssn into df2
df <- subset(df, select = c(PII_ssn, ID))

df3 <- merge(df2, df, by = "ID")

#Create marker for general PII

df3$PII <- ifelse(df3$PII_phone + df3$PII_ssn > 0, 1, 0)

df <- df3

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

#Tokenization ----

create_dict <- function(data){
  #Remove snomed codes
  corpus <- str_remove_all(data$ChiefComplaintOrig, "_;\\d+|;\\d+;\\d*")
  
  #Replacing all punctuation fully wrapped in digits with a .
  corpus <- str_replace_all(corpus, '(?<=\\b[0-9]{1,100})[[:punct:]]', ".")
  
  corpus <- str_replace_all(corpus, 
                            "[0-9]", "Z")
  
  #Replacing any ### ## #### with ###.##.####
  corpus <- str_replace_all(corpus, ' ZZZ ZZ ZZZZ ', " ZZZ.ZZ.ZZZZ ")
  corpus <- str_replace_all(corpus, ' ZZZ ZZZ ZZZZ ', " ZZZ.ZZZ.ZZZZ ")
  corpus <- str_replace_all(corpus, ' ZZZ ZZZZ ', " ZZZ.ZZZZ ")
  
  #Tokenization
  corpus <- str_remove_all(corpus, '["()),]')
  corpus <- str_remove(corpus, "c")
  corpus <- purrr::map(corpus, function(x) str_split(tolower(x),"\\s+") %>% unlist) 
  
  #Remove stop-words ("the", "in", etc.)
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))])
  
  #stem
  corpus <- purrr::map(corpus, function(x) text_tokens(x, stemmer="en") %>% unlist)
  
  #Count word frequency and remove rare words
  words <- as.data.frame(sort(table(unlist(corpus)), decreasing=T), stringsAsFactors = F)
  words <- words$Var1[which(words$Freq >=15)]
  
  return(words)  
}

dict <- create_dict(d_train)


create_dtm <- function(data){
  #Remove snomed codes
  corpus <- str_remove_all(data$ChiefComplaintOrig, "_;\\d+|;\\d+;\\d*")
  
  #Replacing all punctuation fully wrapped in digits with a .
  corpus <- str_replace_all(corpus, '(?<=\\b[0-9]{1,100})[[:punct:]]', ".")
  
  corpus <- str_replace_all(corpus, 
                            "[0-9]", "Z")
  
  #Replacing any ### ## #### with ###.##.####
  corpus <- str_replace_all(corpus, ' ZZZ ZZ ZZZZ ', " ZZZ.ZZ.ZZZZ ")
  corpus <- str_replace_all(corpus, ' ZZZ ZZZ ZZZZ ', " ZZZ.ZZZ.ZZZZ ")
  corpus <- str_replace_all(corpus, ' ZZZ ZZZZ ', " ZZZ.ZZZZ ")
  
  #Tokenization
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

dtm_train <- create_dtm(d_train)
dtm_test <- create_dtm(d_test)



#xgbTree ----

train_model <- function(data, dtm, target_topic){
  t <- factor(unlist(data[,target_topic]), levels=c(1,0))
  
  caret::train(dtm, t, method="xgbTree",
               trControl = trainControl(method="cv", number=10, 
                                        search = "random", 
                                        verboseIter=T))
}

mod <- train_model(d_train, dtm_train, "PII")
mod


evaluate_model <- function(model, data_test, target_topic, dtm_test){
  t <- factor(unlist(data_test[,target_topic]), levels=c(1,0))
  
  predictions <- predict(mod, newdata = dtm_test)
  confusionMatrix(predictions, t)
  
}

#97.5% accuracy, with 2 false positives and 38 false negatives
#V2 (number = 10 instead of number = 5): 99.5% accuracy, 9 false negatives

evaluate_model(mod, d_test, "PII", dtm_test)

#Random Forest ----

dtm_train_rf <- dtm_train
dtm_train_rf$PII <- d_train$PII

classifier = randomForest(x = dtm_train_rf[-length(dtm_train_rf)],
                          y = dtm_train_rf$PII,
                          ntree = 10)

dtm_test_rf <- dtm_test
dtm_test_rf$PII  <- d_test$PII

y_pred = predict(classifier, newdata = dtm_test_rf[-length(dtm_test_rf)])

y_pred_round <- round(y_pred)

#99.9% accuracy, with 1 false positive and 1 false negative.
cm=table(y_pred_round,dtm_test_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy

#Which ones are wrong?
d_test$rf <- y_pred_round
d_test$xgbtree <- predictions <- predict(mod, newdata = dtm_test)

#xgbTree is missing the 8-digit SSNs, and incorrectly classifying 3-digit numbers
#as SSN (such as 911). Random forest is doing much better. 
wrong <- subset(d_test, xgbtree != PII | rf != PII)

#QA ----
#Just SSN
qa_ssn <- readxl::read_xlsx("~/PII Project/QA Test Data (SSN).xlsx")
names(qa_ssn) <- c("ChiefComplaintOrig", "PII")

dtm_qa_ssn <- create_dtm(qa_ssn)

#xgbTree
#Accuracy for v2: 90.91%
#5 false negatives, 4 false positives.
evaluate_model(mod, qa_ssn, "PII", dtm_qa_ssn)

#Random forest
dtm_qa_ssn_rf <- dtm_qa_ssn
dtm_qa_ssn_rf$PII  <- qa_ssn$PII

y_pred = predict(classifier, newdata = dtm_qa_ssn_rf[-length(dtm_qa_ssn_rf)])

y_pred_round <- round(y_pred)

#89.9% accuracy, with 6 false positives and 4 false negatives.
cm=table(y_pred_round,dtm_qa_ssn_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy

#Just phone
qa_phone <- readxl::read_xlsx("~/PII Project/QA Test Data (Phone).xlsx")
names(qa_phone) <- c("ChiefComplaintOrig", "PII")

dtm_qa_phone <- create_dtm(qa_phone)

#xgbTree
#Accuracy for v2: 84%
#16 false negatives (got more wrong than right on the real PII)
evaluate_model(mod, qa_phone, "PII", dtm_qa_phone)

#Random forest
dtm_qa_phone_rf <- dtm_qa_phone
dtm_qa_phone_rf$PII  <- qa_phone$PII

y_pred = predict(classifier, newdata = dtm_qa_phone_rf[-length(dtm_qa_phone_rf)])

y_pred_round <- round(y_pred)

#84% accuracy, with 16 false negatives. Same as xgbTree
cm=table(y_pred_round,dtm_qa_phone_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy

#Which ones are wrong?
qa_phone$rf <- y_pred_round
qa_phone$xgbtree <- predictions <- predict(mod, newdata = dtm_qa_phone)

#All kinda funky formats. Switching up formats, like (123) 123.1234, or 
#(123)-1231234, or +1-123-123-1234. Need to add some of those to the training
#set.
wrong <- subset(qa_phone, rf != PII | xgbtree != PII)

#Add some more formats of phone number to the training set and change up tokenization
#a bit. I think instead of converting punctuation into a period, it's better to remove
#it entirely and concatenate. 

#Try 3 ----
#Create vector of 24 phone numbers with formats that the models missed
#Try 4: Vector of 42 phone numbers
phone_v2 <- c("+1 (285) 433-0125", "512-7683458", "+19852234891", "+1321-789-5346",
              "(417) 429.5164 ", "(330) 874-4141", "+1 (411) 205-4832 ", "(579)-1243961",
              "+1 (285) 433-0125", "512-7683458", "+19852234891", "+1321-789-5346",
              "(417) 429.5164 ", "(330) 874-4141", "+1 (411) 205-4832 ", "(579)-1243961",
              "+1 (285) 433-0125", "512-7683458", "+19852234891", "+1321-789-5346",
              "(417) 429.5164 ", "(330) 874-4141", "+1 (411) 205-4832 ", "(579)-1243961",
              "(105)3018143", "+1321-789-5346", "+1-879-231-4917", " 1-954-612-8392", 
              "(831)501-2839", "+19852234891",
              "(105)3018143", "+1321-789-5346", "+1-879-231-4917", " 1-954-612-8392", 
              "(831)501-2839", "+19852234891",
              "(105)3018143", "+1321-789-5346", "+1-879-231-4917", " 1-954-612-8392", 
              "(831)501-2839", "+19852234891")

phone_v2 <- as.data.frame(phone_v2)

#Combine with df
chief_complaints <- subset(d_train, select = c(ChiefComplaintOrig, ID), PII_phone == 0)

samp <- sample(nrow(chief_complaints), 42)

sample <- as.data.frame(chief_complaints[samp,])
phone_free <- as.data.frame(chief_complaints[-samp,])

df2 <- cbind(sample, phone_v2)
names(df2) <- c("cc", "ID", "phones")

#Combining the two columns into one
df2$ChiefComplaintOrig <- paste(df2$cc, df2$phones)

#Marking all rows with phone numbers as PII = 1
df2$PII_phone <- 1

names(phone_free) <- c("ChiefComplaintOrig", "ID")

phone_free$PII_phone <- 0

df2 <- rbind.fill(df2, phone_free)

#Add back in the 2k original phone number observations
og_phones <- subset(d_train, PII_phone == 1, select = -c(PII_ssn, PII, id))

df3 <- rbind(og_phones, df2)

#Add PII_ssn back in
df_ssn <- subset(d_train, select = c(PII_ssn, ID))

df3 <- merge(df3, df_ssn, by = "ID")

#Recreating PII variable
df3$PII <- ifelse(df3$PII_phone + df3$PII_ssn > 0, 1, 0)

#New tokenization
create_dict <- function(data){
  #Remove snomed codes
  #Changed d to w to catch snomeds with characters as well as numbers. May
  #be a mistake.
  corpus <- str_remove_all(data$ChiefComplaintOrig, 
                           '_;[\\da-zA-Z]+|;[\\da-zA-Z]+;[\\da-zA-Z]*|[\\da-zA-Z]+;[\\da-zA-Z]*')
  
  #Removing dates
  corpus <- str_remove_all(corpus, "\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}")
  
  #REMOVING all punctuation fully wrapped in digits with a .
  corpus <- str_remove_all(corpus, '(?<=\\b[0-9]{1,100})[-/.,;:]')
  
  #Replacing () with PAREN, to maybe try and catch things like area codes.
  corpus <- str_replace_all(corpus, '[()]', 'PAREN')
  
  #Keep the + sign
  corpus <- str_replace_all(corpus, "\\+", "plussign")
  
  
  #Removing any spaces between numbers
  #corpus <- str_remove_all(corpus, '(?<=\\b[0-9]{1,100})\\s')
  
  corpus <- str_replace_all(corpus, 
                            "[0-9]", "Z")
  
  #Replacing - with space
  corpus <- str_replace_all(corpus, "-", " ")
  
  #Add space after parenzzzparen
  corpus <- str_replace_all(corpus, "parenzzzparen", "parenzzzparen ")
  
  #Tokenization
  corpus <- str_remove_all(corpus, '["(),;]')
  corpus <- purrr::map(corpus, function(x) str_split(tolower(x),"\\s+") %>% unlist) 
  
  #Remove stop-words ("the", "in", etc.)
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))])
  
  #stem
  corpus <- purrr::map(corpus, function(x) text_tokens(x, stemmer="en") %>% unlist)
  
  #Count word frequency and remove rare words
  words <- as.data.frame(sort(table(unlist(corpus)), decreasing=T), stringsAsFactors = F)
  words <- words$Var1[which(words$Freq >=15)]
  
  return(words)  
}

dict <- create_dict(df3)


create_dtm <- function(data){
  #Remove snomed codes
  #Changed d to w to catch snomeds with characters as well as numbers. May
  #be a mistake.
  corpus <- str_remove_all(data$ChiefComplaintOrig, 
                           '_;[\\da-zA-Z]+|;[\\da-zA-Z]+;[\\da-zA-Z]*|[\\da-zA-Z]+;[\\da-zA-Z]*')
  
  #Removing dates
  corpus <- str_remove_all(corpus, "\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}")
  
  #REMOVING all punctuation fully wrapped in digits with a .
  corpus <- str_remove_all(corpus, '(?<=\\b[0-9]{1,100})[-/.,;:]')
  
  #Replacing () with PAREN, to maybe try and catch things like area codes.
  corpus <- str_replace_all(corpus, '[()]', 'PAREN')
  
  #Keep the + sign
  corpus <- str_replace_all(corpus, "\\+", "plussign")
  
  
  #Removing any spaces between numbers
  #corpus <- str_remove_all(corpus, '(?<=\\b[0-9]{1,100})\\s')
  
  corpus <- str_replace_all(corpus, 
                            "[0-9]", "Z")
  
  #Replacing - with space
  corpus <- str_replace_all(corpus, "-", " ")
  
  #Add space after parenzzzparen
  corpus <- str_replace_all(corpus, "parenzzzparen", "parenzzzparen ")
  
  #Tokenization
  corpus <- str_remove_all(corpus, '["(),;]')
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

dtm_train <- create_dtm(df3)
dtm_test <- create_dtm(d_test)

mod <- train_model(df3, dtm_train, "PII")
mod

#98.72% accuracy, with 23 false negatives
#Try 2: 99.5% accuracy, 9 false negatives
#Try 3: 99.5% accuracy, 9 false negatives
#Try 4: 99.1% accuracy, 7 false negatives, 9 false positives
#Try 5: 99.5% accuracy, 9 false negatives
evaluate_model(mod, d_test, "PII", dtm_test)

#Random forest
dtm_train_rf <- dtm_train
dtm_train_rf$PII <- df3$PII

classifier = randomForest(x = dtm_train_rf[-length(dtm_train_rf)],
                          y = dtm_train_rf$PII,
                          ntree = 10)

dtm_test_rf <- dtm_test
dtm_test_rf$PII  <- d_test$PII

y_pred = predict(classifier, newdata = dtm_test_rf[-length(dtm_test_rf)])

y_pred_round <- round(y_pred)

#98.67% accuracy, with 24 false negatives
#Try 2: 99.94% accuracy, 1 false positive
#Try 3: 99.94% accuracy, 1 false positive, again.
#Try 4: 99.94% accuracy, 1 false positive, AGAIN
#Try 5: 99.89% accuracy. 1 false positive, 1 false negative
cm=table(y_pred_round,dtm_test_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy

#What are they?
d_test$rf <- y_pred_round
d_test$xgbtree <- predictions <- predict(mod, newdata = dtm_test)

wrong <- subset(d_test, rf != PII | xgbtree != PII)

#Eh, it's the deleting of spaces between digits that's throwing it off. They're all
#observations with numbers before the PII (like COVID-19).
#Try 2: Mostly wrong-number-of-digit SSNs for xgbTree, but the RF didn't have that
#problem. Try QA set now!

#QA try 3 ----
#Just SSN
qa_ssn <- readxl::read_xlsx("~/PII Project/QA Test Data (SSN).xlsx")
names(qa_ssn) <- c("ChiefComplaintOrig", "PII")

dtm_qa_ssn <- create_dtm(qa_ssn)

#xgbTree
#Accuracy: 90.91%
#5 false negatives, 4 false positives. Same as before, weirdly.
#V2: 90.92% accuracy, 5 false positives and 3 false negatives.
evaluate_model(mod, qa_ssn, "PII", dtm_qa_ssn)


#Random forest
dtm_qa_ssn_rf <- dtm_qa_ssn
dtm_qa_ssn_rf$PII  <- qa_ssn$PII

y_pred = predict(classifier, newdata = dtm_qa_ssn_rf[-length(dtm_qa_ssn_rf)])

y_pred_round <- round(y_pred)

#89.9% accuracy, with 6 false positives and 4 false negatives.
cm=table(y_pred_round,dtm_qa_ssn_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy

qa_ssn$rf <- y_pred_round
qa_ssn$xgbtree <- predictions <- predict(mod, newdata = dtm_qa_ssn)
wrong <- subset(qa_ssn, rf != PII | xgbtree != PII)
#Interestingly, some of the ones it got "wrong" seemed to contain PII, just not
#SSN. Things like phone numbers, which is what this model is meant to do, so
#I think the accuracy is higher than we though.
#Also, a few were wrong bc the PII was found AFTER the snomeds, which are 
#deleted in the tokenization process.

#Just phone
qa_phone <- readxl::read_xlsx("~/PII Project/QA Test Data (Phone).xlsx")
names(qa_phone) <- c("ChiefComplaintOrig", "PII")

dtm_qa_phone <- create_dtm(qa_phone)

#xgbTree
#Accuracy: 85%
#15 false negatives. Again, more wrong than right on this one.
#V2: Same outcome
#V3: Same outcome
evaluate_model(mod, qa_phone, "PII", dtm_qa_phone)

#Random forest
dtm_qa_phone_rf <- dtm_qa_phone
dtm_qa_phone_rf$PII  <- qa_phone$PII

y_pred = predict(classifier, newdata = dtm_qa_phone_rf[-length(dtm_qa_phone_rf)])

y_pred_round <- round(y_pred)

#93% accuracy, with 7 false negatives.
#V2: 97% accuracy, which is a slight improvement.
#3 false negatives.
#V3, same.
cm=table(y_pred_round,dtm_qa_phone_rf$PII)
cm
accuracy=((cm[1,1]+cm[2,2])/sum(cm))*100
accuracy

#Which ones are wrong? Looking only at RF because it seems to perform
#much better
qa_phone$rf <- y_pred_round
#qa_phone$xgbtree <- predictions <- predict(mod, newdata = dtm_qa_phone)

#Still missing a few formats. Things like (105)3018143 and +1321-789-5346.
#I think I need to add in more with the 1 at the beginning, and also find
#a better way to include the parentheses. 
#V2: I have added the parentheses, but I think I will add a space after any
#parenzzzparen so that the ones that are included in with the full thing will not
#be missed.
wrong <- subset(qa_phone, rf != PII)

#CCQV Testing ----
#A new function that saves the corpus to compare, to see what tokenization is doing
create_corpus <- function(data){
  #Remove snomed codes
  #Changed d to w to catch snomeds with characters as well as numbers. May
  #be a mistake.
  corpus <- str_remove_all(data$ChiefComplaintOrig, 
          '_;[\\da-zA-Z]+|;[\\da-zA-Z]+;[\\da-zA-Z]*|[\\da-zA-Z]+;[\\da-zA-Z]*')
  
  #Removing dates
  corpus <- str_remove_all(corpus, "\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}")
  
  #REMOVING all punctuation fully wrapped in digits with a .
  # corpus <- str_remove_all(corpus, '(?<=\\b[0-9]{1,100})[-/.,;:]')
  # 
  # #Replacing () with PAREN, to maybe try and catch things like area codes.
  # corpus <- str_replace_all(corpus, '[()]', 'PAREN')
  # 
  # #Keep the + sign
  # corpus <- str_replace_all(corpus, "\\+", "plussign")
  # 
  # 
  # #Removing any spaces between numbers
  # #corpus <- str_remove_all(corpus, '(?<=\\b[0-9]{1,100})\\s')
  # 
  # corpus <- str_replace_all(corpus, 
  #                           "[0-9]", "Z")
  
  #Replacing - with space
  corpus <- str_replace_all(corpus, "-", " ")
  
  #Add space after parenzzzparen
  corpus <- str_replace_all(corpus, "parenzzzparen", "parenzzzparen ")
  
  return(corpus)
}

#Data
load(file="~/PII Project/ccqv_sample.rda")

names(ccqv_sample) <- "ChiefComplaintOrig"

tictoc::tic()
ccqv_sample$corpus_check <- create_corpus(ccqv_sample)
tictoc::toc()

tictoc::tic()
dtm_ccqv <- create_dtm(ccqv_sample)
tictoc::toc()

#Save the dtm since it takes 18 hours to run
save(dtm_ccqv, file = "~/PII Project/ccqv_dtm_phonessn_v2.rda")

#xgbTree
#18,935 flags
#V2: 23,981 flags
#V3: 13,238 flags
ccqv_sample$predictions_xgbTree <- predict(mod, newdata = dtm_ccqv)

#random forest
#4,388 flags
#V2: 4,637 flags. Weird
#V3: 5,925 flags. It went up! :c
y_pred = predict(classifier, newdata = dtm_ccqv)

ccqv_sample$predictions_rf <- round(y_pred)

#Flagged only
#Generally not looking bad, there are a lot of correct flags! But also there
#are some things like, dates 01/01/2023 being mistaken, and also just observations
#that are long with a lot of different numbers sprinkled throughout.
#I don't think we can do much about the second, but the tokenizer could specifically
#exclude dates perhaps.
#V2: Looks like I got rid of the dates, but the snowmeds still aren't being properly
#gotten rid of. I'm going to mess with it here in the flagged section.
#V3: Looking a lot better! I think time to save and send to Natasha and Jeremy.
flagged <- subset(ccqv_sample, predictions_xgbTree == 1 | predictions_rf == 1)

phone_ssn_rf_predictions <- subset(flagged, predictions_rf == 1)

save(phone_ssn_rf_predictions, file = "~/PII Project/phone_ssn_rf_predictions.rda")

#Also saving the rf model
save(classifier, file = "~/PII Project/phone_ssn_rf_classifier.rda")
