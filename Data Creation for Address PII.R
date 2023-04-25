#Author: Alyssa Venn
#Date created: 3/27/2023

#Libraries
library(tidyverse)
library(doParallel)

#Data import
set.seed(13)
# Set the directory where the CSV files are located
csv_directory <- "~/PII Project/python_project/addresses"

# Get a list of all CSV files in the directory
csv_files <- list.files(csv_directory, pattern = "*.csv", full.names = TRUE)

# Initialize an empty DataFrame to store the combined data
combined_data <- data.frame()

# Loop through each CSV file and append its data to the combined data
for (csv_file in csv_files) {
  data <- read_csv(csv_file)
  
  data$state <- substr(csv_file, nchar(csv_file)-5, nchar(csv_file)-4)
    
  combined_data <- rbind(combined_data, data)
}

  # Reset the row names of the combined data
row.names(combined_data) <- NULL

#So, the final address will include number, street, city, postcode, and state, potentially.
#State is in shorthand lowercase format, so some should be converted to uppercase, some
#should be fully spelled out (in a variety of cases).
#Number and street should always be included, but city, postcode, and state are not
#necessary to be PII and so should be randomly included or not.
#Typos should be randomly incorporated. 
#Separators should be randomly generated, of a random length. Can be spaces, commas, 
#periods, or back/forward slashes. 
#Combined_data has 75,068,091 observations. This better be the sickest model ever.

#Save file
save(combined_data, file = "~/PII Project/python_project/combined_addresses.rda")

#Read in the above file
load(file = "~/PII Project/python_project/combined_addresses.rda")

#Randomly sample 10 million of them
combined_data <- subset(combined_data, NUMBER != "" & STREET != "")
index <- sample(1:nrow(combined_data), size = 10000000, replace = FALSE)
df_sample <- combined_data[index, ]

save(df_sample, file = "~/PII Project/python_project/sample_addresses.rda")

load(file = "~/PII Project/python_project/sample_addresses.rda")

#Sample 1000 of them, just for testing
index <- sample(1:nrow(df_sample), size = 1000, replace = FALSE)
df_sample_small <- df_sample[index, ]

save(df_sample_small, file = "~/PII Project/python_project/sample_addresses_small.rda")

df_sample <- df_sample_small

#State ID ----
#Maybe convert half to their full state names
state_names <- c("AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona",
                 "AR" = "Arkansas", "CA" = "California", "CO" = "Colorado",
                 "CT" = "Connecticut", "DE" = "Delaware", "FL" = "Florida",
                 "GA" = "Georgia", "HI" = "Hawaii", "ID" = "Idaho",
                 "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
                 "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana",
                 "ME" = "Maine", "MD" = "Maryland", "MA" = "Massachusetts",
                 "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi",
                 "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska",
                 "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey",
                 "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina",
                 "ND" = "North Dakota", "OH" = "Ohio", "OK" = "Oklahoma",
                 "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island",
                 "SC" = "South Carolina", "SD" = "South Dakota", "TN" = "Tennessee",
                 "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont",
                 "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia",
                 "WI" = "Wisconsin", "WY" = "Wyoming")

#Select half of the dataframe
index <- sample(1:nrow(df_sample), size = nrow(df_sample)/2, replace = FALSE)
df_longstates <- df_sample[index, ]
df_shortstates <- df_sample[-index, ]

# Use match() to get the full state names
df_longstates$state <- state_names[match(toupper(df_longstates$state), names(state_names))]

#Re-combine long and short states
df <- rbind(df_longstates, df_shortstates)

#Combining with separators
#So, separator needs to be randomly selected. Number of separators needs to be randomly
#selected. And which values are included needs to be randomly selected.
#Number, street always included. Unit, include when it's there since it's rare
#anyways. City, state, postcode, not always. Perhaps give each of them a 50% chance of being there?
#Sometimes it'll be just one, sometimes two, sometimes all three, sometimes none. 

#Function for creating a random separator
random_separator <- function(min_length = 2, max_length = 4, possible_sep_chars = ",./\\  ") {
  # generate a vector of space characters of random length between min_length and max_length
  chars <- rep(" ", sample(min_length:max_length, size = 1))
  if (length(chars) > 0 & !is.null(possible_sep_chars)) {
    # randomly select a separator character from possible_sep_chars and replace a random space with it
    sep_char <- substr(possible_sep_chars, sample(nchar(possible_sep_chars), size = 1), 1)
    chars[sample(length(chars), size = 1)] <- sep_char
  }
  # concatenate the vector of characters into a string
  return(paste(chars, collapse = ""))
}


#Address faster
vec_sep <- sample(c(' ', '  ', '   '), nrow(df), replace = TRUE)
df$separator <- vec_sep

#First turn all NAs into "" 
df$NUMBER[is.na(df$NUMBER)] <- ""
df$STREET[is.na(df$STREET)] <- ""

df$Address <- paste0(df$NUMBER, df$separator, df$STREET)

#Unit faster
vec_sep <- replicate(nrow(df), random_separator())
df$separator <- vec_sep
df$Address <- ifelse(is.na(df$UNIT), df$Address, 
                       paste0(df$Address, df$separator, df$UNIT))

#State, city, zip code faster
#Edit 4/5/2023: Actually, not sure if we should include state/city/zip?
#It's really rare in the sample of real CCQV addresses, seemingly,
#and could cause the model to weight it too highly.
df$concatenated <- apply(df, 1, function(x) {
  # Create a vector with the three columns in a random order
  selected_cols <- sample(c("CITY", "state", "POSTCODE"), 3)
  
  # Only include each column with a 50% chance
  selected_cols <- sample(selected_cols, 3, replace = FALSE, prob = c(0.1, 0.05, 0.1))
  
  # Create a vector with the selected values and separators
  values <- c(
    ifelse(is.na(x[selected_cols[1]]), "", x[selected_cols[1]]),
    random_separator(),
    ifelse(is.na(x[selected_cols[2]]), "", x[selected_cols[2]]),
    random_separator(),
    ifelse(is.na(x[selected_cols[3]]), "", x[selected_cols[3]])
  )
  
  #print(values)
  # Combine the values into a single string and remove the trailing separator
  if (length(values) == 0) {
    return("")
  } else {
    return(paste(values, collapse = ""))
  }
})

#Add df$concatenated to df$address
vec_sep <- replicate(nrow(df), random_separator())
df$separator <- vec_sep
df$Address <- paste0(df$Address, df$separator, df$concatenated)

#Save
save(df, file = "~/PII Project/python_project/noisy_addresses.rda")


#They also need to be inserted into the CCQV data. First, I need to get 10,000,000 
#examples of CCQV data
# * Scan through multiple database tables -----
library(tidyverse)
library(DBI)
library(odbc)
library(foreach)
library(doParallel)
# create list of items to cycle through (e.g., site names)
source("~/HTML Reports/query_functions.R")
dim_site <- function(db = "BIOSENSE_DW_DB"){
  
  qry <- paste0("SELECT * FROM [", db, "].[dbo].[Dim_Site]")
  
  cubes_hitter(qry)
  
} 

sites <- dim_site()
site_list <- as.vector(sites$Site_Short_Name)
# set-up cluster
cl <- makeCluster(21)
registerDoParallel(cl)
# cycle through each value
library(tidyverse)
  library(DBI)
  library(odbc)
  
  #Figure out how to look at specific sites for cache_er_base
  con <- dbConnect(odbc::odbc(), dsn = "PROD_NSSP_WEB")
  ssn_qry <- paste0("SELECT TOP (10000000) [ChiefComplaintOrig] ",
                    "FROM Cache_ER_Base WITH (NOLOCK) ",
                    "ORDER BY NEWID()")
  
  ssn_example <- dbGetQuery(con, ssn_qry)
  
  dbDisconnect(con)


stopCluster(cl)

save(ssn_example, file = "~/PII Project/python_project/large_ccdd_sample.rda")

load(file = "~/PII Project/python_project/large_ccdd_sample.rda")
#Inserting Address into the CCQV data, at random points, not just the end.
#For some addresses, I want to start them with an @, like we see in the
#real addresses from CCQV. Some with no space, some with a space. 
#Some will begin with "resides at" or "picked up at."
#Some wrapped in parentheses (with or without spaces)
#some will begin with "Pt from" or "picked up from" or "patient from"
#Some will just be stuck in there randomly
phrases <- c("(", "@", " ", "resides at", "picked up at", "pt from", "picked up from",
             "patient from", "at", "from")

address_phrase <- sample(phrases, nrow(df), replace = TRUE, 
            prob = c(0.028, 0.027, 0.75, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027,
                     0.027))
df$address_phrase <- address_phrase

vec_sep <- replicate(nrow(df), random_separator(min_length = 1, max_length = 3, 
                                                possible_sep_chars = " "))
df$separator <- vec_sep
df$Address_v2 <- paste0(df$address_phrase, df$separator, df$Address)

#Add a parenthesis at the end if there's one at the beginning
df$Address_v2 <- ifelse(str_detect(df$Address_v2, "\\("), paste0(df$Address_v2, 
                                                               sample(c('', ' ', ' '), 1), ")"), 
                        df$Address_v2)

#Randomize whether they are ALL CAPS, Hump Case, or lower case. 
#Do that with the whole address column perhaps? If Address is ALL UPPER,
#the other words should be, too
#Exception is state.... hm. 
#The addresses themselves already vary in case. Maybe specifically adjust 
#state to vary in capitalization, and then the added keywords mentioned above
#will vary in case, unrelated to the addresses themselves. 

test <- df[1:100,]
test2 <- as.data.frame(ssn_example[1:100,])
names(test2) <- "ChiefComplaintOrig"

# Define a function to insert the new string into the original string

insert_string <- function(original_string, new_string) {
  # Check that original_string is not empty
  if (nchar(original_string) == 0 | is.na(original_string)) {
    return(new_string)
  }
  
  words <- unlist(strsplit(original_string, " "))
  insert_point <- sample(seq_along(words), 1)
  
  if (insert_point == 1) {
    words <- c(new_string, words)
    new_string <- paste(words, collapse = " ")
    return(new_string)
  }
  
  
  words <- c(words[1:(insert_point-1)], new_string, words[insert_point:length(words)])
  new_string <- paste(words, collapse = " ")
  return(new_string)
}

# Apply the function to each row of the dataframe using parallel processing
library(parallel)
cl <- makeCluster(21)
registerDoParallel(cl)
df_final <- mcmapply(insert_string, ssn_example$ChiefComplaintOrig, 
                     df$Address_v2, SIMPLIFY = TRUE)
stopCluster(cl)

df_final <- as.data.frame(df_final)
names(df_final) <- "ChiefComplaintOrig"
df_final$Address <- df$Address
df_final$CCO_old <- ssn_example$ChiefComplaintOrig

save(df_final, file = "~/PII Project/python_project/combo_addresses_ccqv_2.rda")

#OLD CODE ----
#Combine number, street, and unit
#I think number and street will pretty  much always only have a space separating
#them
df$Address <- ""

library(foreach)
library(doParallel)

# set up parallel processing
#NO!!!!!!!!
#cl <- makeCluster(detectCores())
#Try this
#cl <- makeCluster(4 cores)
registerDoParallel(cl)

tictoc::tic()

# apply the paste function in parallel
df$Address <- foreach(i = 1:nrow(df), .combine = c) %dopar% {
  paste(df$NUMBER[i], df$STREET[i], sep = random_separator(possible_sep_chars = " "))
}


# stop the parallel processing
stopCluster(cl)

tictoc::toc()

test <- df[1:1000000,]


#Old address code

for (i in 1:nrow(df)) {
  df$Address[i] <- paste(df$NUMBER[i], df$STREET[i], sep = random_separator(possible_sep_chars = " "))
}

save(df, file = "~/PII Project/python_project/noisy_addresses_incomplete.rda")

#If UNIT exists, add it to the address, otherwise do nothing
for (i in 1:nrow(df)) {
  df$Address[i] <- ifelse(is.na(df$UNIT[i]), df$Address[i], paste(df$Address[i], df$UNIT[i], sep = random_separator()))
}

#50% of the time, add post code, state, or city
for (i in 1:nrow(df)) {
  #Randomize an order
  vector <- c("CITY", "POSTCODE", "state")
  index <- sample(1:3, size = 3, replace = FALSE)
  
  for (j in index) {
    random_number <- sample(1:2, size = 1)
    
    name <- vector[j]
    
    if (random_number == 1 & !is.na(df[i,name])){
      df$Address[i] <- paste(df$Address[i], df[i,name], sep = random_separator())
    }
  }
  
}

#Save for work on Monday
save(df, file = "~/PII Project/python_project/noisy_addresses.rda")

#Paste addresses into CCQV data
#Sample data from Cache_ER_Base DB in NSSP. 6000 random rows from between
#3-30-2022 through 8-03-2022
load(file="~/PII Project/sample_dataset.rda")

chief_complaints <- as.data.frame(sample_dataset$ChiefComplaintOrig)

samp <- sample(nrow(chief_complaints), 1000)

sample <- as.data.frame(chief_complaints[samp,])

addresses_only <- as.data.frame(df$Address)

sample$ChiefComplaintOrig<- paste(sample$`chief_complaints[samp, ]`, 
                                             addresses_only$`df$Address`)

#Combine with real addresses from CCQV
names(sample) <- c("ccdd", "ChiefComplaintOrig")

sample$address <- df$Address

sample$cc_address_flag <- 1

real_addresses <- subset(test, select = c("ChiefComplaintOrig", "address", "cc_address_flag"))
real_addresses$ccdd <- NA

test_2 <- rbind(sample, real_addresses)

#Save
save(test_2, file = "~/PII Project/python_project/training_set_small.rda")

#Pull down 10 million, do a 70/30 split of training/test
#Use some cross-validating to avoid overtraining


