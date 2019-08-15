library(SpeedReader)
library(quanteda)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('~/Box Sync/*OxfordMSc/Thesis/')

# function converts variations of U.S. and U.K. abbreviations to uniform token
us_uk <- function(data) {
  
  data$clean_body <- gsub(' U.S. ', ' U_S_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' u.s. ', ' U_S_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' U.S ', ' U_S_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' u.s ', ' U_S_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' US ', ' U_S_ ', data$clean_body, fixed = TRUE)
  
  data$clean_body <- gsub(' U.K. ', ' U_K_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' u.k. ', ' U_K_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' U.k ', ' U_K_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' u.k ', ' U_K_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' UK ', ' U_K_ ', data$clean_body, fixed = TRUE)
  data$clean_body <- gsub(' uk ', ' U_K_ ', data$clean_body, fixed = TRUE)
  
  return(data)
  
}

# function for created Document Feature Matrices
create_dfm <- function(data, class) {
  data <- data[!duplicated(data$id),]
  data$class <- class
  corp <- corpus(data, docid_field = 'id', text_field = 'clean_body')
  docvars(corp, field = 'class') <- data.frame(data$class)
  dfm <- tokens_select(tokens(corp, remove_punct = TRUE, remove_symbols = TRUE),
                       pattern = stopwords('en'),
                       selection = 'remove') %>% dfm(stem = TRUE)
  dfm_new <- dfm_subset(dfm, ntoken(dfm) > 0)
  return(dfm_new)
}

################################################################################
################ Before Removing Comments Based on Length ######################
################################################################################

# read in data
progun <- read.csv('./ProcessedData/progunAug7.csv', as.is = TRUE)
liberal <- read.csv('./ProcessedData/liberalAug7.csv', as.is = TRUE)
guns_a <- read.csv('./ProcessedData/guns_aAug7.csv', as.is = TRUE)
guns_b <- read.csv('./ProcessedData/guns_bAug7.csv', as.is = TRUE)
guns_full <- rbind(guns_a, guns_b)

# remove remaining quoted material
progun_idx <- grep('>', progun$clean_body)
progun$clean_body[progun_idx] <- gsub('>.*', '', progun$clean_body[progun_idx])
progun_raw <- progun
progun$body_length <- nchar(progun$clean_body)
progun <- progun[progun$body_length > 0,]
progun$first_char <- substr(progun$clean_body, 1, 1)

liberal_idx <- grep('>', liberal$clean_body)
liberal$clean_body[liberal_idx] <- gsub('>.*', '', liberal$clean_body[liberal_idx])
liberal_raw <- liberal
liberal$body_length <- nchar(liberal$clean_body)
liberal <- liberal[liberal$body_length > 0,]

guns_idx <- grep('>', guns_full$clean_body)
guns_full$clean_body[guns_idx] <- gsub('>.*', '', guns_full$clean_body[guns_idx])
guns_raw <- guns_full
guns_full$body_length <- nchar(guns_full$clean_body)
guns_full <- guns_full[guns_full$body_length > 0,]

# standardize U.S. and U.K. abbreviations in each comment set 
progun <- us_uk(progun)
liberal <- us_uk(liberal)
guns_full <- us_uk(guns_full)

# create DFMs for analysis
progun_dfm <- create_dfm(progun, 'progun')
liberal_dfm <- create_dfm(liberal, 'progun')
guns_dfm <- create_dfm(guns_full, 'guns')

save(progun_dfm, liberal_dfm, guns_dfm, progun, liberal, guns_full,
     file = './ProcessedData/RDataAug12_full.RData')

################################################################################
################## After Removing Comments Based on Length #####################
################################################################################

# filter data based on comment length
progun_trimmed <- progun[progun$body_length > 49 & progun$body_length < 751,]
liberal_trimmed <- liberal[liberal$body_length > 49 & liberal$body_length < 751,]
guns_trimmed <- guns_full[guns_full$body_length > 49 & guns_full$body_length < 751,]

# create_dfms
progun_dfm_trimmed <- create_dfm(progun_trimmed, 'progun')
liberal_dfm_trimmed <- create_dfm(liberal_trimmed, 'liberal')
guns_dfm_trimmed <- create_dfm(guns_trimmed, 'guns')

save(progun_dfm_trimmed, liberal_dfm_trimmed, guns_dfm_trimmed,
     progun_trimmed, liberal_trimmed, guns_trimmed,
     file = './ProcessedData/RDataAug12_trimmed.RData')








