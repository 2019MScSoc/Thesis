library(SpeedReader)
library(quanteda)
library(readtext)
library(wordcloud)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('~/Box Sync/*OxfordMSc/Thesis/')

select_random_comments <- function(data, feature, dfm, seed) {
  idx <- which(featnames(dfm) == feature)
  df <- convert(dfm[,idx], to = 'data.frame')
  docs <- df$document[df[,2] != 0]
  docs_text <- data[data$id %in% docs,]
  docs_text$pos_score[docs_text$score <= 0] <- 0
  docs_text$pos_score[docs_text$score > 0] <- docs_text$score[docs_text$score > 0]
  set.seed(seed)
  weighted_sample <- dplyr::sample_n(docs_text, size = 30, weight = docs_text$pos_score)
  return(weighted_sample)
}

load('./ProcessedData/RDataAug12_full.RData')
load('./ProcessedData/RDataAug12_trimmed.RData')

################################################################################
############ Analysis Before Removing Comments Based on Length #################
################################################################################

# calculate cosine similarity
full_dfm <- rbind(progun_dfm, liberal_dfm, guns_dfm)
docvars(full_dfm, 'class') <- c(rep('liberal', ndoc(liberal_dfm)), 
                                rep('progun', ndoc(progun_dfm)),
                                rep('guns', ndoc(guns_dfm)))
grouped <- dfm_group(full_dfm, groups = 'class')
textstat_simil(grouped, method = 'cosine')

# fw 1: /r/progun vs /r/liberalgunowners
comp1 <- rbind(progun_dfm, liberal_dfm)
docvars(comp1, 'class') <- c(rep('progun', ndoc(progun_dfm)), 
                             rep('liberal', ndoc(liberal_dfm)))
comp1_dtm <- convert_quanteda_to_slam(comp1)
doc_covariates1 <- data.frame(sub = docvars(comp1, 'class'))
cont_table1 <- contingency_table(metadata = doc_covariates1,
                                 document_term_matrix = comp1_dtm)
fw1 <- feature_selection(cont_table1, alpha = mean(ntoken(comp1)), 
                         method = 'informed Dirichlet')

# fw: /r/guns vs. /r/liberalgunowners 
comp2 <- rbind(guns_dfm, liberal_dfm)
docvars(comp2, 'class') <- c(rep('guns', ndoc(guns_dfm)),
                             rep('liberal', ndoc(liberal_dfm)))
comp2_dtm <- convert_quanteda_to_slam(comp2)
doc_covariates2 <- data.frame(sub = docvars(comp2, 'class'))
cont_table2 <- contingency_table(metadata = doc_covariates2,
                                 document_term_matrix = comp2_dtm)
fw2 <- feature_selection(cont_table2, alpha = mean(ntoken(comp2)),
                         method = 'informed Dirichlet')

# fw: /r/guns vs. /r/progun
comp3 <- rbind(guns_dfm, progun_dfm)
docvars(comp3, 'class') <- c(rep('guns', ndoc(guns_dfm)),
                             rep('progun', ndoc(progun_dfm)))
comp3_dtm <- convert_quanteda_to_slam(comp3)
doc_covariates3 <- data.frame(sub = docvars(comp3, 'class'))
cont_table3 <- contingency_table(metadata = doc_covariates3,
                                 document_term_matrix = comp3_dtm)
fw3 <- feature_selection(cont_table3, alpha = mean(ntoken(comp3)),
                         method = 'informed Dirichlet')

################################################################################
############### Analysis After Removing Comments Based on Length ###############
################################################################################

# calculate cosine similarity 
full_trimmed_dfm <- rbind(progun_dfm_trimmed,
                          liberal_dfm_trimmed,
                          guns_dfm_trimmed)
docvars(full_trimmed_dfm, 'class') <- c(rep('progun', ndoc(progun_dfm_trimmed)),
                                        rep('liberal', ndoc(liberal_dfm_trimmed)),
                                        rep('guns', ndoc(guns_dfm_trimmed)))
grouped_trimmed <- dfm_group(full_trimmed_dfm, groups = 'class')
textstat_simil(grouped_trimmed, method = 'cosine')

# fw: /r/liberalgunowners vs. /r/progun
comp1_trimmed <- rbind(liberal_dfm_trimmed, progun_dfm_trimmed)
docvars(comp1_trimmed, 'class') <- c(rep('liberal', ndoc(liberal_dfm_trimmed)),
                                     rep('progun', ndoc(progun_dfm_trimmed)))
comp1_trimmed_dtm <- convert_quanteda_to_slam(comp1_trimmed)
doc_covariates1_trimmed <- data.frame(sub = docvars(comp1_trimmed, 'class'))
cont_table1_trimmed <- contingency_table(metadata = doc_covariates1_trimmed,
                                         document_term_matrix = comp1_trimmed_dtm)
fw1_trimmed <- feature_selection(cont_table1_trimmed, 
                                 alpha = mean(ntoken(comp1_trimmed)),
                                 method = 'informed Dirichlet')

# fw: /r/guns vs. /r/liberalgunowners
comp2_trimmed <- rbind(guns_dfm_trimmed, liberal_dfm_trimmed)
docvars(comp2_trimmed, 'class') <- c(rep('guns', ndoc(guns_dfm_trimmed)),
                                     rep('liberal', ndoc(liberal_dfm_trimmed)))
comp2_trimmed_dtm <- convert_quanteda_to_slam(comp2_trimmed)
doc_covariates2_trimmed <- data.frame(sub = docvars(comp2_trimmed, 'class'))
cont_table2_trimmed <- contingency_table(metadata = doc_covariates2_trimmed,
                                         document_term_matrix = comp2_trimmed_dtm)
fw2_trimmed <- feature_selection(cont_table2_trimmed, 
                                 alpha = mean(ntoken(comp2_trimmed)),
                                 method = 'informed Dirichlet')

# fw: /r/guns vs. /r/progun
comp3_trimmed <- rbind(guns_dfm_trimmed, progun_dfm_trimmed)
docvars(comp3_trimmed, 'class') <- c(rep('guns', ndoc(guns_dfm_trimmed)),
                                     rep('progun', ndoc(progun_dfm_trimmed)))
comp3_trimmed_dtm <- convert_quanteda_to_slam(comp3_trimmed)
doc_covariates3_trimmed <- data.frame(sub = docvars(comp3_trimmed, 'class'))
cont_table3_trimmed <- contingency_table(metadata = doc_covariates3_trimmed,
                                         document_term_matrix = comp3_trimmed_dtm)
fw3_trimmed <- feature_selection(cont_table3_trimmed, 
                                 alpha = mean(ntoken(comp3_trimmed)))

# comparing trimmed and non-trimmed (comparison 1)

length(intersect(fw1_trimmed$liberal$term[1:20], fw1$liberal$term[1:20]))
length(intersect(fw1_trimmed$progun$term[1:20], fw1$progun$term[1:20]))
length(intersect(fw1_trimmed$progun$term[1:30], fw1$progun$term[1:30]))

# comparing trimmed and non-trimmed (comparison 2) 
length(intersect(fw2_trimmed$liberal$term[1:20], fw2$liberal$term[1:20]))
length(intersect(fw2_trimmed$guns$term[1:20], fw2$guns$term[1:20]))

# comparing trimmed and non-trimmed (comparison 3)
length(intersect(fw3_trimmed$progun$term[1:20], fw3$progun$term[1:20]))
length(intersect(fw3_trimmed$guns$term[1:20], fw3$guns$term[1:20]))

# random sampling for key features

liberal_liber <- select_random_comments(liberal_trimmed,
                                        'liber',
                                        liberal_dfm_trimmed,
                                        5)

guns_post <- select_random_comments(guns_trimmed,
                                    'post',
                                    guns_dfm_trimmed,
                                    42)

progun_govern <- select_random_comments(progun_trimmed,
                                        'govern',
                                        progun_dfm_trimmed,
                                        10)

liberal_govern <- select_random_comments(liberal_trimmed,
                                         'govern', 
                                         liberal_dfm_trimmed,
                                         12)

progun_crime <- select_random_comments(progun_trimmed,
                                       'crime',
                                       progun_dfm_trimmed,
                                       44)

liberal_crime <- select_random_comments(liberal_trimmed,
                                        'crime',
                                        liberal_dfm_trimmed,
                                        50)

progun_crime <- select_random_comments(progun_trimmed,
                                       'crime',
                                       progun_dfm_trimmed,
                                       100)

liberal_nra <- select_random_comments(liberal_trimmed,
                                      'nra',
                                      liberal_dfm_trimmed,
                                      420)
