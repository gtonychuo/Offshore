# library(topicmodels)
# library(tm)
# library(reshape2)
options(digits = 10)
library(dplyr)
library(readr)
library(udpipe)
library(text2vec)

offshore = read_csv("./topic model/offshore_wind_pos.csv")

# dtm = offshore %>%
#   group_by(title,text) %>% 
#   summarise(
#     f = n()
#   ) %>% dcast(title ~ text,value.var = "f")
clean_offshore  = offshore %>% filter(pos_ %in% c("PROPN","NOUN") & is_stop == F)
x <- document_term_frequencies(clean_offshore,document = "title", term = "lemma_")
dtm = document_term_matrix(x)
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
dim(dtm_clean)
# dtm_clean <- dtm_remove_tfidf(dtm_clean, top=50)
# dim(dtm_clean)
# dtm_clean[1:2,1:10]

# m <- LDA(dtm_clean, k = 4, method = "Gibbs", 
#          control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
# m = LDA(dtm_clean,k = 10,control = list(seed=1234))
# m
lda_model = LDA$new(n_topics = 10, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm_clean, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)
# 取得個主題前10重要的字
lda_model$get_top_words(n = 10, topic_number = 1:10, lambda = 1)

# 取得視覺化的結果
lda_model$plot()
servr::daemon_stop(1)
# 輸入doc index 可以得知文件屬於最高機率的主題
which.max(doc_topic_distr[1,])

