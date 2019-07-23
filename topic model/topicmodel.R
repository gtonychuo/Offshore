# library(topicmodels)
library(tm)
# library(reshape2)
options(digits = 10)
library(dplyr)
library(readr)
library(udpipe)
library(text2vec)
library(ggplot2)

offshore = read_csv("./topic model/offshore_wind_pos0723.csv")

# dtm = offshore %>%
#   group_by(title,text) %>% 
#   summarise(
#     f = n()
#   ) %>% dcast(title ~ text,value.var = "f")
clean_offshore  = offshore %>% filter(pos_ %in% c("PROPN","NOUN") & is_stop == F)

clean_offshore  = offshore %>% filter(!text %in% stopwords() & is_stop == F & pos_ %in% c("PROPN","NOUN"))

x <- document_term_frequencies(offshore,document = "title", term = "lemma_")
dtm = document_term_matrix(x)
dim(dtm)


dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 20)
dim(dtm_clean)

dtm_clean <- dtm_remove_tfidf(dtm_clean,cutoff = 0.03)
dim(dtm_clean)
# dtm_clean <- dtm_remove_tfidf(dtm_clean, top=50)
# dim(dtm_clean)
# dtm_clean[1:2,1:10]

# m <- LDA(dtm_clean, k = 4, method = "Gibbs", 
#          control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
# m = LDA(dtm_clean,k = 10,control = list(seed=1234))
# m
set.seed(12345)
lda_model = LDA$new(n_topics = 18, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm_clean, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)
# 取得個主題前20重要的字
c = lda_model$get_top_words(n = 20, lambda = 0.5)

write.csv(c, "./0723new/18/top20words.csv")

# save(lda_model,dtm_clean,file = "./lda.rdata")

# 取得視覺化的結果
lda_model$plot(out.dir = './0723new/30')
servr::daemon_stop(3)
# 輸入doc index 可以得知文件屬於最高機率的主題
which.max(doc_topic_distr[1,])

barplot(doc_topic_distr[1, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1), 
        names.arg = 1:ncol(doc_topic_distr))

# 複雜度計算，越低越好
perplexity(dtm, topic_word_distribution = lda_model$topic_word_distribution,
           doc_topic_distribution = doc_topic_distr)


perplexity_df <- data.frame(perplexity=numeric())
topics <- c(1:25)
set.seed(12345)
for (i in topics){
  lda_model = 
    LDA$new(n_topics = i, 
            doc_topic_prior = 0.1, topic_word_prior = 0.01)
  doc_topic_distr = 
    lda_model$fit_transform(dtm_clean, n_iter = 1000, convergence_tol = 0.01, 
                            check_convergence_every_n = 10)
  topic_word_distr_10 = lda_model$topic_word_distribution
  perplexity_df[i,1] <-perplexity(dtm_clean, topic_word_distr_10, doc_topic_distr)
}
perplexity_df$log_per = log(perplexity_df$perplexity)
g <- ggplot(data=perplexity_df, aes(x= as.numeric(row.names(perplexity_df)))) +
  labs(y="Perplexity",x="Number of topics") +
  ggtitle("Perplexity of data") +
  geom_line(aes(y=log_per), colour="red")
g

hc = doc_topic_distr %>% dist%>% hclust(method = "ward.D2")
plot(hc)
rect.hclust(hc, k=16, border="red")


