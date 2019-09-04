# 匯入基本套件
options(digits = 10)
pacman::p_load(dplyr,readr,udpipe,text2vec,ggplot2,stringr,tm)

# 匯入資料集
# 最新版本
offshore = read_csv("./topic model/offshore_wind_pos_0807.csv")
# offshore = read_csv("./topic model/offshore_wind_pos0723.csv")
entity = read_csv("./topic model/E5.csv")

offshore %>% filter(subject == "Grid Connection") %>% group_by(title) %>%  summarise(n =n(),date= date[1],tx = sentences[1]) -> y
table(y$date)
z = X %>% filter(subject == "Grid Connection")
# 篩選只有名詞的字彙
clean_offshore  = offshore %>%
  filter(!(text %in% stopwords()) & is_stop == F & pos_ %in% c("PROPN","NOUN"))

# 轉換文件字頻表成dtm
x <- document_term_frequencies(clean_offshore,document = "title", term = "lemma_")
dtm = document_term_matrix(x)
dim(dtm)

# 基本資料篩除
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 20)
dim(dtm_clean)

dtm_clean <- dtm_remove_tfidf(dtm_clean,cutoff = 0.03)
dim(dtm_clean)

# 取得詞頻，tf-idf
feq = txt_freq(offshore$text)
tfidf <-dtm_tfidf(dtm_clean)
hist(tfidf, breaks = "scott")

tfidf_ranking = sort(tfidf, decreasing = TRUE) %>% as.data.frame
colnames(tfidf_ranking) = "tfidf"
tfidf_ranking$key = rownames(tfidf_ranking)

entity2 =  str_replace_all(entity$name," ","_")

new_words = filter(tfidf_ranking,!(tfidf_ranking$key %in% entity2))
new_words = left_join(new_words,feq,by="key")

summary(new_words$tfidf)
summary(new_words$freq)

filterd = filter(new_words,tfidf>0.4 & freq > 100)

write.csv(filterd,"filter_keyworde.csv")
# dtm_clean <- dtm_remove_tfidf(dtm_clean, top=50)
# dim(dtm_clean)
# dtm_clean[1:2,1:10]

# m <- LDA(dtm_clean, k = 4, method = "Gibbs", 
#          control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
# m = LDA(dtm_clean,k = 10,control = list(seed=1234))
# m
set.seed(12345)
lda_model = LDA$new(n_topics = 24, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm_clean, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)


# 主題與字的分佈
topic_word_distr = lda_model$topic_word_distribution

save(doc_topic_distr,topic_word_distr, file = "nn_24.rdata")

# 取得個主題前20重要的字
c = lda_model$get_top_words(n = 20, lambda = 0.5)



# save(lda_model,dtm_clean,file = "./lda.rdata")

# 取得視覺化的結果
lda_model$plot(out.dir = './0728/30')
servr::daemon_stop(3)
write.csv(c, "./0728/24/top20words.csv")
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


