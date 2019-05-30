library(readr)
library(readxl)
library(stringr)
library(dplyr)

# K = read_excel("./keyworddetection/10805-Keywords-of-Interest-0520.xlsx")$Keywords
K = read_csv("./keyworddetection/keyword_new.csv")$word
Z = read_csv("./keyworddetection/wind_pos_new.csv")
attr(Z, "spec") = NULL
# Z$pos_ %>% factor
# Z$is_alpha %>% as.logical()
# Z$is_stop %>% as.logical()

t0 = Sys.time()
mx = sapply(Z$sentence, str_detect, fixed(K, ignore_case=T)) %>% t
Sys.time() - t0


nvEdges = do.call(rbind, lapply(which(colSums(mx) > 0), function(i) {
  df = Z$lemma_[mx[,i] & !Z$is_stop & Z$pos_=="VERB"] %>%
    table %>% data.frame %>% setNames(c("Source", "Weight"))
  df$Source = as.character(df$Source)
  df$Target = K[i]
  df$Type = "Undirected"
  df }))

nnEdges = do.call(rbind, lapply(which(colSums(mx) > 0), function(i) {
  df = Z$lemma_[mx[,i] & !Z$is_stop & Z$pos_ %in% c("NOUN", "PROPN")] %>% 
    table %>% data.frame %>% setNames(c("Source", "Weight"))
  df$Source = as.character(df$Source)
  df$Target = K[i]
  df$Type = "Undirected"
  df }))

filter_nn = nnEdges %>% 
  filter(Weight > 10)

write.csv(filter_nn, "nnedges.csv", quote=F, row.names=F,
          fileEncoding="utf-8")

K

