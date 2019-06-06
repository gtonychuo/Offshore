library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(reshape2)
library(tm)

# K = read_excel("./keyworddetection/10805-Keywords-of-Interest-0520.xlsx")$Keywords
K = read_csv("./keyworddetection/keyword_new.csv")$word
Z = read_csv("./keyworddetection/wind_pos_new.csv")
# attr(Z, "spec") = NULL
# Z$pos_ %>% factor
# Z$is_alpha %>% as.logical()
# Z$is_stop %>% as.logical()
K = subset(hc_keyword,label==4)$word %>% as.character
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
  filter(Weight > 90)

termmatrix = filter_nn %>% 
  dcast(formula = Target ~ Source,value.var = c("Weight"))

rownames(termmatrix) <- as.character(termmatrix$Target)
colnames(termmatrix)
termDocMatrix = termmatrix[,-1]
termDocMatrix[1:10,1:10]

termDocMatrix = as.matrix(termDocMatrix)
termDocMatrix[is.na(termDocMatrix)] = 0
termDocMatrix[termDocMatrix>1] = 1

termtermMatrix <- termDocMatrix %*% t(termDocMatrix) 

hc = termtermMatrix %>% dist %>% hclust
plot(hc)
k =4
rect.hclust(hc, k=k, border="red")
g = cutree(hc,k=k)        # cut into K clusters
hc_keyword = data.frame(word = hc$labels,label=g)
table(g) %>% as.vector %>% sort         # sizes of clusters

write.csv(filter_nn, "group4.csv", quote=F, row.names=F,
          fileEncoding="utf-8")
