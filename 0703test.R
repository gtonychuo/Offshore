library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
load("./lda.rdata")
entity= read_csv("./topic model/E.csv")
# 需要篩選與wind_pos相同筆數
wind= read_csv("./topic model/0413_news.csv")
wind = subset(wind,!is.na(wind$text))
wind$index = 1:nrow(wind)

# 篩選與lda dtm想同的文件數
a = dtm_clean@Dimnames[[1]] %>% as.integer
q = wind[wind$index %in% a,]

# 每個文章的主題分佈
offshore_theme = doc_topic_distr %>% as.data.frame
offshore_theme$index = 1:nrow(offshore_theme)
company = subset(entity,class == "COM")


mx = sapply(company$alias, function(z) str_detect(q$text, regex(z, ignore_case = T)))
colnames(mx) = company$name

mx = as.data.frame(mx)
# mx$index = 1:nrow(mx)
mx_theme = cbind(mx,offshore_theme)
mx_theme$sub = q$sub

mx_theme$date = as.Date(q$date,"%d/%m/%Y")

min(mx_theme$date , na.rm = TRUE)
#"2010-02-17"
max(mx_theme$date, na.rm = TRUE)
#2019-04-12

clean_mx_theme = subset(mx_theme,!is.na(mx_theme$date))
S = clean_mx_theme %>% mutate(quarter = quarters(date)) %>%
  mutate(quarter = paste(year(date) ,quarter,sep = ""))
# 去除不需要的V16
S$V16 <- NULL
S2 = S %>%
  group_by(quarter)%>%
  summarise(total = sum(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V17,V18),
            V1 =sum(V1)/total,
            V2 =sum(V2)/total,
            V3 =sum(V3)/total,
            V4 =sum(V4)/total,
            V5 =sum(V5)/total,
            V6 =sum(V6)/total,
            V7 =sum(V7)/total,
            V8 =sum(V8)/total,
            V9 =sum(V9)/total,
            V10 =sum(V10)/total,
            V11 =sum(V11)/total,
            V12 =sum(V12)/total,
            V13 =sum(V13)/total,
            V14 =sum(V14)/total,
            V15 =sum(V15)/total,
            V17 =sum(V17)/total,
            V18 =sum(V18)/total
  )
# 集合了公司數 x 文件數 = 72 * 10756 = 774432筆
CS = gather(S, key = company_name, value= tag,1:72)

empty_theme = data.frame(quarter = unique(CS$quarter))
company_name = unique(CS$company_name)
# 找出個別公司有出現文件，並計算每個quarter總和，才可以轉換成該公司在各主題的比例

colSums(mx) %>% sort() 
# 前10高的公司
top10 = c("SHL","GE","SSE","EMO","OPT","Siemens","Orsted","RWE","MHI","TenneT")

plot_trend = function(x){
  y = CS %>%
    filter(company_name == x & tag == T) %>%
    group_by(quarter) %>% 
    summarise(
      total = sum(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V17,V18),
      V1 =sum(V1)/total,
      V2 =sum(V2)/total,
      V3 =sum(V3)/total,
      V4 =sum(V4)/total,
      V5 =sum(V5)/total,
      V6 =sum(V6)/total,
      V7 =sum(V7)/total,
      V8 =sum(V8)/total,
      V9 =sum(V9)/total,
      V10 =sum(V10)/total,
      V11 =sum(V11)/total,
      V12 =sum(V12)/total,
      V13 =sum(V13)/total,
      V14 =sum(V14)/total,
      V15 =sum(V15)/total,
      V17 =sum(V17)/total,
      V18 =sum(V18)/total
    )
  empty_theme_name = empty_theme
  empty_theme_name$company_name = x
  g = right_join(y,empty_theme_name,by ="quarter") 
  g[is.na(g)] = 0
  g %>% as.data.frame %>% gather(., key = topic, value=percent,3:19) %>% 
    mutate(topic = ifelse(topic =="V1","Vessels",
                          ifelse(topic =="V2","Enviromental Survey",
                                 ifelse(topic =="V3","European Wind Farm Dynamics",
                                        ifelse(topic =="V4","Metaocean Assessment",
                                               ifelse(topic=="V5","Ocean Energy",
                                                      ifelse(topic=="V6","North America Market Dynamics",
                                                             ifelse(topic=="V7","Global Market Dynamics",
                                                                    ifelse(topic=="V8","Underwater Technology",
                                                                           ifelse(topic=="V9","European Market Dynamics",
                                                                                  ifelse(topic=="V10","Floating Technology",
                                                                                         ifelse(topic=="V11","Grid Connection",
                                                                                                ifelse(topic=="V12","Port Services",
                                                                                                       ifelse(topic=="V13","Cable/ Subsea Cable",
                                                                                                              ifelse(topic=="V14","Electrical System",
                                                                                                                     ifelse(topic=="V15","Training",
                                                                                                                            ifelse(topic=="V16","",
                                                                                                                                   ifelse(topic=="V17","Emergency Handling",
                                                                                                                                          ifelse(topic=="V18","Wind Turbine",topic
                                                                                                                                          ))))))))))))))))))) %>% 
    ggplot(.,aes(x = quarter,y=percent,color=as.factor(topic))) +
    geom_point(alpha=0.3) + stat_smooth(aes(group=as.factor(topic)),method = "loess",span = .8,se=FALSE,alpha=0.6) +
    theme(axis.text.x = element_text(size = 10,angle = 45, hjust = 0.5, vjust = 0.5))+
    labs(color = "company",title = x)
  ggsave(paste("./company_trend/",x,".png",sep = ""),units = "mm",width = 400)
}
lapply(top10,plot_trend)

asd = find[1] %>% as.data.frame
asd = gather(asd, key = topic, value=percent,3:19)
ggplot(asd,aes(x = as.factor(quarter),y=percent,fill=as.factor(topic))) +
  geom_bar(stat = "identity")


ggplot(asd,aes(x = quarter,y=percent,color=as.factor(topic))) +
  geom_point(alpha=0.5) + stat_smooth(aes(group=as.factor(topic)),method = "loess",span = .6,se=FALSE) +
  theme(axis.text.x = element_text(size = 10,angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(color = "company")

asd = asd %>%
  mutate(topic = ifelse(topic =="V1","Vessels",
                        ifelse(topic =="V2","Enviromental Survey",
                               ifelse(topic =="V3","European Wind Farm Dynamics",
                                      ifelse(topic =="V4","Metaocean Assessment",
                                             ifelse(topic=="V5","Ocean Energy",
                                                    ifelse(topic=="V6","North America Market Dynamics",
                                                           ifelse(topic=="V7","Global Market Dynamics",
                                                                  ifelse(topic=="V8","Underwater Technology",
                                                                         ifelse(topic=="V9","European Market Dynamics",
                                                                                ifelse(topic=="V10","Floating Technology",
                                                                                       ifelse(topic=="V11","Grid Connection",
                                                                                              ifelse(topic=="V12","Port Services",
                                                                                                     ifelse(topic=="V13","Cable/ Subsea Cable",
                                                                                                            ifelse(topic=="V14","Electrical System",
                                                                                                                   ifelse(topic=="V15","Training",
                                                                                                                          ifelse(topic=="V16","",
                                                                                                                                 ifelse(topic=="V17","Emergency Handling",
                                                                                                                                        ifelse(topic=="V18","Wind Turbine",topic
                                                                                                                                        )))))))))))))))))))







mtcars$wt %>% class




S3 = S %>% 
  group_by(quarter)%>%
  mutate(total = sum(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V17,V18),
            V1 =sum(V1)/total,
            V2 =sum(V2)/total,
            V3 =sum(V3)/total,
            V4 =sum(V4)/total,
            V5 =sum(V5)/total,
            V6 =sum(V6)/total,
            V7 =sum(V7)/total,
            V8 =sum(V8)/total,
            V9 =sum(V9)/total,
            V10 =sum(V10)/total,
            V11 =sum(V11)/total,
            V12 =sum(V12)/total,
            V13 =sum(V13)/total,
            V14 =sum(V14)/total,
            V15 =sum(V15)/total,
            V17 =sum(V17)/total,
            V18 =sum(V18)/total
  )
tx1 <- gather(S2, key = topic, value= percent,3:19)
company2 <- gather(S3, key = topic, value= percent,73:89)
company3 <- gather(company2, key = company_name, value= tag,1:72)

company3 = company3 %>%
  mutate(topic = ifelse(topic =="V1","Vessels",
                        ifelse(topic =="V2","Enviromental Survey",
                               ifelse(topic =="V3","European Wind Farm Dynamics",
                                      ifelse(topic =="V4","Metaocean Assessment",
                                             ifelse(topic=="V5","Ocean Energy",
                                                    ifelse(topic=="V6","North America Market Dynamics",
                                                           ifelse(topic=="V7","Global Market Dynamics",
                                                                  ifelse(topic=="V8","Underwater Technology",
                                                                         ifelse(topic=="V9","European Market Dynamics",
                                                                                ifelse(topic=="V10","Floating Technology",
                                                                                       ifelse(topic=="V11","Grid Connection",
                                                                                              ifelse(topic=="V12","Port Services",
                                                                                                     ifelse(topic=="V13","Cable/ Subsea Cable",
                                                                                                            ifelse(topic=="V14","Electrical System",
                                                                                                                   ifelse(topic=="V15","Training",
                                                                                                                          ifelse(topic=="V16","",
                                                                                                                                 ifelse(topic=="V17","Emergency Handling",
                                                                                                                                        ifelse(topic=="V18","Wind Turbine",topic
                                                                                                                                        )))))))))))))))))))


tx2 = tx1 %>%
  mutate(topic = ifelse(topic =="V1","Vessels",
                        ifelse(topic =="V2","Enviromental Survey",
                               ifelse(topic =="V3","European Wind Farm Dynamics",
                                      ifelse(topic =="V4","Metaocean Assessment",
                                             ifelse(topic=="V5","Ocean Energy",
                                                    ifelse(topic=="V6","North America Market Dynamics",
                                                           ifelse(topic=="V7","Global Market Dynamics",
                                                                  ifelse(topic=="V8","Underwater Technology",
                                                                         ifelse(topic=="V9","European Market Dynamics",
                                                                                ifelse(topic=="V10","Floating Technology",
                                                                                       ifelse(topic=="V11","Grid Connection",
                                                                                              ifelse(topic=="V12","Port Services",
                                                                                                     ifelse(topic=="V13","Cable/ Subsea Cable",
                                                                                                            ifelse(topic=="V14","Electrical System",
                                                                                                                   ifelse(topic=="V15","Training",
                                                                                                                          ifelse(topic=="V16","",
                                                                                                                                 ifelse(topic=="V17","Emergency Handling",
                                                                                                                                        ifelse(topic=="V18","Wind Turbine",topic
                                                                                                                                        )))))))))))))))))))



