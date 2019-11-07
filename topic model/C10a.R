# S$tx[ str_detect(S$tx, "Hung Hua")]
# str_extract_all(S$tx, "Hung Hua( Construction)?( Co\\. Ltd\\.)?|\\bHHC\\b", ) %>% unlist %>% table
# 
# S$tx[ str_detect(S$tx, "GeoSea") ]
# str_extract_all(S$tx, "\\bGeoSea\\b", ) %>% unlist %>% table
# 
# str_extract_all(S$tx, "\\bActeon\\b", ) %>% unlist %>% table
# S$tx[ str_detect(S$tx, "cteon") ]
# 
# "Siem Offshore( Contractors( GmbH )?)?|Seaway Offshore Cables|\bSOC\b"
# S$tx[ str_detect(S$tx, "\\bSOC\\b") ]  # 77
# S$tx[ str_detect(S$tx, "Siem Offshore( Contractors( GmbH )?)?") ]  # 92
# S$tx[ str_detect(S$tx, "Seaway Offshore( Cables)?") ]  # 6

pacman::p_load(stringr,dplyr)
load("data/udPOS10.rdata")
rm(UD); gc()
E = read.csv("E10a.csv",stringsAsFactors=F) # 633
which(duplicated(E$name))

mx = sapply(1:nrow(E), function(i) str_detect(
  S$tx, regex(E$alias[i], ignore_case=E$ignore[i]) ))
dim(mx) # 132054    633
colnames(mx)=E$name
E$freq = colSums(mx); range(E$freq) # 6, 14203
i = order(E$class, -colSums(mx))
E = E[i,]; mx = mx[,i]
# E[E$freq < 10,] %>% View
# E = subset(E, freq >=10)
table(E$class=="ORG", is.na(E$sub_class))
write.csv(E, "E10a.csv", row.names=F, quote=T)

xSent = mx 
xPara = t(sapply(
  split(data.frame(mx), group_indices(S, id, paragraph_id)),
  colSums)) > 0
xDocu = t(sapply(split(data.frame(mx), S$id), colSums)) > 0
XX = list(Sent=xSent, Para=xPara, Docu=xDocu)
CO = list(Sent = t(xSent) %*% xSent,
          Para = t(xPara) %*% xPara,
          Docu = t(xDocu) %*% xDocu)
CR = list(Sent = cor(xSent),
          Para = cor(xPara),
          Docu = cor(xDocu))

save(X, E, S, XX, CO, CR, file="data/C10a.rdata" )

