##################################################
rm(list=ls(all=T)); gc()
pacman::p_load(magrittr,visNetwork,reshape2,stringr,dplyr)
load("./C3.rdata")

#####  REBUILD DATA FROM CSV
E = read.csv("E.csv",stringsAsFactors=F)
E =  E[!duplicated(E$name),]
mx = sapply(E$alias, function(z) str_count(S$tx, z) > 0)
E$freq = colSums(mx)
colnames(mx) = E$name
i = order(E$class, -colSums(mx))
E = E[i,]; mx = mx[,i]
write.csv(E, "E.csv", row.names=F)

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
#  save(E, S, XX, CO, CR, file="C3.rdata" )

##### TESTING
s0 = table(E$class) %>% cumsum %>% as.integer
s1 = rbind(COM=c(2,10),COUNTRY=c(4,7),FARM=c(3,12),
           GOV=c(4,8),KWORD=c(3,7),ORG=c(3,7))
si = do.call(c, lapply(1:6, function(i) s0[i]+(s1[i,1]:s1[i,2]) ))
x = CO[[1]][si,si]
# x = CR[[1]][si,si]
nodes = data.frame(
  id = 1:nrow(x), label=E$name[si], title=E$name[si], 
  group=E$class[si], value=sqrt(diag(x)), 
  stringsAsFactors=F)

colnames(x) = rownames(x) = 1:nrow(x)
x[lower.tri(x,T)] = 0
x = subset(melt(x), value > 0)

K = 0.8
links = subset(x, value >= quantile(value, K))
links$value = sqrt(links$value)
colnames(links)[1:2] = c("from","to")
i = 1:nrow(nodes) %in% unique(c(links[,1], links[,2]))
nodes$hidden = !i; nodes$physics = i

visNetwork(nodes, links) %>% 
  # visEdges(smooth=F) %>% 
  visPhysics("forceAtlas2Based") %>% 
  visLegend(width=0.1, stepX=100, stepY=50) %>% 
  visInteraction(navigationButtons=T) %>% 
  visOptions(manipulation=T, highlightNearest=T)

save(E, S, XX, CO, CR, file="C3.rdata" )