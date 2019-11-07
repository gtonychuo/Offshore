pacman::p_load(dplyr, stringr, googleVis, d3heatmap, RColorBrewer)
load("topic model/trend/C6.rdata")
load("topic model/trend/09_12_nn_24.rdata")

pSpectral = brewer.pal(11,"Spectral")
TP = data.frame(doc_topic_distr) %>% setNames(c(
  "Subsea Investigation","Subsea Cables","Ocean Energy","OWF O&M",
  "Wind Turbines Test and Verify",	"Talents Training","UK OWF Dynamics",
  "Maritime Engineering","Global OWF Dynamics I","Grid Connection",	
  "Scientific Researches","Underwater Technology","Global OWF Dynamics II",
  "Energy Issues","Foundations",	"North America Market Dynamics","Vessels",
  "Environmental Survey","Metaocean and Weather Assessment",	
  "Asian Market Dynamics","Wind Turbines","Business Management Issues I",
  "Business Management Issues II",	"OWF Tenders"
  ))

# averaged topic weight per year
split(TP, format(X$date,'%Y')) %>% sapply(colMeans) %>% 
  d3heatmap(T, F, col=rev(pSpectral),yaxis_font_size='10px')
c = split(TP, X$source)
c
# averaged topic weight by sources
split(TP, X$source) %>% sapply(.['OSW'],colMeans) %>% 
  d3heatmap(T, F, col=rev(pSpectral),yaxis_font_size='10px')

# averaged topic weight by subjects
split(TP, X$subject) %>% sapply(colMeans) %>% 
  {.[,colnames(.)!="subseacable"]} %>% d3heatmap(
    T, T, col=rev(pSpectral),yaxis_font_size='10px',xaxis_font_size='10px')

# averaged topic weight by countries
sapply(which(E$class=="COUNTRY"), function(i){ 
  colMeans(TP[str_detect(X$tx, E$alias[i]),]) 
  }) %>% data.frame %>% 
  setNames(subset(E,class=="COUNTRY")$name) %>% 
  select(-Vietnam) %>% d3heatmap(
    col=rev(pSpectral),yaxis_font_size='10px',xaxis_font_size='10px')







