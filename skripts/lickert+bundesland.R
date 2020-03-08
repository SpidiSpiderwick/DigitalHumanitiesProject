library(haven)
library(tidyverse)
library(viridisLite)
library(viridis)
library(sf)
library(cowplot)


#read in data
load("de_Laender.RData")
database <- read_stata("ZA4586_v1-0-0.dta")
database2016 <- database[!(database$year!=2016),]
database2006 <- database[!(database$year!=2006),]
items <- c("land","mp02","mp03","mp04","mp05","mp06","mp07","mp08")

database2016[database2016 == -9] <- NA
database2006[database2006 == -9] <- NA

database2006 <- database2006[complete.cases(database2006),items]
database2006$mp04 = 7 + 1 - database2006$mp04
database2006$mp04 = 7 + 1 - database2006$mp04
database2006$mp04 = 7 + 1 - database2006$mp04
database2006$lickert <- rowSums(database2006[,tail(items,7)])/7

database2016 <- database2016[complete.cases(database2016),items]
database2016$mp04 = 7 + 1 - database2016$mp04
database2016$mp04 = 7 + 1 - database2016$mp04
database2016$mp04 = 7 + 1 - database2016$mp04
database2016$lickert <- rowSums(database2016[,tail(items,7)])/7

df <- as.data.frame(do.call(cbind, list(1:16)))
bundeslaender <- list("Schleswig-Holstein","Hamburg","Niedersachsen","Bremen","Nordrhein-Westfalen",
                      "Hessen","Rheinland-Pfalz","Baden-Württemberg","Bayern","Saarland","Berlin",
                      "Brandenburg","Mecklenburg-Vorpommern","Sachsen","Sachsen-Anhalt","Thüringen")
rownames(df) <- bundeslaender
colnames(df) <- c("Lickertdurschnitt")
df$GEN <- factor(unlist(bundeslaender))

  for(i in 1:length(bundeslaender)){
    if(i==11){
      df[bundeslaender[[11]],1] <- mean(database2006$lickert[database2006$land == "111" || database2006$land == "112"])
      next()
    }
    df[bundeslaender[[i]],1] <- mean(database2006$lickert[database2006$land == (i*10)])
  }

df %>% left_join(de_Laender) %>%
  ggplot +
  aes(fill = Lickertdurschnitt, geometry = geometry) +
  theme_void() +
  scale_fill_viridis(limits = c(3.4,4.2)) +
  labs(caption = "Attitudes towards asylum applicants\nthe more yellow the more positive",
       fill = "") +
  geom_sf() -> p_de2
p_de2

for(i in 1:length(bundeslaender)){
  if(i==11){
    df[bundeslaender[[11]],1] <- mean(mean(database2016$lickert[database2016$land == "111"]), 
                                      mean(database2016$lickert[database2016$land == "112"]))
    next()
  }
  df[bundeslaender[[i]],1] <- mean(database2016$lickert[database2016$land == (i*10)])
}
df %>% left_join(de_Laender) %>%
  ggplot +
  aes(fill = Lickertdurschnitt, geometry = geometry) +
  theme_void() +
  scale_fill_viridis(limits = c(3.4,4.2)) +
  labs(caption = "Attitudes towards asylum applicants\nthe more yellow the more positive",
       fill = "") +
  geom_sf() -> p_de2016
p_de2016
df[1,1] <- mean(database2016$lickert[database2016$land == "111" || database2016$land == "112"])

plot_grid(p_de2,p_de2016, labels = c("06","16"))
  
