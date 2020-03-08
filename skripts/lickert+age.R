library(haven)
library(data.table)
library(ggplot2)
library(tidyverse)
library(cowplot)

#read in data
database <- read_stata("ZA4586_v1-0-0.dta")
database2016 <- database[!(database$year!=2016),]
database2006 <- database[!(database$year!=2006),]
items <- c("age","mp02","mp03","mp04","mp05","mp06","mp07","mp08")

database2016[database2016 == -9] <- NA
database2016$age[database2016$age == -32] <- NA
database2006[database2006 == -9] <- NA
database2006$age[database2006$age == -32] <- NA

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

df <- as.data.frame(do.call(rbind, list(c(0,0,0,0,0,0,0))))
colnames(df) <- c("1", "2", "3","4","5","6","7")
for(i in 1:5){
  df <- rbind(df, c(0,0,0,0,0,0,0))
}
rownames(df) <- c("18-25", "26-35", "36-45", "46-55", "56-65", "66+")

for(i in 1:nrow(database2006)){
  age <- database2006[i,"age"]
  dlickert <- as.character(round(as.double(database2006[i,"lickert"])))
  if(age<26){
    df[1,dlickert] <- df[1,dlickert] + 1
    next()
  }
  if(age<36){
    df[2,dlickert] <- df[2,dlickert] + 1
    next()
  }
  if(age<46){
    df[3,dlickert] <- df[3,dlickert] + 1
    next()
  }
  if(age<56){
    df[4,dlickert] <- df[4,dlickert] + 1
    next()
  }
  if(age<66){
    df[5,dlickert] <- df[5,dlickert] + 1
    next()
  }
  if(age>65){
    df[6,dlickert] <- df[6,dlickert] + 1
    next()
  }
}

df$p1 <- 0
df$p2 <- 0 
df$p3 <- 0
df$p4 <- 0
df$p5 <- 0
df$p6 <- 0
df$p7 <- 0

for(i in 1:nrow(df)){
  df[i,"p1"] <- df[i,"1"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"]) 
  df[i,"p2"] <- df[i,"2"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p3"] <- df[i,"3"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p4"] <- df[i,"4"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p5"] <- df[i,"5"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p6"] <- df[i,"6"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p7"] <- df[i,"7"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
}

dfc06 <- data.frame(alter = 1:6,
                    sehrschlecht = df$`1`,
                    schlecht = df$`2`,
                    eherschlecht = df$`3`,
                    mittel = df$`4`,
                    ehergut = df$`5`,
                    gut = df$`6`,
                    sehrgut = df$`7`)
#that function will melt for every political attitude and every possible lickert score the related value that the persons have
dfc06 <- melt(dfc06 ,  id.vars = 'alter', variable.name = 'EinstellungAsyl')

#same for our percentage data
dfp06 <- data.frame(alter = 1:6,
                    sehrschlecht = df$p1,
                    schlecht = df$p2,
                    eherschlecht = df$p3,
                    mittel = df$p4,
                    ehergut = df$p5,
                    gut = df$p6,
                    sehrgut = df$p7)

dfp06 <- melt(dfp06 ,  id.vars = 'alter', variable.name = 'EinstellungAsyl')

pc_06 <- dfc06 %>% mutate(EinstellungAsyl = factor(EinstellungAsyl)) %>% #we need to factor our data so ggplot can work with it
  ggplot() +
  aes(x = alter, y = value, fill = EinstellungAsyl) + #specify the axsis and the variable where to find the number of interviewees
  geom_bar(stat = "identity") + #tell ggplot its a bar plot
  scale_x_continuous("Age groups", labels = c("18-25", "26-35", "36-45", 
                                                 "46-55", "56-65", "66+"), 
                     breaks = 1:6) +
  theme_bw() + #set the theme
  theme(legend.position = "none") + #position the legend
  ylab("Number of respondents")
pc_06

pp_06 <- ggplot(dfp06, aes(x = alter, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Age groups", labels = c("18-25", "26-35", "36-45", 
                                                 "46-55", "56-65", "66+"), 
                     breaks = 1:6) +
  scale_y_continuous("Percentage of respondents", labels = scales::percent) + #set the scale to percentage instead of numeric value
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))  #set the corresponding values to the plot
pp_06

#reset the dataframe
df[,] <- 0

for(i in 1:nrow(database2016)){
  age <- database2016[i,"age"]
  dlickert <- as.character(round(as.double(database2016[i,"lickert"])))
  if(age<26){
    df[1,dlickert] <- df[1,dlickert] + 1
    next()
  }
  if(age<36){
    df[2,dlickert] <- df[2,dlickert] + 1
    next()
  }
  if(age<46){
    df[3,dlickert] <- df[3,dlickert] + 1
    next()
  }
  if(age<56){
    df[4,dlickert] <- df[4,dlickert] + 1
    next()
  }
  if(age<66){
    df[5,dlickert] <- df[5,dlickert] + 1
    next()
  }
  if(age>65){
    df[6,dlickert] <- df[6,dlickert] + 1
    next()
  }
}

for(i in 1:nrow(df)){
  df[i,"p1"] <- df[i,"1"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"]) 
  df[i,"p2"] <- df[i,"2"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p3"] <- df[i,"3"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p4"] <- df[i,"4"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p5"] <- df[i,"5"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p6"] <- df[i,"6"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p7"] <- df[i,"7"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
}

dfc16 <- data.frame(alter = 1:6,
                    verybad = df$`1`,
                    bad = df$`2`,
                    ratherbad = df$`3`,
                    neutral = df$`4`,
                    rathergood = df$`5`,
                    good = df$`6`,
                    verygood = df$`7`)
#that function will melt for every political attitude and every possible lickert score the related value that the persons have
dfc16 <- melt(dfc16 ,  id.vars = 'alter', variable.name = 'Opinion')

#same for our percentage data
dfp16 <- data.frame(alter = 1:6,
                    verybad = df$p1,
                    bad = df$p2,
                    ratherbad = df$p3,
                    neutral = df$p4,
                    rathergood = df$p5,
                    good = df$p6,
                    verygood = df$p7)

dfp16 <- melt(dfp16 ,  id.vars = 'alter', variable.name = 'EinstellungAsyl')

pc_16 <- dfc16 %>% mutate(Opinion= factor(Opinion)) %>% #we need to factor our data so ggplot can work with it
  ggplot() +
  aes(x = alter, y = value, fill = Opinion) + #specify the axsis and the variable where to find the number of interviewees
  geom_bar(stat = "identity") + #tell ggplot its a bar plot
  scale_x_continuous("Age groups", labels = c("18-25", "26-35", "36-45", 
                                                 "46-55", "56-65", "66+"), 
                     breaks = 1:6) +
  theme_bw() + #set the theme
  theme(legend.position = c(0.1, 0.7)) + #position the legend
  ylab("Number of respondents")

pp_16 <- ggplot(dfp16, aes(x = alter, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Age groups", labels = c("18-25", "26-35", "36-45", 
                                                 "46-55", "56-65", "66+"), 
                     breaks = 1:6) +
  scale_y_continuous("Percentage of respondents", labels = scales::percent) + #set the scale to percentage instead of numeric value
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) #set the corresponding values to the plot

plot_grid(pp_16, pc_16, pp_06, pc_06, labels = c("16", "16", "06", "06"))

