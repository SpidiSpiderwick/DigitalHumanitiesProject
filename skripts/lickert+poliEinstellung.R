#by Christopher Penndorf
library(haven)
library(tidyverse)
library(cowplot)
  
#read the whole data and extract the relevant data
database <- read_stata("ZA4586_v1-0-0.dta")
database2016 <- database[!(database$year!=2016),]
database2006 <- database[!(database$year!=2006),]

#create a vector with all the data we need for the visualization
items <- c("pa01","mp02","mp03","mp04","mp05","mp06","mp07","mp08")

#create a new datatable with our interesting items
mpdata06 <- database2006[ ,items]
#filter "Not stated"
mpdata06[mpdata06 == -9] <- NA
#pole the variables correctly
mpdata06$mp04 = 7 + 1 - mpdata06$mp04
mpdata06$mp06 = 7 + 1 - mpdata06$mp06
mpdata06$mp07 = 7 + 1 - mpdata06$mp07

#delete all rows that have "NA" in them
mpdata06 <- mpdata06[complete.cases(mpdata06),]
#create a new collum for our lickert score and round it 
mpdata06$lickert <- rowSums(mpdata06[,tail(items,7)])/7
mpdata06[,"lickert"] <- round(mpdata06[,"lickert"])

#do the same for the 2016 data
mpdata16 <- database2016[ ,items]
mpdata16[mpdata16 == -9] <- NA
mpdata16$mp04 = 7 + 1 - mpdata16$mp04
mpdata16$mp06 = 7 + 1 - mpdata16$mp06
mpdata16$mp07 = 7 + 1 - mpdata16$mp07

mpdata16 <- mpdata16[complete.cases(mpdata16),]
mpdata16$lickert <- rowSums(mpdata16[,tail(items,7)])/7
mpdata16[,"lickert"] <- round(mpdata16[,"lickert"])

#create a new dataframe which has as columns for all possible lickert values and rows
#all possible political attitudes
df <- as.data.frame(do.call(rbind, list(c(0,0,0,0,0,0,0))))
colnames(df) <- c("1", "2", "3","4","5","6","7")
for(i in 1:9){
  df <- rbind(df, c(0,0,0,0,0,0,0))
}
rownames(df) <- c("1","2","3","4","5","6","7","8","9","10")

#for every row in our whole database, count how many people with which political attitude
#have which lickert score
for(i in 1:nrow(mpdata06)){
  polEinstellung <- as.character(mpdata06[i,"pa01"]) #thats our political attitude
  lickert <- as.character(mpdata06[i,"lickert"]) #thats our related lickert score
  df[polEinstellung,lickert] <- df[polEinstellung,lickert] + 1 #the is the cell we want to change
}
#create new collums for the corresponding percentage value
df$p1 <- 0
df$p2 <- 0 
df$p3 <- 0
df$p4 <- 0
df$p5 <- 0
df$p6 <- 0
df$p7 <- 0

#calc the percentage value
for(i in 1:nrow(df)){
  df[i,"p1"] <- df[i,"1"]/rowSums(df[i,]) 
  df[i,"p2"] <- df[i,"2"]/rowSums(df[i,]) 
  df[i,"p3"] <- df[i,"3"]/rowSums(df[i,]) 
  df[i,"p4"] <- df[i,"4"]/rowSums(df[i,]) 
  df[i,"p5"] <- df[i,"5"]/rowSums(df[i,]) 
  df[i,"p6"] <- df[i,"6"]/rowSums(df[i,]) 
  df[i,"p7"] <- df[i,"7"]/rowSums(df[i,])
}

#create a new dataframe that has the right form for ggplot to plot our stackted bar plot
dfc06 <- data.frame(poliEinstellung = 1:10,
                  verybad = df$`1`,
                  bad = df$`2`,
                  ratherbad = df$`3`,
                  neutral = df$`4`,
                  rathergood = df$`5`,
                  good = df$`6`,
                  verygood = df$`7`)
#that function will melt for every political attitude and every possible lickert score the related value that the persons have
dfc06 <- melt(dfc06 ,  id.vars = 'poliEinstellung', variable.name = 'Opinion')

#same for our percentage data
dfp06 <- data.frame(poliEinstellung = 1:10,
                  sehrschlecht = df$p1,
                  schlecht = df$p2,
                  eherschlecht = df$p3,
                  mittel = df$p4,
                  ehergut = df$p5,
                  gut = df$p6,
                  sehrgut = df$p7)

dfp06 <- melt(dfp06 ,  id.vars = 'poliEinstellung', variable.name = 'EinstellungAsyl')

#reset the dataframe
df[,] <- 0

#do the counting for the 2016 data
for(i in 1:nrow(mpdata16)){
  polEinstellung <- as.character(mpdata16[i,"pa01"])
  lickert <- as.character(mpdata16[i,"lickert"])
  df[polEinstellung,lickert] <- df[polEinstellung,lickert] + 1
}

#calc the percentage again
for(i in 1:nrow(df)){
  df[i,"p1"] <- df[i,"1"]/rowSums(df[i,]) 
  df[i,"p2"] <- df[i,"2"]/rowSums(df[i,]) 
  df[i,"p3"] <- df[i,"3"]/rowSums(df[i,]) 
  df[i,"p4"] <- df[i,"4"]/rowSums(df[i,]) 
  df[i,"p5"] <- df[i,"5"]/rowSums(df[i,]) 
  df[i,"p6"] <- df[i,"6"]/rowSums(df[i,]) 
  df[i,"p7"] <- df[i,"7"]/rowSums(df[i,])
}

#same as 2006 data
dfc16 <- data.frame(poliEinstellung = 1:10,
                    sehrschlecht = df$`1`,
                    schlecht = df$`2`,
                    eherschlecht = df$`3`,
                    mittel = df$`4`,
                    ehergut = df$`5`,
                    gut = df$`6`,
                    sehrgut = df$`7`)
dfc16 <- melt(dfc16 ,  id.vars = 'poliEinstellung', variable.name = 'EinstellungAsyl')

dfp16 <- data.frame(poliEinstellung = 1:10,
                    sehrschlecht = df$p1,
                    schlecht = df$p2,
                    eherschlecht = df$p3,
                    mittel = df$p4,
                    ehergut = df$p5,
                    gut = df$p6,
                    sehrgut = df$p7)

dfp16 <- melt(dfp16 ,  id.vars = 'poliEinstellung', variable.name = 'EinstellungAsyl')

#plot our stackt bar plot
pc_06 <- dfc06 %>% mutate(Opinion = factor(Opinion)) %>% #we need to factor our data so ggplot can work with it
  ggplot() +
  aes(x = poliEinstellung, y = value, fill = Opinion) + #specify the axsis and the variable where to find the number of interviewees
  geom_bar(stat = "identity") + #tell ggplot its a bar plot
  scale_x_continuous("Political view (left-wing - right-wing)", labels = as.character(1:10), breaks = 1:10) + #define the axis devision
  theme_bw() + #set the theme
  theme(legend.position = c(0.85, 0.65)) + #position the legend
  ylab("Number of respondents")


pp_06 <- ggplot(dfp06, aes(x = poliEinstellung, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Political view (left-wing - right-wing)", labels = as.character(1:10), breaks = 1:10) +
  scale_y_continuous("Percentage of respondents", labels = scales::percent) + #set the scale to percentage instead of numeric value
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) #set the corresponding values to the plot


pc_16 <- dfc16 %>% mutate(EinstellungAsyl = factor(EinstellungAsyl)) %>%
  ggplot() +
  aes(x = poliEinstellung, y = value, fill = EinstellungAsyl) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Political view (left-wing - right-wing)", labels = as.character(1:10), breaks = 1:10) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Number of respondents")


pp_16 <- ggplot(dfp16, aes(x = poliEinstellung, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Political view (left-wing - right-wing)", labels = as.character(1:10), breaks = 1:10) +
  scale_y_continuous("Percentage of respondents", labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

#plot everything in one plot
plot_grid(pp_06, pc_06, pp_16, pc_16, labels = c("06", "06", "16", "16"))



