#by Konrad Nissen 
library(broom)
library(mosaic)
library(tidyverse)
library(stringr)
library(car)
library(data.table)
library(haven)
library(viridis)
library(scales)
library(likert)
library(foreign)
library(psych)
library(sjPlot)
library(sjlabelled)
library(cowplot)
# Create Datasets for 2016 and 2006 wave
dataset <- read_stata("ZA4586_v1-0-0.dta")

database2016 <- subset(dataset, year==2016)
database2006 <- subset(dataset, year==2006)

##recode important variables##
#categorization of "Heirat von Asylbewerber": -3 to -1 negative, 
#0 to 3 positive/neutrale 
database2006$mg15dicho <- car::recode(database2006$mg15, '0:3=0; 4:7=1; else=NA')
database2016$mg15dicho <- car::recode(database2016$mg15, '0:3=0; 4:7=1; else=NA')


prop.table(table(database2016$mg15dicho))
prop.table(table(database2006$mg15dicho))

#educational background
str(database2016$educ)
str(database2006$educ)

database2006$Bildung <- car::recode(database2006$educ, '-9=NA; 7=NA; 4:5=4; 6=NA')
database2016$Bildung <- car::recode(database2016$educ, '-41=NA; -9=NA; 7=NA; 4:5=4; 6=NA')

print_labels(database2006$Bildung)
table(database2006$Bildung)
#contact with foreigners
database2006$Kontakt <- car::recode(database2006$mc04, '-11:-1=NA')
database2016$Kontakt <- car::recode(database2016$mc04, '-11:-1=NA')


str(database2006$mc03)
str(database2016$mc03)

print_labels(database2006$mc03)
print_labels(database2016$mc03)

## Likert Skala
#create a vector with all the data we need for the visualization
items <- c("pa01","mp02","mp03","mp04","mp05","mp06","mp07","mp08", "Bildung", "Kontakt")

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

#create a new dataframe which has as columns all possible likert values and as rows
#all possible educational characteristics
likertedu <- as.data.frame(do.call(rbind, list(c(0,0,0,0,0,0,0))))
colnames(likertedu) <- c("1", "2", "3","4","5","6","7")
for(i in 1:3){
  likertedu <- rbind(likertedu, c(0,0,0,0,0,0,0))
}
rownames(likertedu) <- c("1","2","3","4")

#for every row in our whole database, count how many people with which educational characteristics
#have which lickert score
for(i in 1:nrow(mpdata06)){
  Bildung <- as.character(mpdata06[i,"Bildung"]) #thats education
  lickert <- as.character(mpdata06[i,"lickert"]) #thats our related lickert score
  likertedu[Bildung,lickert] <- likertedu[Bildung,lickert] + 1 #the is the cell we want to change
}

#create new collums for the corresponding percentage value
likertedu$p1 <- 0
likertedu$p2 <- 0 
likertedu$p3 <- 0
likertedu$p4 <- 0
likertedu$p5 <- 0
likertedu$p6 <- 0
likertedu$p7 <- 0

#calc the percentage value
for(i in 1:nrow(likertedu)){
  likertedu[i,"p1"] <- likertedu[i,"1"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"]) 
  likertedu[i,"p2"] <- likertedu[i,"2"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p3"] <- likertedu[i,"3"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p4"] <- likertedu[i,"4"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p5"] <- likertedu[i,"5"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p6"] <- likertedu[i,"6"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p7"] <- likertedu[i,"7"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
}

dfc06 <- data.frame(Education = 1:4,
                    verybad = likertedu$`1`,
                    bad = likertedu$`2`,
                    ratherbad = likertedu$`3`,
                    neutral = likertedu$`4`,
                    rathergood = likertedu$`5`,
                    good = likertedu$`6`,
                    verygood = likertedu$`7`)
#that function will melt for every political attitude and every possible lickert score the related value that the persons have
dfc06 <- melt(dfc06 ,  id.vars = 'Education', variable.name = 'EinstellungAsyl')

dfp06 <- data.frame(Education = 1:4,
                    verybad = likertedu$p1,
                    bad = likertedu$p2,
                    ratherbad = likertedu$p3,
                    neutral = likertedu$p4,
                    rathergood = likertedu$p5,
                    good = likertedu$p6,
                    verygood = likertedu$p7)

dfp06 <- melt(dfp06 ,  id.vars = 'Education', variable.name = 'EinstellungAsyl')

#reset the dataframe
likertedu[,] <- 0

#do the counting for the 2016 data
for(i in 1:nrow(mpdata16)){
  Bildung <- as.character(mpdata16[i,"Bildung"])
  lickert <- as.character(mpdata16[i,"lickert"])
  likertedu[Bildung,lickert] <- likertedu[Bildung,lickert] + 1
}

#calc the percentage again
for(i in 1:nrow(likertedu)){
  likertedu[i,"p1"] <- likertedu[i,"1"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"]) 
  likertedu[i,"p2"] <- likertedu[i,"2"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p3"] <- likertedu[i,"3"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p4"] <- likertedu[i,"4"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p5"] <- likertedu[i,"5"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p6"] <- likertedu[i,"6"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
  likertedu[i,"p7"] <- likertedu[i,"7"]/(likertedu[i,"1"]+likertedu[i,"2"]+likertedu[i,"3"]+likertedu[i,"4"]+likertedu[i,"5"]+likertedu[i,"6"]+likertedu[i,"7"])
}

#same as 2006 data
dfc16 <- data.frame(Education = 1:4,
                    verybad = likertedu$`1`,
                    bad = likertedu$`2`,
                    ratherbad = likertedu$`3`,
                    neutral = likertedu$`4`,
                    rathergood = likertedu$`5`,
                    good = likertedu$`6`,
                    verygood = likertedu$`7`)
dfc16 <- melt(dfc16 ,  id.vars = 'Education', variable.name = 'EinstellungAsyl')

dfp16 <- data.frame(Education = 1:4,
                    verybad = likertedu$p1,
                    bad = likertedu$p2,
                    ratherbad = likertedu$p3,
                    neutral = likertedu$p4,
                    rathergood = likertedu$p5,
                    good = likertedu$p6,
                    verygood = likertedu$p7)

dfp16 <- melt(dfp16 ,  id.vars = 'Education', variable.name = 'EinstellungAsyl')

#plot our stackt bar plot
pc_06 <- dfc06 %>% mutate(EinstellungAsyl = factor(EinstellungAsyl)) %>% #we need to factor our data so ggplot can work with it
  ggplot() +
  aes(x = Education, y = value, fill = EinstellungAsyl) + #specify the axsis and the variable where to find the number of interviewees
  geom_bar(stat = "identity") + #tell ggplot its a bar plot
  scale_x_continuous("Education", labels = as.character(1:4), breaks = 1:4) + #define the axis devision
  theme_bw() + #set the theme
  theme(legend.position = c(0.2, 0.65)) + #position the legend
  ylab("number of respondents")
print(pc_06)

pp_06 <- ggplot(dfp06, aes(x = Education, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Education", labels = as.character(1:4), breaks = 1:4) +
  scale_y_continuous("percentage of respondents", labels = scales::percent) + #set the scale to percentage instead of numeric value
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))  + #set the corresponding values to the plot
  ylab("percentage of respondents") #label the y-axis

pc_16 <- dfc16 %>% mutate(EinstellungAsyl = factor(EinstellungAsyl)) %>%
  ggplot() +
  aes(x = Education, y = value, fill = EinstellungAsyl) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Education", labels = as.character(1:4), breaks = 1:4) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("number of respondents")


pp_16 <- ggplot(dfp16, aes(x = Education, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Education", labels = as.character(1:4), breaks = 1:4) +
  scale_y_continuous("percentage of respondents", labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))  +
  ylab("percentage of respondents")

#plot everything in one plot
plot_grid(pp_06, pc_06, pp_16, pc_16, labels = c("06", "06", "16", "16"))

#create a new dataframe which has as columns all possible lickert values and as rows
#all possible political attitudes
df <- as.data.frame(do.call(rbind, list(c(0,0,0,0,0,0,0))))
colnames(df) <- c("1", "2", "3","4","5","6","7")
for(i in 1:1){
  df <- rbind(df, c(0,0,0,0,0,0,0))
}
rownames(df) <- c("1","2")

#for every row in our whole database, count how many people with which educational characteristics
#have which lickert score
for(i in 1:nrow(mpdata06)){
  Kontakt <- as.character(mpdata06[i,"Kontakt"]) #thats Contact
  lickert <- as.character(mpdata06[i,"lickert"]) #thats our related lickert score
  df[Kontakt,lickert] <- df[Kontakt,lickert] + 1 #the is the cell we want to change
}

df$p1 <- 0
df$p2 <- 0 
df$p3 <- 0
df$p4 <- 0
df$p5 <- 0
df$p6 <- 0
df$p7 <- 0

#calc the percentage value
for(i in 1:nrow(df)){
  df[i,"p1"] <- df[i,"1"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"]) 
  df[i,"p2"] <- df[i,"2"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p3"] <- df[i,"3"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p4"] <- df[i,"4"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p5"] <- df[i,"5"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p6"] <- df[i,"6"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p7"] <- df[i,"7"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
}

#create a new dataframe that has the right form for ggplot to plot our stackted bar plot
dfc06 <- data.frame(Kontakt = 1:2,
                    verybad = df$`1`,
                    bad = df$`2`,
                    ratherbad = df$`3`,
                    neutral = df$`4`,
                    rathergood = df$`5`,
                    good = df$`6`,
                    verygood = df$`7`)
#that function will melt for every political attitude and every possible lickert score the related value that the persons have
dfc06 <- melt(dfc06 ,  id.vars = 'Kontakt', variable.name = 'EinstellungAsyl')

#same for our percentage data
dfp06 <- data.frame(Kontakt = 1:2,
                    verybad = df$p1,
                    bad = df$p2,
                    ratherbad = df$p3,
                    neutral = df$p4,
                    rathergood = df$p5,
                    good = df$p6,
                    verygood = df$p7)

dfp06 <- melt(dfp06 ,  id.vars = 'Kontakt', variable.name = 'EinstellungAsyl')

#reset the dataframe
df[,] <- 0

#do the counting for the 2016 data
for(i in 1:nrow(mpdata16)){
  Kontakt <- as.character(mpdata16[i,"Kontakt"])
  lickert <- as.character(mpdata16[i,"lickert"])
  df[Kontakt,lickert] <- df[Kontakt,lickert] + 1
}

#calc the percentage again
for(i in 1:nrow(df)){
  df[i,"p1"] <- df[i,"1"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"]) 
  df[i,"p2"] <- df[i,"2"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p3"] <- df[i,"3"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p4"] <- df[i,"4"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p5"] <- df[i,"5"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p6"] <- df[i,"6"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
  df[i,"p7"] <- df[i,"7"]/(df[i,"1"]+df[i,"2"]+df[i,"3"]+df[i,"4"]+df[i,"5"]+df[i,"6"]+df[i,"7"])
}

#same as 2006 data
dfc16 <- data.frame(Kontakt = 1:2,
                    verybad = df$`1`,
                    bad = df$`2`,
                    ratherbad = df$`3`,
                    neutral = df$`4`,
                    rathergood = df$`5`,
                    good = df$`6`,
                    verygood = df$`7`)
dfc16 <- melt(dfc16 ,  id.vars = 'Kontakt', variable.name = 'EinstellungAsyl')

dfp16 <- data.frame(Kontakt = 1:2,
                    verybad = df$p1,
                    bad = df$p2,
                    ratherbad = df$p3,
                    neutral = df$p4,
                    rathergood = df$p5,
                    good = df$p6,
                    verygood = df$p7)

dfp16 <- melt(dfp16 ,  id.vars = 'Kontakt', variable.name = 'EinstellungAsyl')

#plot our stackt bar plot
pc_06 <- dfc06 %>% mutate(EinstellungAsyl = factor(EinstellungAsyl)) %>% #we need to factor our data so ggplot can work with it
  ggplot() +
  aes(x = Kontakt, y = value, fill = EinstellungAsyl) + #specify the axsis and the variable where to find the number of interviewees
  geom_bar(stat = "identity") + #tell ggplot its a bar plot
  scale_x_continuous("Contact with immigrants", labels = as.character(1:2), breaks = 1:2) + #define the axis devision
  theme_bw() + #set the theme
  theme(legend.position = c(0.85, 0.65)) + #position the legend
  ylab("number of respondents")


pp_06 <- ggplot(dfp06, aes(x = Kontakt, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Contact with immigrants", labels = as.character(1:2), breaks = 1:2) +
  scale_y_continuous("percentage of respondents", labels = scales::percent) + #set the scale to percentage instead of numeric value
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))  + #set the corresponding values to the plot
  ylab("percentage of respondents") #label the y-axis

pc_16 <- dfc16 %>% mutate(EinstellungAsyl = factor(EinstellungAsyl)) %>%
  ggplot() +
  aes(x = Kontakt, y = value, fill = EinstellungAsyl) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Contact with immigrants", labels = as.character(1:2), breaks = 1:2) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("number of respondents")


pp_16 <- ggplot(dfp16, aes(x = Kontakt, y = value, fill = factor(EinstellungAsyl), label = round(value,2))) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Contact with immigrants", labels = as.character(1:2), breaks = 1:2) +
  scale_y_continuous("percentage of respondents", labels = scales::percent) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))  +
  ylab("percentage of respondents")

#plot everything in one plot
plot_grid(pp_06, pc_06, pp_16, pc_16, labels = c("06", "06", "16", "16"))



