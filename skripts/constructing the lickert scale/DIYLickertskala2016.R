#by Julius Hehenkamp
### Rotiertes Balkendiagram für Likertskalen ###

# tidyverse troubleshooting - ultra nervig gewesen #

install.packages("stringi")

### importiere notwendige libraries
library(haven)
library(foreign)
library(psych)
library(sjPlot)
library(sjlabelled)
library(ggplot2)

### importiere ALLBUS und erstelle Database ALLBUS2016 ###

database <- read_stata("ZA4586_v1-0-0.dta")
database <- database[!(database$year!=2016),]
View(database)

### Erstelle datensatz für die Items mp03 bis mp08 ###

mpdata <- database[ ,228:233]
View(mpdata)

### Formatiere Fehlwerte von -9 auf NA ###

mpdata[mpdata == -9] <- NA

### Umpolung negativer Items ###

mpdata$mp04 = 7 + 1 - mpdata$mp04
mpdata$mp06 = 7 + 1 - mpdata$mp06
mpdata$mp07 = 7 + 1 - mpdata$mp07

##### Erstelle eigenen Likertplot #####

library(likert)
library(viridis)
library(scales)
library(tidyverse)

extra_items <- dplyr::select(mpdata, 1:6)

extra_items %>%
  gather(key = items, value = Antwort) %>%
  mutate(items = factor(items),
         Antwort = factor(Antwort)) -> extra_items_long

p2 <- ggplot(data = extra_items_long) +
  aes(x = items) +
  geom_bar(aes(fill = Antwort), position = "fill")
theme(legend.position = "bottom",
      text = element_text(size = 4)) +
  scale_fill_viridis(discrete = TRUE)

# droppe Fehlwerte # 

extra_items_long <- drop_na(extra_items_long)

# erste Ausgabe # 

p2

# Reverse X-Achse und Itemflip # 

p2_flip <- p2 + coord_flip()
p2_flip

extra_items_long %>%
  mutate(Antwort = factor(Antwort,
                          levels = rev(levels(Antwort)))) ->
  extra_items_long_rev

extra_items_long_rev %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = Antwort), position = "fill") +
  coord_flip()

# Labels definieren # 

item_labels <- c("They are an enrichment for the german culture",
                 "Their presence in Germany leads Problems on the Housing market (reversed)",
                 "They help to secure pensions",
                 "They steal jobs from the german population (reversed)",
                 "They commit crime more often then germans (reversed)", "foreigners living in Germany create more jobs") %>% factor()

antwort_labels_rev <- fct_inorder(c("i totally agree",
                                    "i agree", "i partly agree", "neutral",
                                    "i partly disagree", "i disagree",
                                    "i totally disagree"))
extra_items_long_rev$Antwort2 <- extra_items_long_rev$Antwort
levels(extra_items_long_rev$Antwort2) <- antwort_labels_rev

# labels hinzufügen # 

ggplot(extra_items_long_rev, aes(x = items)) +
  geom_bar(aes(fill = Antwort2), position = "fill") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  theme(legend.position = "bottom") +
  labs(x = "", y = "") +
  scale_x_discrete(labels = rev(item_labels)) +
  guides(fill = guide_legend(reverse = TRUE)) -> p_itemnummern

p_itemnummern


