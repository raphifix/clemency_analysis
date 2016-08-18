# clear the way
rm(list=ls())

# load libraries
library(ggplot2)
library(rvest)
library(tidyr)
library(zoo)

# extract table
clemency_info <- 
  read_html("https://www.justice.gov/pardon/obama-commutations") %>%
  html_nodes(xpath = "/html/body/div[1]/div[2]/div/div/div[2]/article/div[1]/div/div/div/table[1]") %>% 
  html_table()

# convert to df
clemency_info <- as.data.frame(clemency_info)

# change names
names(clemency_info) <- c('attribute', 'description')

# make empty attributes NA
clemency_info$attribute[which(regexpr('[:alnum:]', clemency_info$attribute) == -1)] <- NA

# drop trailing colons
clemency_info$attribute <- gsub(':', '', clemency_info$attribute)

# Names always precede the offense but aren't labeled in the table
clemency_info$attribute[which(clemency_info[,1] == "Offense") - 1] <- 'Name'

# Create indicator for person
clemency_info$person_indic <- NA
clemency_info$person_indic[which(clemency_info$attribute == 'Name')] <- seq(1:length(which(clemency_info$attribute == 'Name')))

# fill the rest of the NAs with the preceding label
clemency_info <- na.locf(clemency_info)
clemency_info$person_indic <- as.numeric(clemency_info$person_indic)

# make attributes unique by person
for(i in 1:length(clemency_info$attribute)){
  print(clemency_info$attribute[i] == clemency_info$attribute[i-1])
}
# Extract offenses
offenses <- clemency_info$description[which(clemency_info$attribute == 'Offense')]
offenses <- tolower(offenses)
offenses_vec <- c()
for(i in 1:length(offenses)){
  offenses_vec <- append(offenses_vec, strsplit(offenses[i], ';')[[1]])
}
offenses <- offenses_vec
offenses_vec <- NULL

drugs_parsed <- 
  read_html("http://drugabuse.com/library/drugs-a-z/") %>% 
  html_nodes('dt')
drugs_list <- gsub('drug-', '', unlist(html_attrs(drugs_parsed)))

drug_related <- c()
drug_involved <- c()
firearm_related <- c()
for(i in 1:length(offenses)){
  off <- strsplit(gsub('[[:punct:]]', '', offenses[i]), ' ')[[1]]
  if(!is.na(match(TRUE, drugs_list %in% off)) || !is.na(match(TRUE, 'drug' %in% off))){
    drug_related[i] <- 1
    drug_involved[i] <- drugs_list[match(TRUE, drugs_list %in% strsplit(gsub('[[:punct:]]', '', offenses[i]), ' ')[[1]])]
  }else{
    drug_related[i] <- 0
    drug_involved[i] <- NA
  }
  if(!is.na(match(TRUE, 'firearm' %in% off))){
    firearm_related[i] <- 1
  }else{
    firearm_related[i] <- 0
  }
}

offenses_df <- data.frame('offense' = offenses, drug_related, drug_involved, firearm_related)
offenses_df$offense <- as.character(offenses_df$offense)

ggplot(na.omit(offenses_df), aes(x = drug_involved, fill = drug_involved)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = 'Drug',
       y = 'Percentage of Drug-Related Offenses',
       fill = 'Drug',
       title = 'Types of Drugs Involved in Drug-Related Offenses Among Communited Sentences')

# made dataset more readible
clemency_info <- 
  clemency_info %>% 
  dplyr::group_by(person_indic, attribute) %>% 
  dplyr::summarise(description=toString(unique(description))) %>% 
  tidyr::spread(attribute, description)