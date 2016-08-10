rm(list=ls())
library(ggplot2)
library(rvest)
library(zoo)

url <- "https://www.justice.gov/pardon/obama-commutations"
clemency_info <- 
  url %>% 
  read_html() %>%
  html_nodes(xpath = "/html/body/div[1]/div[2]/div/div/div[2]/article/div[1]/div/div/div/table[1]") %>% 
  html_table()
clemency_info <- as.data.frame(clemency_info)
names(clemency_info) <- c('attribute', 'description')
clemency_info$attribute[which(regexpr('[:alnum:]', clemency_info$attribute) == -1)] <- NA
clemency_info$attribute <- gsub(':', '', clemency_info$attribute)

for(i in 1:nrow(clemency_info)){
  if(is.na(clemency_info[i,1]) & clemency_info[i+1,1] == "Offense"){
    clemency_info[i,1] <- 'Name'
  }
}

clemency_info <- na.locf(clemency_info)

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
