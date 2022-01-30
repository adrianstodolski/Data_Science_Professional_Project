###############################################################################
# Packages installation and loading

if(!("tidyverse" %in% installed.packages())){
  install.packages("tidyverse")
}
if(!("ggplot2" %in% installed.packages())){
  install.packages("ggplot2")
}

library(tidyverse)
library(ggplot2)
###############################################################################
# set working directory if necessary
getwd()
#setwd("C:/Users/ryszard.siwy/Documents/studia/DS profesional project/Projekt")


###############################################################################
# reread data frame from previouse writed file, cleaned data
dane <- read.csv2('Grupa_5_clean.csv', encoding = 'UTF-8')

###############################################################################
# calculating NPS factor

company = c("Arnika","Babka","Chaber","Dzwonek","Fiołek",
            "Goździk","Jaskier","Łubin","Mięta","Narcyz")

###############################################################################
# create empty dataframe for nps pivot table
nps_df <- data.frame(
  redid=as.integer(),
  education=as.character(),
  ocupation=as.character(),
  company=as.character(),
  promotor=as.double(),
  neutral=as.double(),
  detractor=as.double(),
  total=as.double())

###############################################################################
# combine data from column T14M1:T14M10
for (i in 1:10) {
  source_column = c("T3LB","T4LB","T5ALB","T2LBL",
                    paste("T14M", as.character(i),sep=""))
  
  dest_column = c("education","ocupation","area",
                  "age group", "recomendation")
  
  one_firm = dane[source_column]
  names(one_firm) <- dest_column
  
  one_firm %>% 
    mutate(firma=company[i]) -> one_firm
  
  one_firm <- mutate(one_firm, 
                     score= case_when(
                       between(recomendation, 0, 6) ~ 10,
                       between(recomendation, 7, 8) ~ 20,
                       between(recomendation, 9, 10) ~ 30
                       )
                    )
  if (i == 1) {
    nps_result <- one_firm
  }
  else {
    nps_result <- rbind(nps_result, one_firm)
    
  }
}

###############################################################################
# add binary columns detractor, neutral, promoter, total
nps_result <- mutate(nps_result, 
                     detractor=ifelse(score == 10, 1, 0)
                     )
nps_result <- mutate(nps_result, 
                     neutral=ifelse(score == 20, 1, 0)
)
nps_result <- mutate(nps_result, 
                     promoter=ifelse(score == 30, 1, 0)
)
nps_result$total = 1

levels = c(10,20,30)
labels = c("Detractor", "Neutral", "Promoter")
nps_result$score = factor(nps_result$score, levels = levels, labels = labels)

nps_result <- na.omit(nps_result)



write.csv2(nps_result, file = 'result_NPS.csv')
