###############################################################################
# ustalenie katalogu roboczego
getwd()
#setwd("C:/Users/ryszard.siwy/Documents/studia/DS profesional project/Projekt")

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
# reread data frame from previouse writed file, cleaned data
raw_data <- read.csv2('Grupa_5_clean.csv', encoding = 'UTF-8')

###############################################################################
# Number of all answer
Count = nrow(raw_data)

company = c("Arnika","Babka","Chaber","Dzwonek","Fiołek",
            "Goździk","Jaskier","Łubin","Mięta","Narcyz",
            "Inna1","Inna2","Inna3","brak odpowiedzi")
company_nr = c(101,102,103,104,105,106,107,108,109,110,995,996,997,-1)


std_col = c("T2LBL","T3LB", "T4LB", "T5ALB","T5BLB", "Id")
raw_single <- raw_data[sel_col]

###############################################################################
# joining column T6M1:T6M10 in to unaided_data
for (i in 1:10) {
  sel_col = c("T2LBL","T3LB", "T4LB", "T5ALB","T5BLB",
              paste("T6M",as.character(i),sep=""))
  raw_single <- raw_data[sel_col]
  names(raw_single) <- std_col
  raw_single["index"] <- i
  if (i == 1) {
    unaided_data = raw_single    
  }
  else {
    unaided_data = rbind(unaided_data, raw_single)
  }
}

###############################################################################
# change the identifier to a name
unaided_data$Company <- factor(unaided_data$Id,
                               levels = company_nr,
                               labels = company)


###############################################################################
# remove records with N/A
unaided_data <- na.omit(unaided_data)
###############################################################################
# joining column T7M1:T7M10 in to unaided_data
for (i in 1:10) {
  sel_col = c("T2LBL","T3LB", "T4LB", "T5ALB","T5BLB",
              paste("T7M",as.character(i),sep=""))
  raw_single <- raw_data[sel_col]
  names(raw_single) <- std_col
  raw_single["index"] <- i
  if (i == 1) {
    aided_data = raw_single    
  }
  else {
    aided_data = rbind(aided_data, raw_single)
  }
}

###############################################################################
# change the identifier to a name
aided_data$Company <- factor(aided_data$Id,
                              levels = company_nr,
                              labels = company)

# remove records with N/A
aided_data <- na.omit(aided_data)

###############################################################################
# binding total awareness for top-of-mind
total_data <- rbind(aided_data,unaided_data)

###############################################################################
# set index = 1
total_data %>% 
  filter(index==1) -> top_data 
unaided_data$index <- 1
aided_data$index <- 1
total_data$index <- 1
std_col = c("Age","Education", "Ocupation", "Current crops","Planned crops", 
            "CompanyId", "Index", "Company")

names(total_data) <- std_col
names(unaided_data) <- std_col
names(aided_data) <- std_col
names(total_data) <- std_col

###############################################################################
# write data to csv 
write.csv2(unaided_data, file="BA_unaided_data.csv", row.names = FALSE)
write.csv2(aided_data, file="BA_aided_data.csv", row.names = FALSE)
write.csv2(total_data, file="BA_total_data.csv", row.names = FALSE)
write.csv2(top_data, file="BA_top_data.csv", row.names = FALSE)

