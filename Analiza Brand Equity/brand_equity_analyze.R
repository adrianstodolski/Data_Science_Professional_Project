#----------------------------- BRAND EQUITY ANALYSIS --------------------------#
# Analysis made for final project for Data Science Professional at CDV.
# Analysis based on Plants dataset.
# Author: Adrian Stodolski


#---------------------------- INITIAL REQUIREMENTS ----------------------------#
# 1. Put both files data and .R script code into the same location (folder).
# 2. Set working directory.
setwd("./")
# 3. Install require package with dependencies (comment after finished installation).
install.packages("tidyverse", dependencies = TRUE)
# 4. Load require package.

library(tidyverse)


# -------------------------------- AWARNESS ------------------------------------
# According to Brand Health Index description, awarness questions are T6 and T7.

# Load file.
df1 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv")

# Select only important data.
df1 <- select(df1, RecordNo, T6M1:T7M10)
head(df1)
# Split data T6 and T7 questions into one column.
df1 <- pivot_longer(df1,
                    cols = matches("T[67]M")
                    ,names_to = c("T","index")
                    ,names_pattern = "(T6|T7)M(\\d+)"
)
head(df1)
# Transform index as numeric 
df1 <- mutate(df1, index=as.numeric(index))
head(df1)
# Separate T6 and T7 questions.
df1 <- pivot_wider(df1
                   ,names_from = T
                   ,values_from = value
)
head(df1)
# Create RecordNo dataset, will be useful to later join data for all calculations.
RecordNo <- df1 %>% select(RecordNo) %>% distinct() %>% mutate(project_id=1)
head(RecordNo)
# Create plants names dataset, will be useful to later join data for all calculations.
plants <- bind_rows(df1 %>% select(plants=T6), df1 %>% select(plants=T7)) %>%
  filter(!is.na(plants)) %>% distinct() %>% mutate(project_id=1)
head(plants)
# Join plants with RecordNo data.
data_together <- RecordNo %>% left_join(plants)
head(data_together)

# Create Awareness score 

#For T6 question
data_together <- data_together %>% left_join(df1 %>% select(-T7) %>% rename(plants=T6))
data_together <- mutate(data_together, Awareness_score=1)
data_together <- mutate(data_together, Awareness_score=if_else(index==1,5,Awareness_score,missing = Awareness_score))
data_together <- mutate(data_together, Awareness_score=if_else(index>=2,4,Awareness_score,missing = Awareness_score))
data_together <- select(data_together, -index)

#For T7 question
data_together <- data_together %>% left_join(df1 %>% select(-T6) %>% rename(plants=T7))
data_together <- mutate(data_together, Awareness_score=if_else(!is.na(index),2,Awareness_score,missing = Awareness_score))
data_together <- select(data_together, -index)


# -------------------------------- USAGE ---------------------------------------
# According to Brand Health Index description, usage questions are T9 and T10.

# Load file.
df2 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv")

# Select only important data.
df2 <- select(df2, RecordNo, T9M1:T10M10)
head(df2)
# Split data T9 and T10 questions into one column.
df2 <- pivot_longer(df2,
                    cols = matches("T[9]M, T[10]M")
                    ,names_to = c("T","index")
                    ,names_pattern = "(T9|T10)M(\\d+)"
)
head(df2)
# Transform index as numeric 
df2 <- mutate(df2, index=as.numeric(index))
head(df2)
# Separate T9 and T10 questions.
df2 <- pivot_wider(df2
                   ,names_from = T
                   ,values_from = value
)
head(df2)
# Join plants with RecordNo data.
data_together2 <- RecordNo %>% left_join(plants)
head(data_together2)
# Create Usage score

# For T9 question
data_together2 <- data_together2 %>% left_join(df2 %>% select(-T10) %>% rename(plants=T))
data_together2 <- mutate(data_together2, Usage_score=1)
data_together2 <- mutate(data_together2, Usage_score=if_else(index==1,5,Usage_score,missing = Usage_score))
data_together2 <- mutate(data_together2, Usage_score=if_else(index>=2,4,Usage_score,missing = Usage_score))
data_together2 <- select(data_together2, -index)

# For T10 question
data_together2 <- data_together2 %>% left_join(df2 %>% select(-T) %>% rename(plants=T10))
data_together2 <- mutate(data_together2, Usage_score=if_else(!is.na(index),2,Usage_score,missing = Usage_score))
data_together2 <- select(data_together2, -index)


### ------------------------------ PREFERENCE ----------------------------------
# According to Brand Health Index description, preference question is T13.

# Load file.
df3 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv")

# Select only important data.
df3 <- select(df3, RecordNo, T13M1:T13M3)

# Split data T13 question into one column.
df3 <- pivot_longer(df3,
                    cols = starts_with("T")
                    ,names_to = c("T","index")
                    ,names_pattern = "(T13)M(\\d+)"
)

# Transform index as numeric 
df3 <- mutate(df3, index=as.numeric(index))

# Split T13 question and value into one column.
df3 <- pivot_wider(df3
                   ,names_from = T
                   ,values_from = value
)

# Join plants with RecordNo data.
data_together3 <- RecordNo %>% left_join(plants)

# Create Preference score 
# For X10 question
data_together3 <- data_together3 %>% left_join(df3 %>% select(index,T13) %>% rename(plants=T13))
data_together3 <- mutate(data_together3, Preference_score=1)
data_together3 <- mutate(data_together3, Preference_score=if_else(index==1,5,Preference_score,missing = Preference_score))
data_together3 <- mutate(data_together3, Preference_score=if_else(index>=2,4,Preference_score,missing = Preference_score))
data_together3 <- select(data_together3, -index)
