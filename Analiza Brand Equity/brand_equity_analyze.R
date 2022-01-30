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
df1 <- read.csv2("~/data.csv")

# Select only important data.
df1 <- select(df1, RecordNo, T6M1:T7M10)

# Split data T6 and T7 questions into one column.
df1 <- pivot_longer(df1,
                    cols = matches("T[34]M")
                    ,names_to = c("T","index")
                    ,names_pattern = "(T6|T7)M(\\d+)"
)

# Transform index as numeric 
df1 <- mutate(df1, index=as.numeric(index))

# Separate T6 and T7 questions.
df1 <- pivot_wider(df1
                   ,names_from = T
                   ,values_from = value
)

# Create RecordNo dataset, will be useful to later join data for all calculations.
RecordNo <- df1 %>% select(RecordNo) %>% distinct() %>% mutate(project_id=1)

# Create plants names dataset, will be useful to later join data for all calculations.
plants <- bind_rows(df1 %>% select(plants=T6), df1 %>% select(plants=T7)) %>%
  filter(!is.na(plants)) %>% distinct() %>% mutate(project_id=1)

# Join plants with RecordNo data.
All.together1 <- RecordNo %>% left_join(plants)

# Create Awareness score 

#For T6 question
All.together1 <- All.together1 %>% left_join(df1 %>% select(-T7) %>% rename(plants=T6))
All.together1 <- mutate(All.together1, Awareness_score=1)
All.together1 <- mutate(All.together1, Awareness_score=if_else(index==1,5,Awareness_score,missing = Awareness_score))
All.together1 <- mutate(All.together1, Awareness_score=if_else(index>=2,4,Awareness_score,missing = Awareness_score))
All.together1 <- select(All.together1, -index)

#For T7 question
All.together1 <- All.together1 %>% left_join(df1 %>% select(-T6) %>% rename(plants=T7))
All.together1 <- mutate(All.together1, Awareness_score=if_else(!is.na(index),2,Awareness_score,missing = Awareness_score))
All.together1 <- select(All.together1, -index)
