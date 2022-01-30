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


# -------------------------------- AWARNESS -----------------------------------#
# According to Brand Health Index description, awarness questions are T6 and T7.

# Load file.
df1 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv")

# Select only important data.
df1 <- select(df1, RecordNo, T6M1:T7M10)

# Split data T6 and T7 questions into one column.
df1 <- pivot_longer(df1,
                    cols = matches("T[67]M")
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
data_together <- RecordNo %>% left_join(plants)


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


# -------------------------------- USAGE --------------------------------------#
# According to Brand Health Index description, usage questions are T9 and T4.

# Load file.
df2 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv")

# Select only important data.
df2 <- select(df2, RecordNo, T9M1:T4M10)

# Split data T9 and T4 questions into one column.
df2 <- pivot_longer(df2,
                    cols = matches("T[94]M")
                    ,names_to = c("T","index")
                    ,names_pattern = "(T9|T4)M(\\d+)"
)

# Transform index as numeric 
df2 <- mutate(df2, index=as.numeric(index))

# Separate T9 and T4 questions.
df2 <- pivot_wider(df2
                   ,names_from = T
                   ,values_from = value
)

# Join plants with RecordNo data.
data_together2 <- RecordNo %>% left_join(plants)

# Create Usage score

# For T9 question
data_together2 <- data_together2 %>% left_join(df2 %>% select(-T4) %>% rename(plants=T9))
data_together2 <- mutate(data_together2, Usage_score=1)
data_together2 <- mutate(data_together2, Usage_score=if_else(index==1,5,Usage_score,missing = Usage_score))
data_together2 <- mutate(data_together2, Usage_score=if_else(index>=2,4,Usage_score,missing = Usage_score))
data_together2 <- select(data_together2, -index)

# For T4 question
data_together2 <- data_together2 %>% left_join(df2 %>% select(-T9) %>% rename(plants=T4))
data_together2 <- mutate(data_together2, Usage_score=if_else(!is.na(index),2,Usage_score,missing = Usage_score))
data_together2 <- select(data_together2, -index)


### ------------------------------ PREFERENCE ---------------------------------#
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
# For T13 question
data_together3 <- data_together3 %>% left_join(df3 %>% select(index,T13) %>% rename(plants=T13))
data_together3 <- mutate(data_together3, Preference_score=1)
data_together3 <- mutate(data_together3, Preference_score=if_else(index==1,5,Preference_score,missing = Preference_score))
data_together3 <- mutate(data_together3, Preference_score=if_else(index>=2,4,Preference_score,missing = Preference_score))
data_together3 <- select(data_together3, -index)


# -------------------------------- FAMILIARITY --------------------------------#
# According to Brand Health Index description, familiartity question is T8.

# Familiarity_score
df4 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv") %>%
  select(RecordNo, T8M1:T8M10) %>%
  pivot_longer(T8M1:T8M10, names_to = "plants", values_to = "Familiarity_score", names_pattern = "T8M(\\d+)") %>%
  mutate(Familiarity_score=as.numeric(Familiarity_score)) %>%
  mutate(plants=as.numeric(plants)) %>%
  mutate(plants = plants + 100) %>%
  mutate(Familiarity_score=if_else(is.na(Familiarity_score), 1,  Familiarity_score)) %>%
  mutate(Familiarity_score=if_else(Familiarity_score==999,1,Familiarity_score, missing = Familiarity_score))


# -------------------------------- FUTURE USE----------------------------------#
# According to Brand Health Index description, Future use question is T11.

# Future usage score
df5 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv") %>%
  select(RecordNo, T11M1:T11M10) %>%
  pivot_longer(T11M1:T11M10, names_to = "plants", values_to = "FutureUsage_score", names_pattern = "T11M(\\d+)") %>%
  mutate(FutureUsage_score=as.numeric(FutureUsage_score)) %>%
  mutate(plants=as.numeric(plants)) %>%
  mutate(plants = plants + 100) %>%
  mutate(FutureUsage_score=if_else(is.na(FutureUsage_score), 1,  FutureUsage_score)) %>%
  mutate(FutureUsage_score=if_else(FutureUsage_score==999,1,FutureUsage_score, missing =FutureUsage_score))


# -------------------------------- SATISFACTION -------------------------------#
# According to Brand Health Index description, Future use question is T12.

# Satisfaction score
df6 <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/data.csv") %>%
  select(RecordNo, T12M1:T12M10) %>%
  pivot_longer(T12M1:T12M10, names_to = "plants", values_to = "Satisfaction_score", names_pattern = "T12M(\\d+)") %>%
  mutate(Satisfaction_score=as.numeric(Satisfaction_score)) %>%
  mutate(plants=as.numeric(plants)) %>%
  mutate(plants = plants + 100) %>%
  mutate(Satisfaction_score=if_else(is.na(Satisfaction_score), 1,  Satisfaction_score)) %>%
  mutate(Satisfaction_score=if_else(Satisfaction_score==999,1,Satisfaction_score, missing =Satisfaction_score))


# -------------------------------- TOTAL --------------------------------------#
# This is final calculations. Here there is joining all above analysis results and
# eliminate outliers.

# Join all datasets to single.
Total <- left_join(data_together, data_together2)
Total <- left_join(Total, data_together3)
Total <- left_join(Total, df4)
Total <- left_join(Total, df5)
Total <- left_join(Total, df6)

# Drop NA values
Total$Awareness_score[is.na(Total$Awareness_score)] <- 999
Total$Usage_score[is.na(Total$Usage_score)] <- 999
Total$Preference_score[is.na(Total$Preference_score)] <- 999
Total$Familiarity_score[is.na(Total$Familiarity_score)] <- 999
Total$FutureUsage_score[is.na(Total$FutureUsage_score)] <- 999
Total$Satisfaction_score[is.na(Total$Satisfaction_score)] <- 999

# Filter only valid (1-5) data
Total <- filter(Total, Awareness_score <6)
Total <- filter(Total, Usage_score <6)
Total <- filter(Total, Preference_score <6)
Total <- filter(Total, Familiarity_score <6)
Total <- filter(Total, FutureUsage_score <6)
Total <- filter(Total, Satisfaction_score <6)

# Verifying dataset answers with plants names in questionnaire (should be 101-110).
summary(Total$plants)

# Add plants labels
AllLabels <- source("~/Code/Data_Science_Professional_Project/Analiza Brand Equity/labels.R", encoding = "UTF8")
Total$plants <- MyLabels(Total$plants)

# Write data to outfile.
write.csv2(Total, "Equity_Results.csv", row.names = FALSE)
