### Set all files into one folder
setwd("./")

### Needed library
install.packages("tidyverse", dependencies = TRUE)
install.packages("datasets")
install.packages("boot")

library(boot)
library(tidyverse)

#Loaded file and select 3 columns
file <- read.csv2("~/Code/Data_Science_Professional_Project/Analiza Bootstrap/data.csv")
file <- select(file, RecordNo, T5a:T5b)

#Change commas into dots in column X2a called "Wartosc tegoroczna"
file$T5a <- filed$T5a %>%
  { gsub(" ", "", .) } %>%
  { gsub(",", ".", .) } %>%
  as.numeric()

#Change column names
colnames(file)[1] <- "RecordNo"
colnames(file)[2] <- "Wartosc tegoroczna"
colnames(file)[3] <- "Wartosc przyszloroczna"

#Eliminate zero values
file <- file[apply(file!=0, 1, all),]

#Calculate change of current and next year
file <- mutate(file, 'change' = file$`Wartosc tegoroczna` - file$`Wartosc przyszloroczna`)

#Sample parameteres
median(file$`Wartosc tegoroczna`)
mean(file$`Wartosc tegoroczna`)
median(file$`Wartosc przyszloroczna`)
mean(file$`Wartosc przyszloroczna`)
cor(file$`Wartosc tegoroczna`, file$`Wartosc przyszloroczna`, method='s')
mean(file$change)

#Estimating function
foo <- function(data, indices){
  dt<-data[indices,]
  c(
    mean(dt[,2] - dt[,3]),
    mean(dt[,2]),
    mean(dt[,3])
  )
}

#set seed
set.seed(12345)

#Bootstrap method
myBootstrap <- boot(file, foo, R=1000)

#View results
View(myBootstrap)
print(myBootstrap)
View(myBootstrap$t)

#Comparing with the original mean
mean(file$change)
head(myBootstrap$t0)

#Normal distriburation
plot(myBootstrap, index=1)
boot.ci(myBootstrap, index=1)

#Mean for current year
plot(myBootstrap, index=2)
boot.ci(myBootstrap, index=2)

#Mean for next year
plot(myBootstrap, index=3)
boot.ci(myBootstrap, index=3)