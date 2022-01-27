EPI_data <- read.csv("C:\\Users\\bartha4\\Documents\\Data Analytics/2010EPI_data.csv")
View(EPI_data)
summary(EPI_data$EPI)
fivenum(EPI_data$EPI, na.rm=TRUE)
