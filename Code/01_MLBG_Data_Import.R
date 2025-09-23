#Load Library
library(tidyverse)
library(ggplot2)


#Import Clean Readings####
dir_path <- "Data/"
name_of_file <- "202408_MLBG_TEMPDO_CLEAN.csv"
df.cl <- read_csv(paste0(dir_path, name_of_file))

#Adjusting data Frame##
#Remove top line of dataframe
df.cl <- df.cl[-c(1),]
#renaming columns
df.cl <- df.cl %>%
  rename("SAMPLE_DO" = `Sample DO`, "BEAKER_DO" = `Beaker DO`)
#fixing numerics
df.cl$SAMPLE_DO <- as.numeric(df.cl$SAMPLE_DO)
df.cl$BEAKER_DO <- as.numeric(df.cl$BEAKER_DO)
df.cl$TEMP <- as.numeric(df.cl$TEMP)
#removing PM from the TIME column
df.cl$TIME <-gsub("PM","", df.cl$TIME)
#Adjusting date to proper formatting
df.cl$DATE <-mdy(df.cl$DATE)
#merging time and date to new column
df.cl$DATE_TIME <-paste(df.cl$DATE, df.cl$TIME, sep = " ")
df.cl$DATE_TIME <-as.POSIXct(df.cl$DATE_TIME, "%y/%m/%d %H:%M:%S")
#Creating new column to define treatments
df.cl$TREAT <- substring(df.cl$BEAKER, 2) #Pulls only the second character

#Initial data visualization with an Exploratory Plot
EXPL_PLOT<- ggplot(df.cl, aes(x = DATE_TIME, group = BEAKER))+
  geom_smooth(aes(y = SAMPLE_DO), colour = "blue")+
  geom_smooth(aes(y = BEAKER_DO), colour = "red")+
  geom_point(aes(y = SAMPLE_DO, colour = CYCLE))+
  facet_wrap(~BEAKER, nrow = 2, ncol = 7)

#Save the plot
ggsave(file=file.path("Plots/20250922_MLBG_DO_V_TIME_FULL.PNG"), plot =EXPL_PLOT, width = 10,
       height = 10, units = "in", dpi = 1300)
#Code Mausoleum####

#Meant to create DATE_TIME column formatted as date_time. Not needed
#df.cl$DATE_TIME <- with(df.cl, mdy(df.cl$DATE, hms(df.cl$TIME)))
