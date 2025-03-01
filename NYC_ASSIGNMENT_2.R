##task 1

install.packages("corrplot")
install.packages("odbc")
install.packages("DBI")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tibbletime")
install.packages("factoextra")
install.packages("forecast")
install.packages("tidyverse")
install.packages("lubridate")

library(corrplot)
library(odbc)
library(DBI)
library(dplyr)
library(ggplot2)
library(tibbletime)
library(factoextra)
library(forecast)
library(tidyverse)
library(lubridate)

AllData_Joined <- NYC_TRANSACTION_DATA %>% 
  left_join(BUILDING_CLASS, by = c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID")) %>% 
  left_join(NEIGHBORHOOD, by = c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID"))

##Task 2

BAYSIDE <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", TYPE=="RESIDENTIAL") %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)

summary(BAYSIDE)

## Task 3 - CLEANING DATA of BAYSIDE

CLEAN_BAYSIDE <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)

summary(CLEAN_BAYSIDE)

## Task 5 - Including Other Neraby Neighborhoods

ASTORIA <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="ASTORIA", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)


COLLEGE_POINT <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="COLLEGE POINT", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)


BRIARWOOD <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="BRIARWOOD", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)


JACKSON_HEIGHTS <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="JACKSON HEIGHTS", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)

rm(FLUSHING_SOUTH)
rm(BRIARWOOD)
rm(JACKSON_HEIGHTS)


FLUSHING_SOUTH <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="FLUSHING-SOUTH", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)


FLUSHING_NORTH <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="FLUSHING-NORTH", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)


#PLotting Graphs
## COMPARING BAYSIDE DATA RAW AND CLEAN BAYSIDE DATA

ggplot()+
  geom_line(data=CLEAN_BAYSIDE, size=1, aes(x=SaleYear, y=Avg,colour="royalblue3"))+
  geom_line(data=BAYSIDE,linewidth=1,aes(x=SaleYear, y = Avg, color="chocolate1"))+
  scale_color_identity(name="Neighborhood",breaks=c("royalblue3","chocolate1"),labels=c("CLEAN_BAYSIDE", "BAYSIDE"), guide = "legend")+
  scale_x_continuous(breaks=seq(2003, 2022, by =3))+
  scale_y_continuous(breaks=seq(0, 6000, by = 500))+ theme(panel.background = element_rect(fill = "lightskyblue1", color = "black"))+
  labs(title = "Average Sales Performance by Year")

#Plotting only Clean_Bayside on Graph to see trends by EACH years

ggplot()+
  geom_line(data=CLEAN_BAYSIDE, size=1, aes(x=SaleYear, y=Avg,colour="royalblue3"))+
  scale_color_identity(name="Neighborhood",breaks=c("royalblue3"),labels=c("CLEAN_BAYSIDE"), guide = "legend")+
  scale_x_continuous(breaks=seq(2003, 2022, by =1))+
  scale_y_continuous(breaks=seq(0, 6000, by = 500))+ theme(panel.background = element_rect(fill = "lightskyblue1", color = "black"))

#COMPARING BAYSIDE WITH OTHER NEIGHBORHOOD
## From 2003 to 2021


ggplot()+
  geom_line(data=CLEAN_BAYSIDE, size=2, aes(x=SaleYear, y=Avg,colour="royalblue3"))+
  geom_line(data=ASTORIA,linewidth=1,aes(x=SaleYear, y = Avg, color="springgreen3"))+
  geom_line(data=COLLEGE_POINT,linewidth=1,aes(x=SaleYear, y = Avg, color="red"))+
  geom_line(data=FLUSHING_NORTH,linewidth=1,aes(x=SaleYear, y = Avg, color="yellow"))+
  geom_line(data=FLUSHING_SOUTH,linewidth=1,aes(x=SaleYear, y = Avg, color="grey34"))+
  scale_color_identity(name="Neighborhood",breaks=c("royalblue3","springgreen3","red","yellow","grey34"),labels=c("CLEAN_BAYSIDE","ASTORIA","COLLEGE_POINT","FLUSHING_NORTH","FLUSHING_SOUTH"), guide = "legend")+
  scale_x_continuous(breaks=seq(2003, 2022, by =2))+
  scale_y_continuous(breaks=seq(0, 6000, by = 300)) + theme(panel.background = element_rect(fill = "thistle2", color = "black"))+
  labs(title = "Average Sale Price by Year")


# PRICE BY SQUAREFOOT


combined_neighborhood<- bind_rows(CLEAN_BAYSIDE %>% mutate(Neighborhood = "CLEAN_BAYSIDE"),ASTORIA %>% mutate(Neighborhood = "ASTORIA"),COLLEGE_POINT %>% mutate(Neighborhood = "COLLEGE POINT"),FLUSHING_SOUTH %>% mutate(Neighborhood = "FLUSHING-SOUTH"),FLUSHING_NORTH %>% mutate(Neighborhood = "FLUSHING-NORTH"))


combined_neighborhood %>%
  ggplot(aes(x = SaleYear, y = TotalSales / TotalSqft, color = Neighborhood)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Price per Square Foot by Year and Neighborhood", x = "Year", y = "Price per Square Foot") +
  scale_x_continuous(breaks = seq(min(combined_data$SaleYear), max(combined_data$SaleYear), by = 2)) +
  scale_color_manual(values = c("CLEAN_BAYSIDE" = "royalblue","ASTORIA" = "springgreen3","COLLEGE POINT" = "red","FLUSHING-SOUTH" = "yellow","FLUSHING-NORTH" = "grey34")) +
  theme(panel.background = element_rect(fill = "thistle2", color = "black"))




##Correlation 

correlation_neighborhood <- AllData_Joined %>%
  filter(NEIGHBORHOOD_NAME %in% c("CLEAN_BAYSIDE", "ASTORIA", "COLLEGE POINT", "FLUSHING-SOUTH", "FLUSHING-NORTH"), TYPE == "RESIDENTIAL", SALE_PRICE > 100000, GROSS_SQUARE_FEET > 350) %>%
  mutate(SaleYear = year(SALE_DATE)) %>%
  select(SALE_PRICE, GROSS_SQUARE_FEET, SaleYear)

correlation_matrix <- cor(correlation_neighborhood, use = "complete.obs")

corrplot(correlation_matrix, method = "circle", title = "Correlation Between sale price, gross square feet, and year")


#SUmmary

summary(ASTORIA)
summary(COLLEGE_POINT)
summary(CLEAN_BAYSIDE)
summary(FLUSHING_NORTH)
summary(FLUSHING_SOUTH)

