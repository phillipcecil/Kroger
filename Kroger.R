#Analysis of Kroger Data

############################
#Contents
############################
#Preparing the data
#Number of Unique Items by Store
#Mean Adjustment Unit Quantity by Store
#Mean Adjustment Unit Quantity by Store for Each Commodity Name
#Commodities with the Highest and Lowest Mean AUQ
#Focus on  University Plaza Kroger

############################
#Preparing the data
############################

library(tidyverse)
setwd("/Users/phillipcecil/Desktop")
library(readr)
Inventory_Data <- read_csv("Inventory DataB.csv")
glimpse(Inventory_Data)
#Find the classes of the variables
lapply(Inventory_Data, class) 
#Renaming variables
Inventory_Data$store_number <- factor(Inventory_Data$`Store #`)
Inventory_Data$Base_GTIN_Number <- factor(Inventory_Data$`Base GTIN Number`)
Inventory_Data$AUQ <- Inventory_Data$`Adjustment Unit Quantity`
#Search for missing values
library(DataExplorer)
plot_missing(Inventory_Data) 


############################
#Number of Unique Items by Store
############################

funct_store_stock <- function(x){length(unique(Inventory_Data$Base_GTIN_Number[which(Inventory_Data$store_number==x)]))}
unique_stores <- unique(Inventory_Data$store_number)
store_stock_table <- data.frame('number_unique_items'=mapply(funct_store_stock, unique_stores))
store_stock_table <- cbind(store_stock_table,'store_id'=unique_stores )
#Barplot of Number of Unique Items by Store
store_stock_barplot <- ggplot(store_stock_table, aes(x = reorder(store_id,number_unique_items ), y = number_unique_items))+
  geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  labs(title="Number of Unique Items by Store", y="Number of Unique Items", x="Store ID" ) +
  theme(  axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5)  ) 
store_stock_barplot

############################
#Mean Adjustment Unit Quantity by Store
############################

#Here I calculate the mean AUQ for each store
funct_store_mean <- function(x){mean(Inventory_Data$AUQ[which(Inventory_Data$store_number==x)])}
store_mean_table <- data.frame('mean_AUQ'=mapply(funct_store_mean, unique_stores))
store_mean_table <- cbind(store_mean_table,'store_id'=unique_stores )
#Making a barplot
mean_barplot <- 
  ggplot(store_mean_table, aes(x = reorder(store_id,mean_AUQ ), y = mean_AUQ)) +
  geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  labs(title="Mean Adjustment Unit Quantity by Store", y="AUQ", x="Store ID" ) + 
  theme(  axis.text.x = element_text(angle = 90, hjust = 1), 
          plot.title = element_text(hjust = 0.5)  ) 
mean_barplot

#Density Plot 
mean_density_plot <-
  ggplot(store_mean_table, aes(x=mean_AUQ)) +  
  geom_density() + 
  labs(title="Frequency Distribution of mean AUQ", y="Density", x="mean AUQ" ) +
  geom_vline(aes(xintercept=mean(mean_AUQ)),
             color="blue", linetype="dashed", size=1)
mean_density_plot



############################
##Mean Adjustment Unit Quantity by Store for Each Commodity Name
############################

funct_mean_each_comm <- function(x){mean(Inventory_Data$`Adjustment Unit Quantity`[which(Inventory_Data$`Commodity Name`==x)])}
unique_comm <- unique(Inventory_Data$`Commodity Name`)
mean_each_comm_table <- data.frame('mean_AUQ'=mapply(funct_mean_each_comm, unique_comm))
mean_each_comm_table <- cbind(mean_each_comm_table,'commodity_name'=unique_comm)
#remove XXX NO COMMODITY DESCRIPTION
mean_each_comm_table <- subset(mean_each_comm_table, mean_each_comm_table$commodity_name != 'XXX NO COMMODITY DESCRIPTION')

#frequency of each commodity
#I need data on average number of items in inventory --- there are a lot of onions on sale but maybe 3 or 4 copies of a non-tabloid magazine -- 
#so TROPICAL FRUIT has a high mean AUQ but the variance is higher than the variance for books -- so it's hard to compare 
each_commodity_freq_table <- data.frame(table(Inventory_Data$`Commodity Name`))
mean_each_comm_table <- cbind(mean_each_comm_table,'Freq'=each_commodity_freq_table$Freq)

############################
#Commodities with the Highest and Lowest Mean AUQ
############################
##get the objects with the highest and lowest mean AUQ
mean_each_comm_lowest <- mean_each_comm_table[order(mean_each_comm_table$mean_AUQ)[1:10],]
mean_each_comm_highest <- mean_each_comm_table[order(-mean_each_comm_table$mean_AUQ)[1:10],]

#Remove outliers and do it again
mean_each_comm_table_nosmalls <- subset(mean_each_comm_table, mean_each_comm_table$Freq >= 20)
mean_each_comm_nosmall_lowest <- mean_each_comm_table_nosmalls[order(mean_each_comm_table_nosmalls$mean_AUQ)[1:10],]
mean_each_comm_nosmall_highest <- mean_each_comm_table_nosmalls[order(-mean_each_comm_table_nosmalls$mean_AUQ)[1:10],]


############################
#Focus on  University Plaza Kroger
############################
#University PLaza Kroger -- 00929
#subset University PLaza
subset(locations_dfC, select=c("store_id", "store_name", "addresses"))

Inventory_Data_univ <- subset(Inventory_Data, Inventory_Data$`Store #`=='00929')
#get top ten lowest and highest commodites in corryville
funct_mean_each_comm_univ <- function(x){mean(Inventory_Data_univ$`Adjustment Unit Quantity`[which(Inventory_Data_univ$`Commodity Name`==x)])}
unique_comm_univ <- unique(Inventory_Data_univ$`Commodity Name`)
mean_each_comm_table_univ <- data.frame('mean_AUQ'=mapply(funct_mean_each_comm_univ, unique_comm_univ))
mean_each_comm_table_univ <- cbind(mean_each_comm_table_univ,'commodity_name'=unique_comm_univ)
#remove XXX NO COMMODITY DESCRIPTION
mean_each_comm_table_univ <- subset(mean_each_comm_table_univ, mean_each_comm_table_univ$commodity_name != 'XXX NO COMMODITY DESCRIPTION')
#frequency of each commodity
each_commodity_freq_table_univ <- data.frame(table(Inventory_Data_univ$`Commodity Name`))
mean_each_comm_table_univ <- cbind(mean_each_comm_table_univ,'Freq'=each_commodity_freq_table_univ$Freq)

#Commodities with the Highest and Lowest Mean AUQ
##get the objects with the highest and lowest mean AUQ
mean_each_comm_lowest_univ <- mean_each_comm_table_univ[order(mean_each_comm_table_univ$mean_AUQ)[1:10],]
mean_each_comm_highest_univ <- mean_each_comm_table_univ[order(-mean_each_comm_table_univ$mean_AUQ)[1:10],]
#Remove outliers and do it again
mean_each_comm_table_univ_nosmalls <- subset(mean_each_comm_table_univ, mean_each_comm_table_univ$Freq >= 20)
mean_each_comm_univ_nosmall_lowest <- mean_each_comm_table_univ_nosmalls[order(mean_each_comm_table_univ_nosmalls$mean_AUQ)[1:10],]
mean_each_comm_univ_nosmall_highest <- mean_each_comm_table_univ_nosmalls[order(-mean_each_comm_table_univ_nosmalls$mean_AUQ)[1:10],]


#Export Files
library(writexl)
write_xlsx(mean_each_comm_table, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_table.xlsx')
write_xlsx(mean_each_comm_lowest, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_lowest.xlsx')
write_xlsx(mean_each_comm_highest, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_highest.xlsx')
write_xlsx(mean_each_comm_nosmall_lowest, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_nosmall_lowest.xlsx')
write_xlsx(mean_each_comm_nosmall_highest, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_nosmall_highest.xlsx')

write_xlsx(mean_each_comm_table_univ, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_table_univ.xlsx')
write_xlsx(mean_each_comm_lowest_univ, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_lowest_univ.xlsx')
write_xlsx(mean_each_comm_highest_univ, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_highest_univ.xlsx')
write_xlsx(mean_each_comm_univ_nosmall_lowest, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_univ_nosmall_lowest.xlsx')
write_xlsx(mean_each_comm_univ_nosmall_highest, '/Users/phillipcecil/Desktop/Kroger/mean_each_comm_univ_nosmall_highest.xlsx')

#1. show graph of top and bottom 10 commodities include info on Freq--- whoah! look at those outliers!Maybe they don't have enough observations.....
#2. show graph again withou small freqs removed
#Remark that you need information on number of items usually in the inventory -- otherwise it is difficult to compare different commodities -- 
#books are noteable however since you wouldn't expect a lot of items in the inventory  -- same with backpacks and camping


############################
#Getting the Locations of the Stores
############################

#A google search revealed that the addresses of each Kroger store in the dataset are available from Kroger's website
#the URL's for each store look like this: https://www.kroger.com/stores/details/014/00438
#use selector gadget in Google Chrome
library('rvest')
kroger_urls <-  paste0('https://www.kroger.com/stores/details/014/' ,unique(Inventory_Data$store_number))  #a list of urls for each moth
length(unique(Inventory_Data$store_number))#30
datalist <- list()
for (i in 1:30) {
  kroger_html <- read_html(kroger_urls[i])
  kroger_nodes <- html_nodes(kroger_html, ".StoreAddress-storeAddressGuts")
  kroger_text <- html_text(kroger_nodes)[1] #here you need [1] because for some reason the Kroger website repeats the data twice. usually you don't need [1]
  dat <- data.frame(kroger_text)
  dat$i <- i
  datalist[[i]] <- dat
}
all_the_links = do.call(rbind, datalist)
all_the_links_B <- cbind(all_the_links, 'store_number'=unique(Inventory_Data$store_number))
#one of the links doesn't work (number 16) so I found the address on google
#1934 needmore rd dayton oh 45414 --- 00933
all_the_links_B$kroger_text <- as.character(all_the_links_B$kroger_text)
all_the_links_B$kroger_text[16]  <- '1934 needmore rd dayton oh 45414'

############################
##Getting Latitude and Longitude from the addresses and Store Names
############################

#Coordinates
library(ggmap)
register_google(key = "AIzaSyA6UDojGFb5ko4y-uRICOV-54OG4wObAzE")
geocode.list <- data.frame("addresses"=all_the_links_B$kroger_text)
geocode.list$addresses <- as.character(geocode.list$addresses)
df_for_mapping <- cbind(geocode.list,store_mean_table )
locations_df <- mutate_geocode(df_for_mapping, addresses)

locations_dfB <- cbind(locations_df, 'number_unique_items'=store_stock_table$number_unique_items)
locations_dfB

#Get Store Names
datalistB <- list()
for (i in 1:30) {
  kroger_html <- read_html(kroger_urls[i])
  kroger_nodes <- html_nodes(kroger_html, ".StoreDetails-header")
  kroger_text <- html_text(kroger_nodes)[1] #here you need [1] because for some reason the Kroger website repeats the data twice. usually you don't need [1]
  dat <- data.frame(kroger_text)
  dat$i <- i
  datalistB[[i]] <- dat
}
all_the_linksB = do.call(rbind, datalistB)
all_the_linksB_B <- cbind(all_the_linksB, 'store_number'=unique(Inventory_Data$store_number))
all_the_linksB_B$kroger_text <- as.character(all_the_linksB_B$kroger_text)
all_the_linksB_B$kroger_text[16]  <- '1934 needmore rd dayton'


