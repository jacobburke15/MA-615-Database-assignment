library(tidyverse)
library(readxl)
library(dplyr)
library(magrittr)
library(DBI)
library(RSQLite)

contribute <- read.csv("TopDonors.csv") ## read function here allows specific sheet reading

## Credit to Harry Cao for both these cleaning functions used below on contributer names 

contribute["contrib"] = gsub(contribute$contrib, pattern = ", ", replacement = ",") ## cleaning last name abnormalities in "contrib" column
contribute["contrib"] = gsub(contribute$contrib, pattern = "\\s\\w", replacement = "")

## replacing groups of same name rows with their respective single unique, minimum length name 

mini = function(x){ 
  
  len = x %>%unique%>% sapply(nchar) 
  
  opt = unique(x) 
  
  mod = head(opt[which(len==min(len))],1) 
  
  return(mod)
  
}

contrib = contribute %>% group_by(contribid,fam)%>% summarise(contrib = mini(contrib))

contribute = contribute %>% select(-"contrib")

contribute = left_join(contribute,contrib,by = c("contribid","fam"))

## Selecting desired tables 

contributor <- select(contribute, contribid, contrib, fam, City, State, Zip, Fecoccemp, lastname, cycle)

contributor %<>% distinct()

donation <- select(contribute, fectransid, contribid, recipid, date, amount, type, cmteid)

donation %<>% distinct()

recipient <- select(contribute, recipid, recipient, party, recipcode)

recipient %<>% distinct()

org <- select(contribute, contribid, orgname, ultorg)

org %<>% distinct()

## Building the database

Donordb <- dbConnect(SQLite(), "donor-db4.sqlite")

dbWriteTable(Donordb, "Contributor", contributor)
dbWriteTable(Donordb, "Donation", donation)
dbWriteTable(Donordb, "Recipient", recipient)
dbWriteTable(Donordb, "Organization", org)

dbDisconnect(Donordb)
 