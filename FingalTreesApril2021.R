## Mairead Manifold - Fingal Trees - Project - April 2020
## Lecturer: Dr Eugene O'Loughlin 
install.packages("readr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("stingr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right"))


## DUBLIN Trees
dublinTrees <- read_csv("data_smart_dublin_dataset_trees - 36000.csv")  ## 35,925 obs

dublinTrees$Town <- as.factor(dublinTrees$Town)
unique(dublinTrees$Town) ## identify towns in Fingal and created a vector with these town names. 
dublinTrees[is.na(dublinTrees$Town),] ## 845 are NAs 

towns <- c("Airways Industrial", "Balbriggan", "Baldoyle", "Bayside", "Corduff", "Castlehaney", "Balgriffin", "Ballyboughal", "Balrothery", "Blanchardstown", "Castaheany", "Castleknock", "Carpenterstown", "Clonee", "Clonsilla", "Demesne", "Donabate", "Elmgreen", "Forrest", "Garristown", "Hartstown", "Howth", "Huntstown", "Hunter's Run",  "Kinsealy",  "Lanesborough", "Littlepace", "Loughshinny", "Lusk", "Malahide", "Melrose", "Mulhuddart", "Naul", "Oldtown","Pheasant Run", "Portmarnock", "Portrane", "Royal Oak", "Rush", "Santry", "Saint Margaret", "Seatown", "Skerries", "Sutton", "Swords", "Waterville")


## Several enteries don't have the town listed. Using following function to locate towns in the address field
## and update the missing town details in the town field. There are 845 Town NAs where an address is give. Running following function
## reducted the number of NAs from 845 to 67
dublinTrees <- dublinTrees %>% mutate(Town =  sub(paste0("^(?:.*(", paste(towns,collapse = "|"), ").*|.*)$"), "\\1", Address))


dublinTrees[is.na(dublinTrees$Town),] ## no of NAs reduced to 67


dublinTrees <- dublinTrees[!is.na(dublinTrees$Town),] ## 35,857 observations remaining

## Merge some towns to their nearest neighbour town district in instances where I don't have either 
## corresponding population or property price data

dublinTrees[dublinTrees$Town == "Bayside", "Town",] <- "Baldoyle"

dublinTrees[dublinTrees$Town == "Castaheany" | dublinTrees$Town == "Hunter's Run" | dublinTrees$Town == "Huntstown" | dublinTrees$Town == "Pheasant Run", "Town"] <- "Mulhuddart"

dublinTrees[dublinTrees$Town == "Demesne" | dublinTrees$Town == "Forrest" | dublinTrees$Town == "Melrose", "Town"] <- "Swords"

dublinTrees[dublinTrees$Town == "Loughshinny", "Town"] <- "Rush"

dublinTrees[dublinTrees$Town == "Elmgreen" | dublinTrees$Town == "Littlepace" | dublinTrees$Town == "Waterville", "Town"] <- "Blanchardstown"

dublinTrees[dublinTrees$Town == "Royal Oak", "Town"] <- "Santry"

unique(dublinTrees$Town) ## checking what towns I have narrowed down my selection to = 43



## remove Airways Industrial, Ballboughal, Lanesborough and Saint Margaret's as there are not suitable for 
## incorporation in other towns and I don't have comparative data. 

dublinTrees <- dublinTrees %>% 
  filter(Town != "Airways Industrial" & Town != "Ballyboughal" & Town != "Lanesborough" & Town != "Saint Margaret")

## similar issue - removing towns where I don't have any property price details: Corduff, Seatown and Hartstown

dublinTrees <- dublinTrees %>% ## removing unwanted towns
  filter(Town != "Corduff" & Town != "Seatown" & Town != "Hartstown" & Town != "" )


dublinTrees %>%  ## 1,699  of the Common Name have NAs 
  filter(is.na(dublinTrees$Common_Name))


sum(!is.na(dublinTrees$Common_Name))## 31,969 tree names can be identified


dublinTrees %>%  ## 9,846 of 28,862 
  filter(Age_Desc == "Semi-Mature")

dublinTrees %>%  ## 34 of 28,862 
  filter(Age_Desc == "Over-Mature")

dublinTrees %>%  ## 5,988 of 28,862 
  filter(Age_Desc == "Young")

dublinTrees %>%  ## 1,184 of 28,862  
  filter(Age_Desc == "Newly Planted")

dublinTrees %>%  ## 19 of 28,862
  filter(Age_Desc == "Veteran")

  ## INSIGHT 1 - Trees per neighbourhood in Fingal 

treesByTown <-  
  dublinTrees %>%
  group_by(Town) %>%
  summarize(TotalTrees = sum(!is.na(Tree_ID))) ## totals to 32,971
  
ggplot(treesByTown, aes(x = Town, y = TotalTrees )) +
   geom_col(fill = "dark green") +
  labs(x = "Towns", y = "Identified Public Trees", title = "Fingal County Council Tree Inventory",
       subtitle = "For Selected Towns",
       caption = "Source: Dublinked: Open Data Store") + 
       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

dublinPop <- read_csv("myHoneIEDemographics.csv")  ## 35,925 obs
unique(dublinPop$Town)

ggplot(dublinPop, aes(x = Town, y = Pop )) +
  geom_col(fill = "light blue") +
  labs(x = "Towns", y = "Population", title = "Fingal County Council Population Breakdown",
       subtitle = "For Selected Towns",
       caption = "Source: Central Statistics Office (CSO)") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))



ggplot(dublinPop2, aes(x = Pop, y = TotalTrees, color = Town )) +
  geom_point(size = 3) +
  labs(x = "Population", y = "Public Trees", title = "Population Vs Trees in Fingal Towns ",
       subtitle = "For Selected Finga Towns",
       caption = "Source: Central Statistics Office (CSO) & DublinKed") + 
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))

lm(dublinPop2$Pop ~ dublinPop2$TotalTrees)

cor(dublinPop2$Pop, dublinPop2$TotalTrees)


## END OF INSIGHT 1 


##PROPERTIES - House prices DATA CLEANSING 

properties <- read_csv("PPR-ALL.csv", col_names = c("Date_of_Sale", "Address", "Postal_Code", "County", "Price","Not_Full_Market_Price", "Vat_Exclusive", "Description_of_Property", "Property_Size_Description"))


## keep properties that reached full value only as they may skew the results
properties %>%
  filter(Not_Full_Market_Price == "Yes") # = 20,396 rows, so removing them

#remove 20,396 rows so now have 398,797 
properties <- properties %>%
  filter(Not_Full_Market_Price == "No") # = Removing 20,396 rows and left with 398,787

# drop the columns: "Postal_Code"(3), Not_Full_Market_Price" (6), "Vat_Exclusinve" (7) 
properties <- properties[-c(3,6,7)] ## 6 variables remaining


unique(properties$Description_of_Property) #identify Irish language dwelling descriptions

## subset properties to only English language property descriptions - removed 31 so down to 398,766
properties <- properties %>%
  filter(Description_of_Property == "Second-Hand Dwelling house /Apartment" | Description_of_Property =="New Dwelling house /Apartment")

## subset properties to only those after 2015 where dates are in the format day/month/year and in character class
unique(properties$Date_of_Sale)

#changed date field to date format. 
properties$Date_of_Sale <- as.Date(properties$Date_of_Sale, format = "%d/%m/%Y")
unique(properties$Date_of_Sale)

## reduced list down to 267,123
properties <- properties %>%
  filter(Date_of_Sale > "2015-01-01") %>%
  arrange(Date_of_Sale)



unique(properties$Price)

##using stringr package, remove the pence from price.

properties$Price <- str_sub(properties$Price, end=-3)

## alternative data cleansing - properties$Price <- gsub(".{3}$", "", properties$Price)

## alternative data cleansing - pproperties$Price = substr(properties$Price, 1, nchar(properties$Price)-3)


##removing all other non numerical characters from price such as x80, commas and full stop. 

properties$Price <- gsub("[^A-Za-z0-9]", "", properties$Price)
unique(properties$Price)
# converting prices variable to numeric and updating other variable formats
properties$Price <- as.numeric(properties$Price)
properties$Description_of_Property <- as.factor(properties$Description_of_Property)
properties$Property_Size_Description <- as.factor(properties$Property_Size_Description )

townsP <- c("Balbriggan", "Baldoyle", "Balrothery", "Blanchardstown", "Castleknock",  "Clonee", "Clonsilla", "Corduff", "Donabate", "Hartstown", "Howth", "Kinsealy", "Lusk", "Malahide", "Mulhuddart", "Naul", "Oldtown","Portmarnock", "Portrane", "Rush", "Santry", "Seatown", "Skerries", "Sutton", "Swords")

## Find town in Address and use it to create a Town column which does not exist 
properties <- properties %>% mutate(Town =  sub(paste0("^(?:.*(", paste(townsP,collapse = "|"), ").*|.*)$"), "\\1", Address)) 

unique(properties$Town)


properties %>%  ## 85,681 of these are in Dublin county
  select(Date_of_Sale, Address, County, Price, Town) %>%
  filter(County == "Dublin")

properties %>%  ## 181,441 of these are outside Dublin
  select(Date_of_Sale, Address, County, Price, Town) %>%
  filter(County != "Dublin")


## convert "" in Town to NAs
properties$Town <- gsub("^$|^ $", NA, properties$Town)

properties %>% #4,673
  filter(!is.na(Town)) ## only 4,673 relate to Fingal towns I am investigating

#checking no "" remaining
unique(properties$Town)

properties %>%  ## 4,543
  filter(Town %in% towns) ## 4,872 in total 

properties %>%  ## 4,543
  filter(Town %in% towns, County == "Dublin") 


## looking at the town distribution, only the Clonee in Co Meath should
## be included in my property prices list. 
propertiesCloneeMeath <- properties %>%  ## 111
  filter(Town %in% towns, County == "Meath", Town == "Clonee")  


propertiesMyTowns <- properties %>%  ## 4,543
  filter(Town %in% towns, County == "Dublin") 

##combine the Clonee properties in Co Meath in my properties data set
## using the rbind() function. 
propertiesMyTowns <- rbind(propertiesMyTowns, propertiesCloneeMeath) ## 5,175 

propertiesMyTowns$Town <- as.factor(propertiesMyTowns$Town)
dublinTrees$Town <- as.factor(dublinTrees$Town)

unique(propertiesMyTowns$Town)
unique(dublinTrees$Town)


## DUBLIN POPULATION Dataset

dublinPop <- read_csv("myHoneIEDemographics.csv")  ## 35,925 obs
unique(dublinPop$Town)

propertiesMyTownsMeanPriceByPrDe <-  
  propertiesMyTowns %>%
  group_by(Town, Description_of_Property) %>%
  summarize(meanPrice = mean(Price))


## To include property prices in the dublinPopulation table, I used the spread() function 
housePricesByPropertyType <- spread(propertiesMyTownsMeanPriceByPrDe, Description_of_Property, meanPrice)

adddf_prices <- merge(dublinPop, housePricesByPropertyType, by = "Town")
adddf_trees <- merge(adddf_prices, treesByTown, by = "Town")

## to facilitate ggplot inclusion of a third variable with legends, I used the gather() function to group 
## related columns together and make it possible to include the columns in a ggplot graph

dublinPop2 <- gather(adddf_trees, FamilyType, PerFamType, 5:9)
dublinPop3 <- gather(dublinPop2, ResAge, PerResAge, 5:9)
dublinPop4 <- gather(dublinPop3, HouseAge, PerHouseAge, 5:10)
dublinPop5 <- gather(dublinPop4, HouseType, PerHouseType, 5:7)
dublinPop6 <- gather(dublinPop5, HouseOld_New, MeanPriceHouse, 5:6)




## INSIGHT 2 - Mean Property prices across Ireland in 2015 Vs 2019 - using t-test to see if they
## are the same 
## t.test mean property price 2015 versu mean property price 2019

## gives you mean house price by Town district
propertiesMyTownsMeanPriceByPrDe <-  
  propertiesMyTowns %>%
  group_by(Town, Description_of_Property) %>%
  summarize(meanPrice = mean(Price))


## Trees by age profile of residents
ggplot(dublinPop6, aes(x = Town, y = TotalTrees/900, color = MeanPriceHouse )) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "Towns", y = "Identified Public Trees", title = "Fingal County Council Public Space Trees",
       subtitle = "By house price",
       caption = "Dublinked and PSRA", color = "Mean House Price") 




ggplot(dublinPop6, aes(x = Town, y = MeanPriceHouse/900, color = ResAge )) +
  geom_col(fill = "grey") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))  +
  labs(x = "Towns", y = "Average Property Prices", title = "Fingal County Council House Prices",
       subtitle = "Vs Population Age Profile",
       caption = "Central Statistics Office (CSO)", color = "Resident  Age")



propertiesMeanPrice2015 <- properties %>% ## 72k observations
  filter(Date_of_Sale >= "2015-01-01" & Date_of_Sale >= "2015-12-01", County == "Dublin") %>%
  arrange(Date_of_Sale) %>%
  mutate(meanPropertyPriceDublin_2015 = mean(Price)) ## don't need this

propertiesMeanPrice2015 <-propertiesMeanPrice2015 [-c(1:3,5:8)] 

propertiesMeanPrice2016 <- properties %>% ## 57k observations
  filter(Date_of_Sale >= "2016-01-01" & Date_of_Sale >= "2016-12-01", County == "Dublin") %>%
  arrange(Date_of_Sale) %>%
  mutate(meanPropertyPriceDublin_2016 = mean(Price)) ## don't need this

propertiesMeanPrice2016 <-propertiesMeanPrice2016 [-c(1:3,5:8)] 

propertiesMeanPrice2017 <- properties %>% ## 41k observations
  filter(Date_of_Sale >= "2017-01-01" & Date_of_Sale >= "2017-12-01", County == "Dublin") %>%
  arrange(Date_of_Sale) %>% 
  mutate(meanPropertyPriceDublin_2017 = mean(Price))  ## don't need this

propertiesMeanPrice2017 <-propertiesMeanPrice2017  [-c(1:3,5:8)] 

propertiesMeanPrice2018 <- properties %>% ## 23k observations
  filter(Date_of_Sale >= "2018-01-01" & Date_of_Sale >= "2018-12-01", County == "Dublin") %>%
  arrange(Date_of_Sale) %>%
  mutate(meanPropertyPriceDublin_2018 = mean(Price)) ## don't need this

propertiesMeanPrice2018 <-propertiesMeanPrice2018  [-c(1:3,5:8)] 

propertiesMeanPrice2019 <- properties %>% ## 5k observations
  filter(Date_of_Sale >= "2019-01-01" & Date_of_Sale >= "2019-12-01", County == "Dublin") %>%
  arrange(Date_of_Sale) %>%
  mutate(meanPropertyPriceDublin_2019 = mean(Price)) ## don't need this

propertiesMeanPrice2019 <-propertiesMeanPrice2019 [-c(1:3,5:8)]  


##T.TEST INSIGHT 2 

t.test(propertiesMeanPrice2016$Price, propertiesMeanPrice2017$Price)


## END OF INSIGHT 2


## INSIGHT 3 

## Checking the presence of outliers in property prices I used to calculate mean property prices for the various 
## towns under review in Fingal. 


boxplot(Price ~ Town, data = propertiesMyTowns) 


## INSIGHT 4


## gives you mean house price by Town district
propertiesMyTownsMeanPriceByPrDe <-  
  propertiesMyTowns %>%
  group_by(Town, Description_of_Property) %>%
  summarize(meanPrice = mean(Price))

ggplot(propertiesMyTownsMeanPriceByPrDe, aes(x = Town, y = meanPrice, color = Description_of_Property )) +
  geom_col(fill = "white")  +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(x = "Towns", y = "Property Prices", title = "Fingal County Council House Prices",
       subtitle = "New Dwellings vs Second Hand Dwellings",
       caption = "Source: Property Services Regulatory Authority (PSRA)") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  

lm(dublinPop2$`New Dwelling house /Apartment` ~ dublinPop2$`Second-Hand Dwelling house /Apartment`)

plot(dublinPop2$`New Dwelling house /Apartment`,dublinPop2$`Second-Hand Dwelling house /Apartment`, xlab = "New House Prices", ylab = "Second Hand House Prices" )




#
