pacman::p_load(readr,tidyr,dplyr)


TidyPricesx<-read_csv(paste(Sys.Date(), "mysupermarket.csv"))
TidyPrices2<-read_csv(paste(Sys.Date()-1, "mysupermarket.csv"))
TidyPrices_test<-as.data.frame(TidyPrices)



df<-rbind(TidyPrices,TidyPrices2)%>%
select(UID,Category, Item, Retailer, Shelf_Price,Date)%>%
  spread(key=Date,value=Shelf_Price)%>%
  rename(Price_Today = as.character(Sys.Date()), Price_Yesterday = as.character(Sys.Date()-1))%>%
  mutate(Price_Change = Price_Today-Price_Yesterday, Percent_Change = (Price_Today-Price_Yesterday)/Price_Yesterday)%>%
  filter(!is.na(Price_Today))%>%
  select(UID,Price_Change,Percent_Change)%>%
  right_join(TidyPrices)

str(TidyPrices)
str(TidyPricesx)
