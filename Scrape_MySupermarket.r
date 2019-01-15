if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, purrr, dplyr, radarchart, tidyr)

#########List of retailers###############
stores <-
  sort(
    c(
      "Tesco",
      "ASDA",
      "Sainsburys",
      "Morrisons",
      "Ocado",
      "Amazon",
      "Aldi",
      "Lidl",
      "Superdrug",
      "Boots",
      "Iceland",
      "Poundland",
      "Poundstretcher"
      #,"M_And_S"
    )
  )


###########List of search terms/categories ######################

prods <-
  sort(
    c(
      "Pears",
      "Tomatoes",
      "Lettuce",
      "Grapes",
      "Melons",
      "Plums",
      "Apricots",
      "Peaches",
      "Nectarines",
      "Avocado",
      "Kiwi",
      "Cherries",
      "Mushrooms",
      "Blackberries",
      "Blueberries",
      "Raspberries",
      "Strawberries",
      "Mangoes",
      "Exotic_Fruit",
      "Minced_Beef",
      "Diced_Beef",
      "Fresh_Beef",
      "Prepared_Beef",
      "Plain_Yogurt"
    )
 )

###########Loop for generating URLs to scrape ###################
URLs <- list()
for (i in seq_along(prods)) {
  URLs[[i]] <- list()
  for (j in seq_along(stores)) {
    URLs[[i]][j] <- paste0(
      "http://www.mysupermarket.co.uk/Shopping/FindProducts.aspx?query=",
      prods[i],
      "&store=",
      stores[j],
      "&_fcategory=",
      prods[i]
    )
  }
  names(URLs[[i]]) <- stores
}
names(URLs) <- prods


############Function for scraping prices from mysupermarket.co.uk

Scrape_all<-function(URL_list){

products <-
  map(URL_list,  ~ html_text(html_nodes(read_html(.x), ".Prefix")))
prices <- map(URL_list,  ~ html_text(html_nodes(read_html(.x), ".Price")))
price_per_unit <-
  map(URL_list,  ~ html_text(html_nodes(read_html(.x), "#PPU")))

out<-data.frame(product=stack(products)$values[stack(products)$values!=""],
                store=as.character(stack(products)$ind)[stack(products)$values!=""],
                price=stack(prices)$values,
                price_per_unit=stack(price_per_unit)$values)
return(out)
}

#############Run Scraping Function (Takes a few minutes)###############
Out<-map(URLs,Scrape_all)


#############binds output into dataframe and adds column for type of product #############
Out_df <-
  do.call(rbind,
          map2(Out,
               map2(prods,
                    map_int(Out,
                            nrow),
                    ~ rep(.x,
                          .y)), ~
                        cbind(.x,
                              type = .y)))

row.names(Out_df)<-NULL

#######Wrangle prices from currency to number##############
pence<-grep("p",Out_df$price)
Price<-as.numeric(gsub("\\\u00A3|p|\r\n|\\ ","",Out_df$price))
Price[pence]<-Price[pence]/100
Out_tidy<-Out_df
Out_tidy$price<-Price

######Wrangle price per unit from currency to number##############
PPU<-strsplit(as.character(Out_tidy$price_per_unit)," / ")
Units<-unlist(PPU)[cumsum(map(PPU,length))]
PPU_out<-as.character(lapply(PPU, `[[`, 1))
PPU_pence<-grep("p",PPU_out)
PPU_tidy<-as.numeric(gsub("\\\u00A3|p","",PPU_out))
PPU_tidy[PPU_pence]<-PPU_tidy[PPU_pence]/100

PPU_out<-data.frame(Price_per_unit=PPU_tidy,Unit=as.character(Units))

######Wrangle units fron grams to kilos and from item names to "each"###################
grams<-grep("100g",PPU_out$Unit)
PPU_out$Price_per_unit[grams]<-PPU_out$Price_per_unit[grams]*10
PPU_out$Unit[grams]<-"Kg"
PPU_out$Unit<-as.character(PPU_out$Unit)
PPU_out$Unit[PPU_out$Unit!="Kg"]<-"Each"


########Output results#############################
TidyPrices <-
  data.frame(
    Category = Out_tidy$type,
    Item = Out_tidy$product,
    Retailer = Out_tidy$store,
    Shelf_Price = Out_tidy$price,
    Price_per_unit = PPU_out$Price_per_unit,
    Unit = PPU_out$Unit,
    Date = Sys.Date()
  )

write.csv(TidyPrices, row.names = FALSE, file = paste(Sys.Date(), "mysupermarket.csv", sep = " "))


# ##########Create summary plot###########
# 
# TidyPrices %>%
#   filter(Unit=="Kg")%>%
#   group_by(Category,Retailer)%>%
#   summarise(Mean_Price_per_KG = mean(Price_per_unit))%>%
#                spread(Retailer,Mean_Price_per_KG)%>%
#   chartJSRadar(main="Average price (£/kg) for fresh food by retailer where supply chain has been identified as potentially vulnerable to disruption")
#              




