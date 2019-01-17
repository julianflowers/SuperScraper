if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, purrr, dplyr)

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
  
  Product_ID<-map(URL_list,  ~ html_attr(html_nodes(read_html(.x), "#ProductImage"),"src"))
  Product_ID<-stack(Product_ID)$values[!is.na(stack(Product_ID)$values)]
  Product_ID<-do.call(rbind,strsplit(do.call(rbind,strsplit(Product_ID,"/"))[,7],".jpg"))[,1]
  
  Full_Name<-map(URL_list,  ~ html_attr(html_nodes(read_html(.x), "#ProductImage"),"alt"))
  Full_Name<-stack(Full_Name)$values[!is.na(stack(Full_Name)$values)]
  
  store_and_product <-
    map(URL_list,  ~ html_text(html_nodes(read_html(.x), ".Prefix")))
  
  
  store<- as.character(stack(store_and_product)$ind)[stack(store_and_product)$values!=""]
  
  product<- stack(store_and_product)$values[stack(store_and_product)$values!=""]
  
  price <- map(URL_list,  ~ html_text(html_nodes(read_html(.x), ".Price")))
  price <-stack(price)$values
  
  price_per_unit <- map(URL_list,  ~ html_text(html_nodes(read_html(.x), "#PPU")))
  price_per_unit <-stack(price_per_unit)$values
  
  suffix <-map(URL_list,  ~ html_text(html_nodes(read_html(.x), ".Suffix")))
  suffix<-stack(suffix)$values[stack(suffix)$values!=""]
  
  if (length(suffix)!=length(product)){suffix<-rep("NA",length(product))}
  
  out<-data.frame(Product_ID,
                  Full_Name,
                  product,
                  store,
                  price,
                  price_per_unit,
                  suffix)
  return(out)
}

#############Run Scraping Function (Takes a few minutes)###############
Out<-map(URLs,Scrape_all)

saveRDS(Out, file = paste(Sys.Date(),"out.rds"))

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
    Product_ID = Out_tidy$Product_ID,
    Full_Name = Out_tidy$Full_Name,
    Category = Out_tidy$type,
    Item = Out_tidy$product,
    Retailer = Out_tidy$store,
    Shelf_Price = Out_tidy$price,
    Price_per_unit = PPU_out$Price_per_unit,
    Unit = PPU_out$Unit,
    Suffix = Out_tidy$suffix,
    Date = Sys.Date()
  )

TidyPrices$Product_ID<- gsub("w_74.png","NA",TidyPrices$Product_ID)

TidyPrices%>%
  group_by(Product_ID)%>%
  summarise(N = n())%>%
  arrange(desc(N))%>%
  filter(N>1,Product_ID!="NA")->duplicates

'%ni%' <- Negate('%in%')

TidyPrices<-filter(TidyPrices,Product_ID %ni% duplicates$Product_ID)
TidyPrices$Product_ID[TidyPrices$Product_ID=="NA"]<-as.character(TidyPrices$Full_Name)[TidyPrices$Product_ID=="NA"]


write.csv(TidyPrices,row.names = FALSE, file = paste(Sys.Date(), "mysupermarket.csv", sep = " "))



