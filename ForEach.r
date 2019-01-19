if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, purrr, dplyr, tidyr, readr, doParallel, foreach)

#########List of retailers###############
stores <-
  sort(
    c(
      "Tesco",
      "ASDA",
      "Sainsburys",
      "Morrisons",
      "Ocado",
      #"Amazon",
      "Aldi",
      "Lidl",
      #"Superdrug",
      #"Boots",
      "Iceland"
      #"Poundland",
      #"Poundstretcher",
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


detectCores()
X<-1

myCluster <- makeCluster(8, # number of cores to use
                         type = "PSOCK") # type of cluster
registerDoParallel(myCluster)


Product_ID <- foreach(y = seq_along(URLs[[x]]),.packages = c("purrr","rvest")) %dopar%
  {
    Product_ID<-read_html(as.character(URLs[[x]][y]))%>%
      html_nodes("#ProductImage")%>%
      html_attr("src")
    
  }

Full_name <- foreach(y = seq_along(URLs[[x]]),.packages = c("purrr","rvest")) %dopar%
{
  Product_ID<-read_html(as.character(URLs[[x]][y]))%>%
    html_nodes("#ProductImage")%>%
    html_attr("alt")
  
}
stopCluster(myCluster)





