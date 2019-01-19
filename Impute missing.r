
Lookup<-rbind(Out1,Out2)%>%
  select(UID,Product_ID, Full_Name, Category, Item, Size, Date, Shelf_Price)%>%
  spread(key=Date,value=Shelf_Price)
Lookup<-Lookup[,-c(8)]
colnames(Lookup)[7]<-"Shelf_Price"
Lookup<-df2
Lookup2<-select(df2, Item, xID =Product_ID)

Out3<-read_csv(paste(Sys.Date()-2, "mysupermarket.csv"))

Impute<-function(old_df){
  
  Join1<-left_join(old_df,Lookup)
  Join2<-left_join(Join1,Lookup2)
  Join2$Product_ID[is.na(Join2$Product_ID)]<-Join2$xID[is.na(Join2$Product_ID)]
  Join2<-select(Join2, c(names(old_df),Product_ID))%>%
    distinct()
  
  return(Join2)
}

Impute(Out3)%>%
  View()





Out3_test<-left_join(Out3_test,Lookup2)

Out3_test$Product_ID[is.na(Out3_test$Product_ID)]<-Out3_test$xID[is.na(Out3_test$Product_ID)]

