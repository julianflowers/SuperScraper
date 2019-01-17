pacman::p_load(rvest)


URL<-"http://www.mysupermarket.co.uk/Shopping/FindProducts.aspx?query=Tomatoes&store=Tesco&_fcategory=Tomatoes"


UID<-read_html(URL)%>%
  html_nodes("#NgMspProductCell")%>%
  html_attr("productid")

