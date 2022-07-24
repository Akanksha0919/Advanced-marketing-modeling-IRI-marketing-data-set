# read in data

# read carbbev

setwd("C:/Users/ASUS/Desktop/AMM")
getwd()
library(lavaan)
install.packages("eqs2lavaan")
library(eqs2lavaan)
par(mfrow = c(2,2))
boxplot(data$VOL_EQ,xlab = "vol_eq", ylab = "value")
boxplot(data$UNITS, xlab = "units sold/sales", ylab = "value")
boxplot(data$DOLLARS, xlab = "dollars/revenue generated",ylab = "values")
boxplot(data$price, xlab ="price", ylab ="values")
summary <- summary(data_replaced)
write.csv(summary,"summary.csv")
d <- density(data$price)
plot(d, main="Kernel Density of Miles Per Gallon")

sort(table(data$VOL_EQ))

nrow(data)
ncol(data)
str(data)
head(data,10)
table(data$MARKET)
#EAU CLAIRE PITTSFIELD 
table(data$PACKAGE)
#CAN    GLAS BOTTLE PLASTIC BOTTLE 
unique(data$YEAR)
unique(data$PRODUCT.TYPE)
# "SODA"          "SELTZER WATER" "CLUB SODA"     "TONIC WATER"   "BITTER LEMON" 
unique(data$FLAVOR.SCENT) # 161 values
unique(data$L4) #57 producers
unique(data$L5) #185 brands
unique(data$VOL_EQ)
unique(data$MARKET)

table(data$MARKET)
table(data$PACKAGE)

tmp_weird_values<-subset(data, data$feature_all < 1 & data$feature_all > 0 | data$display_all < 1 & data$display_all > 0)
write.csv(tmp_weird_values,"tmp_weird_values.csv")
data_replaced <- data
data_replaced$feature_all<-ifelse(data_replaced$feature_all < 0.5,0,1)
data_replaced$feature_small<-ifelse(data_replaced$feature_small < 0.5,0,1)
data_replaced$feature_medium<-ifelse(data_replaced$feature_medium < 0.5,0,1)
data_replaced$feature_large<-ifelse(data_replaced$feature_large < 0.5,0,1)
data_replaced$display_minor<-ifelse(data_replaced$display_minor < 0.5,0,1)
data_replaced$display_major<-ifelse(data_replaced$display_major < 0.5,0,1)
data_replaced$display_all<-ifelse(data_replaced$display_all < 0.5,0,1)


revenue_display_minor <-aggregate(cbind(DOLLARS, UNITS)~display_minor,
                                  data = data_replaced, sum, na.rm = TRUE)
revenue_display_major <-aggregate(cbind(DOLLARS, UNITS)~display_major,
                                  data = data_replaced, sum, na.rm = TRUE)

display_name <- c("minor","major")
revenue_by_display = round(c(revenue_display_minor[2,"DOLLARS"]/1e6,revenue_display_major[2,"DOLLARS"]/1e6),3)
df_display <- data.frame(display_name,revenue_by_display)
barplot(height=df_display$revenue_by_display, names=df_display$display_name, col=c("blue","orange"), ylim = c(0,12),ylab = "Revenue in Million",xlab = "Product Dispplay" )



revenue_feature_small <-aggregate(cbind(DOLLARS, UNITS)~feature_small,
                                  data = data_replaced, sum, na.rm = TRUE)

revenue_feature_medium <-aggregate(cbind(DOLLARS, UNITS)~feature_medium,
                                   data = data_replaced, sum, na.rm = TRUE)

revenue_feature_large <-aggregate(cbind(DOLLARS, UNITS)~feature_large,
                                  data = data_replaced, sum, na.rm = TRUE)

feature_name <- c("small","medium","large")
revenue_by_feature = round(c(revenue_feature_small[2,"DOLLARS"]/1e6,revenue_feature_medium[2,"DOLLARS"]/1e6,revenue_feature_large[2,"DOLLARS"]/1e6),3)
paste(round(revenue_by_feature / 1e6, 1))

df <- data.frame(feature_name,revenue_by_feature)
barplot(height=df$revenue_by_feature, names=df$feature_name, col=c("blue","orange","purple"), ylim = c(0,5),ylab = "Revenue in Million",xlab = "Feature/Promotional Ads" )
tmp<-aggregate(cbind(DOLLARS, UNITS)~L5,
               data = tmp_weird_values, sum, na.rm = TRUE)
tmp
nrow(data)
summary(data)
head(data)
table(data$FLAVOR.SCENT)


rev_flavor<-aggregate(cbind(DOLLARS)~FLAVOR.SCENT, 
                      data = data, sum, na.rm = TRUE)

tmp_mean_flavor<-aggregate(cbind(DOLLARS)~FLAVOR.SCENT, 
                           data = data, mean, na.rm = TRUE)


head(tmp)
nrow(tmp)

write.csv(rev_flavor,"rev_flavor.csv")


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<------------------take out only cola data------------------>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

cola <- subset(data_replaced,data_replaced$FLAVOR.SCENT=="COLA")
nrow(cola)
ncol(cola)
str(cola)
summary(cola)
table(cola$L5)


revenue_display_minor_cola <-aggregate(cbind(DOLLARS, UNITS)~display_minor,
                                       data = cola, sum, na.rm = TRUE)
revenue_display_major_cola <-aggregate(cbind(DOLLARS, UNITS)~display_major,
                                       data = cola, sum, na.rm = TRUE)

display_name <- c("minor","major")
revenue_by_display_cola = round(c(revenue_display_minor_cola[2,"DOLLARS"]/1e6,revenue_display_major_cola[2,"DOLLARS"]/1e6),3)
df_display_cola <- data.frame(display_name,revenue_by_display_cola)
barplot(height=df_display_cola$revenue_by_display_cola, names=df_display_cola$display_name, col=c("blue","orange"), ylim = c(0,12),ylab = "Revenue in Million",xlab = "Product Dispplay",main = "Cola Data")



revenue_feature_small <-aggregate(cbind(DOLLARS, UNITS)~feature_small,
                                  data = cola, sum, na.rm = TRUE)

revenue_feature_medium <-aggregate(cbind(DOLLARS, UNITS)~feature_medium,
                                   data = cola, sum, na.rm = TRUE)

revenue_feature_large <-aggregate(cbind(DOLLARS, UNITS)~feature_large,
                                  data = cola, sum, na.rm = TRUE)

feature_name <- c("small","medium","large")
revenue_by_feature = round(c(revenue_feature_small[2,"DOLLARS"]/1e6,revenue_feature_medium[2,"DOLLARS"]/1e6,revenue_feature_large[2,"DOLLARS"]/1e6),3)
paste(round(revenue_by_feature / 1e6, 1))

df <- data.frame(feature_name,revenue_by_feature)
barplot(height=df$revenue_by_feature, names=df$feature_name, col=c("blue","orange","purple"), ylim = c(0,5),ylab = "Revenue in Million",xlab = "Feature/Promotional Ads" ,main = "Cola Data")


library(ggplot2)
library(dplyr)

tmp<-aggregate(cbind(DOLLARS)~MARKET, 
               data = cola, sum, na.rm = TRUE)
tmp


units_sold_per_brand<-aggregate(cbind(UNITS,DOLLARS)~L5, 
                                data = cola, sum, na.rm = TRUE)
write.csv(units_sold_per_brand,"units_sold_per_brand_2.csv")
units_sold_per_brand[order(units_sold_per_brand$UNITS),]

rev_vs_iri_key<-aggregate(cbind(DOLLARS)~IRI_KEY, 
                          data = cola, sum, na.rm = TRUE)
rev_vs_price<-aggregate(cbind(DOLLARS)~price, 
                        data = cola, sum, na.rm = TRUE)
head(rev_vs_price)
plot(rev_vs_price$price,rev_vs_price$DOLLARS/1e6,xlab = "price",ylab = "revenue in million",type = 'l')
write.csv(rev_vs_price,"rev_vs_price.csv")


rev_iri_mark<-aggregate(cbind(DOLLARS)~IRI_KEY+MARKET, 
                        data = cola, sum, na.rm = TRUE)
rev_iri_mark$DOLLARS <- rev_iri_mark$DOLLARS/1e6 
rev_iri_mark$IRI_KEY = factor(rev_iri_mark$IRI_KEY)
ggplot(rev_iri_mark, aes(fill=IRI_KEY, y=DOLLARS, x=MARKET)) + 
  geom_bar(position="dodge", stat="identity")+labs(y = 'Revenue in Million')

iri_store <- c(228037,653776,652159)
iri_data <- subset(cola,cola$IRI_KEY %in% 228037)
rev_iri_brand_228037<-aggregate(cbind(DOLLARS)~L5, 
                                data = iri_data, sum, na.rm = TRUE)
write.csv(rev_iri_brand_228037,'rev_iri_brand_228037.csv')

iri_data <- subset(cola,cola$IRI_KEY %in% 653776)
rev_iri_brand_653776<-aggregate(cbind(DOLLARS)~L5, 
                                data = iri_data, sum, na.rm = TRUE)
write.csv(rev_iri_brand_653776,'rev_iri_brand_653776.csv')

iri_data <- subset(cola,cola$IRI_KEY %in% 652159)
rev_iri_brand_652159<-aggregate(cbind(DOLLARS)~L5, 
                                data = iri_data, sum, na.rm = TRUE)
price_iri_brand_652159<-aggregate(cbind(price)~L5, 
                                  data = iri_data, mean, na.rm = TRUE)
write.csv(price_iri_brand_652159,'price_iri_brand_652159.csv')
write.csv(rev_iri_brand_652159,'rev_iri_brand_652159.csv')
library(viridis)
library(hrbrthemes)



rev_vol_UNITS <-aggregate(cbind(DOLLARS,UNITS)~VOL_EQ, 
                          data = cola, sum, na.rm = TRUE)

rev_vol_UNITS_whole_data <-aggregate(cbind(DOLLARS,UNITS)~VOL_EQ, 
                                     data = data_replaced, sum, na.rm = TRUE)


rev_vol_UNITS_pack <-aggregate(cbind(DOLLARS,UNITS)~VOL_EQ+PACKAGE, 
                               data = data_replaced, sum, na.rm = TRUE)


write.csv(rev_vol_UNITS_pack,"rev_vol_UNITS_pack.csv")

rev_vol_UNITS_pack_price <-aggregate(cbind(DOLLARS,UNITS)~VOL_EQ+PACKAGE+price, 
                                     data = data_replaced, sum, na.rm = TRUE)


write.csv(rev_vol_UNITS_pack_price,"rev_vol_UNITS_pack_price.csv")

write.csv(rev_vol_UNITS,"rev_vol_UNITS.csv")
tmp<-aggregate(cbind(DOLLARS)~PACKAGE, 
               data = cola, sum, na.rm = TRUE)


rev_vol_pack<-aggregate(cbind(DOLLARS)~VOL_EQ+PACKAGE, 
                        data = cola, sum, na.rm = TRUE)



rev_week<-aggregate(cbind(DOLLARS)~WEEK, 
                    data = cola, sum, na.rm = TRUE)

plot(rev_week$WEEK,rev_week$DOLLARS, type="l")

summary(cola$WEEK)

cola$week <- cola$WEEK-1269
tmp<-aggregate(cbind(DOLLARS)~week, 
               data = cola, sum, na.rm = TRUE)
tmp
plot(tmp$week,tmp$DOLLARS, type="l")

Brands <- c('COKE CLASSIC','DIET COKE','PEPSI','DIET PEPSI')


selected_brands <- subset(cola,cola$L5 %in% Brands)
rev_week_flavor<-aggregate(cbind(DOLLARS)~week+L5, 
                           data = selected_brands, sum, na.rm = TRUE)


price_week_flavor<-aggregate(cbind(price)~week+L5, 
                             data = selected_brands, sum, na.rm = TRUE)
# Plot

ggplot( data = rev_week_flavor,aes(x=week, y=DOLLARS, group=L5, color=L5)) +
  geom_line()+
  facet_wrap(~L5, scale="free_y")+ggtitle("Weekly Sales Fluctuations from 2004-2006")

ggplot( data = price_week_flavor,aes(x=week, y=price, group=L5, color=L5)) +
  geom_line()+
  facet_wrap(~L5, scale="free_y")+ggtitle("Weekly Price Fluctuations from 2004-2006")


ggplot( data = rev_week_flavor,aes(x=week, y=DOLLARS, group=L5, color=L5)) +
  geom_line()

ggplot( data = price_week_flavor,aes(x=week, y=price, group=L5, color=L5)) +
  geom_line()+ggtitle("Weekly Price Fluctuations from 2004-2006")

install.packages('ggbump')
library(ggbump)


ggplot( data = rev_week_flavor,aes(x=week, y=DOLLARS, group=L5, color=L5)) +  geom_point(size = 2) +
  geom_bump() 

ggplot( data = price_week_flavor,aes(x=week, y=price, group=L5, color=L5)) +  geom_point(size = 2) +
  geom_bump()+ggtitle("Weekly Price Fluctuations from 2004-2006")

rev_brand <-aggregate(cbind(DOLLARS)~L5, 
                      data = cola, sum, na.rm = TRUE)
rev_brand
write.csv(rev_brand,"rev_brand.csv")


vol <- c(0.75,1.5,0.3521,0.1042)
xyz <- subset(cola,cola$VOL_EQ %in% vol)


rev_vol_pack <-aggregate(cbind(DOLLARS)~VOL_EQ+PACKAGE, 
                         data = xyz, sum, na.rm = TRUE)
rev_vol_pack$VOL_EQ = factor(rev_vol_pack$VOL_EQ)

ggplot(rev_vol_pack, aes(fill=VOL_EQ, y=DOLLARS, x=PACKAGE)) + 
  geom_bar(position="dodge", stat="identity")+labs(y = 'Revenue')+ggtitle("Revenue generated from different Volume and Package Combinations")

can_data <- subset(cola,cola$PACKAGE == 'CAN')
can_brand <- aggregate(cbind(DOLLARS)~L5, 
                       data = can_data, sum, na.rm = TRUE)
write.csv(can_brand,'can_brand.csv')


plastic_bottle_data <- subset(cola,cola$PACKAGE == 'PLASTIC BOTTLE')
plastic_brand <- aggregate(cbind(DOLLARS)~L5, 
                           data = plastic_bottle_data, sum, na.rm = TRUE)
write.csv(plastic_brand,'plastic_brand.csv')


price_vol_pack <-aggregate(cbind(price)~VOL_EQ+PACKAGE, 
                           data = xyz, mean, na.rm = TRUE)
price_vol_pack$VOL_EQ = factor(price_vol_pack$VOL_EQ)

ggplot(price_vol_pack, aes(fill=VOL_EQ, y=price, x=PACKAGE)) + 
  geom_bar(position="dodge", stat="identity")+labs(y = 'price')+ggtitle("Price variation of different Volume and Package Combinations")


vol_pack_week <-aggregate(cbind(VOL_EQ)~week+PACKAGE, 
                          data = cola, mean, na.rm = TRUE)

ggplot( data = vol_pack_week,aes(x=week, y=VOL_EQ, group=PACKAGE, color=PACKAGE)) +
  geom_bump()+ggtitle("Weekly volume trend from 2004-2006 of different packaging")


write.csv(price_vol_pack,"price_vol_pack.csv")
# take out only Coke Classic

cc <- subset (cola, cola$L5=="COKE CLASSIC")

tmp<-aggregate(cbind(DOLLARS)~VOL_EQ, 
               data = cc, sum, na.rm = TRUE)
tmp


tmp<-aggregate(cbind(DOLLARS)~week, 
               data = cc, sum, na.rm = TRUE)
tmp
plot(tmp$week,tmp$DOLLARS, type="l")
tmp<-aggregate(cbind(DOLLARS)~YEAR, 
               data = cc, sum, na.rm = TRUE)
tmp

# check prices of coke classic by vol_eq

tmp<-aggregate(cbind(price)~VOL_EQ+PACKAGE, 
               data = cc, mean, na.rm = TRUE)
tmp


cc$liter<-cc$VOL_EQ/0.3521*2
summary(cc$liter)  
cc$price.per.liter<-cc$price/cc$liter

tmp<-aggregate(cbind(price,price.per.liter)~VOL_EQ+PACKAGE, 
               data = cc, mean, na.rm = TRUE)
tmp

vol_pack<-aggregate(cbind(VOL_EQ)~PACKAGE, 
                    data = cc, mean, na.rm = TRUE)
vol_pack$ounce <- vol_pack$VOL_EQ*192

vol_pack$litre <- vol_pack$VOL_EQ*192*0.03

# take only Coke Classic in 2 liter bottle

cc2liter<-subset(cc,cc$VOL_EQ==0.3521)
nrow(cc2liter)/157

tmp<-aggregate(cbind(DOLLARS)~week, 
               data = cc2liter, sum, na.rm = TRUE)
plot(tmp$week,tmp$DOLLARS, type="l")

tmp<-aggregate(cbind(price)~week, 
               data = cc2liter, mean, na.rm = TRUE)
plot(tmp$week,tmp$price, type="l")

glm(data = cola,DOLLARS)

