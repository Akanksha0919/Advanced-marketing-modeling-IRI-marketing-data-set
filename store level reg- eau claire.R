setwd("C:/Users/ASUS/Desktop/AMM/SWP 2")
data<-read.csv("dat.coke.pepsi.csv")

####################################################################
#### check some descriptives of the data
####################################################################

data$liter <- data$VOL_EQ/0.3521*2
data$price.per.liter <- data$price/data$liter
data$liter<-data$UNITS*data$liter
data<-subset(data,data$L5!="COKE ZERO")



####################################################################
####
#### convert units to liter and price to price per liter
####
#### Aggregate data to the market level (ignore stores)
####
####################################################################


nrow(data)
str(data)
data$volprice<-data$price.per.liter*data$liter
data$vold1<-data$display_minor*data$liter
data$vold2<-data$display_major*data$liter
data$volf1<-data$feature_small*data$liter
data$volf2<-data$feature_medium*data$liter
data$volf3<-data$feature_large*data$liter

data$voldall<-data$display_all*data$liter
data$volfall<-data$feature_all*data$liter





tmp<-aggregate(cbind(liter, DOLLARS, volprice, vold1, vold2, volf1, volf2, volf3, voldall,volfall)
               ~  YEAR+WEEK+MARKET+L4+L5+VOL_EQ+PACKAGE, 
               data = data, sum, na.rm = TRUE)

data.prepared <-tmp
data.prepared$price.per.liter <- tmp$volprice/tmp$liter
data.prepared$display_minor <- tmp$vold1/tmp$liter
data.prepared$display_major <- tmp$vold2/tmp$liter

data.prepared$feature_small  <- tmp$volf1/tmp$liter
data.prepared$feature_medium <- tmp$volf2/tmp$liter
data.prepared$feature_large  <- tmp$volf3/tmp$liter

data.prepared$display_all <- tmp$voldall/tmp$liter
data.prepared$feature_all <- tmp$volfall/tmp$liter
str(data.prepared)
data.prepared<-data.prepared[,-c(10:17)]

cola<-data.prepared

data_replaced <- data
data_replaced$feature_all<-ifelse(data_replaced$feature_all < 0.5,0,1)
data_replaced$feature_small<-ifelse(data_replaced$feature_small < 0.5,0,1)
data_replaced$feature_medium<-ifelse(data_replaced$feature_medium < 0.5,0,1)
data_replaced$feature_large<-ifelse(data_replaced$feature_large < 0.5,0,1)
data_replaced$display_minor<-ifelse(data_replaced$display_minor < 0.5,0,1)
data_replaced$display_major<-ifelse(data_replaced$display_major < 0.5,0,1)
data_replaced$display_all<-ifelse(data_replaced$display_all < 0.5,0,1)
data<-data_replaced

data_replaced <- cola
data_replaced$feature_all<-ifelse(data_replaced$feature_all < 0.5,0,1)
data_replaced$feature_small<-ifelse(data_replaced$feature_small < 0.5,0,1)
data_replaced$feature_medium<-ifelse(data_replaced$feature_medium < 0.5,0,1)
data_replaced$feature_large<-ifelse(data_replaced$feature_large < 0.5,0,1)
data_replaced$display_minor<-ifelse(data_replaced$display_minor < 0.5,0,1)
data_replaced$display_major<-ifelse(data_replaced$display_major < 0.5,0,1)
data_replaced$display_all<-ifelse(data_replaced$display_all < 0.5,0,1)
cola<-data_replaced

cola.iri_key <- data
cola.market <- cola
nrow(cola.iri_key)
nrow(cola.market)
table(cola.market$display_all)
table(cola.iri_key$display_all)


cola.iri_key$IRI_KEY2 <- as.factor(ifelse((cola.iri_key$IRI_KEY == 653776|cola.iri_key$IRI_KEY == 233779|cola.iri_key$IRI_KEY == 652159|cola.iri_key$IRI_KEY == 1085053), 'high',
                            ifelse((cola.iri_key$IRI_KEY == 257871|cola.iri_key$IRI_KEY == 248128|cola.iri_key$IRI_KEY == 213290|cola.iri_key$IRI_KEY == 264075), 'medium', 'low' 
                                          )))
table(cola.iri_key$IRI_KEY2)
cola.iri_key_ec<-subset(cola.iri_key,MARKET=="EAU CLAIRE")
nrow(cola.iri_key_ec)
cola.iri_key_pitts<-subset(cola.iri_key,MARKET=="PITTSFIELD")


revenue_per_store_ec = aggregate(cbind(DOLLARS)~IRI_KEY, data = cola.iri_key_ec, sum, na.rm = TRUE)

write.csv(revenue_per_store,"revenue_per_store_ec.csv")

aggregate(cbind(liter, price.per.liter)
          ~  MARKET, 
          data = data, mean, na.rm = TRUE)

aggregate(cbind(liter, price.per.liter)
          ~  L5+VOL_EQ+PACKAGE, 
          data = data, mean, na.rm = TRUE)

t1<-(lm(log(liter)~-1+L5:MARKET,data=cola.iri_key))

t1<-(lm(log(liter)~-1+L5+as.factor(IRI_KEY)+L5:MARKET:as.factor(VOL_EQ):log(price.per.liter)+feature_all+display_all,data=cola.iri_key))
summary(t1)
table(cola.iri_key$MARKET)

t1<-(lm(log(liter)~-1+L5+as.factor(IRI_KEY)+log(price.per.liter)+feature_all+display_all,data=cola.iri_key_ec))
summary(t1)

head(cola.market)
tmp<-subset(cola.market,MARKET=="EAU CLAIRE")
nrow(tmp)
t1<-(lm(log(liter)~-1+L5+as.factor(VOL_EQ)+as.factor(VOL_EQ):log(price.per.liter)+feature_all+display_all,data=tmp))
summary(t1)
t1<-(lm(log(liter)~-1+L5:as.factor(VOL_EQ)+as.factor(VOL_EQ):log(price.per.liter)+feature_all+display_all,data=tmp))
summary(t1)
t1<-(lm(log(liter)~-1+L5:as.factor(VOL_EQ)+as.factor(VOL_EQ):L5:log(price.per.liter)+feature_all+display_all,data=tmp))
summary(t1)



# 12 cans
tmp<-subset(cola.iri_key_ec,VOL_EQ=="0.75")
nrow(tmp)

aggregate(cbind(liter)
          ~  L5+VOL_EQ+PACKAGE, 
          data = tmp, sum, na.rm = TRUE)



t1<-(lm(log(liter)~-1+IRI_KEY2:as.factor(VOL_EQ)+L5:MARKET:as.factor(VOL_EQ):log(price.per.liter)+feature_all+display_all,data=cola.iri_key))
export_summs(t1,model.names = c("Model 1"),to.file = "word",file.name = "general reg whole data.docx")

t1<-(lm(log(liter)~-1+L5+as.factor(IRI_KEY)+log(price.per.liter)+feature_all+display_all,data=tmp))
summary(t1)

cola.iri_key_pitts_600 <- subset(cola.iri_key_pitts,VOL_EQ == 0.1042)
cola.iri_key_pitts_2l <- subset(cola.iri_key_pitts,VOL_EQ == 0.3521)
cola.iri_key_pitts_12cans <- subset(cola.iri_key_pitts,VOL_EQ == 0.75)
cola.iri_key_pitts_24cans <- subset(cola.iri_key_pitts,VOL_EQ == 1.5)

#<<<<<<<<<<<<<<<<<--- PITTSFIELD----------------------->>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<--------------600ml -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
table(cola.iri_key_ec_600ml$IRI_KEY2)
table(cola.iri_key_ec_600ml$display_all)

t1<-(lm(log(liter)~-1+IRI_KEY2+L5:log(price.per.liter)+display_all,data=cola.iri_key_pitts_600))
summary(t1)
t1_final<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_pitts_600))
summary(t1_final)

t1<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+display_all+feature_all,data=cola.iri_key_ec_600ml))
summary(t1)

#<<<<<<<<<<<<<<<<<<--------------2 litre -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

t2_final<-(lm(log(liter)~-1+L5+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_pitts_2l))
summary(t2_final)


#<<<<<<<<<<<<<<<<<<--------------12 CANS -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

table(cola.iri_key_pitts_12cans$IRI_KEY2)
table(cola.iri_key_pitts_12cans$L5)
table(cola.iri_key_pitts_12cans$MARKET)
table(cola.iri_key_pitts_12cans$VOL_EQ)

t4_final<-(lm(log(liter)~-1+L5+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_pitts_12cans))
summary(t4_final)

table(cola.iri_key_pitts_12cans$display_all)
table(cola.iri_key_pitts_12cans$feature_all)

#<<<<<<<<<<<<<<<<<<--------------24 CANS -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

table(cola.iri_key_pitts_24cans$IRI_KEY2)
table(cola.iri_key_pitts_24cans$L5)
table(cola.iri_key_pitts_24cans$MARKET)
table(cola.iri_key_pitts_24cans$VOL_EQ)

t5_final<-(lm(log(liter)~-1+L5+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_pitts_24cans))
summary(t5_final)

export_summs(t1_final,t2_final,t4_final,t5_final,model.names = c("600ml","2 litre","12 cans","24 cans"),to.file = "word",file.name = "store category wise reg pittsfield.docx")













#<<<<<<<<<<<<<<----EAU CLAIRE------------------->>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<--------------600ml -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cola.iri_key_ec_600ml <- subset(cola.iri_key_ec,VOL_EQ == '0.1042')
table(cola.iri_key_ec_600ml$IRI_KEY2)
table(cola.iri_key_ec_600ml$display_all)

t1<-(lm(log(liter)~-1+IRI_KEY2+L5:log(price.per.liter)+display_all,data=cola.iri_key_ec_600ml))
summary(t1)
t1_final<-(lm(log(liter)~-1+L5+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_ec_600ml))
summary(t1_final)

t1<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+display_all+feature_all,data=cola.iri_key_ec_600ml))
summary(t1)

#<<<<<<<<<<<<<<<<<<--------------2 litre -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

cola.iri_key_ec_2l <- subset(cola.iri_key_ec,VOL_EQ == '0.3521')
table(cola.iri_key_ec_2l$MARKET)
t2_final<-(lm(log(liter)~-1+L5+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_ec_2l))
summary(t2_final)


#<<<<<<<<<<<<<<<<<<--------------12 CANS -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

cola.iri_key_ec_12_cans <- subset(cola.iri_key_ec,VOL_EQ == '0.75')
table(cola.iri_key_ec_12_cans$IRI_KEY2)
table(cola.iri_key_ec_12_cans$L5)
table(cola.iri_key_ec_12_cans$MARKET)
table(cola.iri_key_ec_12_cans$VOL_EQ)

t4_final<-(lm(log(liter)~-1+L5+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_ec_12_cans))
summary(t4_final)

table(tmp$display_all)
table(tmp$feature_all)

#<<<<<<<<<<<<<<<<<<--------------24 CANS -------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

cola.iri_key_ec_24_cans <- subset(cola.iri_key_ec,VOL_EQ == '1.5')
table(cola.iri_key_ec_24_cans$IRI_KEY2)
table(cola.iri_key_ec_24_cans$L5)
table(cola.iri_key_ec_24_cans$MARKET)
table(cola.iri_key_ec_24_cans$VOL_EQ)

t5_final<-(lm(log(liter)~-1+L5+IRI_KEY2+log(price.per.liter)+display_all*feature_all,data=cola.iri_key_ec_24_cans))
summary(t5_final)

export_summs(t1_final,t2_final,t4_final,t5_final,model.names = c("600ml","2 litre","12 cans","24 cans"),to.file = "word",file.name = "store category wise reg eau claire v2.docx")



table(cola.iri_key_ec_24_cans$display_all)
table(cola.iri_key_ec_24_cans$feature_all)

# include competitor price
# Vol_eq = 0.1042 only with price and compprice
library(plyr)


#<<<<<<<<<<<<<<<competitor's price-----------------------------------------?>>>>>>>>>>>>>>>
###<<<<<<<<<<<<<<<-----600ml bottle--------------------------------------->>>>>>>>>>>>>>>>>
temp <- subset(cola.iri_key_ec_600ml,L5=="PEPSI")
tmp <- subset(cola.iri_key_ec_600ml,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price1<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price1)

#<<<<<<<<<<,--------2 litre-------------------------------------------------->>>>>>>>>>>>>>.
temp <- subset(cola.iri_key_ec_2l,L5=="PEPSI")
tmp <- subset(cola.iri_key_ec_2l,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price2<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price2)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<-------------------12 cans------------------------------->>>>>>>>>>>>>
temp <- subset(cola.iri_key_ec_12_cans,L5=="PEPSI")
tmp <- subset(cola.iri_key_ec_12_cans,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price3<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price3)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<-------------------24 cans------------------------------->>>>>>>>>>>>>
temp <- subset(cola.iri_key_ec_24_cans,L5=="PEPSI")
tmp <- subset(cola.iri_key_ec_24_cans,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price4<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price4)

export_summs(comp_price1,comp_price2,comp_price3,comp_price4,model.names = c("600ml","2 litre","12 Cans","24 Cans"),to.file = "word",file.name = "competitive price coke eau claire.docx")

#<<<<<-------DIET COKE competitors price---------------------------------------------->>>>>>>>>>>>>>>
#<<<<<<<<<<--------------600 ml----------------------------------------------->>>>>>>>>>>>>>>>
temp <- subset(cola.iri_key_ec_600ml,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_ec_600ml,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price1<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price1)


#<<<<<<<<<<--------------2 litre----------------------------------------------->>>>>>>>>>>>>>>>
temp <- subset(cola.iri_key_ec_2l,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_ec_2l,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price2<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price2)

#<<<<<<<<<<--------------12 cans----------------------------------------------->>>>>>>>>>>>>>>>
temp <- subset(cola.iri_key_ec_12_cans,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_ec_12_cans,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price3<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price3)

#<<<<<<<<<<--------------24 cans----------------------------------------------->>>>>>>>>>>>>>>>
temp <- subset(cola.iri_key_ec_24_cans,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_ec_24_cans,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price4<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price4)



export_summs(comp_price1,comp_price2,comp_price3,comp_price4,model.names = c("600ml","2 litre","12 Cans","24 Cans"),to.file = "word",file.name = "competitive price diet coke eau claire.docx")


#<<<<<<<<<,,- pittsfield----------->>>>>>>>>>>>>>>......
temp <- subset(cola.iri_key_pitts_600,L5=="PEPSI")
tmp <- subset(cola.iri_key_pitts_600,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price1<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price1)
#2l
#<<<<<<<<<,,- pittsfield----------->>>>>>>>>>>>>>>......

temp <- subset(cola.iri_key_pitts_2l,L5=="PEPSI")
tmp <- subset(cola.iri_key_pitts_2l,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price2<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price2)

#<<<<<<<<<,,- 12 cans----------->>>>>>>>>>>>>>>......

temp <- subset(cola.iri_key_pitts_12cans,L5=="PEPSI")
tmp <- subset(cola.iri_key_pitts_12cans,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price3<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price3)

#<<<<<<<<<,,- 24 cans----------->>>>>>>>>>>>>>>......
temp <- subset(cola.iri_key_pitts_24cans,L5=="PEPSI")
tmp <- subset(cola.iri_key_pitts_24cans,L5=="COKE CLASSIC")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price4<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price4)

export_summs(comp_price1,comp_price2,comp_price3,comp_price4,model.names = c("600ml","2 litre","12 Cans","24 Cans"),to.file = "word",file.name = "competitive price coke pittsfield.docx")


#<<<-----diet coke pitts
cola.iri_key_pitts_2l <- subset(cola.iri_key_pitts,VOL_EQ == 0.3521)
temp <- subset(cola.iri_key_pitts_600,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_pitts_600,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price1<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price1)
#<<<<<<<<<,,- pittsfield----------->>>>>>>>>>>>>>>......
cola.iri_key_pitts_2l <- subset(cola.iri_key_pitts,VOL_EQ == 0.3521)
temp <- subset(cola.iri_key_pitts_2l,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_pitts_2l,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price2<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price2)

#check for AC
dat <-tmp
# make stores dummies because we cannot work with iri
dat$high <-ifelse(dat$IRI_KEY2=="high",1,0)
dat$med <-ifelse(dat$IRI_KEY2=="medium",1,0)
dat$low  <-ifelse(dat$IRI_KEY2=="low",1,0)

res <- ts(residuals(comp_price2))
lag1res <- lag(res, -1)
lagdata1 <- ts.intersect(res, lag1res)
rho <- coef(lm(res ~ lag1res -1, data=lagdata1)) 
rho # autocorrelation of residuals on lag residuals
dwt(comp_price2)

t1<-(lm(log(liter)~-1+high+med+low+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=dat))
summary(t1)

y.ts <- ts(log(dat$liter))

xp.ts <- ts(log(dat$price.per.liter))
xd.ts <- ts(dat$display_all)
xf.ts <- ts(dat$feature_all)
xdf.ts <- ts(dat$feature_all*dat$display_all)
xcp.ts <- ts(log(dat$compprice))
xhigh.ts <- ts(dat$high)
xlow.ts <- ts(dat$low)
xmed.ts <- ts(dat$med)

# compute the lag of dependent variable and explanatory variables
lag1y <- lag(y.ts, -1)

lagxp.ts <- lag(xp.ts, -1)

lagxd.ts <- lag(xd.ts, -1)
lagxf.ts <- lag(xf.ts, -1)
lagxdf.ts <- lag(xdf.ts, -1)
lagxcp.ts <- lag(xcp.ts, -1)
lagxhigh.ts <- lag(xhigh.ts, -1)
lagxlow.ts <- lag(xlow.ts, -1)
lagxmed.ts <- lag(xmed.ts, -1)

# compute first order differences for dependent and explanatory variable (.co)
y.co <- y.ts-rho*lag1y

xprice.co <- xp.ts-rho*lagxp.ts
xdisplay_all.co <- xd.ts-rho*lagxd.ts
xfeature_all.co <- xf.ts-rho*lagxf.ts

xdf.co <- xdf.ts-rho*lagxdf.ts
xcomp.co <- xcp.ts-rho*lagxcp.ts

xhigh.co <- xhigh.ts-rho*lagxhigh.ts

xlow.co <- xlow.ts-rho*lagxlow.ts
xmed.co <- xmed.ts-rho*lagxmed.ts


t2 <- lm(y.co ~ -1 + xhigh.co + xlow.co + xmed.co +xcomp.co + xfeature_all.co +xdf.co+xdisplay_all.co+xprice.co)
summary(t2)

dwt(t1)
dwt(t2)


summary(t1)
dwt(t1)

#<<<<<<<<<,,- 12 cans----------->>>>>>>>>>>>>>>......
cola.iri_key_pitts_12cans <- subset(cola.iri_key_pitts,VOL_EQ == 0.75)
temp <- subset(cola.iri_key_pitts_12cans,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_pitts_12cans,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price3<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price3)

#<<<<<<<<<,,- 24 cans----------->>>>>>>>>>>>>>>......
cola.iri_key_pitts_24cans <- subset(cola.iri_key_pitts,VOL_EQ == 1.5)
temp <- subset(cola.iri_key_pitts_24cans,L5=="DIET PEPSI")
tmp <- subset(cola.iri_key_pitts_24cans,L5=="DIET COKE")
nrow(tmp)
nrow(temp)
temp<-temp[,c(1:3, 9, 10,22,31)]
temp<-rename(temp, c("price.per.liter"="compprice"))
str(temp)
head(temp)

tmp<-merge(tmp,temp,by=c("WEEK","IRI_KEY","PACKAGE","IRI_KEY2"))
head(tmp)
cor(tmp$price.per.liter,tmp$compprice)
comp_price4<-(lm(log(liter)~-1+IRI_KEY2+log(price.per.liter)+log(compprice)+display_all+feature_all+display_all*feature_all,data=tmp))
summary(comp_price4)

export_summs(comp_price1,comp_price2,comp_price3,comp_price4,model.names = c("600ml","2 litre","12 Cans","24 Cans"),to.file = "word",file.name = "competitive price diet coke pittsfield.docx")


























# Read in weather data
weather<-read.csv("weather_pittsfield_eauclaire.csv")
head(weather)
weather$Date<-as.Date(weather$date, format =  "%m/%d/%Y")
weather$W <- as.Date(cut(weather$Date, "week"))
str(weather)
weather<-aggregate(cbind(Temp.Max,Temp.Avg,Temp.Min,
                         DP.Max,DP.Mean,DP.Min,
                         Humidity.Max,Humidity.Mean,Humidity.Min,
                         Wind.Max,Wind.Mean,Wind.Min,
                         Pressure.Max,Pressure.Mean,Pressure.Min) 
                   ~ MARKET+W, data=weather, mean, na.rm = TRUE)

W<-unique(weather$W)
W<-as.data.frame(W)
W$WEEK<-1270:(1269+nrow(W))
weather<-merge(weather,W,by=c("W"))
weather<-weather[,-1]
head(weather)

# merge weather data with cola.iri_key
cola.iri_key<-merge(cola.iri_key,weather, by=c("MARKET","WEEK"))
head(cola.iri_key)

cola.iri_key$week <- cola.iri_key$WEEK-1269
cola.iri_key_ec_weather <- subset(cola.iri_key,cola.iri_key$MARKET=="EAU CLAIRE")
cola.iri_key_pitts_weather <- subset(cola.iri_key,cola.iri_key$MARKET=="PITTSFIELD")
table(cola.iri_key_ec_weather$week)
nrow(x)
x <- subset(cola.iri_key_ec_weather,week > 13)
x <- subset(x,week < 41)
y<- subset(cola.iri_key_ec_weather,week > 66)
y<- subset(y,week < 94)
z<- subset(cola.iri_key_ec_weather,week > 118)
z<- subset(z,week < 146)

table(x$week)
table(y$week)
table(z$week)

cola.iri_key_ec_weather$season <- as.factor(ifelse((cola.iri_key_ec_weather$week %in% x$week) | (cola.iri_key_ec_weather$week %in% y$week) | (cola.iri_key_ec_weather$week %in% z$week),'summer','winter'))

cola.iri_key$week <- cola$WEEK-1269
library(ggplot2)
price_week_flavor<-aggregate(cbind(price.per.liter)~week+L5, 
                             data = cola.iri_key, mean, na.rm = TRUE)


litre_week_flavor<-aggregate(cbind(liter)~week+L5, 
                             data = cola.iri_key, sum, na.rm = TRUE)
ggplot( data = litre_week_flavor,aes(x=week, y=liter, group=L5, color=L5)) +
  geom_line()+
  facet_wrap(~L5, scale="free_y")+ggtitle("Weekly Liters sold Fluctuations from 2004-2006")
nrow(data)

ggplot( data = price_week_flavor,aes(x=week, y=price.per.liter, group=L5, color=L5)) +
  geom_line()+
  facet_wrap(~L5, scale="free_y")+ggtitle("Weekly Price Fluctuations from 2004-2006")

price_week_flavort<-aggregate(cbind(price.per.liter)~week+L5, 
                             data = datat, mean, na.rm = TRUE)

ggplot( data = price_week_flavort,aes(x=week, y=price.per.liter, group=L5, color=L5)) +
  geom_line()+
  facet_wrap(~L5, scale="free_y")+ggtitle("Weekly Price Fluctuations from 2004-2006")


litre_week_flavort<-aggregate(cbind(liter)~week+L5, 
                             data = datat, sum, na.rm = TRUE)
ggplot( data = litre_week_flavort,aes(x=week, y=liter, group=L5, color=L5)) +
  geom_line()+
  facet_wrap(~L5, scale="free_y")+ggtitle("Weekly Price Fluctuations from 2004-2006")
#<------------600 ml------------------------------------>>>>
cola.iri_key_ec_weather_600 <- subset(cola.iri_key_ec_weather,VOL_EQ==0.1042)
nrow(cola.iri_key_ec_weather_600)

t1<-(lm(log(liter)~IRI_KEY2+log(price.per.liter),data=cola.iri_key_ec_weather_600))
summary(t1)
t1<-(lm(log(liter)~-1+season+IRI_KEY2+Temp.Max*log(price.per.liter),data=cola.iri_key_ec_weather_600))
summary(t1)
#<<<------- 2 litre-------------------------------->>>>>>>>>>>>>>>>
cola.iri_key_ec_weather_2l <- subset(cola.iri_key_ec_weather,VOL_EQ==0.3521)
nrow(cola.iri_key_ec_weather_2l)

t1<-(lm(log(liter)~IRI_KEY2+log(price.per.liter),data=cola.iri_key_ec_weather_2l))
summary(t1)
t1<-(lm(log(liter)~-1+season+IRI_KEY2+Temp.Max*log(price.per.liter),data=cola.iri_key_ec_weather_2l))
summary(t1)
t1<-(lm(log(liter)~-1+season:L5,data=cola.iri_key_ec_weather_2l))
summary(t1)
#<<<------- 12 cans-------------------------------->>>>>>>>>>>>>>>>
cola.iri_key_ec_weather_12cans <- subset(cola.iri_key_ec_weather,VOL_EQ==0.75)
nrow(cola.iri_key_ec_weather_12cans)

t1<-(lm(log(liter)~IRI_KEY2+log(price.per.liter),data=cola.iri_key_ec_weather_12cans))
summary(t1)
t1<-(lm(log(liter)~-1+season+IRI_KEY2+Temp.Max*log(price.per.liter),data=cola.iri_key_ec_weather_12cans))
summary(t1)
t1<-(lm(log(liter)~-1+season:L5,data=cola.iri_key_ec_weather_12cans))
summary(t1)
#<<<------- 24 cans-------------------------------->>>>>>>>>>>>>>>>
cola.iri_key_ec_weather_24cans <- subset(cola.iri_key_ec_weather,VOL_EQ==1.5)
nrow(cola.iri_key_ec_weather_24cans)

t1<-(lm(log(liter)~IRI_KEY2+log(price.per.liter),data=cola.iri_key_ec_weather_24cans))
summary(t1)
t1<-(lm(log(liter)~-1+season+log(price.per.liter)+IRI_KEY2,data=cola.iri_key_ec_weather_24cans))
summary(t1)
t1<-(lm(log(liter)~-1+season:L5,data=cola.iri_key_ec_weather_24cans))
summary(t1)

###<<<<-----------PITTSFIELD------------------------------------->>>>>>>>>>
cola.iri_key_pitts_weather$season <- as.factor(ifelse((cola.iri_key_pitts_weather$week %in% x$week) | (cola.iri_key_pitts_weather$week %in% y$week) | (cola.iri_key_pitts_weather$week %in% z$week),'summer','winter'))

cola.iri_key_pitts_weather_24cans <- subset(cola.iri_key_pitts_weather,VOL_EQ==1.5)
nrow(cola.iri_key_pitts_weather_24cans)

t1<-(lm(log(liter)~IRI_KEY2+log(price.per.liter),data=cola.iri_key_pitts_weather_24cans))
summary(t1)
t1<-(lm(log(liter)~-1+season+log(price.per.liter)+IRI_KEY2,data=cola.iri_key_pitts_weather_24cans))
summary(t1)
t1<-(lm(log(liter)~-1+season:L5,data=cola.iri_key_pitts_weather_24cans))
summary(t1)




