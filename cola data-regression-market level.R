setwd("C:/Users/ASUS/Desktop/AMM/SWP 2")
data<-read.csv("dat.coke.pepsi.csv")
datat<-read.csv("dat.coke.pepsi.csv")
nrow(datat)
install.packages('huxtable')
install.packages('jtools')
install.packages('officer')
install.packages('flextable')
library(huxtable)
library(jtools)
library(officer)
library(flextable)
####################################################################
#### check some descriptives of the data
####################################################################

nrow(data)
head(data)
str(data)

tmp<-aggregate(cbind(DOLLARS)
               ~  VOL_EQ, 
               data = data, sum, na.rm = TRUE)
write.csv(tmp,"tmp.csv")
write.csv(tmp,"tmp2.csv")

revenue_per_store = aggregate(cbind(DOLLARS)~IRI_KEY, data = data, sum, na.rm = TRUE)
revenue_per_market = aggregate(cbind(DOLLARS)~MARKET, data = data, sum, na.rm = TRUE)

write.csv(revenue_per_store,"revenue_per_store.csv")

revenue_per_brand= aggregate(cbind(DOLLARS)~L5, data = data, sum, na.rm = TRUE)
write.csv(revenue_per_brand,"revenue_per_brand.csv")

revenue_per_packaging = aggregate(cbind(DOLLARS)~VOL_EQ, data = data, sum, na.rm = TRUE)
write.csv(revenue_per_packaging,"revenue_per_packaging.csv")


####################################################################
####
#### convert units to liter and price to price per liter
####
#### Aggregate data to the market level (ignore stores)
####
####################################################################

data$liter <- data$VOL_EQ/0.3521*2 #<<<<litres of one unit/pack of units like pack of 12/24 cans
#> summary(data$liter)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.5919  0.5919  2.0000  3.4180  4.2602  8.5203
data$price.per.liter <- data$price/data$liter
str(data)
data$liter<-data$UNITS*data$liter # <<<<<<<------total litres sold by the store in one week----------->>>>>>>
#> summary(data$UNITS*data$liter)
#> 
#> 
#> 
datat$liter <- datat$VOL_EQ/0.3521*2 #<<<<litres of one unit/pack of units like pack of 12/24 cans
datat$price.per.liter <- datat$price/datat$liter
datat$liter<-datat$UNITS*datat$liter # <<<<<<<------total litres sold by the store in one week----------->>>>>>>

datat$week <- datat$WEEK-1269


#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.592    42.000   102.244   413.385   344.000 31418.631 
t1<-(lm(log(liter)~-1+log(price.per.liter),data=data))
t2<-(lm(log(liter)~log(price.per.liter)+display_all,data=data))
t3<-(lm(log(liter)~-1+L5+log(price.per.liter)+display_all,data=data))
export_summs(t3)
anova(t1,t2) # check ANOVA

summary(t3)
anova(t2,t3)

t4<-(lm(log(liter)~-1+L5+L5:log(price.per.liter)+display_all,data=data))
summary(t4)
export_summs(t4)

anova(t1,t2,t3,t4)


str(data)
data$volprice<-data$price.per.liter*data$liter #<-----simply the revenue, value is same for Dollars and volprice--->>
data$vold1<-data$display_minor*data$liter
data$vold2<-data$display_major*data$liter
data$volf1<-data$feature_small*data$liter
data$volf2<-data$feature_medium*data$liter
data$volf3<-data$feature_large*data$liter

data$voldall<-data$display_all*data$liter
data$volfall<-data$feature_all*data$liter



#<<<<-----removing the store wise data---------------->>>
#aggreagting data across YEAR+WEEK+MARKET+L4+L5+VOL_EQ+PACKAGE and removing IRI_KEY from the data

tmp<-aggregate(cbind(liter, DOLLARS, volprice, vold1, vold2, volf1, volf2, volf3, voldall,volfall)
               ~  YEAR+WEEK+MARKET+L4+L5+VOL_EQ+PACKAGE, 
               data = data, sum, na.rm = TRUE)
head(tmp)
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
nrow(cola)
data_replaced <- cola
data_replaced$feature_all<-ifelse(data_replaced$feature_all < 0.5,0,1)
data_replaced$feature_small<-ifelse(data_replaced$feature_small < 0.5,0,1)
data_replaced$feature_medium<-ifelse(data_replaced$feature_medium < 0.5,0,1)
data_replaced$feature_large<-ifelse(data_replaced$feature_large < 0.5,0,1)
data_replaced$display_minor<-ifelse(data_replaced$display_minor < 0.5,0,1)
data_replaced$display_major<-ifelse(data_replaced$display_major < 0.5,0,1)
data_replaced$display_all<-ifelse(data_replaced$display_all < 0.5,0,1)
cola<-data_replaced
table(cola$L5)
aggregate(cbind(liter, price.per.liter)
          ~  L5+VOL_EQ+PACKAGE, 
          data = data, mean, na.rm = TRUE)

cola <- cola[ which(cola$L5 !="COKE ZERO"),]
nrow(cola)
cola$thanksgiving <- ifelse(cola$WEEK==1317,1,
                            ifelse(cola$WEEK==1369,1,ifelse(cola$WEEK==1421,1,0)))
cola$week <- cola$WEEK-1269


ggplot( data = price_week_flavor,aes(x=week, y=price, group=L5, color=L5)) +
  geom_line()+ggtitle("Weekly Price Fluctuations from 2004-2006")

ggplot( data = price_week_flavor,aes(x=week, y=price, group=L5, color=L5)) +
  geom_line()+
  facet_wrap(~L5, scale="free_y")+ggtitle("Weekly Price Fluctuations from 2004-2006")


cola$july4 <- ifelse(cola$week==27,1,
                     ifelse(cola$week==79,1,ifelse(cola$week==131,1,0)))


cola$pre_chris <- ifelse(cola$week==51,1,
                         ifelse(cola$week==103,1,ifelse(cola$week==156,1,0)))
cola$chris <- ifelse(cola$week==52,1,
                     ifelse(cola$week==104,1,ifelse(cola$week==157,1,0)))

cola$new_year <- ifelse(cola$week==1,1,
                        ifelse(cola$week==53,1,ifelse(cola$week==105,1,0)))

reg.results.1<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving+july4+pre_chris+chris,data=cola)
summary(reg.results.1)

reg.results.2<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving:as.factor(VOL_EQ)+july4:as.factor(VOL_EQ)+pre_chris:as.factor(VOL_EQ)+chris:as.factor(VOL_EQ),data=cola)
summary(reg.results.2)
export_summs(reg.results.2,model.names = c("Model 1"),to.file = "word",file.name = "festivals.docx")


###<<<<<<<<<<<<---- subset cola data---------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
all_brands <- cola
nrow(all_brands)
table(cola$L5)
cola.cc<-subset(cola,L5 == "COKE CLASSIC")
cola.cz<-subset(cola,L5 == "COKE ZERO")
cola.dc<-subset(cola,L5 == "DIET COKE")
cola.p<-subset(cola,L5 == "PEPSI")
cola.dp<-subset(cola,L5 == "DIET PEPSI")
cola <- rbind(cola.cc,cola.cz,cola.dc)
table(cola$L5)
nrow(cola)
pepsi <- rbind(cola.p,cola.dp)
nrow(pepsi)
cola_600ml <- subset(cola,VOL_EQ==0.1042)
nrow(cola_600ml)
cola_2l <- subset(cola,VOL_EQ==0.3521)
nrow(cola_2l)
cola_24cans <- subset(cola,VOL_EQ==1.5)
nrow(cola_24cans)
tmp<-subset(cola,VOL_EQ=="0.1042")
nrow(tmp)
summary(lm(log(liter)~-1+L5+log(price.per.liter)+display_all,data=tmp)) 
t1_cola_data <- lm(log(liter)~-1+L5+log(price.per.liter)+display_all,data=tmp)
write.csv(t1_cola_data$coefficients,"t1_cola_data.csv")
export_summs(t1_cola_data)


tmp<-subset(all_brands,VOL_EQ=="1.5")
nrow(tmp)
t2_cola_data<- lm(log(liter)~L5+log(price.per.liter)+display_all,data=tmp)
summary(t2_cola_data)
export_summs(t2_cola_data,to.file = "word",file.name = "reg_test.docx")



cola$size<-as.factor(cola$VOL_EQ)

reg.results <- lm(log(liter)~-1+MARKET:L5:size+MARKET:L5:size:log(price.per.liter),data=cola)
summary(reg.results)

reg.results <- lm(log(liter)~-1+MARKET:L5:size+MARKET:L5:size:log(price.per.liter)+MARKET:L5:size:display_all+size:thanksgiving,data=cola)
summary(reg.results)


####################################################################
#### drop Coke Zero and Pick the 12 cans
####################################################################

cola<-subset(cola,VOL_EQ==0.75)




str(cola)
reg.results.1<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving+july4+pre_chris+chris+new_year,data=cola)
summary(reg.results.1)
reg.results.1<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving,data=cola)
summary(reg.results.1)
nrow(cola)


# make explanatory variable brand specific
reg.results.1<-lm(log(liter)~-1+L5+log(price.per.liter)+display_all+thanksgiving,data=cola)
summary(reg.results.1)

reg.results.2<-lm(log(liter)~-1+L5+L5:log(price.per.liter)+display_all+thanksgiving,data=cola)
summary(reg.results.2)


reg.results.3<-lm(log(liter)~-1+L5+L5:log(price.per.liter)+L5:display_all+thanksgiving,data=cola)
summary(reg.results.3)

export_summs(reg.results.1,reg.results.2,reg.results.3,model.names = c("Model 1","Model 2","Model 3"),to.file = "word",file.name = "reg_result_cola.docx")
head(cola)

reg.results.4<-t1<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+display_all,data=cola))
reg.results.5<-t1<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+display_major+display_minor,data=cola))
reg.results.6<-t1<-(lm(log(liter)~-1+MARKET+L5:MARKET+MARKET:L5:log(price.per.liter)+MARKET:L5:display_major+MARKET:L5:display_minor,data=cola))
summary(reg.results.6)
#export_summs(reg.results.4,reg.results.5,reg.results.6,model.names = c("Model 1","Model 2","Model 3"),to.file = "word",file.name = "reg_result_cola_interation.docx")

#effect of display and feature together by brand and market
reg.results.7<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:feature_large+MARKET:L5:feature_medium+MARKET:L5:feature_small,data=cola))
summary(reg.results.7)
#export_summs(reg.results.7,model.names = c("Model 1"),to.file = "word",file.name = "reg_result_feature_interaction_v2.docx")

reg.results.8<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+display_all:feature_all,data=cola))
summary(reg.results.8)
#export_summs(reg.results.8,model.names = c("Model 1"),to.file = "word",file.name = "reg_result_dis_and_feature_interation_v2.docx")

reg.results.9<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:display_all:feature_all,data=cola))
summary(reg.results.9)

reg.results.10<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:(display_all*feature_all),data=cola))
summary(reg.results.10)
export_summs(reg.results.10,model.names = c("Coke Data"),to.file = "word",file.name = "reg.results.10.docx")


export_summs(reg.results.9,model.names = c("Model 1"),to.file = "word",file.name = "dis_and_feature_interationby_market+brand_v2.docx")


##<<===========================2 litre bottle=======================================>>>>
table(cola_2l$L5)
cola_2l <- cola_2l[ which(cola_2l$L5 !="COKE ZERO"),]

head(cola_2l)
reg.results.10<-t1<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+MARKET:L5:display_major+MARKET:L5:display_minor,data=cola_2l))
summary(reg.results.10)
export_summs(reg.results.10,model.names = c("Model 1"),to.file = "word",file.name = "display_interation_2_litre.docx")

#effect of display and feature together by brand and market
reg.results.11<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+MARKET:L5:feature_large+MARKET:L5:feature_medium+MARKET:L5:feature_small,data=cola_2l))
summary(reg.results.11)

reg.results.12<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+display_all:feature_all,data=cola_2l))
summary(reg.results.12)
reg.results.13<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+MARKET:L5:display_all:feature_all,data=cola_2l))
summary(reg.results.13)
export_summs(reg.results.11,reg.results.12,reg.results.13,model.names = c("Model 1","Model 2","Model 3"),to.file = "word",file.name = "dis_and_feature_interation_2_litre_v2.docx")

reg.results.10<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:(display_all*feature_all),data=cola_2l))
summary(reg.results.10)
export_summs(reg.results.10,model.names = c("Coke Data"),to.file = "word",file.name = "reg.results.10_2l.docx")

pepsi_2l <- subset(pepsi,VOL_EQ == 0.3521)
nrow(pepsi_2l)

reg.results.10<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:(display_all*feature_all),data=pepsi_2l))
summary(reg.results.10)
export_summs(reg.results.10,model.names = c("Coke Data"),to.file = "word",file.name = "reg.results.10_2l_pepsi.docx")

##<<===========================600ml bottle=======================================>>>>
pepsi_600ml <- subset(pepsi, VOL_EQ == 0.1042)
table(cola_600ml$L5)
cola_600ml <- cola_600ml[ which(cola_600ml$L5 !="COKE ZERO"),]

head(cola_600ml)
reg.results.14<-t1<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+MARKET:L5:display_major+MARKET:L5:display_minor,data=cola_600ml))
summary(reg.results.14)

#effect of display and feature together by brand and market
reg.results.11<-(lm(log(liter)~-1+MARKET:L5:log(price.per.liter)+MARKET:L5:feature_large+MARKET:L5:feature_medium+MARKET:L5:feature_small,data=cola_600ml))
summary(reg.results.11)
reg.results.11<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:feature_large+MARKET:L5:feature_medium+MARKET:L5:feature_small,data=cola_600ml))
###<<<-weird???????when i added just brand and market interaction, price elasticity by market and brand redcued??? why?? how??
reg.results.12<-(lm(log(liter)~-1+MARKET:L5:log(price.per.liter)+display_all:feature_all,data=cola_2l))
summary(reg.results.12)
reg.results.13<-(lm(log(liter)~-1+MARKET:L5:log(price.per.liter)+MARKET:L5:display_all:feature_all,data=cola_2l))
summary(reg.results.13)
export_summs(reg.results.10,reg.results.11,reg.results.12,reg.results.13,model.names = c("Model 1","Model 2","Model 3","Model 4"),to.file = "word",file.name = "dis_and_feature_interation_2_litre.docx")



reg.results.10<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:(display_all*feature_all),data=cola_600ml))
summary(reg.results.10)
export_summs(reg.results.10,model.names = c("Coke Data"),to.file = "word",file.name = "reg.results.10_600.docx")

reg.results.10<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:(display_all*feature_all),data=pepsi_600ml))
summary(reg.results.10)
export_summs(reg.results.10,model.names = c("Coke Data"),to.file = "word",file.name = "reg.results.10_600_pepsi.docx")


####<<<<<<<<<<<<<<,-----24 CANS-------------------------------------------------->>>>>>>>>
table(cola_24cans$L5)
cola_24cans <- cola_24cans[ which(cola_24cans$L5 !="COKE ZERO"),]

head(cola_24cans)


reg.results.10<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:(display_all*feature_all),data=cola_24cans))
summary(reg.results.10)
export_summs(reg.results.10,model.names = c("Coke Data"),to.file = "word",file.name = "reg.results.10_24cans.docx")



reg.results.14<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+MARKET:L5:display_major+MARKET:L5:display_minor,data=cola_24cans))
summary(reg.results.14)
##<<<---reg.results.14 is Question Number 3--------------------------->>>>>>>>>>>>>>>>>>>>>>>


#effect of display and feature together by brand and market
reg.results.11<-(lm(log(liter)~-1+MARKET:L5:log(price.per.liter)+MARKET:L5:feature_large+MARKET:L5:feature_medium+MARKET:L5:feature_small,data=cola_24cans))
summary(reg.results.11)
reg.results.11<-(lm(log(liter)~-1+MARKET:L5+MARKET:L5:log(price.per.liter)+MARKET:L5:feature_large+MARKET:L5:feature_medium+MARKET:L5:feature_small,data=cola_24cans))
###Question 1 - when i added just brand and market interaction, price elasticity by market and brand redcued??? why?? how??
reg.results.12<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+display_all:feature_all,data=cola_2l))
summary(reg.results.12)
reg.results.13<-(lm(log(liter)~-1+L5:MARKET+MARKET:L5:log(price.per.liter)+MARKET:L5:display_all:feature_all,data=cola_24cans))
summary(reg.results.13)
export_summs(reg.results.10,reg.results.11,reg.results.12,reg.results.13,model.names = c("Model 1","Model 2","Model 3","Model 4"),to.file = "word",file.name = "dis_and_feature_interation_2_litre.docx")

##>>>>>>-----check it later??????????????????????????
reg.results.15<-t1<-(lm(log(liter)~-1+MARKET+L5:MARKET+MARKET:L5:log(price.per.liter)+display_all,data=cola_24cans))
#Question 2 - adding brand or market without interaction is removing some regression terms?why?

summary(reg.results.15)


# <<<<,<<<<<<--------------------very imp regression-------------------->>>>>>>>>
reg.results.1<-lm(log(liter)~-1+L5+L5:log(price.per.liter)+L5:display_all+thanksgiving,data=cola) 
summary(reg.results.1)
confint(reg.results.1) # check this Confidence Interval concept, what is meant by overlapping confidence interval bounds
# check autocorrelation
library(car)
dwt(reg.results.1)
head(cola[150:160,1:6],20)
head(reg.results.1$residuals)


# correct for autocorrelation
cola$cc<-ifelse(cola$L5=="COKE CLASSIC",1,0)
cola$dc<-ifelse(cola$L5=="DIET COKE",1,0)
cola$p<-ifelse(cola$L5=="PEPSI",1,0)
cola$dp<-ifelse(cola$L5=="DIET PEPSI",1,0)

reg.results.1<-lm(log(liter)~-1+cc+dc+p+dp+L5:log(price.per.liter),data=cola)
summary(reg.results.1)

res.ts <- ts(residuals(reg.results.1))
lag1res <- lag(res.ts, -1)
?lag
?ts.intersect
lagdata1 <- ts.intersect(res.ts, lag1res)
rho <- coef(lm(res.ts ~ lag1res -1, data=lagdata1)) 
rho
y.ts <- ts(log(cola$liter))
#<<<<<<<<<<<--wtf is this code?????????????????????????-------------->>>>>>>>>>>>>
xp.cc.ts <- ts(cola$cc*log(cola$price.per.liter))
plot(xp.cc.ts)
xp.dc.ts <- ts(cola$dc*log(cola$price.per.liter))
xp.p.ts <- ts(cola$p*log(cola$price.per.liter))
xp.dp.ts <- ts(cola$dp*log(cola$price.per.liter))
#<<<<<<<<<<<<<<<fuck ends here----------------------------------->>>>>>>>>>>>>>>>>>>>>>>>
x.cc <- ts(cola$cc) #plot this time series
plot(x.cc)
x.dc <- ts(cola$dc)
x.p <- ts(cola$p)
x.dp <- ts(cola$dp)

lag1y <- lag(y.ts, -1)
lag1xp.cc.ts <- lag(xp.cc.ts, -1)
lag1xp.dc.ts <- lag(xp.dc.ts, -1)
lag1xp.p.ts  <- lag(xp.p.ts, -1)
lag1xp.dp.ts <- lag(xp.dp.ts, -1)

lag1cc <- lag(x.cc,-1)
lag1dc <- lag(x.dc,-1)
lag1p<- lag(x.p,-1)
lag1dp <- lag(x.dp,-1)


y.co <- y.ts-rho*lag1y

x1.co <- xp.cc.ts-rho*lag1xp.cc.ts
x2.co <- xp.dc.ts-rho*lag1xp.dc.ts
x3.co <- xp.p.ts-rho*lag1xp.p.ts
x4.co <- xp.dp.ts-rho*lag1xp.dp.ts
x5.co <- x.cc-rho*lag1cc
x6.co <- x.dc-rho*lag1dc
x7.co <- x.p-rho*lag1p
x8.co <- x.dp-rho*lag1dp


reg.results.2 <- lm(y.co ~ -1+x5.co+x6.co+x7.co+x8.co+x1.co+x2.co+x3.co+x4.co)
summary(model.2)

dwt(reg.results.1)
dwt(reg.results.2)

cola[150:160,1:7]
test <- subset(data,MARKET == 'EAU CLAIRE')
nrow(test)
test<- subset(test,WEEK == '1270')
head(test)
# one should further drop all observation of week= 1270 ??????????????????????????why???????


cola$thanksgiving <- ifelse(cola$WEEK==1317,1,
                            ifelse(cola$WEEK==1369,1,ifelse(cola$WEEK==1421,1,0)))



# F-test model comparison
reg.results.1<-lm(log(liter)~-1+L5+log(price.per.liter)+display_all,data=data)
reg.results.2<-lm(log(liter)~-1+L5+log(price.per.liter),data=data)
anova(reg.results.1, reg.results.2)
summary(reg.results.1)



# for expositional purposes we only look at DIET COKE and only PITTSFIELD
cola.remember <- cola ########<<<<<<<<<<-------wtffffff------------------------->>>>>>>>>>>>>>
cola <- subset(cola,cola$L5=="DIET COKE")
cola <- subset(cola,cola$MARKET=="PITTSFIELD")
nrow(cola)

# Create a scatterplot of the data with a fitted simple linear regression
# of liter on price and a horizontal line at the mean of price
model.red<-lm(log(liter)~log(price.per.liter),data=cola)
model.full<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving,data=cola)
summary(model.red)
summary(model.full)

plot(cola$price.per.liter,cola$liter,xlab="Price",ylab="Unit Sales Cola",
     panel.first = c(lines(sort(cola$price.per.liter),fitted(model.red)[order(cola$price.per.liter)]), #dont understand this code???
                     abline(v=mean(cola$price.per.liter),lty=2))) #here its mean of litre not price

plot(cola$price.per.liter,cola$liter,xlab="Price",ylab="Unit Sales Cola",
     panel.last=c(lines(sort(cola$price.per.liter),fitted(model.full)[order(cola$price.per.liter)]),
                  abline(v=mean(cola$price.per.liter),lty=2)))
?abline
nrow(cola)
# Calculate th SSE for the full and the reduced model
SSE.red<-sum(residuals(model.red)^2)
SSE.full<-sum(residuals(model.full)^2)

# Calculate the F-st to compare the reduced vs. the full model
Ftest<- (SSE.red-SSE.full)/(model.red$df.residual-model.full$df.residual)*model.full$df.residual/SSE.full
Ftest
anova(model.red, model.full)


# F-Test
# full
MSR<-sum((model.full$fitted.values-mean(log(cola$liter)))^2)/(nrow(cola)-model.full$df.residual-1)
MSE<-sum((log(cola$liter)-model.full$fitted.values)^2)/model.full$df.residual
MSR
MSE
MSR/MSE
summary(model.full)
pf(MSR/MSE,1,nrow(cola)-2,lower.tail=F)
# reduced
MSR<-sum((model.red$fitted.values-mean(log(cola$liter)))^2)/(nrow(cola)-model.red$df.residual-1)
MSE<-sum((log(cola$liter)-model.red$fitted.values)^2)/model.red$df.residual
MSR
MSE
MSR/MSE
summary(model.red)
pf(MSR/MSE,1,nrow(cola)-2,lower.tail=F)
nrow(cola)


SSE.red<-sum((log(cola$liter)-mean(log(cola$liter)))^2)
Ftest<- (SSE.red-SSE.full)/(nrow(cola)-model.full$df.residual)*nrow(cola)/SSE.full
Ftest
summary(model.full)
str(model.full)
nrow(cola)
model.full$df.residual



summary(model.full)
summary(model.red)
head(cola)

confint(model.full)
confint(model.red)


model.red<-lm(log(liter)~log(price.per.liter),data=cola)
model.full<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving,data=cola)

# Prediction with 95% confidence interval (?????????????????????????????????????)
x<-predict(model.full, data=cola,interval="confidence", se.fit=T,
           newdata=data.frame(price.per.liter=0.7,display_all=1,thanksgiving=0))

predict(model.full, data=cola,interval="confidence", se.fit=T,
        newdata=data.frame(price.per.liter=c(0.5,1,1.5),display_all=c(1,0.5,0),thanksgiving=c(0,0,0)))




#Display residual plot with fitted (predicted) values on the horizontal axis.
plot(x=fitted(model.full), y=residuals(model.full),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=fitted(model.red), y=residuals(model.red),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

#Display residual plot with price on the horizontal 
plot(x=cola$price.per.liter, y=residuals(model.full),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=cola$price.per.liter, y=residuals(model.red),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

#Display residual plot with display on the horizontal axis.
plot(x=cola$display_all, y=residuals(model.full),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))



#Display residual plot with price*feature on the horizontal axis.
plot(x=(cola$price.per.liter*cola$display_all), y=residuals(model.full),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))


#Display histogram of the residuals.
hist(residuals(model.full), main="",breaks=20)
hist(residuals(model.full), main="",breaks=50)



#Display normal probability plot of the residuals and add a diagonal line to the plot. The argument "datax" determines which way round to plot the axes (false by default, which plots the data on the vertical axis, or true, which plots the data on the horizontal axis).
qqnorm(residuals(model.full), main="", datax=TRUE)
qqline(residuals(model.full), datax=TRUE)

qqnorm(residuals(model.red), main="", datax=TRUE)
qqline(residuals(model.red), datax=TRUE)



###############################################
###   TESTING ASSUMPTIONS #####################
###############################################
#Load the nortest package to access normality tests:
#  - Anderson-Darling
#  - Lilliefors (Kolmogorov-Smirnov)

model.red<-lm(log(liter)~log(price.per.liter),data=cola)
model.full<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving,data=cola)

#install.packages("nortest")
library(nortest)
ad.test(residuals(model.full)) 
ad.test(residuals(model.red)) 
shapiro.test(residuals(model.full)) 
shapiro.test(residuals(model.red)) 
lillie.test(residuals(model.full)) 
lillie.test(residuals(model.red)) 


#Create lotgroup factor variable splitting the sample into two groups.
#Load the car package to access tests for constant error variance:

#  Modified Levene (Brown-Forsythe)
#Cook-Weisberg score (Breusch-Pagan)
#install.packages("car")
library(car)
#install.packages("lmtest")
library(lmtest)
model <- lm(log(liter) ~ log(price.per.liter)+display_all+thanksgiving, data=cola)
summary(model)
cola$pricegroup <- factor(cola$price.per.liter<=mean(cola$price.per.liter))
head(cola)
mean(cola$price.per.liter)

leveneTest(residuals(model), group=cola$pricegroup)
leveneTest(residuals(model), group=factor(cola$display_all>0.5))
ncvTest(model)


# Identify Multicollinearity
vif(model)

hist(rnorm(1000,sd=100),50)

model.test<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving+rnorm(nrow(cola),sd=100), data=cola)
summary(model.test)
vif(model.test)
cola.<-cola
cola.$dd<-cola.$display_all+0.5*cola.$display_all*rnorm(nrow(cola),sd=101)
model.test<-lm(log(liter)~log(price.per.liter)+display_all+dd+thanksgiving, data=cola.)
vif(model.test)
cor(cola.$display_all,cola.$dd)
dwtest(model.full)


summary(model.full)
?exp
exp(predict(model.full, interval="prediction",
            newdata=data.frame(price.per.liter=c((1),(1.5),(2)),display_all=c(1,0.5,0),thanksgiving=c(0,0,0))))

(coefficients(model.full)[c(1,2,3,4)]) 
(confint(model.full)[c(1,2,3,4),]) # 95% CI
summary(model.full)
vif(model.full)

dwt(model.full)
#???solution for multicollinearity and normality
# Weighted least squares = solution for heteroscedasticity).go through this part
model.red<-lm(log(liter)~log(price.per.liter),data=cola)
model.full<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving,data=cola)
summary(model.full)


summary(lm(abs(residuals(model.full)) ~ log(price.per.liter)+display_all+thanksgiving,data=cola)) #<<<<<<whar is this-------------->>>>>???
wls <- 1/fitted(lm(abs(residuals(model.full)) ~ log(price.per.liter)+display_all+thanksgiving,data=cola))^2

model.full.w<-lm(log(liter)~log(price.per.liter)+display_all+thanksgiving,weights=wts,data=cola)
summary(model.full)
summary(model.full.w)




plot(fitted(model.full), rstandard(model.full), col=cola$display_all+1)
plot(fitted(model.full.w), rstandard(model.full.w), col=cola$display_all+1)


# AUTOCORRELATION !!!!!!!!!!!!!!!!!!!!
model <- lm(log(liter) ~ log(price.per.liter)+display_all+thanksgiving, data=cola)
summary(model) 

model <- lm(log(liter) ~ log(price.per.liter), data=cola)
summary(model) 


dwt(model)

# Cochrane-Orcutt Procedure
res.ts <- ts(residuals(model))
lag1res <- lag(res.ts, -1)
lagdata1 <- ts.intersect(res.ts, lag1res)
rho <- coef(lm(res.ts ~ lag1res -1, data=lagdata1)) 
rho
y.ts <- ts(log(cola$liter))
x.ts <- ts(log(cola$price.per.liter))
lag1y <- lag(y.ts, -1)
lag1x <- lag(x.ts, -1)
y.co <- y.ts-rho*lag1y
x.co <- x.ts-rho*lag1x
model.2 <- lm(y.co ~ x.co)
summary(model.2)

dwt(model.2)
summary(model.red)
summary(model.2)

acp <- rho
b0 <- coef(model.2)[1]/(1-acp) 
sqrt(vcov(model.2)[1,1])/(1-acp) 
b1 <- coef(model.2)[2] 
b1





###########################################
##### WEATHER
###########################################

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
cor(weather[,2:ncol(weather)])


cola<-merge(cola,weather, by=c("MARKET","WEEK"))


reg.results.1<-lm(log(liter)~-1+L5:MARKET+log(price.per.liter)+MARKET:Temp.Max+display_all,data=c)

summary(reg.results.1)

