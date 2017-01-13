### Use dataset from https://www.kaggle.com/START-UMD/gtd

### National Consortium for the Study of Terrorism and Responses to Terrorism (START). (2016). 
### Global Terrorism Database [Data file]. Retrieved from https://www.kaggle.com/START-UMD/gtd

# 1 Full Graphocal Analysis-------------------------------------
require(ggplot2)
library(RColorBrewer)
library(data.table)
require(reshape2)
require(ggmap)
require(scales)
require(rgeos)
require(rangeMapper)
require(zoo)
library(rattle)
source("http://peterhaschke.com/Code/multiplot.R")


dt.crimes_ld <- fread("~/Downloads/Crimes_chi.csv", header=TRUE,sep=",")
dt.crimes <- dt.crimes_ld[Year>=2007]
colnames(dt.crimes)
dt.crimes[, c("Beat","FBI Code","X Coordinate","Y Coordinate","Location") := NULL]
rexp <- "\\d{2}/\\d{2}/\\d{4}\\s?(.*)$"
dt.crimes$Time=sub(rexp,"\\1",dt.crimes$Date)
dt.crimes$Date<-as.Date(dt.crimes$Date,format='%m/%d/%Y')

# p1 Relative Amount of Crime by Type Year Domestic ------
## Relative Amount of Crime by Type Year Domestic
t1<-as.data.table(dt.crimes[,.N,by=list(`Primary Type`,Domestic)][order(-N)])

labeller1 <- c(
  `true` = "Domestic Violence",
  `false` = "Non-Domestic")

p1=ggplot(t1[N>=500],aes(x=`Primary Type`,y=N, fill=`Primary Type`,colour=`Primary Type`))+ 
  geom_bar(alpha=c(0.6),stat = "identity") +
  facet_grid(~Domestic ,margins = FALSE,labeller = as_labeller(labeller1)) +
  ggtitle("Frequency by Crime Type") + ylab("Count")+
  scale_x_discrete(limits=t1[N>=500,sum(N),by=`Primary Type`][order(V1)]$`Primary Type`)+
  scale_y_sqrt(breaks=c(2e3,1e4,5e4,1e5,3e5,7e5),labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position="none")+
  coord_flip()


t2<-as.data.table(dt.crimes[Domestic=="false",.N,by=list(`Primary Type`,Year)][order(-N)])
t2<-t2[,SD:=N/max(N),by=`Primary Type`]

# p2 Heatmap plot showing change of different crimes within group over years------
p2=ggplot(t2[,MEAN:=min(N),by=`Primary Type`][MEAN>85], aes(Year, `Primary Type`)) + geom_tile(aes(fill = SD),colour = "white") + 
  scale_fill_gradient(low = "#FFCCCC", high = "#990000")+
  scale_x_continuous(breaks=seq(2007,2016))+
  
# 2 Indexed crimes -------
crime_index <- fread("~/Downloads/IUCR__Codes.csv", header=TRUE,sep=",",colClasses = c(IUCR="character"), 
                     drop = c("PRIMARY DESCRIPTION","SECONDARY DESCRIPTION"))

dt.crimes$IUCR<-sub("[0](?=\\d{3})", "", dt.crimes$IUCR,perl = TRUE) 
setkey(crime_index,"IUCR")
setkey(dt.crimes,"IUCR")
dt.crimes.ind<-dt.crimes[crime_index]

### Indexed crimes are more serious crimes that need to be reported to FBI
ind.crimes<-dt.crimes.ind[`INDEX CODE`=="I"& Domestic=="false"]

### Further clean data based on location types --
### Remove some locations we are not interested in these locations now
locations=ind.crimes[,.N,by=`Location Description`][order(-N)][1:70]$`Location Description`
ind.crimes.u=ind.crimes[`Location Description` %in% locations]
ind.crimes.u[, c("Beat","Updated On","FBI Code","X Coordinate","Y Coordinate","Location") := NULL]

rlocations=locations[grep("AIRPORT|CONSTRUCTION|CHA |YARD|ABANDONED|HOUSE|VACANT|OTHER|SCHOOL|NURSING|DAY CARE CENTER|PORCH|FACTORY/MANUFACTURING BUILDING",locations)]

ind.crimes.u=ind.crimes.u[!`Location Description` %in% rlocations]
ind.crimes.u=ind.crimes.u[!`Location Description` %in% c("","ATHLETIC CLUB")]

ind.crimes.u[`Location Description` %in% c("CONVENIENCE STORE", "GROCERY FOOD STORE"),"Location Description"]="SMALL RETAIL STORE"
ind.crimes.u[`Location Description` %in% c("PARK PROPERTY","LAKEFRONT/WATERFRONT/RIVERBANK"),"Location Description"]="PARK"
ind.crimes.u[`Location Description` %in% c("CTA BUS","CTA TRAIN","CTA PLATFORM","CTA STATION","CTA BUS STOP"),"Location Description"]="PUBLIC TRANSPORTATION"
ind.crimes.u[`Location Description` %in% c("CAR WASH"),"Location Description"]="GAS STATION"
ind.crimes.u[`Location Description` %in% c("BAR OR TAVERN","RESTAURANT"),"Location Description"]="RESTAURANT/BAR"
ind.crimes.u[`Location Description` %in% c("BANK","CURRENCY EXCHANGE","ATM (AUTOMATIC TELLER MACHINE)"),"Location Description"]="BANK/ATM"
ind.crimes.u[`Location Description` %in% c("HOSPITAL BUILDING/GROUNDS","MEDICAL/DENTAL OFFICE"),"Location Description"]="MEDICAL BUILDINGS"
ind.crimes.u[`Location Description` %in% c("BRIDGE","SIDEWALK"),"Location Description"]="STREET"
ind.crimes.u[`Location Description` %in% c("TAXICAB"),"Location Description"]="VEHICLE-COMMERCIAL"
ind.crimes.u[`Location Description` %in% c("CLEANING STORE","BARBERSHOP","APPLIANCE STORE"),"Location Description"]="SMALL BUSINESS STORE"
ind.crimes.u[`Location Description` %in% c("PARKING LOT/GARAGE(NON.RESID.)"),"Location Description"]="PARKING LOT"

ind.crimes.u$Time = as.ITime(ind.crimes.u$Time,format = "%I:%M:%S %p")
ind.crimes.u$Date<-as.Date(ind.crimes.u$Date,format='%m/%d/%Y')


nrow(ind.crimes.u)/nrow(ind.crimes)


### Seperate these crimes into violent crimes (robbery, assault,battery sexsual assault, HOMICIDE, kidnapping), 
### weapon violation, alcohol-related, drugs, Motor Vehicle theft, Burglary

violent.u<-ind.crimes.u[`Primary Type` %in% c("ASSAULT","BATTERY","CRIM SEXUAL ASSAULT","HOMICIDE","KIDNAPPING","ROBBERY")]

murder=dt.crimes[`Primary Type` == "HOMICIDE"]
rob=dt.crimes[`Primary Type` == "ROBBERY"]

drug=dt.crimes[`Primary Type` %in% c("NARCOTICS","OTHER NARCOTIC VIOLATION")]
nrow(drug)
drunk=dt.crimes[`Primary Type` == "LIQUOR LAW VIOLATION"]
nrow(drunk)
theft=dt.crimes[`Primary Type` == "THEFT"]
nrow(theft)
motor=dt.crimes[`Primary Type` == "MOTOR VEHICLE THEFT"]
nrow(motor)


### Plot of violent crimes in Chicago and near Uchicago
violent.u$IUCR1<-gsub("(^[0])|(\\D)","",violent.u$IUCR)
violent.u$IUCR1<-ifelse(as.numeric(violent.u$IUCR1)<100,310,as.numeric(violent.u$IUCR1))


community=fread("~/Downloads/CommAreas.csv",header=TRUE)
district=fread("~/Downloads/PoliceDistrictDec2012.csv",header=TRUE)
district$DIST_NUM[4]=31.1
district$DIST_LABEL[4]="31ST1"

district$DIST_NUM[8]=31.2
district$DIST_LABEL[8]="31ST2"


  str <- lapply(community$the_geom, "readWKT", p4s=CRS("+proj=longlat +datum=WGS84"))
  coords <- mapply(spChFIDs, str, as.character(community$AREA_NUM_1))
  community=community[,c("the_geom"):=NULL]
  
  spDF <- SpatialPolygonsDataFrame(SpatialPolygons(unlist(lapply(coords, function(x) x@polygons)),
                                                   proj4string=CRS("+proj=longlat +datum=WGS84")),community)
  csp=fortify(spDF,region = "AREA_NUM_1")


str <- lapply(community$the_geom, "readWKT", p4s=CRS("+proj=longlat +datum=WGS84"))
coords <- mapply(spChFIDs, str, as.character(community$AREA_NUM_1))
data <- SpatialPolygonsDataFrame(SpatialPolygons(unlist(lapply(coords, function(x) x@polygons)),proj4string=CRS("+proj=longlat +datum=WGS84")), community[,-1])
csp=fortify(data,region = 'AREA_NUM_1')
centerids=coordinates(data)
community[,the_geom := NULL]

cmmArea=ggplot()+  geom_polygon(data = csp,aes(x=long,y=lat,group=group),
                        color=alpha("white",0.8),size=0.3,fill=alpha('grey',0.6))+
          geom_text(data=dddd,aes(x=long,y=lat,label=AREA_NUM_1),size=4)

theme_set(theme_grey(16))

chitown=geocode("3300 S Federal St, Chicago, IL 60616")
uchi=geocode("5801 S Ellis Ave, Chicago, IL 60637")
myMap <-get_map(location=chitown,source="google",maptype = "roadmap", color="bw",zoom = 11)
UMap <-get_map(location=uchi,source="google",maptype = "roadmap", color="bw",zoom = 14)

# p3 Points violent crimes in Chicago and near Uchicago ------

g1<-ggmap(myMap,darken = c(0.2,"white")) +
  geom_point(aes(x = Longitude, y = Latitude,colour=`Primary Type`),alpha=0.3,size=0.4,data = violent[Year==2016,])+
  theme_minimal()+
  ggtitle("Violent Crimes in Chicago and Near UChicago")+
  scale_color_hue(limits=c("HOMICIDE","KIDNAPPING","CRIM SEXUAL ASSAULT","ASSAULT","BATTERY","ROBBERY"),l=70, c=150,h=c(0,100))+
  theme(legend.position="None",
        plot.title = element_text(hjust = 0),
        plot.margin=unit(c(8, 5.5, 5.5, 5.5), "points"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))+
  geom_polygon(data = csp,aes(x=long,y=lat,group=group),color=alpha("white",0.8),size=0.3,fill=alpha('grey',0.6))
  
g2<-ggmap(UMap) +
  geom_point(aes(x = Longitude, y = Latitude, colour=`Primary Type`,shape=`Primary Type`,size=(1000/IUCR1)),alpha=0.5,data = violent[Year==2016,])+
  scale_shape_discrete(limits=c("HOMICIDE","KIDNAPPING","CRIM SEXUAL ASSAULT","ASSAULT","BATTERY","ROBBERY"))+
  scale_color_hue(limits=c("HOMICIDE","KIDNAPPING","CRIM SEXUAL ASSAULT","ASSAULT","BATTERY","ROBBERY"),l=70, c=150,h=c(0,100))+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0.5,0.5,0.5), "lines"),
        legend.margin=unit(0,"cm"),
        legend.key.size=unit(4,"mm"),
        legend.text=element_text(size=9,vjust = 0),
        legend.position="top",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )+
  guides(size=FALSE,shape = guide_legend(title="Primary Type",override.aes = list(size=3,linetype=0)),
                                                colour = guide_legend(title="Primary Type"))
dev.new()
multiplot(g1, g2, cols=2)



# 3 By Month Weekday Hour        ---------------------

motor$Month=cut(motor$Date,
               breaks = "month")
drug$Month=cut(drug$Date,
               breaks = "month")
drunk$Month=cut(drunk$Date,
                breaks = "month")
theft$Month=cut(theft$Date,
                breaks = "month")
rob$Month=cut(rob$Date,
                breaks = "month")
murder$Month=cut(murder$Date,
                breaks = "month")


a=motor[,.N,,by=Month]
a$Type = "Motor Theft"
b=drug[,.N,,by=Month]
b$Type = "Narcotics"
c=drunk[,.N,,by=Month]
c$Type = "Alcohol Related"
d=theft[,.N,,by=Month]
d$Type = "Theft"
e=rob[,.N,,by=Month]
e$Type = "Robbery"
f=murder[,.N,,by=Month]
f$Type = "Homicide"

panel1=rbind(a,b,c,d,e,f)
panel1$Type=with(panel1, factor(Type))

setkey(panel1,Month)
panel1[,mav:=rollapply(N, 12, mean, fill=NA,na.rm=TRUE,align="right"),by=Type]

# p4 Treads of different crimes with moving average over period of 12 months------

p4=ggplot()+
  geom_line(data=panel1,aes(as.Date(Month),N,colour = "Monthly Tot."))+
  geom_line(data=panel1,aes(as.Date(Month),mav,colour = "Moving Avg."))+
  labs(y="Count")+
  facet_grid(Type~.,scales="free_y",labeller = labeller(Type = label_wrap_gen(12)))+
  scale_colour_manual("",values=c("blue","red"))+
  scale_x_date(date_breaks="6 month")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=60,vjust=0.5))

panel1$m=month(panel1$Month)

# p5 Distribution of over period of 12 months------

p5=ggplot(data=panel1,aes(m,N,fill=as.factor(m),colour=as.factor(m)))+
  geom_bar(stat='identity',width = 0.95,position="stack")+
  guides(colour="none")+
  labs(y="Count",fill="Month")+
  facet_grid(Type~.,scales="free_y",labeller = labeller(Type = label_wrap_gen(12)))+
  scale_x_discrete("Month",limits=seq(1,12),labels=seq(1,12))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=60,vjust=0.5),
        strip.text = element_text(size=10))

murder=violent[`Primary Type` == "HOMICIDE"]
rob=violent[`Primary Type` == "ROBBERY"]
drug=dt.crimes[`Primary Type` %in% c("NARCOTICS","OTHER NARCOTIC VIOLATION")]
drunk=dt.crimes[`Primary Type` == "LIQUOR LAW VIOLATION"]
theft=ind.crimes[`Primary Type` == "THEFT"]
motor=ind.crimes[`Primary Type` == "MOTOR VEHICLE THEFT"]

murder$WD=weekdays(murder$Date)
rob$WD=weekdays(rob$Date)
drug$WD=weekdays(drug$Date)
theft$WD=weekdays(theft$Date)
drunk$WD=weekdays(drunk$Date)
motor$WD=weekdays(motor$Date)



a=motor[,.N,,by=list(Year,WD)]
a$Type = "Motor Theft"
b=drug[,.N,,by=list(Year,WD)]
b$Type = "Narcotics"
c=drunk[,.N,,by=list(Year,WD)]
c$Type = "Alcohol Related"
d=theft[,.N,,by=list(Year,WD)]
d$Type = "Theft"
e=rob[,.N,,by=list(Year,WD)]
e$Type = "Robbery"
f=murder[,.N,,by=list(Year,WD)]
f$Type = "Homicide"

panel2=rbind(a,b,c,e,f)
panel2=panel2[,SD:=N/max(N),by=list(Year,Type)]

# p6 Heatmap weekdays ~ year ------
p6=ggplot(panel2, aes(WD, Year)) + geom_tile(aes(fill = SD),colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue",limits=c(0,1), labels = percent)+
  scale_y_continuous(breaks=seq(2007,2016))+
  scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))+
  facet_grid(.~Type,labeller = label_wrap_gen(12))+
  labs(fill="")+
  theme(axis.text.x = element_text(vjust=0.5, angle = 90,size=18),
        axis.text.y = element_text(size=18),
        strip.text = element_text(size = 18),
        panel.background= element_blank())
  

# 4 Density Plot of Homicide Sexucal Assault and Robbery -----

dmap=ggmap(myMap)

m1=dmap +
  stat_density2d(
    aes(x = Longitude, y = Latitude,fill = ..level..,alpha=..level..),
    size = 0.5, bins = 15, data = murder,
    geom = "polygon"
  )+
  scale_fill_gradient("Level",low = "white", high = "red")+
  guides(alpha=FALSE)+theme_void()

m2=dmap +
  geom_point(aes(x = Longitude, y = Latitude,colour=`Primary Type`),size=0.4,data = murder)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0))+
  geom_polygon(data = csp,aes(x=long,y=lat,group=group),color=alpha("blue",0.5),size=0.3,fill=alpha('grey',0.5))

multiplot(m1,m2,cols = 2,main="Density and Scatter Plot of ")

# 5 Define gun crimes      ---------------------
### Define gun crimes

violent.u<-ind.crimes.u[`Primary Type` %in% c("ASSAULT","BATTERY","CRIM SEXUAL ASSAULT","HOMICIDE","KIDNAPPING","ROBBERY")]


subtypes=ind.crimes.u[,.N,by= Description][order(-N)]
subtypesv=violent.u[,.N,by= Description][order(-N)]
guns=subtypes$Description[grep("HANDGUN|FIREARM|GUN",subtypes$Description)]
gun.crimes=violent.u[Description %in% guns]

ind.crimes.u[Description %in% guns,Gun:=1]
ind.crimes.u[!Description %in% guns,Gun:=0]
violent.u[Description %in% guns,Gun:=1]
violent.u[!Description %in% guns,Gun:=0]


# 6 Explore Gun Crimes ------
### Percentage of gun involved in all indexed crimes and violent crimes
nrow(gun.crimes)/nrow(ind.crimes.u)
nrow(gun.crimes)/nrow(violent.u)

#gun.crimes$Date=as.Date(gun.crimes$Date)
gun.crimes$WD=weekdays(gun.crimes$Date)
gun.crimes$month=month(gun.crimes$Date)
#gun.crimes$Time=as.ITime(gun.crimes$Time,format = "%I:%M:%S %p")
gun.crimes$hour=hour(gun.crimes$Time)

# p7 ggmap gun -------

p7<-dmap +
  geom_point(aes(x = Longitude, y = Latitude),color="red",size=0.4,alpha=0.3,data = gun.crimes[Year>=2016,])+
  theme_minimal()+
  facet_wrap(~Year)+
  theme(legend.position="None",
        plot.title = element_text(hjust = 0),
        plot.margin=unit(c(8, 5.5, 5.5, 5.5), "points"))+
  geom_polygon(data = csp,aes(x=long,y=lat,group=group),color=alpha("brown",0.5),size=0.3,fill=alpha('grey',0.))


# d2= dmap+
#   stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
#                  bins = 20, geom = "polygon",
#                  data = gun.crimes[Year>=2014,]) +
#   scale_fill_gradient(low = "white", high = "red") +
#   facet_wrap(~ Year,labeller = label_both)


m3=dmap+
  stat_density2d(data=gun.crimes[Year>=2016], aes(x=Longitude
                                  , y=Latitude
                                  ,size=ifelse(..density..<=1,0,..density..),
                                  colour="orange"
                                  ,alpha=..density..)
                 ,geom="tile",contour=F) +
  scale_size_continuous(range = c(0, 1), guide = "none") +
  scale_alpha(range = c(0,.5)) +
  ggtitle("Seattle Crime")

m4=dmap+
  stat_density2d(data=gun.crimes[Year>=2014], aes(x=Longitude, y=Latitude,
                                                  size=ifelse(..density..<=1,0,..density..^3),
                                                  color="red",
                                                  alpha=..density..)
                 ,geom="point",contour=F) +
  geom_polygon(data = csp,aes(x=long,y=lat,group=group),color=alpha("brown",0.3),size=0.3,fill=alpha('grey',0.1))+
  
  scale_size_continuous(range = c(0, 1.5), guide = "none") +
  scale_alpha(range = c(0,1)) +
  guides(colour="none",alpha=guide_legend(title = "Density",override.aes = list(colour= "red")))+
  facet_wrap(~Year)


# p8 Heatmap gun ------
gun.crimes$WD=factor(gun.crimes$WD,levels =  c("Sunday","Monday","Tuesday","Wednesday","Thursday", "Friday","Saturday"))

t3=gun.crimes[,.N,by=list(WD,hour)]

t3[,SD:=N/max(N),by=list(WD)]

p8=ggplot(t3, aes(WD, hour)) + geom_tile(aes(fill = SD),colour = "white") + 
  scale_fill_distiller(palette = "Spectral")+labs(x="Day of Week",y="Hour of Day")+
  ggtitle("Distribution of Gun Crimes by Weekday and Hour")+
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22),
                     labels = c("0 AM","2 AM", "4 AM","6 AM","8 AM","10 AM",
                                "12 PM","2 PM","4 PM","6 PM","8 PM","10 PM"))+
  theme(axis.text.x=element_text(angle = 60,vjust = 0.5))

### Gun Test/Train Spllt

smp_data=violent.u[Year>=2014]
save(smp_data,file='~/Downloads/smp_data.RData')

load('~/Downloads/smp_data.RData')

smp_data[,c("ID","Case Number","IUCR","Domestic","Arrest","INDEX CODE"):=NULL]
smp_data$WD=factor(weekdays(smp_data$Date),levels =  c("Sunday","Monday","Tuesday","Wednesday","Thursday", "Friday","Saturday"))
smp_data$month=as.numeric(month(smp_data$Date))
smp_data$Time=as.ITime(smp_data$Time,format = "%I:%M:%S %p")
smp_data$hour=hour(smp_data$Time)
hour_map=data.table(cbind(rbind(1,2,3,4,5,6,7,8),rbind("12-2AM","3-5AM","6-8AM","9-11AM","12-2PM","3-5PM","6-8PM","9-11PM")))
hour_map=rbind(hour_map,hour_map,hour_map)
hour_map=hour_map[order(as.numeric(V1))]$V2
smp_data$Hour=hour_map[as.numeric(smp_data$hour)+1]

smp_data$Season=ifelse(6<=smp_data$month & smp_data$month <=8,"summer",
                       ifelse(9<=smp_data$month & smp_data$month <=10,"autumn",
                              ifelse(11<=smp_data$month | smp_data$month <=2,"winter","spring")))

  
smp_data$Loc_des=as.factor(smp_data$`Location Description`)
smp_data$Comm_area=smp_data$`Community Area`
set.seed(123)

smp_data$Comm_area[smp_data$Comm_area==77]=ifelse(runif(length(smp_data$Comm_area[smp_data$Comm_area==77]))>=0.5,1,3)
smp_data$Comm_area[smp_data$Comm_area==76]=ifelse(runif(length(smp_data$Comm_area[smp_data$Comm_area==76]))>=0.5,10,17)
smp_data$District=factor(smp_data$District)
smp_data$Gun=factor(smp_data$Gun,levels=c(0,1))
smp_size=floor(nrow(smp_data)*0.75)

train_ind <- sample(seq_len(nrow(smp_data)), size = smp_size)

train = smp_data[train_ind, ]
test = smp_data[-train_ind, ]

# 6 Logistic Regression    ---------- 
formula=Gun~  District + `Location Description`+Comm_area+hour+month+WD+District*Season+District*Hour+`Location Description`*Hour+Season*WD


nullfit <- glm(Gun ~ 1,data=train,family = "binomial",epsilon = 1e-7,trace = TRUE)

logfit <- glm(formula = formula,
             data=train ,family = "binomial",epsilon = 1e-7,trace = TRUE)

fwd.model = step(nullfit, direction='forward', scope=formula,trace=TRUE)

lmfit <- glm(formula = Gun ~  District + `Location Description`+Comm_area+hour+month+WD,
                       data=train ,family = "binomial",epsilon = 1e-7,trace = TRUE)
             

pseduoR <-function(fit,data){
  (1-exp((fit$dev-fit$null)/(nrow(data)/10)))/(1-exp(-fit$null/(nrow(data)/10))) 
}

pseduoR(fwd.model,train)

require(faraway)

par(mfrow=c(1,3))
halfnorm(residuals(lmfit)) 
abline(0,1)
halfnorm(residuals(fwd.model)) 
abline(0,1)
halfnorm(residuals(logfit)) 
abline(0,1)

disp=sum(residuals(logfit,type="pearson")^2) /df.residual(logfit) 
std=drop1(logfit,scale=disp,test="F")

predAccu<-function(prob,data,p_i){
  pred_v=ifelse(prob>p_i,1,0)
  confMat = table(data$Gun,pred_v)
  sum(diag(confMat))/sum(confMat)
}

acc0=predAccu(logfit$fitted.values,train,sum(train$Gun=="1")/nrow(train))
acc01=predAccu(logfit$fitted.values,train,0.5)

acc02=predAccu(fwd.model$fitted.values,train,sum(train$Gun=="1")/nrow(train))
acc03=predAccu(fwd.model$fitted.values,train,0.5)

acc04=predAccu(nullfit$fitted.values,train,sum(train$Gun=="1")/nrow(train))
acc05=predAccu(nullfit$fitted.values,train,0.5)

pred_t1=predict.glm(logfit,newdata = test,type = "response")
acc1=predAccu(pred_t1,test,0.5)
pred_t2=predict.glm(fwd.model,newdata = test,type = "response")
acc2=predAccu(pred_t2,test,0.5)
pred_t3=predict.glm(nullfit,newdata = test,type = "response")
acc3=predAccu(pred_t3,test,0.5)

cbind(rbind(acc0,acc02,acc04),rbind(acc01,acc03,acc05),rbind(acc1,acc2,acc3))

roc_pred <- prediction(pred_t1, test$Gun)
plot(performance(roc_pred, measure="sens", x.measure="spec"), colorize=TRUE)

odds_ratio=exp(cbind(OR = coef(fwd.model), confint.default(fwd.model)))


# 7 Decision Tree          ---------------------

require(rpart)
require(ROCR)
require(randomForest)
library(rpart.plot)


Dtree <- rpart(as.factor(Gun) ~  Season+Hour+ District + WD + as.factor(month)+`Location Description`+`Community Area`,
             data=train,
             method="class",control = rpart.control(cp = 0.001,maxdepth = 5,minsplit = 100))

printcp(Dtree)
dev.new()
prp(Dtree,varlen = 4, faclen = 3,box.palette="RdYlGn",extra=9,type = 2,fallen.leaves = TRUE,space = 1,tweak=1.5)

train_pred=predict(Dtree, train)
predAccu(train_pred[,2],train,0.5)

test_pred=predict(Dtree, test)
predAccu(test_pred[,2],test,0.5)

roc_pred <- prediction(t_pred[,2], test$Violent)
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE)
abline(0,1,lty=2)

# 8 Prediction W/ Random Forrest     ---------

train$Month <- factor(train$month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
train$Hour <- factor(train$hour)
train$Weekday <- factor(train$WD)

test$Month <- factor(test$month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
test$Hour <- factor(test$hour)
test$Weekday <- factor(test$WD)


RF = randomForest(as.factor(Gun) ~ District + Loc_des + Comm_area  + Hour + Weekday + Month,
                   data=train,
                    importance=TRUE, 
                    ntree=2000)


rf_pred=predict(RF,train,type = "prob")

predAccu(rf_pred[,2],test,0.65)

varImpPlot(RF)

# p9 pred_type_distribution           ---------------------

df=data.frame(cbind(pred=t_pred[,2],response=test$Violent))
plot_pred_type_distribution <- function(df,threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$response == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$response == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$response == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$response == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=response, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

plot_pred_type_distribution(df,0.65)



