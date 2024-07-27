setwd("F:/Users/Andy/Google Drive/Booth/TA/Behavioral Finance")

# load data
data = read.csv("MOMps.csv")
data = data[which(is.na(data$ret)==F),] #delete obs with missing return
head(data)


# create mom_i,t for each stock
data$mom=apply(1+data[,14:24],1,prod)-1	
data = data[which(is.na(data$mom)==F),] #delete obs with missing mom
data1 = data[,-(14:24)] #delete some unused variables
head(data1)


# create market cap for each stock
data1$LME=abs(data1$L1prc)*data1$L1shrout
data1 = data1[which(is.na(dat1a$LME)==F),] #delete obs with missing lagged market cap
data2 = data1[,-c(6,7,8,10,11,12,13)] #delete some unused variables
head(data2)


#generate universal date variable
data2$yyyymm = data2$year*100+data2$month
data2 = data2[,-c(2,3,4)] #delete some unused variables
head(data2)

#take only NYSE stocks to calc breakpoints
data2NYSE = data2[which(data2$exchcd==1),]

#calc breakpoints
getpctl20 <- function(datas){ return(quantile(datas,c(.2)))}
p20=aggregate(data2NYSE$mom, list(data2NYSE$yyyymm), getpctl20)

getpctl80 <- function(datas){ return(quantile(datas,c(.8)))}
p80=aggregate(data2NYSE$mom, list(data2NYSE$yyyymm), getpctl80)

names(p20)=c("yyyymm","p20")
names(p80)=c("yyyymm","p80")

head(p20)
head(p80)

#add breakpoints to return data
data3a = merge(data2,p20,by="yyyymm")
data3 = merge(data3a,p80,by="yyyymm")
head(data3)


#generate subsets of winners and losers
winit=data3[which(data3$mom>data3$p80),]
loseit=data3[which(data3$mom<data3$p20),]
head(winit)
head(loseit)

wint=aggregate(winit$ret, list(winit$yyyymm), mean)
loset=aggregate(loseit$ret, list(loseit$yyyymm), mean)

names(wint)=c("yyyymm","win")
names(loset)=c("yyyymm","lose")

head(wint)
head(loset)

mktrf=aggregate(winit$mktrf, list(winit$yyyymm), mean)
smb=aggregate(winit$smb, list(winit$yyyymm), mean)
hml=aggregate(winit$hml, list(winit$yyyymm), mean)

#Q4S4

months = unique(data2$yyyymm)
i=1
momwt = matrix(0,length(months),4)


for (i in 1:length(months)){

	winwt  = weighted.mean(winit$ret[which(winit$yyyymm==months[i])],winit$LME[which(winit$yyyymm==months[i])])
	losewt = weighted.mean(loseit$ret[which(loseit$yyyymm==months[i])],loseit$LME[which(loseit$yyyymm==months[i])])

	

}

momwt=data.frame(momwt)
names(momwt)=c("yyyymm","winwt","losewt","momwt")
head(momwt)

