library(ggplot2)
library(patchwork)
library(corrplot)


##############
#   创作者   #
##############

creator_demographics <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/creator_demographics.csv',)
creator_stats <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/creator_stats.csv',)

## 数据处理
gender <- creator_demographics$gender
for (i in seq(along=gender)){
  if (gender[i]==""){
    gender[i] <- 'unknown'
  }
}
creator_demographics$gender <- gender

summary(creator_demographics)

# 直方图
c1 <- ggplot(data = creator_demographics,mapping = aes(x=gender))+
  geom_bar(stat = 'count')

c2 <- ggplot(data = creator_demographics,mapping = aes(x=registeredMonthCnt))+
  geom_bar(stat = 'count')

c3 <- ggplot(data = creator_demographics,mapping = aes(x=follows))+
  geom_bar(stat = 'count')

c4 <- ggplot(data = creator_demographics,mapping = aes(x=length(followeds)))+
  geom_bar(stat = 'count')

c5 <- ggplot(data = creator_demographics,mapping = aes(x=creatorType))+
  geom_bar(stat = 'count')

c6 <- ggplot(data = creator_demographics,mapping = aes(x=level))+
  geom_bar(stat = 'count')

(c1+c2)/(c3+c4)/(c5+c6)

# 变量相关性
corrplot(cor(creator_demographics[,-c(1,2)]))

# 箱线图
c7 <- ggplot(data = creator_demographics, mapping = aes(x=level ,y = registeredMonthCnt,fill=factor(level))) +
  geom_boxplot()

c8 <- ggplot(data = creator_demographics, mapping = aes(x=level ,y = follows,fill=factor(level))) +
  geom_boxplot()

c9 <- ggplot(data = creator_demographics, mapping = aes(x=level ,y = followeds,fill=factor(level))) +
  geom_boxplot()

c7/c8/c9

#c10
ggplot(data = creator_demographics, mapping = aes(x=gender ,y = level,fill=factor(gender))) +
  geom_boxplot()

# creator_stats

# 日发布
PublishMlogCnt_SumByDt <- aggregate(creator_stats$PushlishMlogCnt,by=list(creator_stats$dt),FUN=sum)
PublishMlogCnt_SumByDt

summary(PublishMlogCnt_SumByDt)


## 创作者发布
publishSum <- aggregate(creator_stats$PushlishMlogCnt,by=list(creator_stats$creatorId),FUN=sum)
summary(publishSum)

##############
#    用户    #
##############

user_demographics <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/user_demographics.csv',nrows = 100000)

## 数据处理
user_demographics[is.na(user_demographics)] <- 0

gender <- user_demographics$gender
for (i in seq(along=gender)){
  if (gender[i]==""){
    gender[i] <- 'unknown'
  }
}
user_demographics$gender <- gender

summary(user_demographics)

# 直方图
u1 <- ggplot(data = user_demographics,mapping = aes(x=age))+
  geom_bar(stat = 'count')

u2 <- ggplot(data = user_demographics,mapping = aes(x=registeredMonthCnt))+
  geom_bar(stat = 'count')

u3 <- ggplot(data = user_demographics,mapping = aes(x=followCnt))+
  geom_bar(stat = 'count')

u4 <- ggplot(data = user_demographics,mapping = aes(x=level))+
  geom_bar(stat = 'count')

(u1+u2)/(u3+u4) 

ggplot(data = user_demographics,mapping = aes(x=province))+
  geom_bar(stat = 'count')

# 箱型图

u5 <- ggplot(data = user_demographics, mapping = aes(x=level ,y = registeredMonthCnt,fill=factor(level))) +
  geom_boxplot()

u6 <- ggplot(data = user_demographics, mapping = aes(x=level ,y = followCnt,fill=factor(level))) +
  geom_boxplot()

u5/u6

ggplot(data = user_demographics, mapping = aes(x=gender ,y = level,fill=factor(gender))) +
  geom_boxplot()

# 变量相关性
corrplot(cor(user_demographics[,-c(1,2,4)]))

##############
#    卡片    #
##############

# mlog_demographics <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/mlog_demographics.csv',norws=1000)
mlog_stats <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/mlog_stats.csv',nrows = 1000)

mlog_stats$mlogClickRate <- mlog_stats$userClickCount/mlog_stats$userImprssionCount

m1 <- ggplot(data = mlog_stats,mapping = aes(x=dt))+
  geom_bar(stat = 'count')

m2 <- ggplot(data = mlog_stats,mapping = aes(x=userImprssionCount))+
  geom_bar(stat = 'count')

m3 <- ggplot(data = mlog_stats,mapping = aes(x=userClickCount))+
  geom_bar(stat = 'count')

m4 <- ggplot(data = mlog_stats,mapping = aes(x=userLikeCount))+
  geom_bar(stat = 'count')

(m1+m2)/(m3+m4)



##############
#    印象    #
##############

## all users
user_demographics <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/user_demographics.csv',)
impression <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/impression_data.csv',nrows = 100000)

summary(impression)

# 点击率
click_rate <- aggregate(impression$isClick,by = list(impression$userId),FUN = mean)
names(click_rate) <- c('userId','clickRate')

# 点赞率
like_rate <- aggregate(impression$isLike,by = list(impression$userId),FUN = mean)
names(like_rate) <- c('userId','likeRate')

## 直方图
hist(click_rate$clickRate)
hist(like_rate$likeRate)

### merge click rate and user demographics
user_demographics.rate <- merge(user_demographics,click_rate,by='userId',all = FALSE)
user_demographics.rate[,-c(9)]

user_demographics.rate <- merge(user_demographics.rate,like_rate,by='userId',all = FALSE)
user_demographics.rate[,-c(10)]

user_demographics.rate[is.na(user_demographics.rate)] <- 0
gender <- user_demographics.rate$gender
for (i in seq(along=gender)){
  if (gender[i]==""){
    gender[i] <- 'unknown'
  } 
}
user_demographics.rate$gender <- gender


# 回归分析
lm_model <- lm(data = user_demographics.rate,clickRate~registeredMonthCnt+followCnt)
summary(lm_model)

glm_model <- glm(data = user_demographics.rate,clickRate~factor(gender)+registeredMonthCnt+followCnt+age)
summary(glm_model)


## 典型相关分析

cancor2<-function(x,y,dec=4){
  #Canonical Correlation Analysis to mimic SAS PROC CANCOR output.
  #Basic formulas can be found in Chapter 10 of Mardia, Kent, and Bibby (1979).
  # The approximate F statistic is exercise 3.7.6b.
  x<-as.matrix(x);y<-as.matrix(y)
  n<-dim(x)[1];q1<-dim(x)[2];q2<-dim(y)[2];q<-min(q1,q2)
  S11<-cov(x);S12<-cov(x,y);S21<-t(S12);S22<-cov(y)
  E1<-eigen(solve(S11)%*%S12%*%solve(S22)%*%S21);E2<-eigen(solve(S22)%*%S21%*%solve(S11)%*%S12)
  #    rsquared<-as.real(E1$values[1:q])
  rsquared<-E1$values[1:q]
  LR<-NULL;pp<-NULL;qq<-NULL;tt<-NULL
  for (i in 1:q){
    LR<-c(LR,prod(1-rsquared[i:q]))
    pp<-c(pp,q1-i+1)
    qq<-c(qq,q2-i+1)
    tt<-c(tt,n-1-i+1)}
  m<-tt-0.5*(pp+qq+1);lambda<-(1/4)*(pp*qq-2);s<-sqrt((pp^2*qq^2-4)/(pp^2+qq^2-5))
  F<-((m*s-2*lambda)/(pp*qq))*((1-LR^(1/s))/LR^(1/s));df1<-pp*qq;df2<-(m*s-2*lambda);pval<-1-pf(F,df1,df2)
  outmat<-round(cbind(sqrt(rsquared),rsquared,LR,F,df1,df2,pval),dec)
  colnames(outmat)=list("R","RSquared","LR","ApproxF","NumDF","DenDF","pvalue")
  rownames(outmat)=as.character(1:q);xrels<-round(cor(x,x%*%E1$vectors)[,1:q],dec)
  colnames(xrels)<-apply(cbind(rep("U",q),as.character(1:q)),1,paste,collapse="")
  yrels<-round(cor(y,y%*%E2$vectors)[,1:q],dec)
  colnames(yrels)<-apply(cbind(rep("V",q),as.character(1:q)),1,paste,collapse="")
  list(Summary=outmat,a.Coefficients=E1$vectors,b.Coefficients=E2$vectors,
       XUCorrelations=xrels,YVCorrelations=yrels)
} 

user.cor <- user_demographics.rate[,c(5,6,7,8,9)]

user.std <- sweep(user.cor,2,sqrt(apply(user.cor, 2, var)),FUN ='/')

vars <-user.std[,1:3]
rates <- user.std[,4:5]
cancor2(vars,rates)

# 用户积极性与用户特征

# 创作者如何受反馈信息影响，以及创作激励

# 什么样的卡片受欢迎，受哪些用户欢迎







