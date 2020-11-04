library(glmnet)
library(tidyverse)

# 性别 注册月数 粉丝数 关注数 创作类型 等级
creator_demographics <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/creator_demographics.csv',)

## 数据处理
gender <- creator_demographics$gender
for (i in seq(along=gender)){
  if (gender[i]==""){
    gender[i] <- 'unknown'
  }
}
creator_demographics$gender <- gender

# 周期+发布数
creator_stats <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/creator_stats.csv',)

## 创作者发布总数 publishSum
publishSum <- aggregate(creator_stats$PushlishMlogCnt,by=list(creator_stats$creatorId),FUN=sum)
names(publishSum) <- c('creatorId','publishSum')
summary(publishSum)
creator_demographics <- merge(creator_demographics,publishSum,by='creatorId',all = FALSE)

# 回归分析
lm_model <- lm(data = creator_demographics,publishSum~registeredMonthCnt+follows+followeds+factor(gender)+factor(creatorType))
summary(lm_model)

lm_model <- lm(data = creator_demographics,publishSum~registeredMonthCnt+follows+factor(creatorType))
summary(lm_model)

## lasso
### one-hot encoding
creator_demographics$creatorType <- factor(creator_demographics$creatorType)
x <- model.matrix(publishSum~.,creator_demographics)[,-c(1,2,5,7)]
model.matrix(publishSum~.,creator_demographics)
## Error: vector memory exhausted (limit reached?)
y <- creator_demographics$publishSum

lasso_fit <- glmnet(x,y,family = "poisson", alpha = 1,nlambda = 50) #  auto test lambda 
lasso_fit
plot(lasso_fit,xvar = "lambda",label = T)


mlog_demographics <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/mlog_demographics.csv')
mlog_stats <- read.csv('/Users/zuqin/Desktop/SJTU_II/NetEaseCould/csv_data/mlog_stats.csv')



