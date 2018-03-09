#데이터 불러오기
library(readr)
USvideos<-data.frame(read_csv("C:/Users/LEESUJAE/Documents/R/Machine-Learning/USvideos.csv"))

#컨텐츠 고유 여부 확인
length(unique(USvideos$video_id))
length(unique(USvideos$category_id))

#1.기초탐색
str(USvideos)
summary(USvideos)
names(USvideos)
head(USvideos)
class(USvideos$comments_disabled)
table(USvideos$comments_disabled)
summary(USvideos$comment_count)

#apply 함수 적용
head(USvideos[,13:15])
dim(USvideos[,13:15])
apply(USvideos[,13:15],2, function(x) as.factor(x))
summary(apply(USvideos[,13:15],2, function(x) as.factor(x)))

#date변환
str(as.Date('2017-12-30'))
USvideos$trending_date<-as.Date(USvideos$trending_date, '%y.%d.%m')
str(USvideos$trending_date)
str(USvideos$publish_time)
as.date(USvideos$trending_date)


#2.json data 불러오기
require(j)
library
category<-fromJSON("US_category_id.json", flatten = TRUE)
# head(category)
# head(category$items)
# head(category[[3]])
# class(head(category[[3]]))
category1<-category$items
category2<-category1[,c("id","snippet.title")]
names(category2)
names(category2)[1]<-"category_id"

USvideos_cate<-merge(USvideos, category2, all.x = T, by="category_id")
View(USvideos_cate)
category2


#3. view에 영향을 미칠 수 있는 독립변수_like, dislike, publishing time, comment count

#4. 댓글금지, 평점금지, 컨텐츠 오류 및 삭제 유무 빈도수
#table 사용, 사용자 정의 함수
table(USvideos$comments_disabled)
table(USvideos$ratings_disabled)
table(USvideos$video_error_or_removed)
apply(USvideos[,13:15],2, function(x) as.factor(x))
summary(apply(USvideos[,13:15],2, function(x) as.factor(x)))

#5.컨텐츠 고유 여부 및 회귀모형
unique(USvideos_cate$video_id)
length(USvideos_cate$video_id)
unique(USvideos_cate$publish_time)
length(USvideos_cate$publish_time)
cor.test()

#6.카테고리에 따른 조회수 
library(ggplot2)
table(USvideos_cate$category_id)
a<-cbind.data.frame(USvideos_cate$category_id, USvideos_cate$views)
aggregate(USvideos_cate$views, by=list(USvideos_cate$category_id), FUN=sum)
aggregate(USvideos_cate$views, by=list(USvideos_cate$category_id), FUN=mean)

aggregate(views~snippet.title, data=USvideos_cate, mean)
aggregate(views~snippet.title, data=USvideos_cate, sum)
require(doBy)

summaryBy(views~snippet.title, data=USvideos_cate, FUN=c(mean,median,length))

#6.1. 시각화
dev.off()
plot<-summaryBy(views~snippet.title, data=USvideos_cate, FUN=c(mean,median,length))
ggplot(data=plot, aes(x=snippet.title, y=views.mean))+
  geom_bar(stat = "identity")
ggplot(data=USvideos_cate,aes(x=snippet.title, y=views, color=snippet.title))+geom_boxplot()+theme_bw()
ggplot(data=subset(USvideos_cate,views<2500000),aes(x=snippet.title, y=views, color=snippet.title))+geom_boxplot()+theme_bw()

plot(USvideos_cate$views)
hist(USvideos_cate$views)
hist(Usvideos_modeling$views)


#모델링 데이터
a<-cbind.data.frame(USvideos_cate$views, USvideos_cate$likes, USvideos_cate$dislikes, USvideos_cate$comment_count)
colnames(a)<-c("views","likes","dislikes","comment")
b<-lm(views~., data=a)
summary(b)

Usvideos_modeling<-subset(USvideos_cate,USvideos_cate$views>200000, select = c(views, likes, dislikes, comment_count, snippet.title))

analisys<-lm(views~., data = Usvideos_modeling)
summary(analisys)

#세부 통계치
summary(USvideos)
options(digits = 2, scipen = 10)
require(pastecs)
stat.desc(USvideos)



# 2018-03-03
# Machine Learning 실습 1

# 1)
library(readr)
USvideos <- data.frame(read_csv("C:/Users/class desk/Desktop/USvideos.csv"))

# 컨텐츠, 고유여부 확인
length(unique(USvideos$video_id))
length(unique(USvideos$category_id))

# 기초 탐색
str(USvideos)
summary(USvideos)
names(USvideos)
head(USvideos)

# 범주형 변수를 factor 유형으로 변환해 빈도 파아
USvideos_sub1<-apply(USvideos[,13:15],2,function(x) as.factor(x))
summary(USvideos_sub1)
summary(apply(USvideos[,13:15],2,function(x) as.factor(x)))

# str(as.Date('2017-12-30'))
# df$var1<-(df$var1+1)

# 날짜 데이터 정리
USvideos$trending_date<-as.Date(USvideos$trending_date,'%y.%d.%m')
USvideos$publish_time<-as.Date(USvideos$publish_time)

#2)
require(jsonlite)
category<-fromJSON("US_category_id.json",flatten = T)
category1<-category$items
# category[[3]]

category2<-category1[,c("id","snippet.title")]
names(category2)[1]<-"category_id"
names(USvideos)

# USvideos<-merge(USvideos,category2,all.x=T,by="category_id")
USvideos_cate<-merge(USvideos,category2,all.x=T,by="category_id")

#3)
summary(USvideos)
options(digits = 2,scipen = 10)
require(pastecs)
stat.desc(USvideos)

#4)
#1 에서 처리

#5)
# 1 에서 처리

#6)
aggregate(views~snippet.title,data=USvideos_cate,mean)

require(doBy)
a<-summaryBy(views~snippet.title,data=USvideos_cate,FUN=c(mean,median,length))
# a[order(-a$views.mean),]$snippet.title[1:5]
# b<-a[order(-a$views.mean),]$snippet.title[1:5]


# 시각화 접근벙
require(ggplot2)
dev.off()

# 카테고리의 조회수 시각화
ggplot(data=USvideos_cate,aes(x=snippet.title,y=views,color=snippet.title))+geom_boxplot()+theme_bw()

# 카테고리의 조회수 시각화 (보정) 
ggplot(data=subset(USvideos_cate,views<25000000,select=(),aes(x=snippet.title,y=views,color=snippet.title))+geom_boxplot()+theme_bw()
       
       
       # 7)
       names(USvideos_cate)
       USvideos_modelling<-subset(USvideos_cate,select=c(views,likes,dislikes,comment_count,snippet.title))
       head(USvideos_modelling)
       
       
       #8)
       fit<-lm(views~.,data=USvideos_modelling)
       summary(fit)
       
       fit$fitted.values
       
       
       #9)
       # hist(USvideos_modelling$views)
       
       
       
       
       
       
