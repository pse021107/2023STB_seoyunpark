# 실습에 필요한 packages 를 라이브러리에 등록
library(dplyr)
library(ggplot2)

# 데이터를 새객체 foodshop 으로 불러오기
# 비어있는 셀은 결측치 처리/ 파라미터 문자형으로 변환
foodshop<-read.csv("seoul_food_2023.csv", na="", stringsAsFactors= F)

# 데이터 구조 확인
str(foodshop)

# 분석변수 추출 및 변수이름 변경
foodshop<-foodshop%>%  
  rename(open_date=인허가일자, status=상세영업상태명,         
         close_date=폐업일자, name=사업장명, type=업태구분명,         
         address=소재지전체주소) %>%  
  select("name","type","status","open_date","close_date", "address")

# 추출된 데이터 구조 확인
str(foodshop)

# YYYYMMDD 형식으로 변경하기
foodshop$open_date<-gsub("-","",foodshop$open_date)
foodshop$close_date<-gsub("-","",foodshop$close_date)

# 문자형 데이터를 정수형 데이터로 변환
foodshop$open_date<-as.integer(foodshop$open_date)
foodshop$close_date<-as.integer(foodshop$close_date)

# 변경된 데이터 구조 확인
str(foodshop)


# 파생변수 만들기

# 1. status 변수
table(foodshop$status)

# 영업상태가 영업/폐업이 아닌 것을 제외한다
foodshop<-foodshop%>%  
  filter(status == '영업' | status == '폐업') %>%  
  select(name,type,status,open_date,close_date,address)

# 처리 결과 확인
table(foodshop$status)

#2. type 변수
table(foodshop$type)

# 업종에 컬럼의 모든 값이 포함된 레코드 제외
foodshop<-foodshop%>%   
  filter(address != '서울특별시 동대문구 용두동 235-18' | open_date!= '20190708' | close_date!= '20210902') %>%  
  select(name,type,status,open_date,close_date,address)

# 처리결과 확인
table(foodshop$type)

# 인허가 일자 데이터에 이상치
range(foodshop$open_date)

# 결측치 확인
table(is.na(foodshop$open_date))

# 인허가년도만 분리해서 변수를 생성
foodshop$open_year<-substr(foodshop$open_date,1,4)

# 폐업 일자 데이터 이상치 확인
range(foodshop$close_date, na.rm = T)

# 폐업년도만 분리해서 변수 생성
foodshop$close_year<-substr(foodshop$close_date,1,4)

# 구만 분리
foodshop$district<-substr(foodshop$address,7,9)

# 이상치 확인
table(foodshop$district)

# 이상하게 분리된 데이터를 제외
foodshop$district<-
  ifelse(foodshop$district%in%c("도 제","시 망","시 수","시 영","시 원","시 일"),NA,foodshop$district)

# 제외 후 데이터 확인
table(foodshop$district)

# 최종 데이터 확인
str(foodshop)

# 새로 생선된 인허가년도, 폐업년도의 문자형을 정수형 데이터로 변경
foodshop$open_year<-as.integer(foodshop$open_year)
foodshop$close_year<-as.integer(foodshop$close_year)

# 변경된 데이터 확인
str(foodshop)

# 총 15가지 항목 분석
# 1. 가장 오래 영업중인 음식점
foodshop%>%  
  filter(!is.na(open_date)&status=="영업") %>% #결측치제거, 영업데이터추출
  filter(open_date==min(open_date)) %>% #개업일이가장빠른데이터추출
  select(name, type, open_date, address)

# 2. 주요 업종별로 가장 오래 영업중인 음식점
foodshop%>%  
  filter(!is.na(open_date)&status=="영업") %>% #결측치제거, 영업데이터추출
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%  
  group_by(type) %>%#업종별분류filter(open_date==min(open_date)) %>% #개업일이가장빠른데이터추출
  select(name, type, open_date, address)

# 3. 업종별 개업 비율
foodshop%>%  
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  #결측치제외
  group_by(type) %>%  
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>% 
  head(10)

# 4. 영업 중인 음식점의 업종별 비율
foodshop%>%  
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  #결측치제외
  filter(status=="영업") %>% #영업만추출
  group_by(type) %>%  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>%  
  head(5)

# 5. 영업 중인 음식점의 업종별 비율
foodshop%>%  
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  #결측치제외
  group_by(status) %>%  
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) #범주별비율계산

# 6. 주요 업종별 영업과 폐업 비율
foodshop%>%  
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  #결측치제외
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%  
  group_by(type,status) %>%#교차차분류
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1))%>% #범주별비율계산
  filter(status=="영업") %>% #영업만추출
  arrange(desc(n))

# 7. 개업이 많았던 연도
foodshop%>%  
  filter(!is.na(open_date)&!is.na(district))%>% #결측치제외
  group_by(open_year) %>%  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>% 
  head(5)

# 8. 폐업이 많았던 연도
foodshop%>%  
  filter(!is.na(close_date)&!is.na(district))%>% #결측치제외
  group_by(close_year) %>%  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>%  
  head(5)

# 9. 연도별 개업 음식점수 그래프
# 연도별 개업 음식점수
open_trend<-foodshop%>%  
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제외
  group_by(open_year) %>%  
  summarise(open_n=n())
#open_trend 구조
str(open_trend)
#연도별개업음식점수막대그래프
ggplot(data=open_trend,aes(x=open_year,y=open_n))+  geom_col()+  xlab("연도") + ylab("개업수")

# 10. 연도별 폐업 음식점수 그래프
#연도별폐업음식점수
close_trend<-foodshop%>%  
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제외
  group_by(close_year) %>%  summarise(close_n=n())
#open_trend 구조
str(close_trend)
#연도별개업음식점수막대그래프
ggplot(data=close_trend,aes(x=close_year,y=close_n))+  geom_col()+  xlab("연도") + ylab("폐업수")

# 11. 개업과 폐업 음식점 통합 그래프
open_trend1<-rename(open_trend,year=open_year)#연도이름변경
close_trend1<-rename(close_trend,year=close_year)#연도이름변경

open_close_trend<-left_join(open_trend1,close_trend1,by="year")#통합

ggplot()+  geom_line(data=open_close_trend, aes(year,open_n))+#개업그래프
  geom_line(data=open_close_trend, aes(year,close_n,color="red"))+#폐업그래프
  xlab("연도") + ylab("개수")

# 12. 폐업음식점수가 개업음식점수보다 많았던 기간 확인
open_close_trend%>%  filter(close_n>open_n)

# 13. 영업중인 음식점수가 가장 많은 5개구
district_business<-foodshop%>%  
  filter(!is.na(open_date)&!is.na(district)&status=="영업") %>% 
  #결측치제거
  group_by(district) %>%  
  summarise(n=n())

district_business%>%  
  arrange(desc(n)) %>%  
  head(5)

# 14. 25개구의 음식점수 막대그래프
ggplot(data = district_business, aes(x=reorder(district,n),y=n))+  
  geom_col()+  
  coord_flip()+#막대90도회전
  xlab("영업구")+  ylab("영업음식점수")

# 15. 25개구의 음식점수 막대그래프
foodshop%>%  
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제거
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%  
  filter(status=="영업") %>% #영업만추출
  group_by(type,district) %>%  
  summarise(n=n()) %>% 
  mutate(total=sum(n),pct=round(n/total*100,1))%>% #범주별비율계산
  group_by(type) %>%  
  filter(pct==max(pct))#type별district비율이가장높은데이터추출

