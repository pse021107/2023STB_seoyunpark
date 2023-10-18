# 실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

# csv형식의 파일 불러와서 subway 객체에 입력하고 구조 확인
str(congestion)

# 결측치 데이터 확인하기
# 변수의 이상치와 결측치 확인하고 처리
summary(congestion)

# 결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

# 결측치를 처리하자
# 6시 출발기차의 결측치를 제거
congestion1 <-congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))

# 23시 30분 출발기차의 결측치를 제거
congestion1 <-congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))

# 나머지 시간대는 결측치를 0으로 대체
congestion1[is.na(congestion1)] <-0
colSums(is.na(congestion1))

# 이상치 확인
ggplot(congestion1, aes(y=s0530))+  
geom_boxplot()

summary(congestion1$s0530)



# 파생변수 만들기
# 1. 수도권 지하철역의 하루 평균 혼잡도
congestion1$day_mean <-
  rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

# 2. 지하철 호선별 하루 평균 혼잡도
congestion1 %>%
  group_by(line)%>%
  summarise(line_mean = mean(day_mean))

# 3. 지하철 호선별 출근시간(07:00 ~ 09:00) 대의 평균 혼잡도
s0700_mean <- congestion1 %>%
  summarise(s0700_mean = mean(s0700, na.rm = TRUE))

s0730_mean <- congestion1 %>%
  summarise(s0730_mean = mean(s0730, na.rm = TRUE))

s0800_mean <- congestion1 %>%
  summarise(s0800_mean = mean(s0800, na.rm = TRUE))

s0830_mean <- congestion1 %>%
  summarise(s0830_mean = mean(s0830, na.rm = TRUE))

s0900_mean <- congestion1 %>%
  summarise(s0900_mean = mean(s0900, na.rm = TRUE))

total_mean <- (s0700_mean + s0730_mean + s0800_mean + s0830_mean + s0900_mean) / 5
print(total_mean)

#01. 기술통계분석
summary(congestion1$s0700)
summary(congestion1$s0730)
summary(congestion1$s0800)
summary(congestion1$s0830)
summary(congestion1$s0900)

line_average = c(congestion1$s0700, congestion1$s0730, congestion1$s0800, congestion1$s0830, congestion1$s0900)
summary(line_average, na_rm=True)

#02. 평균 혼잡도가 가장 높은 시간대의 막대 그래프

congestion1 %>%
  summarise(s0700_mean = mean (s0700, na.rm = TRUE))

congestion1 %>%
  summarise(s0730_mean = mean (s0730, na.rm = TRUE))

congestion1 %>%
  summarise(s0800_mean = mean (s0800, na.rm = TRUE))

congestion1 %>%
  summarise(s0830_mean = mean (s0830, na.rm = TRUE))

congestion1 %>%
  summarise(s0900_mean = mean (s0900, na.rm = TRUE))

v = c(18.7, 24.6,32.3,30.1,29.4)
barplot(v)
name = c('s0700','s0730','s0800','s0830','s0900')
barplot(v,names=name) # barplot(v~name)

#03. 평균 혼잡도 상위 4개 호선의 역별 기여도
 

# 추가 열을 비교하기 위해 필요한 열 이름을 나열
library(dplyr)

# 추가 열을 비교하기 위해 필요한 열 이름을 나열
columns_to_compare <- c("day_mean", "s0700", "s0730", "s0800", "s0830", "s0900")

# "congestion" 데이터 프레임을 이용하여 "line" 별로 열들을 비교
result <- congestion1 %>%
  group_by(line, station) %>%
  summarize(across(all_of(columns_to_compare), mean)) %>%
  arrange(desc(day_mean)) %>%
  head(4)

print(result)


#4. 출발시간 08시 지하철 혼잡도 범주화/범주별 빈도분석

congestion1 %>% 
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%  
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s80_grade=="caution")%>%  
  select(line, s80_grade,n,pct)%>%  arrange(desc(pct))  




# 5. 지하철 호선별 퇴근시간(18:00 ~ 20:00) 대의 평균 혼잡도
s1800_mean <-
  congestion1 %>% summarise(s1800_mean = mean(s1800, na.rm = TRUE))
s1830_mean <- 
  congestion1 %>% summarise(s1830_mean = mean(s1830, na.rm = TRUE))
s1900_mean <-
  congestion1 %>% summarise(s1900_mean = mean(s1900, na.rm = TRUE))
s1930_mean <-
  congestion1 %>% summarise(s1930_mean = mean(s1930, na.rm = TRUE))
s2000_mean <-
  congestion1 %>% summarise(s2000_mean = mean(s2000, na.rm = TRUE))

total_mean1 <- (s1800_mean$s1800_mean + s1830_mean$s1830_mean + s1900_mean$s1900_mean + s1930_mean$s1930_mean + s2000_mean$s2000_mean) / 5

print(total_mean1)

#01. 기술통계분석 결과
summary(congestion1$s1800)
summary(congestion1$s1830)
summary(congestion1$s1900)
summary(congestion1$s1930)
summary(congestion1$s2000)

#02. 가장 높은 시간대의 막대그래프 확인하기기

  congestion1 %>%
    summarise(s1800_mean = mean (s1800, na.rm = TRUE))
  
  congestion1 %>%
    summarise(s1830_mean = mean (s1830, na.rm = TRUE))
  
  congestion1 %>%
    summarise(s1900_mean = mean (s1900, na.rm = TRUE))
  
  congestion1 %>%
    summarise(s1930_mean = mean (s1930, na.rm = TRUE))
  
  congestion1 %>%
    summarise(s2000_mean = mean (s2000, na.rm = TRUE))
  
  v = c(40.8,38.3,30.3,25.3,23.6)
  barplot(v)
  name = c('s1800','s1830','s1900','s1930','s2000')
  barplot(v,names=name) # barplot(v~name) 
  
#03. 평균혼잡동 상위 4개 호선의 역별 기여도
  library(dplyr)
  
  # 추가 열을 비교하기 위해 필요한 열 이름을 나열
  columns_to_compare <- c("day_mean", "s1800", "s1830", "s1900", "s1930", "s2000")
  
  # "congestion" 데이터 프레임을 이용하여 "line" 별로 열들을 비교
  result <- congestion1 %>%
    group_by(line, station) %>%
    summarize(across(all_of(columns_to_compare), mean)) %>%
    arrange(desc(day_mean)) %>%
    head(4)
  
  print(result)
  
# 6. 18시의 지하철 혼잡도 범주화/범주별 빈도분석
  
  congestion1 %>% 
    mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%  
    group_by(line, s18_grade) %>%
    summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
    filter(s18_grade=="bad")%>%  
    select(line, s18_grade,n,pct)%>%  arrange(desc(pct))  
  
