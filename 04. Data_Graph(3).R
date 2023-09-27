# table()함수/구분 1개(Gender)의 인자를 가지고 도수분포표 작성
table(X2023_STB_survey $Gender)

#  Gender 인자를 가지고 상대도수 계산
ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN)

# table()함수/2개의 인자(Gender,Grade)를 가지고 교차표를 작성
table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)

#막대그래프
barplot(table(X2023_STB_survey $Nationality))

#Gender, Grade 2가지 인자를 가지고 막대그래프
entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
barplot(entry, legend = TRUE)

#Grade 인자를 이용하여 파이차트
pie(table(X2023_STB_survey $Grade))

#residential area 1인자를 가지고 가로 막대그래프 작성성
barplot(table(X2023_STB_survey $`residential area`),col='red', xlab= "residential area", ylab= "Number", xlim=c(0,100), horiz=TRUE)

#히스토그램램
hist(X2023_STB_survey$`Age`, main="히스토그램 2023", col=terrain.colors(12))

#step 10
boxplot(X2023_STB_survey$"Grade", X2023_STB_survey$'Age', main="박스플롯2023", col="yellow", names = c("Grade","Age"))
summary(X2023_STB_survey, na.rm=T)
#Gender         Age            Grade        Nationality    residential area
#Min.   :1.0   Min.   :19.00   Min.   :2.000   Min.   :3.000   Min.   : 6.000  
#1st Qu.:1.0   1st Qu.:21.00   1st Qu.:2.000   1st Qu.:3.000   1st Qu.: 7.000  
#Median :1.5   Median :22.00   Median :3.000   Median :3.000   Median : 7.000  
#Mean   :1.5   Mean   :22.52   Mean   :3.125   Mean   :3.438   Mean   : 7.208  
#3rd Qu.:2.0   3rd Qu.:23.25   3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.: 7.000  
#Max.   :2.0   Max.   :27.00   Max.   :4.000   Max.   :5.000   Max.   :11.000  

#step11
plot(x=X2023_STB_survey$`Grade`, y=X2023_STB_survey$`Age`, xlab="Grade 값값", ylab="Age 값값", main="삼전도")