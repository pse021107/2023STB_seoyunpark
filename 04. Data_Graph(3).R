# Step2: table()함수/구분 1개(Gender)의 인자를 가지고 도수분포표 작성
table(X2023_STB_survey $Gender)

#  Step3: Gender 인자를 가지고 상대도수분포표 작성
ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN)

# Step4: table()함수/2개의 인자(Gender,Grade)를 가지고 교차표를 작성
table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)

# Step5: Nationality 1개의 인자를 가지고 막대그래프 작성
barplot(table(X2023_STB_survey $Nationality),col='green', xlab="Nationality area", ylab="Number", ylim=c(0,50))

# Step6: residential area 1인자를 가지고 가로 막대그래프 작성
barplot(table(X2023_STB_survey $`residential area`),col='red', xlab= "Number", ylab= "residential area", xlim=c(0,50), horiz=TRUE)

# Step7: Gender, Grade 2가지 인자를 가지고 막대그래프 작성 
entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
barplot(entry, legend = TRUE)

# Step8: Grade 인자를 이용하여 파이차트 작성 
pie(table(X2023_STB_survey $Grade))

# Step9: Age 인자를 가지고 히스토그램램 작성 
hist(X2023_STB_survey $`Age`, main="Age 히스토그램", col=terrain.colors(12))

#step 10: 각 Grade별 Age 박스 플롯 작성 
boxplot(X2023_STB_survey$"Age" ~ X2023_STB_survey$"Grade", main="각 Grade별 Age의 박스플롯", col="yellow", names = c("2학년","3학년","4학년"))

#step 10: 각 Grade 별 Age에 대한 기술통계분석
by(X2023_STB_survey$Age ,X2023_STB_survey$"Grade",summary,na.rm=T)

#X2023_STB_survey$Grade: 2
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#19.00   20.00   21.00   21.07   22.00   23.00 
#------------------------------------------------------------------------------------------------ 
#X2023_STB_survey$Grade: 3
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#20.00   21.00   23.00   22.92   24.00   27.00 
#------------------------------------------------------------------------------------------------ 
#X2023_STB_survey$Grade: 4
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#20.00   22.00   23.00   23.33   25.00   27.00 

#step11: 산점도 작성 
plot(x=X2023_STB_survey$`Grade`, y=X2023_STB_survey$`Age`, xlab="Grade 값", ylab="Age 값", main="산점도")
