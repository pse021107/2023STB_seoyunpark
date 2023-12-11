library(dplyr)
library(ggplot2)
library(foreign)

koweps23 <- read.spss("Koweps_h17_2022_beta1.sav")
koweps23 <- as.data.frame(koweps23)

#.변수 추출 후 이름 변경
welfare23 <- koweps23 %>%
  select(h1701_4,h1701_5,h1701_6,h17_reg5,h1703_8,h1708_114,h1708_122)

welfare23 <-welfare23 %>%
  rename(sex=h1701_4,         
         birth=h1701_5,         
         edu=h1701_6,         
         reg=h17_reg5,         
         p_salary=h1708_114,        
         t_salary=h1708_122)

str(welfare23)
summary(welfare23)

table(is.na(welfare23$p_salary))
welfare23$p_salary<-ifelse(welfare23$p_salary==0, NA, welfare23$p_salary)
table(is.na(welfare23$p_salary))

table(is.na(welfare23$t_salary))
welfare23$t_salary<-ifelse(welfare23$t_salary==64800, NA, welfare23$t_salary)
table(is.na(welfare23$t_salary))

# 1을 male로, 2를 female로 변경
table(welfare23$sex)
welfare23$sex<-ifelse(welfare23$sex==1, "male","female")
table(welfare23$sex)

# 출생년도
welfare23$age<-2023-welfare23$birth+1
range(welfare23$age)

# 교육수준 9등급 -> 4등급으로 변경
welfare23$edu_grade<-ifelse(welfare23$edu%in%c(1,2,3,4),"중학이하", ifelse(welfare23$edu==5, "고교", ifelse(welfare23$edu==6,"전문대","대학 이상")))
table(welfare23$edu_grade)

# 4. 연령별 평균 급여
age_salary1 <- welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(age)%>%
  summarise(m=mean(p_salary))

age_salary1%>%
  arrange(desc(m))%>%
  head(3)

# 5. 연령별 평균 총급여 그래프 작성
ggplot(data = age_salary1,aes(x=age, y=m))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")

# 4. 사용직을 대상으로 연령별로 총급여를 분석하기 위하여 나이(age)를 그룹화하여 급여가 높은 순서대로 내림차순을 이용하였다. 이러한 분석을 위하여 내림차순인 desc이 필요하고 top 3개를 나타냈다. 74가 7484만원으로 압독적으로 높은 급여를 받고, 53세가 7128만원, 54세가 6713만원 입니다. 취업 후 꾸준히 상승세를 보이다가 50대에 어느정도 직업적 안정도를 찾으면서 급여 또한 높은 금액을 받게 되는것을 확인할 수 있다. 

# 5. 연령대별로 총급여 그래프를 나타내면 해당 그래프가 나타난다.  20대에 취업 후 꾸준히 50대중순까지 어느정도 직업적 안정도를 찾으면서 급여 또한 높은 금액을 받게 되는것을 확인할 수 있다. 50대 중후반부터 하락세를 보이기 시작한다. 하락세를 보이다가 70대 중반에 갑작스럽게 증가하는 것을 확인할 수 있다. 70대 중반에 갑작스럽게 오르는 것을 퇴직급여를 받음으로써, 높은 금액을 한번에 수령하기에 이러한 현상이 나타난 것으로 보인다. 


