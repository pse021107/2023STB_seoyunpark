responce = c(2,3,4,4,5,6,6,7,8)
x=rep(c("model","model2","model3"),c(3,3,3))
levels= factor(x)
ano.result=aov(responce~levels)
summary(ano.result)

# F분포표에 나타난 바와 같이 유의수준 1%보다 0.008이 작으므로 귀문가설을 기각한다.