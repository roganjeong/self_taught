#--------------------------------------------------------#
#         Alcohol onsumption & malformation         #
#                       Binomial data                        #
#--------------------------------------------------------#

alcohol <- c('0','<1','1-2','3-5','>=6') # alcohol consumption
pre <- c(48,38,5,1,1)  # malformation present
abs <- c(17066,14464,788,126,37) # malformation absent
tot <- pre+abs # Total, 원래 mal.pre + mal.abs로 잘못되어 있었음
Malf <- data.frame(alcohol,pre,abs,tot)
Malf

Malf$alc <- c(0, 0.5,1.5,4.0,7.0)  # eta와 y를 이어주는 link function의 집어넣는 변수로써 continuous해야 하므로 

# Identity link with binomial random component
# link function을 적절한 걸 써줘야 하는 상황에 안쓰게 되면 어떻게 되는지 보여주는 예
fit1 <- glm(pre/tot ~ alc, family=quasi (link=identity, variance="mu(1-mu)"), weights=tot,data=Malf)
# pre/tot = [0.002804721 0.002620328 0.006305170 0.007874016 0.026315789]
# alc = [0.0 0.5 1.5 4.0 7.0]
#얘네로 이제 회귀처럼 선형방정식을 만들어주는거지 

summary(fit1, dispersion=1) #어떻게 해석해야 하는거...?

# Logit link with binomial random component
# 올바른 link function을 사용했을 경우의 예
fit2 <- glm(pre/tot ~ alc, family=binomial (link=logit), weights=tot,data=Malf)
summary(fit2)
fit2
fitted(fit2)
confint(fit2)  
# 대충 유의하게 나옴, 해석 자체는 회귀분석이랑 똑같음. 음주는 기형아 출산에 영향을 준다. 


library(car)
Anova(fit2)  # likelihood-ratio tests for effect parameters in a GLM

#--------------------------------------------------------#
#             Silicon  data (count data)                   #
#--------------------------------------------------------#
# count 데이터는 다 이렇게 늘어놔야 하는건가...?
trt <- c(rep('B',10),rep('A',10))
defect <- c(9,9,8,14,8,13,11,5,7,6,8,7,6,6,3,4,7,2,3,4)
silicon <- data.frame(trt,defect)

#가정 자체가 틀린 모델
# identity link
pois.fit1 <- glm(defect ~ factor(trt), family=quasi (link=identity, variance="mu"), data=silicon)
summary(pois.fit1, dispersion=1)

#올바른 가정을 한 모델
# log link
pois.fit2 <- glm(defect ~ factor(trt), family=poisson (link=log), data=silicon)
summary(pois.fit2)

#근데 둘 다 deviance 자체는 똑같네...?

pois.fit3 <-  glm(defect ~ 1, family=poisson (link=log), data=silicon)
#pois.fit3 <- update(pois.fit2,.~.-factor(trt)) # 아 이걸 하면 이제 link function에서 파라미터가 알파만 남는거지
summary(pois.fit3)


#--------------------------------------------------------#
#                    GSS (count data)                       #
#--------------------------------------------------------#

white <- c(1070, 60,14,4,0,0,1)
black <- c(119,16,12,7,3,2,0)
other <- c(55,5,1,0,1,0,0)
res <- c(0,1,2,3,4,5,6)            #i명의 살인피해자를 목격한 사람수 
# 0명의 피해자 목격이 백인은 1070명, 흑인은 119명,,,,

n <- length(white) 
N <- sum(c(white,black))  # 총 목격 횟수 1308
response <- rep(0,N) 
race <- rep(0,N)     # response랑 race 둘 다 빈칸 개수 맞춰주기

# White
num1 <-0
num2 <-0
for(i in 1:n){
   if(white[i]!=0){
     num1 <- num2                 # num2있던 값을 num1으로 넘기고, 
     num2 <- num2+white[i]        # num2에 백인의 살인 목격 횟수를 누적시키고 다시 num2에 할당한다. 
     response[(num1+1):num2] <- res[i]  # 몇번째부터 몇번째까지 목격횟수를 일률적으로 할당한다. 
     race[(num1+1):num2] <- 0     # 백인은 0으로 할당
   }
   print(c(num1,num2))
}
unique(response)

# Black
num1 <-sum(white)
num2 <-sum(white)
for(i in 1:n){
   if(black[i]!=0){
     num1 <- num2
     num2 <- num2+black[i]
     response[(num1+1):num2] <- res[i]
     race[(num1+1):num2] <- 1
   }
   print(c(num1,num2))
}
# 바로 위의 for문이랑 똑같은데 흑인인거. 
unique(response)

GSS <- data.frame(response,race)

# negative binomial regression with log link
library(MASS)
NB.fit <- glm.nb(response ~ factor(race), data=GSS)
summary(NB.fit)

# Poisson regression with log link
pois.fit <- glm(response ~ factor(race), family=poisson (link=log), data=GSS)
summary(pois.fit)

#negative binomial regression이 deviance가 훨씬 낮게 나옴


