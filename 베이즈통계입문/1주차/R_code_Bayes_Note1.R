# 불량품 추정 사후확률 (p13) (p61 Bayes책)
pf <-function(g,prior){
  if(g==1) post <-0.93*prior/(0.93*prior+0.7*(1-prior))  # posterior prob.(Good)
  else post <- 0.07*prior/(0.07*prior+0.3*(1-prior))     # posterior prob.(Bad)
  return(post)
}
dd=c(1,1,0,1,0,1,1,1,1)   # good=1, bad=0
pp=rep(0,length(dd))
pp[1]=0.9
for(i in 1:length(dd))pp[i+1]=pf(dd[i],pp[i])
round(pp,3)


# Monty Hall Problem
doors = c("1", "2", "3")
results = c()
for(i in 1:1000){
  car =sample(doors, 1) # door for car
  pick = sample(doors, 1) # choose a door
  open = sample(doors[which(doors !=pick & doors !=car)],1) # open door
  switchyes = doors[which(doors !=pick & doors !=open)] # change a door
  if(pick==car){
    results = c(results, "noswitchwin")
  }
  if(switchyes==car){
    results=c(results, "switchwin")
  }
}

sum(results=="switchwin")
sum(results=="noswitchwin")
