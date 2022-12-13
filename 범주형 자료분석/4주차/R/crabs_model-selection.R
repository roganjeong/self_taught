# Model selection for horsecrab data

crabs <- read.table("E:/course/SKKU/Category-Data-Analysis/´ëÇÐ¿ø/SAS_R_code/crabs.dat",header=T)
crabs
attach(crabs)

y <- ifelse(satellites>0,1,0) # y=a binary indicator of satellites
weight <- crabs$weight/1000   # weight in kilograms rather than grams
color <- crabs$color-1              # now color takes values 1,2,3,4
color.f <- factor(color)
spine.f <- factor(spine)

fit0 <- glm(y~1, family=binomial(link=logit))  # null model
fit0

fit.main <- update(fit0,.~.+weight+width+color.f+spine.f)
summary(fit)

cor(weight,width)  # Strong linear relation between width and weight

fit.csw <- glm(y~color.f*spine.f*width,family=binomial) 
    # We drop weight and use a backward selection process, starting 
    # with model with three-factor and all lower interactions
fit.csw 

# Remove 3-factor interaction
fit.cs.cw.sw <- update(fit.csw,.~.-color.f:spine.f:width) 
fit.cs.cw.sw

# Remove color-width interaction
update(fit.cs.cw.sw,.~.-color.f:width) 

# Remove color-spine interaction
update(fit.cs.cw.sw,.~.-color.f:spine.f) 

# remove spine-width interaction. This seems best of the three
update(fit.cs.cw.sw,.~.-spine.f:width) 
                                       

# new working model
fit.cs.cw <- update(fit.cs.cw.sw,.~.-spine.f:width)
update(fit.cs.cw,.~.-color.f:width)

update(fit.cs.cw,.~.-color.f:spine.f)

1-pchisq(181.6-173.7,df=6); 1-pchisq(177.6-173.7,df=3)

# neither reduction in model is bad, so let's try dropping both

fit.c.s.w <- glm(y~color.f+spine.f+width, family=binomial)
fit.c.s.w # a main effects model again, but without weight

1-pchisq(186.6-173.7,df=9) # seems Ok to drop both

summary(fit.c.s.w)


update(fit.c.s.w,.~.-spine.f) # remove spine

update(fit.c.s.w,.~.-color.f) # remove color

update(fit.c.s.w,.~.-width) # remove width

summary(update(fit.c.s.w,.~.-spine.f))  # best to remove spine

fit.c.w <- update(fit.c.s.w,.~.-spine.f) # new working model

update(fit.c.w,.~.-color.f) # remove color

1-pchisq(194.45-187.46,df=3)
# probably best to leave color in, but note pattern of 
# the four estimates; last quite different from others

dark <- ifelse(color>3,1,0) # dark indicates whether color is dark
fit.d.w <- glm(y~dark+width, family=binomial)
summary(fit.d.w)

1-pchisq(187.96-187.46,df=2)

cor(y,fitted(fit.csw))
cor(y,fitted(fit.d.w))
# Note a much simpler model does not sacrifice much predictive power

