# In this project, we answer the following questions
# is online advertising effective - i.e., is there a difference in conversion rates between treatment & control groups
# Is there a causal effect - does frequency of advertising effect purchase? Will increasing the frequency of ads, increase the probability of purchase?
# Compare effectiveness of 5 different sites (vendors) and compare to existing one? Which site should star digital put its ad money?

star <- read.csv("star_digital.csv", header = TRUE)

head(star)
# create variables
Timp=star$imp_1+star$imp_2+star$imp_3+star$imp_4+star$imp_5+star$imp_6
Imp_15=star$imp_1+star$imp_2+star$imp_3+star$imp_4+star$imp_5

# t_test 
t.test(Timp ~ star$test)
t.test(star$purchase ~ star$test)

# poisson distribution to analyze effectiveness of display advertising
fit.q1 <- glm(purchase ~ test, 
                data=star, family=binomial())
summary(fit.q1)

# 2) Frequency effects - fit interaction term in the regression
Timp_t<- Timp*star$test
model2 <- glm(purchase ~ Timp+Timp_t, 
                     data=star, family="binomial")

summary(model2)

# 3) How does the effectiveness of Sites 1-5 compare with that of Site 6?
Imp_15_t<-Imp_15*star$test
Imp_6_t<-star$imp_6*star$test

model3 <- glm(purchase ~ Imp_15+Imp_15_t+star$imp_6+Imp_6_t, 
                   data=star, family="binomial")
summary(model3)

confint(model3,level = 0.9)

# 4) Optional Challenge Question -- Which sites should Star Digital advertise on? 
# In particular, should it put its advertising dollars in Site 6 or in Sites 1 through 5?

# calculating offset
PCR<-0.00153
SCR<-0.5
x<-((1-PCR)/PCR)/((1-SCR)/SCR)
star$offset1<-log(x)

# run the regression with offset
imp_6<-star$imp_6
model4<- glm(purchase ~ Imp_15+Imp_15_t+imp_6+Imp_6_t,
                offset = offset1,data=star, family="binomial")
summary(model4)
