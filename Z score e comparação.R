#IN CONTRUÇÃO
library(DescTools)
ZAI<- FisherZ(r = 0.83) #PS: FAZER ZSCORE DE CADA CORRELAÇÃO, ALL DONT SIGNIF. #PS FAZER 1 COLUNA COM TODAS AS CORRELAÇÕES
ZBI<- FisherZ(r = 0.73)
test <- fisher.test(dat)
test$p.value
meanRAI <- mean(matcor)
meanRBI<- mean(matcorBI)
meancondition<- paste(meanRAI, meanRBI) #Concatenar
library(cocor)
cocor.indep.groups(meanRAI, meanRBI, 49, 49, alternative = "two.sided", test = "all",
  alpha = 0.05, conf.level = 0.95, return.htest = FALSE )

plot(ZAI,ZBI)



library(graphics)
  
  
  #barplot(meancondition,
   #       xlab = "Condi??o",
    #      ylab = "Mean r",
     #     col = c("seagreen", "yellowgreen"))