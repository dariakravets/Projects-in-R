employee1 = read.csv("E://DownloadsE//WA_Fn-UseC_-HR-Employee-Attrition (1).csv")
View(employee1)
library(ggplot2)
library(ggpubr)
library(lsmeans)

employee1$JobRole <- factor(employee1$JobRole)

p<-ggplot(employee1, aes(x=MonthlyIncome, y=JobRole, color=JobRole)) +
  geom_jitter(position=position_jitter(0.2))
p + theme(legend.position="none")

boxplot(MonthlyIncome ~ JobRole, data = employee1,
        xlab = "JobRole", ylab = "Monthly Income",
        main = "Income ~ JobRole", col = c("#f8766d", "#d69f22", "#93aa00", "#00ba38", "#00c19f", "#00b9e3", "#619cff", "#db72fb", "#ff61c3"))

aggregate(x = employee1$MonthlyIncome, by = list(employee1$JobRole), FUN = mean)

mod1 <- aov(MonthlyIncome ~ JobRole, data = employee1)
summary(aov(MonthlyIncome ~ JobRole, data = employee1))
summary(lm(MonthlyIncome ~ JobRole, data = employee1))

contrasts(employee1$JobRole) <- contr.sum
contrasts(employee1$JobRole)

summary(lm(MonthlyIncome ~ JobRole, data = employee1))

plot(mod1)

ggplot(employee1, aes(sample = MonthlyIncome, col = JobRole))+
  geom_qq()+
  facet_grid( ~ JobRole, space = "free") + theme(legend.position="none")


hist(mod1$residuals, col = "#48bd94", main="Гістограма залишків")

ggqqplot(mod1$residuals)
shapiro.test(mod1$residuals)
tapply(employee1$MonthlyIncome, employee1$JobRole, var)
bartlett.test(employee1$MonthlyIncome, employee1$JobRole)

mod2 <- lm(MonthlyIncome ~ JobRole, data = employee1)
lsmeans(mod2, ~ JobRole, adjust="scheffe")

plot(TukeyHSD(aov(MonthlyIncome ~ JobRole, data = employee1)))
