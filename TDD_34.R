library(ggplot2)
library(ggeffects)
library(dplyr)
library(broom)


TDD_A <- read.table("C:/Users/sanka/Dropbox/PC (2)/Downloads/TDD.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
TDD_A <- within(TDD, {
  Fx22_PopD5000 <- as.factor(x22_PopD5000)
  FX31_man <- as.factor(X31_man)
  FX33_rent <- as.factor(X33_rent)
  FX34_garden <- as.factor(X34_garden)
  Fy11_move2020 <- as.factor(y11_move2020)
  Fy12_move2021 <- as.factor(y12_move2021)
})
TDD_B <- subset(TDD_A, subset=A01_Group == 1)
TDD_C <- subset(TDD_A, subset=A01_Group == 0)

#Model_1:Density(Liner)

Model1A <- glm(Fy11_move2020 ~ nx21_PopD + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_A)
Model1B <- glm(Fy12_move2021 ~ nx21_PopD + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_B)
Model1C <- glm(Fy12_move2021 ~ nx21_PopD + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_C)

summary(Model1A)
exp(coef(Model1A))
summary(Model1B)
exp(coef(Model1B))
summary(Model1C)
exp(coef(Model1C))
 
Model1A_p <- ggpredict(Model1A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model1A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model1A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model1A", x = "Population Density", y = "Relocation Intention")
Model1B_p <- ggpredict(Model1B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model1B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model1B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model1B", x = "Population Density", y = "Relocation Intention")
Model1C_p <- ggpredict(Model1C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model1C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model1C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model1C", x = "Population Density", y = "Relocation Intention")

#Model_2:Density(Non-liner)

Model2A <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_A)
Model2B <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_B)
Model2C <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_C)

summary(Model2A)
exp(coef(Model2A))
summary(Model2B)
exp(coef(Model2B))
summary(Model2C)
exp(coef(Model2C))

Model2A_p <- ggpredict(Model2A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2A", x = "Population Density", y = "Residential Mobility Intention")
Model2B_p <- ggpredict(Model2B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2B", x = "Population Density", y = "Residential Mobility Intention")
Model2C_p <- ggpredict(Model2C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2C", x = "Population Density", y = "Residential Mobility Intention")


#Model_3:Density(Category)

TDD_A$Fx22_PopD5000 <- with(TDD_A, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))
TDD_B$Fx22_PopD5000 <- with(TDD_B, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))
TDD_C$Fx22_PopD5000 <- with(TDD_C, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))

Model3A <- glm(y11_move2020 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_A)
Model3B <- glm(y12_move2021 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_B)
Model3C <- glm(y12_move2021 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD_C)

summary(Model3A)
exp(coef(Model3A))
summary(Model3B)
exp(coef(Model3B))
summary(Model3C)
exp(coef(Model3C))

Model3A_t <- tidy(Model3A) %>%
  mutate(
    conf.low = confint(Model3A)[, 1],
    conf.high = confint(Model3A)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3A_t_PD <- Model3A_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3A_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3A", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model3B_t <- tidy(Model3B) %>%
  mutate(
    conf.low = confint(Model3B)[, 1],
    conf.high = confint(Model3B)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3B_t_PD <- Model3B_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3B_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3B", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model3C_t <- tidy(Model3C) %>%
  mutate(
    conf.low = confint(Model3C)[, 1],
    conf.high = confint(Model3C)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3C_t_PD <- Model3C_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3C_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3C", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 9) + theme(legend.position = "none")





#Model 4(Not Using)

TDD29 <- read.table("C:/Users/sanka/Dropbox/04東大総括P/2420論文投稿/密度K/R1/R/TokyoDensity_Dist_R1_v29c.csv", header=TRUE, 
  stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
TDD29 <- within(TDD29, {
  Fx22_PopD5000 <- as.factor(x22_PopD5000)
  Fx23_PopD_6rank <- as.factor(x23_PopD_6rank)
  FX31_man <- as.factor(X31_man)
  FX33_rent <- as.factor(X33_rent)
  FX34_garden <- as.factor(X34_garden)
  Fy11_move2020 <- as.factor(y11_move2020)
  Fy12_move2021 <- as.factor(y12_move2021)
})

TDD29 <- within(TDD29, {Ox22_PopD5000 <- as.ordered(x22_PopD5000)})
TDD29$Fx22_PopD5000 <- with(TDD29, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))

TDD29 <- within(TDD29, {Ox22_PopD_6rank <- as.ordered(x23_PopD_6rank)})
TDD29$Fx23_PopD_6rank <- with(TDD29, factor(Fx23_PopD_6rank, levels=c('3','1','2','4','5','6')))


TDD29_2021Tele <- subset(TDD29, subset=A01_Group == 1)
TDD29_2021Comm <- subset(TDD29, subset=A01_Group == 0)


Model4A <- glm(y11_move2020 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29)
Model4B <- glm(y12_move2021 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Tele)
Model4C <- glm(y12_move2021 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Comm)

summary(Model4A)
exp(coef(Model4A))
summary(Model4B)
exp(coef(Model4B))
summary(Model4C)
exp(coef(Model4C))

Model4A_t <- tidy(Model4A) %>%
  mutate(
    conf.low = confint(Model4A)[, 1],
    conf.high = confint(Model4A)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4A_t_PD <- Model4A_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4A_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4A", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4B_t <- tidy(Model4B) %>%
  mutate(
    conf.low = confint(Model4B)[, 1],
    conf.high = confint(Model4B)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4B_t_PD <- Model4B_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4B_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4B", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4C_t <- tidy(Model4C) %>%
  mutate(
    conf.low = confint(Model4C)[, 1],
    conf.high = confint(Model4C)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4C_t_PD <- Model4C_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4C_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4C", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 10) + theme(legend.position = "none")

#Model_2p:Density(Non-liner)

Model2A <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29)
Model2B <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Tele)
Model2C <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Comm)


summary(Model2A)
exp(coef(Model2A))
summary(Model2B)
exp(coef(Model2B))
summary(Model2C)
exp(coef(Model2C))

Model2A4 <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=4) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29)
Model2B4 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=4) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Tele)
Model2C4 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=4) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Comm)


summary(Model2A4)
exp(coef(Model2A4))
summary(Model2B4)
exp(coef(Model2B4))
summary(Model2C4)
exp(coef(Model2C4))

Model2A5 <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=5) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29)
Model2B5 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=5) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Tele)
Model2C5 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=5) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR, family=binomial(logit), data=TDD29_2021Comm)


summary(Model2A5)
exp(coef(Model2A5))
summary(Model2B5)
exp(coef(Model2B5))
summary(Model2C5)
exp(coef(Model2C5))

Model2A_p <- ggpredict(Model2A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2A", x = "Population Density", y = "Residential Mobility Intention")
Model2B_p <- ggpredict(Model2B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2B", x = "Population Density", y = "Residential Mobility Intention")
Model2C_p <- ggpredict(Model2C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2C", x = "Population Density", y = "Residential Mobility Intention")


TDD30 <- read.table("C:/Users/sanka/Dropbox/04東大総括P/2420論文投稿/密度K/R1/R/TokyoDensity_Dist_R1_v30c.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
TDD30 <- within(TDD30, {
  Fx22_PopD5000 <- as.factor(x22_PopD5000)
  Fx23_PopD_6rank <- as.factor(x23_PopD_6rank)
  FX31_man <- as.factor(X31_man)
  FX33_rent <- as.factor(X33_rent)
  FX34_garden <- as.factor(X34_garden)
  Fy11_move2020 <- as.factor(y11_move2020)
  Fy12_move2021 <- as.factor(y12_move2021)
})

TDD30 <- within(TDD30, {Ox22_PopD5000 <- as.ordered(x22_PopD5000)})
TDD30$Fx22_PopD5000 <- with(TDD30, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))

TDD30 <- within(TDD30, {Ox22_PopD_6rank <- as.ordered(x23_PopD_6rank)})
TDD30$Fx23_PopD_6rank <- with(TDD30, factor(Fx23_PopD_6rank, levels=c('3','1','2','4','5','6')))

TDD30_2021Tele <- subset(TDD30, subset=A01_Group == 1)
TDD30_2021Comm <- subset(TDD30, subset=A01_Group == 0)


Model2A <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30)
Model2B <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30_2021Tele)
Model2C <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30_2021Comm)

summary(Model2A)
exp(coef(Model2A))
summary(Model2B)
exp(coef(Model2B))
summary(Model2C)
exp(coef(Model2C))

Model2A_p <- ggpredict(Model2A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2A", x = "Population Density", y = "Residential Mobility Intention")
Model2B_p <- ggpredict(Model2B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2B", x = "Population Density", y = "Residential Mobility Intention")
Model2C_p <- ggpredict(Model2C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2C", x = "Population Density", y = "Residential Mobility Intention")

Model4A <- glm(y11_move2020 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30)
Model4B <- glm(y12_move2021 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30_2021Tele)
Model4C <- glm(y12_move2021 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30_2021Comm)

summary(Model4A)
exp(coef(Model4A))
summary(Model4B)
exp(coef(Model4B))
summary(Model4C)
exp(coef(Model4C))

Model4A_t <- tidy(Model4A) %>%
  mutate(
    conf.low = confint(Model4A)[, 1],
    conf.high = confint(Model4A)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4A_t_PD <- Model4A_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4A_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4A", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4B_t <- tidy(Model4B) %>%
  mutate(
    conf.low = confint(Model4B)[, 1],
    conf.high = confint(Model4B)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4B_t_PD <- Model4B_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4B_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4B", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4C_t <- tidy(Model4C) %>%
  mutate(
    conf.low = confint(Model4C)[, 1],
    conf.high = confint(Model4C)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4C_t_PD <- Model4C_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4C_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4C", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 10) + theme(legend.position = "none")

Model3A <- glm(y11_move2020 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30)
Model3B <- glm(y12_move2021 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30_2021Tele)
Model3C <- glm(y12_move2021 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD30_2021Comm)

summary(Model3A)
exp(coef(Model3A))
summary(Model3B)
exp(coef(Model3B))
summary(Model3C)
exp(coef(Model3C))

Model3A_t <- tidy(Model3A) %>%
  mutate(
    conf.low = confint(Model3A)[, 1],
    conf.high = confint(Model3A)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3A_t_PD <- Model3A_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3A_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3A", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model3B_t <- tidy(Model3B) %>%
  mutate(
    conf.low = confint(Model3B)[, 1],
    conf.high = confint(Model3B)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3B_t_PD <- Model3B_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3B_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3B", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model3C_t <- tidy(Model3C) %>%
  mutate(
    conf.low = confint(Model3C)[, 1],
    conf.high = confint(Model3C)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3C_t_PD <- Model3C_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3C_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3C", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 10) + theme(legend.position = "none")



TDD31 <- read.table("C:/Users/sanka/Dropbox/04東大総括P/2420論文投稿/密度K/R1/R/TokyoDensity_Dist_R1_v31c.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
TDD31 <- within(TDD31, {
  Fx22_PopD5000 <- as.factor(x22_PopD5000)
  Fx23_PopD_6rank <- as.factor(x23_PopD_6rank)
  Fx24_PopD_8rank <- as.factor(x24_PopD_8rank)
  FX31_man <- as.factor(X31_man)
  FX33_rent <- as.factor(X33_rent)
  FX34_garden <- as.factor(X34_garden)
  Fy11_move2020 <- as.factor(y11_move2020)
  Fy12_move2021 <- as.factor(y12_move2021)
})

TDD31 <- within(TDD31, {Ox22_PopD5000 <- as.ordered(x22_PopD5000)})
TDD31$Fx22_PopD5000 <- with(TDD31, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))

TDD31 <- within(TDD31, {Ox24_PopD_8rank <- as.ordered(x24_PopD_8rank)})
TDD31$Fx24_PopD_8rank <- with(TDD31, factor(Fx24_PopD_8rank, levels=c('4','1','2','3','5','6','7','8')))

TDD31_2021Tele <- subset(TDD31, subset=A01_Group == 1)
TDD31_2021Comm <- subset(TDD31, subset=A01_Group == 0)

Model4A <- glm(y11_move2020 ~ Fx24_PopD_8rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD31)
Model4B <- glm(y12_move2021 ~ Fx24_PopD_8rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD31_2021Tele)
Model4C <- glm(y12_move2021 ~ Fx24_PopD_8rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX56_Dist_TKOst, family=binomial(logit), data=TDD31_2021Comm)

summary(Model4A)
exp(coef(Model4A))
summary(Model4B)
exp(coef(Model4B))
summary(Model4C)
exp(coef(Model4C))

Model4A_t <- tidy(Model4A) %>%
  mutate(
    conf.low = confint(Model4A)[, 1],
    conf.high = confint(Model4A)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4A_t_PD <- Model4A_t %>% filter(grepl("Fx24_PopD_8rank", term))
ggplot(Model4A_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4A", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather low", "Rather High", "High", "Very High", "Extream High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4B_t <- tidy(Model4B) %>%
  mutate(
    conf.low = confint(Model4B)[, 1],
    conf.high = confint(Model4B)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4B_t_PD <- Model4B_t %>% filter(grepl("Fx24_PopD_8rank", term))
ggplot(Model4B_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4B", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather low", "Rather High", "High", "Very High", "Extream High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4C_t <- tidy(Model4C) %>%
  mutate(
    conf.low = confint(Model4C)[, 1],
    conf.high = confint(Model4C)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4C_t_PD <- Model4C_t %>% filter(grepl("Fx24_PopD_8rank", term))
ggplot(Model4C_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4C", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather low", "Rather High", "High", "Very High", "Extream High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 10) + theme(legend.position = "none")

TDD32 <- read.table("C:/Users/sanka/Dropbox/04東大総括P/2420論文投稿/密度K/R1/R/TokyoDensity_Dist_R1_v32c.csv", header=TRUE, 
  stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
TDD32 <- within(TDD32, {
  Fx22_PopD5000 <- as.factor(x22_PopD5000)
  Fx23_PopD_6rank <- as.factor(x23_PopD_6rank)
  Fx24_PopD_8rank <- as.factor(x24_PopD_8rank)
  FX31_man <- as.factor(X31_man)
  FX33_rent <- as.factor(X33_rent)
  FX34_garden <- as.factor(X34_garden)
  Fy11_move2020 <- as.factor(y11_move2020)
  Fy12_move2021 <- as.factor(y12_move2021)
})

TDD32 <- within(TDD32, {Ox22_PopD5000 <- as.ordered(x22_PopD5000)})
TDD32$Fx22_PopD5000 <- with(TDD32, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))

TDD32 <- within(TDD32, {Ox24_PopD_8rank <- as.ordered(x24_PopD_8rank)})
TDD32$Fx24_PopD_8rank <- with(TDD32, factor(Fx24_PopD_8rank, levels=c('4','1','2','3','5','6','7','8')))

TDD32_2021Tele <- subset(TDD32, subset=A01_Group == 1)
TDD32_2021Comm <- subset(TDD32, subset=A01_Group == 0)

Model2A <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV, family=binomial(logit), data=TDD32)
Model2B <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV, family=binomial(logit), data=TDD32_2021Tele)
Model2C <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV, family=binomial(logit), data=TDD32_2021Comm)

summary(Model2A)
exp(coef(Model2A))
summary(Model2B)
exp(coef(Model2B))
summary(Model2C)
exp(coef(Model2C))

Model2A_p <- ggpredict(Model2A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2A", x = "Population Density", y = "Residential Mobility Intention")
Model2B_p <- ggpredict(Model2B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2B", x = "Population Density", y = "Residential Mobility Intention")
Model2C_p <- ggpredict(Model2C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2C", x = "Population Density", y = "Residential Mobility Intention")

TDD33 <- within(TDD33, {
  Fx22_PopD5000 <- as.factor(x22_PopD5000)
  Fx23_PopD_6rank <- as.factor(x23_PopD_6rank)
  Fx24_PopD_8rank <- as.factor(x24_PopD_8rank)
  FX31_man <- as.factor(X31_man)
  FX33_rent <- as.factor(X33_rent)
  FX34_garden <- as.factor(X34_garden)
  Fy11_move2020 <- as.factor(y11_move2020)
  Fy12_move2021 <- as.factor(y12_move2021)
})

TDD33 <- within(TDD33, {Ox22_PopD5000 <- as.ordered(x22_PopD5000)})
TDD33$Fx22_PopD5000 <- with(TDD33, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))

TDD33 <- within(TDD33, {Ox24_PopD_8rank <- as.ordered(x24_PopD_8rank)})
TDD33$Fx24_PopD_8rank <- with(TDD33, factor(Fx24_PopD_8rank, levels=c('4','1','2','3','5','6','7','8')))

TDD33_2021Tele <- subset(TDD33, subset=A01_Group == 1)
TDD33_2021Comm <- subset(TDD33, subset=A01_Group == 0)

Model2A <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV + nX58_Dist_CBD, family=binomial(logit), data=TDD33)
Model2B <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV + nX58_Dist_CBD, family=binomial(logit), data=TDD33_2021Tele)
Model2C <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV + nX58_Dist_CBD, family=binomial(logit), data=TDD33_2021Comm)

summary(Model2A)
exp(coef(Model2A))
summary(Model2B)
exp(coef(Model2B))
summary(Model2C)
exp(coef(Model2C))

Model2A_p <- ggpredict(Model2A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2A", x = "Population Density", y = "Residential Mobility Intention")
Model2B_p <- ggpredict(Model2B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2B", x = "Population Density", y = "Residential Mobility Intention")
Model2C_p <- ggpredict(Model2C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2C", x = "Population Density", y = "Residential Mobility Intention")

summary(TDD33)
library(car)
vif(Model2A)
TDD33 <- read.table("C:/Users/sanka/Dropbox/04東大総括P/2420論文投稿/密度K/R1/R/TokyoDensity_Dist_R1_v33c.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".",   strip.white=TRUE)
Model1A <- glm(y11_move2020 ~ nx21_PopD + X31_man + nX32_age + X33_rent + X34_garden + nX35_commerce + nX57_LandV + nX58_Dist_CBD, family=binomial(logit), data=TDD33)
summary(Model1A)
exp(coef(Model1A))  # Exponentiated coefficients ("odds ratios")
TDD33 <- read.table("C:/Users/sanka/Dropbox/04東大総括P/2420論文投稿/密度K/R1/R/TokyoDensity_Dist_R1_v33c.csv", header=TRUE, 
  stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

Model4A <- glm(y11_move2020 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV + nX58_Dist_CBD, family=binomial(logit), data=TDD33)
Model4B <- glm(y12_move2021 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV + nX58_Dist_CBD, family=binomial(logit), data=TDD33_2021Tele)
Model4C <- glm(y12_move2021 ~ Fx23_PopD_6rank + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX57_LandV + nX58_Dist_CBD, family=binomial(logit), data=TDD33_2021Comm)

summary(Model4A)
exp(coef(Model4A))
summary(Model4B)
exp(coef(Model4B))
summary(Model4C)
exp(coef(Model4C))

Model4A_t <- tidy(Model4A) %>%
  mutate(
    conf.low = confint(Model4A)[, 1],
    conf.high = confint(Model4A)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4A_t_PD <- Model4A_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4A_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4A", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4B_t <- tidy(Model4B) %>%
  mutate(
    conf.low = confint(Model4B)[, 1],
    conf.high = confint(Model4B)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4B_t_PD <- Model4B_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4B_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4B", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 2) + theme(legend.position = "none")
Model4C_t <- tidy(Model4C) %>%
  mutate(
    conf.low = confint(Model4C)[, 1],
    conf.high = confint(Model4C)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model4C_t_PD <- Model4C_t %>% filter(grepl("Fx23_PopD_6rank", term))
ggplot(Model4C_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 4C", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("Very low", "low", "Rather High", "High", "Very High")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 10) + theme(legend.position = "none")


TDD34 <- read.table("C:/Users/sanka/Dropbox/04東大総括P/2420論文投稿/密度K/R1/R/TokyoDensity_Dist_R1_v34c.csv", header=TRUE, 
  stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)summary(TDD34)
#valiables
TDD34 <- within(TDD34, {
  Fx22_PopD5000 <- as.factor(x22_PopD5000)
  Fx23_PopD_6rank <- as.factor(x23_PopD_6rank)
  Fx24_PopD_8rank <- as.factor(x24_PopD_8rank)
  Fx25_PopD_12rank <- as.factor(x25_PopD_12rank)
  FX31_man <- as.factor(X31_man)
  FX33_rent <- as.factor(X33_rent)
  FX34_garden <- as.factor(X34_garden)
  Fy11_move2020 <- as.factor(y11_move2020)
  Fy12_move2021 <- as.factor(y12_move2021)
})
TDD34 <- within(TDD34, {Ox22_PopD5000 <- as.ordered(x22_PopD5000)})
TDD34$Fx22_PopD5000 <- with(TDD34, factor(Fx22_PopD5000, levels=c('3','1','2','4','5','6')))
TDD34 <- within(TDD34, {Ox24_PopD_8rank <- as.ordered(x24_PopD_8rank)})
TDD34$Fx24_PopD_8rank <- with(TDD34, factor(Fx24_PopD_8rank, levels=c('4','1','2','3','5','6','7','8')))
TDD34 <- within(TDD34, {Ox25_PopD_12rank <- as.ordered(x25_PopD_12rank)})
TDD34$Fx25_PopD_12rank <- with(TDD34, factor(Fx25_PopD_12rank, levels=c('6','1','2','3','4','5','7','8','9','10','11','12')))
#subset
TDD34_2021Tele <- subset(TDD34, subset=A01_Group == 1)
TDD34_2021Comm <- subset(TDD34, subset=A01_Group == 0)
#Model1
Model1A <- glm(Fy11_move2020 ~ nx21_PopD + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34)
Model1B <- glm(Fy12_move2021 ~ nx21_PopD + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Tele)
Model1C <- glm(Fy12_move2021 ~ nx21_PopD + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Comm)
summary(Model1A)
exp(coef(Model1A))
summary(Model1B)
exp(coef(Model1B))
summary(Model1C)
exp(coef(Model1C))
Model1A_p <- ggpredict(Model1A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model1A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model1A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model1A", x = "Population Density", y = "Relocation Intention")
Model1B_p <- ggpredict(Model1B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model1B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model1B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model1B", x = "Population Density", y = "Relocation Intention")
Model1C_p <- ggpredict(Model1C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model1C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model1C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model1C", x = "Population Density", y = "Relocation Intention")


#Model2
Model2A <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34)
Model2B <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Tele)
Model2C <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=3) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Comm)
summary(Model2A)
exp(coef(Model2A))
summary(Model2B)
exp(coef(Model2B))
summary(Model2C)
exp(coef(Model2C))
Model2A_p <- ggpredict(Model2A, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2A_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2A_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2A", x = "Population Density", y = "Residential Mobility Intention")
Model2B_p <- ggpredict(Model2B, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2B_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2B_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2B", x = "Population Density", y = "Residential Mobility Intention")
Model2C_p <- ggpredict(Model2C, terms = "nx21_PopD")
ggplot() + geom_line(data = Model2C_p, aes(x = x, y = predicted)) + geom_ribbon(data = Model2C_p, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) + xlim(c(0, 40000)) + ylim(c(0, 1)) + labs(title = "Model2C", x = "Population Density", y = "Residential Mobility Intention")
#Model3
Model3A <- glm(y11_move2020 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34)
Model3B <- glm(y12_move2021 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Tele)
Model3C <- glm(y12_move2021 ~ Fx22_PopD5000 + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Comm)
summary(Model3A)
exp(coef(Model3A))
summary(Model3B)
exp(coef(Model3B))
summary(Model3C)
exp(coef(Model3C))
Model3A_t <- tidy(Model3A) %>%
  mutate(
    conf.low = confint(Model3A)[, 1],
    conf.high = confint(Model3A)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3A_t_PD <- Model3A_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3A_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3A", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 3) + theme(legend.position = "none")
Model3B_t <- tidy(Model3B) %>%
  mutate(
    conf.low = confint(Model3B)[, 1],
    conf.high = confint(Model3B)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3B_t_PD <- Model3B_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3B_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3B", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 3) + theme(legend.position = "none")
Model3C_t <- tidy(Model3C) %>%
  mutate(
    conf.low = confint(Model3C)[, 1],
    conf.high = confint(Model3C)[, 2],
    OR = exp(estimate),
    CI_l = exp(conf.low),
    CI_h = exp(conf.high)
  )
Model3C_t_PD <- Model3C_t %>% filter(grepl("Fx22_PopD5000", term))
ggplot(Model3C_t_PD, aes(x = term, y = OR, color = term)) +  geom_point() + geom_errorbar(aes(ymin = CI_l, ymax = CI_h), width = 0.2) + labs(title = "Model 3C", x = "Population Density", y = "Relocation intention") + scale_x_discrete(labels = c("-5000", "5000-10000", "15000-20000", "20000-25000", "25000-")) + theme_minimal() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  ylim(0, 10) + theme(legend.position = "none")

Model2A4 <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=4) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34)
Model2B4 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=4) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Tele)
Model2C4 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=4) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Comm)
summary(Model2A4)
exp(coef(Model2A4))
summary(Model2B4)
exp(coef(Model2B4))
summary(Model2C4)
exp(coef(Model2C4))
Model2A5 <- glm(Fy11_move2020 ~ bs(nx21_PopD , df=5) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34)
Model2B5 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=5) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Tele)
Model2C5 <- glm(Fy12_move2021 ~ bs(nx21_PopD , df=5) + X31_man + nX32_age + X33_rent + X34_garden +nX35_commerce + nx36_GreenR + nX37_LandV + nX38_Dist_CBD, family=binomial(logit), data=TDD34_2021Comm)
summary(Model2A5)
exp(coef(Model2A5))
summary(Model2B5)
exp(coef(Model2B5))
summary(Model2C5)
exp(coef(Model2C5))
