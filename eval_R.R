##08.26.2020##
##all R codes in eval.Rmd tab##

library(EpiEstim)
library(ggplot2)

##Five examples from Cori et al 2013 has been reproduced##
##See at: https://academic.oup.com/aje/article/178/9/1505/89262 ##
# five epidemic series##

data(Measles1861)
data(Flu1918)
data(Smallpox1972)
data(SARS2003)
data(Flu2009) ##Vignette example##



##Estimating Rt with default options: smoothign data for previous 7 days (weekly sliding windows) ##

Rt_measles1861 <- estimate_R(incid = Measles1861$incidence,
                             method = "non_parametric_si",
                             config = make_config(list(si_distr = Measles1861$si_distr)))

Rt_flu1918 <- estimate_R(incid = Flu1918$incidence,
                         method = "non_parametric_si",
                         config = make_config(list(si_distr = Flu1918$si_distr)))

Rt_smallpox1972 <- estimate_R(incid = Smallpox1972$incidence,
                              method = "non_parametric_si",
                              config = make_config(list(si_distr = Smallpox1972$si_distr)))


Rt_sars2003 <- estimate_R(incid = SARS2003$incidence,
                          method = "non_parametric_si",
                          config = make_config(list(si_distr = SARS2003$si_distr)))

Rt_flu2009 <- estimate_R(incid = Flu2009$incidence,
                         method = "non_parametric_si",
                         config = make_config(list(si_distr = Flu2009$si_distr)))




round(head(Rt_measles1861$R[,c(1:4, 5, 8, 11)], 15), 1)
round(head(Rt_flu1918$R[,c(1:4, 5, 8, 11)]),1)
round(head(Rt_smallpox1972$R[,c(1:4, 5, 8, 11)], 25),1)
round(head(Rt_sars2003$R[,c(1:4, 5, 8, 11)]), 1)
round(head(Rt_flu2009$R[,c(1:4, 5, 8, 11)]), 1)

##graph case series##

library(incidence)
library("gridExtra")



case_measles1861 <- plot(as.incidence(Measles1861$incidence)) + ggtitle("Measles 1861 ")
case_flu1918 <- plot(as.incidence(Flu1918$incidence)) + ggtitle("Flu 1918")
case_smallpox1972 <- plot(as.incidence(Smallpox1972$incidence)) + ggtitle("Smallpox 1972")
case_sars2003 <- plot(as.incidence(SARS2003$incidence)) + ggtitle("SARS 2003")
case_flu2009 <- plot(as.incidence(Flu2009$incidence$I)) + ggtitle("Flu 2009")

rt_measleas1861 <- plot(Rt_measles1861, "R") + theme(legend.position = "none", plot.title = element_blank())
rt_flu1918 <- plot(Rt_flu1918, "R") + theme(legend.position = "none", plot.title = element_blank())
rt_smallpox1972 <- plot(Rt_Smallpox1972 , "R")+ theme(legend.position = "none", plot.title = element_blank())
rt_sars2003 <- plot(Rt_sars2003, "R") + theme(legend.position = "none", plot.title = element_blank())
rt_flu2009 <- plot(Rt_flu2009, "R") + theme(legend.position = "none", plot.title = element_blank())

si_measleas1861 <- plot(Rt_measles1861, "SI") + theme(legend.position = "none", plot.title = element_blank()) + ylab("SI frequency")
si_flu1918 <- plot(Rt_flu1918, "SI") + theme(legend.position = "none", plot.title = element_blank()) + ylab("SI frequency")
si_smallpox1972 <- plot(Rt_Smallpox1972 , "SI")+ theme(legend.position = "none", plot.title = element_blank()) + ylab("SI frequency")
si_sars2003 <- plot(Rt_sars2003, "SI") + theme(legend.position = "none", plot.title = element_blank()) + ylab("SI frequency")
si_flu2009 <- plot(Rt_flu2009, "SI") + theme(legend.position = "none", plot.title = element_blank()) + ylab("SI frequency")



grid.arrange(case_measles1861, case_flu1918 , case_smallpox1972, case_sars2003, case_flu2009,
             rt_measleas1861, rt_flu1918, rt_smallpox1972, rt_sars2003, rt_flu2009,
             si_measleas1861, si_flu1918, si_smallpox1972, si_sars2003, si_flu2009,
             ncol = 5, nrow = 3)

##please refer to eval.pdf if you want to check any difference compared to the Cori et al (2013)##