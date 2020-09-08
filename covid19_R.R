##R codes in covid19 tab##

options(warn=-1)
options(message=-1)
library(EpiEstim)
library(ggplot2)
library("gridExtra")
library(incidence)
library(magrittr)
library(readr)  # for read_csv
library(knitr)  # for kable

##data1=read.csv("raw_data1.csv", header=T) ##old ways##
##data2=read.csv("raw_data2.csv", header=T)
##data3=read.csv("raw_data3.csv", header=T)
##data4=read.csv("raw_data4.csv", header=T)
##data5=read.csv("raw_data5.csv", header=T)

#series1<-as.Date(data1[,3], format = "%d/%m/%y")
#series2<-as.Date(data2[,3], format = "%d/%m/%y")
#series3<-as.Date(data3[,3], format = "%d/%m/%y")
#series4<-as.Date(data4[,3], format = "%d/%m/%y")
#series5<-as.Date(data5[,3], format = "%d/%m/%y")

#all_cases <- c(series1, series2, series3, series4, series5)
#all.equal(as.numeric(all_dates), as.numeric(all_cases))

##new way: read in data from the web##
all_dates <- c()
for(k in 1:5){
 
  tab <- paste0("https://api.covid19india.org/csv/latest/raw_data", k, ".csv")
  suppressMessages(suppressWarnings(tt <- readr::read_csv(tab)))
  dates<-as.Date(unlist(tt[,3]), format = "%d/%m/%y")
  
  all_dates <- c(all_dates, as.Date(unlist(tt[,3]), format = "%d/%m/%y"))

}

#data.frame(head(as.Date(all_dates, origin = "1970-01-01")), head(dates))


#cases_covid19_india<-as.numeric(table(all_cases))
india_tab <-as.numeric(table(all_dates))
dates_india <- as.Date(as.numeric(names(table(all_dates))), origin = "1970-01-01")
#all.equal(tab_india, cases_covid19_india) 
#dates_covid19_india <- names(table(all_cases))
##all.equal(as.numeric(dates_india), as.numeric(dates_covid19_india))

head(india_tab, 10)
head(dates_india, 10)

## estimation of SI (prior): Gamma mean = 7 sd = 4.5 ##

t_start <- seq(2, length(india_tab) - 4)
t_end   <- t_start + 4

Rt_covid19 <- EpiEstim::estimate_R(incid = india_tab, method = "parametric_si",
                                   config = make_config(list(mean_si = 7, std_si = 4.5, si_parametric_distr = "G",
                                    t_start = t_start, t_end = t_end, seed = 123)))

### 3.36 (95% confidence interval (CI): [3.03, 3.71]) on March 24 from Basu et al (U Mich paper)#
### slighlt difference in the third row##

##for presentation##
tibble::tibble(
  date_num = Rt_covid19$dates
) %>% dplyr::left_join(
  Rt_covid19$R, by = c("date_num" = "t_end")
) %>%
  dplyr::select(
    date_num, t_start, 'Mean(R)', 'Quantile.0.025(R)', 'Quantile.0.975(R)'
  ) %>%
  tibble::add_column(date = dates_india) %>%
  dplyr::select(-date_num) %>%
  dplyr::select(date, tidyselect::everything()) %>%
  dplyr::slice(c(24:27, 87:94)) ## to select the rows to print#


####the average of estimates from May 25 - May 31 stands at 1.27 (95% CI: [1.26, 1.28]) from Basu et al ##
###LJ:  I can't get closer to this for confidence intervals##

##this table is easy to manipulate ##
##note: date should go with t_end because t_start = t - tau where tau: a window size for smoothing##

india_covid <- cbind(dates_india[-c(1:5)] , round(Rt_covid19$R[,c(1:5, 8, 11)],2))
india_covid[83:89, ]

apply(india_covid[83:89, c(4,7)], 2, mean) ##both mean and median are 1.25, not 1.27##

win <- 89 #sampling for May 31 data ##
##I tried win = c(83:89) but its mean was off as we see above## 
R_median <- Rt_covid19$R$`Median(R)`[win]
R_CrI <- c(Rt_covid19$R$`Quantile.0.025(R)`[win], Rt_covid19$R$`Quantile.0.975(R)`[win])
set.seed(2019) 
R_sample <- sample_posterior_R(Rt_covid19, n = 10000, window = win)

hist(R_sample, col = "grey", breaks=100, main = "R sampled from May 25 - 31")
abline(v = R_median, col = "red") 
abline(v = R_CrI, col = "red", lty = 2) 

R_sample2 <- sample_posterior_R(Rt_covid19, n = 10000, window = 83:89) ## double peaks!#
hist(R_sample2, col = "grey", breaks=100, main = "R sampled from May 25 - 31")
abline(v = R_median, col = "red") 
abline(v = R_CrI, col = "red", lty = 2) 

case_covid19 <- plot(incidence::as.incidence(india_tab)) + ggtitle("Covid19-India-Cases")
rt_covid19 <- plot(Rt_covid19, "R") + theme(legend.position = "none") + ggtitle("Covid19-India-Rt")
si_covid19 <- plot(Rt_covid19, "SI") + theme(legend.position = "none") + ylab("SI frequency") + ggtitle("Covid19-India-SI distribution")

gridExtra::grid.arrange(case_covid19, rt_covid19, si_covid19, nrow = 1)


##test on our dashboard data##


myfile <- "https://raw.githubusercontent.com/saurabhmj/etl-pipeline/draft-data-pipeline/data_pipeline/output/city_stats.csv"
suppressMessages(mumbai<-read_csv(myfile))
kable(head(mumbai))
 
case_series<-as.numeric(unlist(mumbai[-c(1:60,186),5]))
#case_series  ##111st object is negative. I will use numbers before 111##
case_date <- unlist(mumbai[-c(1:60, 186),1])
length(case_series)

k <-21
case_series21 <-c()
for(k in 21:length(case_series)){
case_series21 <- c(case_series21, mean(case_series[seq(k-20, k)]))
}
 

t_start <- seq(4,  110 - 4)
t_end   <- t_start + 4

##model1: covid
Rt_covid_mumbai <- EpiEstim::estimate_R(incid = case_series[1:110], method = "parametric_si",
                                   config = make_config(list(mean_si = 7, std_si = 4.5, si_parametric_distr = "G",
                                                             t_start = t_start, t_end = t_end, seed = 123)))
t_start <- c(seq(4,  length(case_series)-5))
t_end   <- t_start + 4

Rt_covid_mumbai2 <- EpiEstim::estimate_R(incid = case_series[-c(111)], method = "parametric_si",
config = make_config(list(mean_si = 7, std_si = 4.5, si_parametric_distr = "G",
                          t_start = t_start[-c(109)], t_end = t_end[-c(109)], seed = 123)))


t_start <- c(seq(4,  length(case_series21)-4))
t_end   <- t_start + 4

Rt_covid_mumbai3 <- EpiEstim::estimate_R(incid = case_series21, method = "parametric_si",
config = make_config(list(mean_si = 7, std_si = 4.5, si_parametric_distr = "G",
                          t_start = t_start[-c(109)], t_end = t_end[-c(109)], seed = 123)))


head(data.frame(date=as.Date(case_date[Rt_covid_mumbai$R[,2]],  origin = "1970-01-01"), cases_I= Rt_covid_mumbai$I[Rt_covid_mumbai$R[,2]],
round( Rt_covid_mumbai$R[,c(1:5, 8, 11)],2)))
tail(data.frame(date=as.Date(case_date[Rt_covid_mumbai$R[,2]],  origin = "1970-01-01"), cases_I= Rt_covid_mumbai$I[Rt_covid_mumbai$R[,2]],
round( Rt_covid_mumbai$R[,c(1:5, 8, 11)],2)), 10)


tail(data.frame(date=as.Date(case_date[Rt_covid_mumbai2$R[,2]],  origin = "1970-01-01"), cases_I= Rt_covid_mumbai2$I[Rt_covid_mumbai2$R[,2]],
round( Rt_covid_mumbai2$R[,c(1:5, 8, 11)],2)), 10)


tail(data.frame(date=as.Date(case_date[Rt_covid_mumbai3$R[,2]+20],  origin = "1970-01-01"), cases_I= Rt_covid_mumbai3$I[Rt_covid_mumbai3$R[,2]],
round( Rt_covid_mumbai3$R[,c(1:5, 8, 11)],2)), 20)


##up-to-date: U Mich dashboard##

all_dates_recent <- c()
for(k in 1:13){
  
  tab <- paste0("https://api.covid19india.org/csv/latest/raw_data", k, ".csv")
  suppressMessages(suppressWarnings(tt <- readr::read_csv(tab)))
  dates<-as.Date(unlist(tt[,3]), format = "%d/%m/%y")
  
  all_dates_recent <- c(all_dates_recent, as.Date(unlist(tt[,3]), format = "%d/%m/%y"))
  
}


india_tab_recent <-as.numeric(table(all_dates_recent))
dates_india_recent <- as.Date(as.numeric(names(table(all_dates_recent))), origin = "1970-01-01")


## estimation of SI (prior): Gamma mean = 7 sd = 4.5 ##

t_start <- seq(4, length(india_tab_recent) - 4)
t_end   <- t_start + 4

Rt_covid19_recent <- EpiEstim::estimate_R(incid = india_tab_recent, method = "parametric_si",
                                   config = make_config(list(mean_si = 7, std_si = 4.5, si_parametric_distr = "G",
                                                             t_start = t_start, t_end = t_end, seed = 123)))

tail(data.frame(date=dates_india_recent[Rt_covid19_recent$R[,2]] ,
 cases_I= Rt_covid19_recent$I[Rt_covid19_recent$R[,2]],
round(Rt_covid19_recent$R[,c(1:5, 8, 11)],2)))


