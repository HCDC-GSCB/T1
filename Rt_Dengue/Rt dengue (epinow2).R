
library(EpiNow2)

head(EpiNow2::example_confirmed)

?EpiNow2::Distributions

fixed_gamma <- Gamma(mean = 3, sd = 1, max = 10)
fixed_gamma
plot(fixed_gamma)

uncertain_gamma <- Gamma(shape = Normal(3, 2), rate = Normal(1, 0.1), max = 10)
uncertain_gamma


iib <- LogNormal(mean = 5.9, sd = 0.56, max = 10)

eib <- LogNormal(mean = 7,sd = 1.9, max = 15)

#generation_time <- iib + eib
#plot(generation_time)

reported_cases$date <- as.Date(reported_cases$date)

options(mc.cores = 4)

dengue_run1 <- epinow(data = reported_cases,
       generation_time = gt_opts(iib),
       delays = delay_opts(eib))

dengue_run1$plots$R
dengue_run1$plots$infections

#situation 2

generation_time <- LogNormal(mean = 16, sd = 5, max = 30)
plot(generation_time)

options(mc.cores = 8)

dengue_run2 <- epinow(data = reported_cases, 
                      generation_time = gt_opts(generation_time))

dengue_run2$plots$R

#situation 3
generation_time3 <- Gamma(mean = 20, sd = 9, max = 30)
plot(generation_time3)

options(mc.cores = 8)

dengue_run3 <- epinow(data = reported_cases,
                      generation_time = gt_opts(generation_time3))

dengue_run3$plots$R

run3 <- as.data.frame(dengue_run3$estimates$summarised)
plot(dengue_run3)

#situation 4
generation_time4 <- Gamma(mean = 20, sd = 9, max = 40)
plot(generation_time4)

options(mc.cores = 8)

dengue_run4 <- epinow(data = reported_cases,
                      generation_time = gt_opts(generation_time4))

dengue_run4$plots$R
plot(dengue_run4)

#situation 5
generation_time5 <- Gamma(mean = 11.836, sd = 3.052, max = 40)
plot(generation_time5)

options(mc.cores = 8)

dengue_run5 <- epinow(data = reported_cases,
                      generation_time = gt_opts(generation_time5))

dengue_run5$plots$R
plot(dengue_run5)

#situation 6
generation_time6 <- Gamma(mean = 12.085, sd = 3.023819, max = 40)
plot(generation_time6)

options(mc.cores = 8)

dengue_run6 <- epinow(data = reported_cases,
                      generation_time = gt_opts(generation_time6))

dengue_run6$plots$R
plot(dengue_run6)

Rt_sit_6 <- as.data.frame(dengue_run6$estimates$summarised) %>% filter(variable == "R")

#situation 7
generation_time7 <- LogNormal(meanlog = 2.471267116, sdlog = 0.239752982, max = 40)
plot(generation_time7)

options(mc.cores = 8)

dengue_run7 <- epinow(data = reported_cases,
                      generation_time = gt_opts(generation_time7))
dengue_run7$plots$R






