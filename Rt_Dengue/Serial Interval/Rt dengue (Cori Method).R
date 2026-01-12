#Tính toán chỉ số Rt dengue phương pháp Cori

#Làm sạch dữ liệu
library(readxl)
library(tidyverse)
library(EpiEstim)
library(tibble)

dengue_list <- read_xlsx("dengue_cases_2023.xlsx", sheet = "sxh")
head(dengue_list)

dengue_filtered <- dengue_list %>%  select("0_HOTEN","0_TUOI","0_PHAI","0_CDCHINH","Ngày HCDC báo cáo") %>% rename("name" = "0_HOTEN", "age" = "0_TUOI", "sex" = "0_PHAI", "diganosis" = "0_CDCHINH","report_date" = "Ngày HCDC báo cáo")

cases_by_date <- dengue_filtered %>%  filter(diganosis == "Sốt xuất huyết Dengue") %>% group_by(report_date) %>% summarise(cases = n()) 

#Tính toán Rt

#library(epiparameter)
#epi_dist_db <- epidist_db()
#dt <- parameter_tbl(epi_dist_db)
#dt

#dengue_si <- epidist_db(
#  disease = "Dengue"
#)

dengue_si


config1 <- make_config(list(
  mean_si = 15,
  std_si = 6.5,
  t_start = seq(2, nrow(cases_by_date) - 6),
  t_end = seq(8, nrow(cases_by_date))
))

res1 <- estimate_R(
  incid = cases_by_date$cases,
  method = "parametric_si",
  config = config1
)

plot(res1, what = "R", main = "Effective Reproduction Number for Dengue in Southeast Asia")

rt_sit1 <- res1$R

#situation 2
meanlog <- 2.553
sdlog <- 0.481
mean_si <- exp(meanlog + sdlog^2 / 2)  # Mean of lognormal distribution
std_si <- sqrt(exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1))  # SD of lognormal distribution

config2 <- make_config(list(
  mean_si = mean_si,  
  std_si = std_si,    
  t_start = seq(2, nrow(cases_by_date) - 6),
  t_end = seq(8, nrow(cases_by_date))
))

res2 <- estimate_R(
  incid = cases_by_date$cases,
  method = "parametric_si",
  config = config2
)

plot(res2, what = "R", main = "Effective Reproduction Number for Dengue in Southeast Asia")

rt_sit2 <- res2$R

#Sit 3
library(MASS)
set.seed(123)
iip_samples <- rlnorm(10000, meanlog = 1.6, sdlog = 0.56)
eip_samples <- rlnorm(10000, meanlog = 1.9, sdlog = 0.9)
gt_samples <- iip_samples + eip_samples
mean_gt <- mean(gt_samples)  # ~12.3 days
sd_gt <- sd(gt_samples)      # ~7.5 days

config3 <- make_config(list(
  mean_si = mean_gt,
  std_si = sd_gt,
  t_start = seq(2, nrow(cases_by_date) - 6),
  t_end = seq(8, nrow(cases_by_date))
))

res3 <- estimate_R(
  incid = cases_by_date$cases,
  method = "parametric_si",
  config = config3
)
plot(res3, what = "R", main = "Effective Reproduction Number for Dengue in Southeast Asia")

rt_sit3 <- res3$R


#Sit 4
config4 <- make_config(list(
  mean_si = 16,
  std_si = 1.64,
  t_start = seq(2, nrow(cases_by_date) - 6),
  t_end = seq(8, nrow(cases_by_date))
))

res4 <- estimate_R(
  incid = cases_by_date$cases,
  method = "parametric_si",
  config = config4
)

plot(res4, what = "R", main = "Effective Reproduction Number for Dengue in Southeast Asia")

rt_sit4 <- res4$R

#Sit 5
config5 <- make_config(list(
  mean_si = 14.5,
  std_si = 3.25
  #t_start = seq(2, nrow(cases_by_date) - 6),
  #t_end = seq(8, nrow(cases_by_date))
))

res5 <- estimate_R(
  incid = cases_by_date$cases,
  method = "parametric_si",
  config = config5
)        
        
plot(res5, what = "R", main = "Effective Reproduction Number for Dengue in Southeast Asia")        

        
r5 <- tibble(
  dates = res5$dates[-(1:7)],
  r_num = res5$R[["Mean(R)"]],
  sd_R = res5$R[["Std(R)"]],
  #week = isoweek(dates),
  #year = isoyear(dates)
)        

#Sit 6
config6 <- make_config(list(
  mean_si = 20,
  std_si = 9
))
        
res6 <- estimate_R(
  incid = cases_by_date$cases,
  method = "parametric_si",
  config = config6
)           
        
plot(res6, what = "R", main = "Effective Reproduction Number for Dengue in Southeast Asia")

r6 <- tibble(
  dates = res6$dates[-(1:7)],
  r_num = res6$R[["Mean(R)"]],
  sd_R = res6$R[["Std(R)"]],
  #week = isoweek(dates),
  #year = isoyear(dates)
)        
