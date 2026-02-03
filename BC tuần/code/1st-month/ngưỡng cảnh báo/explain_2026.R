### SXH ###############

df_2026 <- out_f_sxh_2026$df %>% filter(year==2026)
df_2025 <- out_f_sxh_2026$df %>% filter(year==2025)

a <- df_2026[4,"cases"] %>% pull() # Current week
b <- df_2026[3, "cases"] %>% pull() # Pre-week


d <- df_2026 %>% filter(week<=4) %>% 
  summarise(n = sum(cases)) %>% 
  pull() #Cummulative cases until current week of 2026

e <- df_2025 %>% filter(week<=4) %>% 
  summarise(n = sum(cases)) %>% 
  pull() #Cummulative cases until current week of 2025

f <- out_f_sxh_2026$result$upperbound[[4]] # Farrington of current week
g <- df_sxh_2026[4, "cdc"] %>% pull() # CDC of current week
h <- remake(df_sxh, c(2016, 2017,2020, 2023, 2024))$seasonal
k <- out_c_sxh_2026$result$upperbound[[3]] # CUSUM of current week

(a-b)/b # Current vs pre-week
(a-h)/h # Current vs Seasonal
(a-k)/k # Current vs CUSUM
(a-f)/f # Current vs Farrington 
(a-g)/g # Current vs CDC
(d-e)/e 


### TCM ###############
df_2026 <- out_f_tcm_2026$df %>% filter(year==2026)
df_2025 <- out_f_tcm_2026$df %>% filter(year==2025)

a <- df_2026[4,"cases"] %>% pull() # Current week
b <- df_2026[3, "cases"] %>% pull() # Pre-week


d <- df_2026 %>% filter(week<=4) %>% 
  summarise(n = sum(cases)) %>% 
  pull() #Cummulative cases until current week of 2026

e <- df_2025 %>% filter(week<=4) %>% 
  summarise(n = sum(cases)) %>% 
  pull() #Cummulative cases until current week of 2025

f <- out_f_tcm_2026$result$upperbound[[4]] # Farrington of current week
g <- df_tcm_2026[4, "cdc"] %>% pull() # CDC of current week
h <- remake(df_tcm, c(2017, 2019, 2020, 2022, 2024))$seasonal
k <- out_c_tcm_2026$result$upperbound[[4]] # CUSUM of current week

(a-b)/b # Current vs pre-week
(a-h)/h # Current vs Seasonal
(a-k)/k # Current vs CUSUM
(a-f)/f # Current vs Farrington 
(a-g)/g # Current vs CDC
(d-e)/e 