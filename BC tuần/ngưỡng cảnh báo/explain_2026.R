df_2026 <- out_f_sxh_2026$df %>% filter(year==2026)
df_2025 <- out_f_sxh_2026$df %>% filter(year==2025)

a <- df_2026[1,"cases"] %>% pull() # Current week
b <- df_2025[52, "cases"] %>% pull() # Pre-week
c <- df_2025 %>% filter(week>49 & week <=52) %>% 
  summarise(n = mean(cases)) %>% 
  pull() # Mean of 4 pre-weeks

d <- df_2026 %>% filter(week<=1) %>% 
  summarise(n = sum(cases)) %>% 
  pull() #Cummulative cases until current week of 2025

e <- df_2025 %>% filter(week<=1) %>% 
  summarise(n = sum(cases)) %>% 
  pull() #Cummulative cases until current week of 2024

f <- out_f_sxh_2026$result$upperbound[[1]] # Farrington of current week
g <- df_sxh_2026[1, "cdc"] %>% pull() # CDC of current week
h <- remake(df_sxh, c(2016, 2017,2020, 2023, 2024))$seasonal # SXH
#h <- remake(df, c(2017, 2019, 2020, 2022, 2024))$seasonal # TCM
k <- out_c_sxh$result$upperbound[[1]] # CUSUM of current week

(a-b)/b # Current vs pre-week
(a-c)/c # Current vs mean of 4 pre-weeks
(a-h)/h # Current vs Seasonal
(a-k)/k # Current vs CUSUM
(a-f)/f # Current vs Farrington 
(a-g)/g # Current vs CDC
(d-e)/e 



##########################################
####### Vẽ biểu đồ ghép 3 khu vực ########
##########################################

#### Lưu ý 1: vẽ SXH thì chạy những object liên quan đến sxh và ngược lại cho TCM
#### Lưu ý 2: khi vẽ biểu đồ ghép 3KV thì qua file functions.R 
#           điền dấu # ở trước dòng code 139, và xóa dấu , dòng 138

sxh_hcm <- sxh_hcm + labs(tag = "KV1")
sxh_bd  <- sxh_bd  + labs(tag = "KV2")
sxh_vt  <- sxh_vt  + labs(tag = "KV3")

tcm_hcm <- tcm_hcm + labs(tag = "KV1")
tcm_bd  <- tcm_bd  + labs(tag = "KV2")
tcm_vt  <- tcm_vt  + labs(tag = "KV3")

final_plot <- 
  (tcm_hcm + tcm_bd + tcm_vt) +
  #(sxh_hcm + sxh_bd + sxh_vt) +
  plot_layout(guides = "collect", widths = c(1, 1, 1)) &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.tag = element_text(face = "bold", size = 14),
    plot.tag.position = "top" 
  )

final_plot <- final_plot + plot_annotation(
  #caption = "Năm lịch sử: 2016, 2017, 2018, 2020, 2023" # SXH
  caption = "Năm lịch sử: 2017, 2019, 2020, 2022, 2024" # TCM 
  # Chỉnh lại số năm lịch sử ở đây cho tương ứng với SXH/TCM
)

# Sửa tên file ảnh trong "" phù hợp với tên SXH/TCM
ggsave("3kv_tcm.jpeg", plot = final_plot, dpi = 1000, 
       height = 5, width = 12, bg = "white")
