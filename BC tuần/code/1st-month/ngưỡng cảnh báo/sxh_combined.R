#Chạy dữ liệu
df_sxh <- load_data("https://docs.google.com/spreadsheets/d/1tkoFRYLPNrojiAFzdbpT2aZGkIjuaIn7EcDSRJcPPHY",
                    sheet = "SXH_3KV")

out_f_sxh <- run_algo(df_sxh, 
                      ref_years = c(2016, 2017, 2020, 2023, 2024, 2025), 
                      method = "farrington")

out_c_sxh <- run_algo(df_sxh, 
                      ref_years = c(2016, 2017, 2020, 2023, 2024, 2025), 
                      method = "cusum")
#Ghép dữ liệu
res_combined_2025 <- data.frame(
  week = 1:52,
  farrington_ub = as.numeric(out_f_sxh$result$upperbound),
  outbreak_f    = as.numeric(out_f_sxh$result$alarm),
  
  cusum_ub      = as.numeric(out_c_sxh$result$upperbound),
  outbreak_c    = as.numeric(out_c_sxh$result$alarm)
)

df_sxh_2025 <- out_f_sxh$df %>%
  filter(year == 2025) %>%
  left_join(res_combined_2025, by = "week")

#Lưu kết quả chạy cho năm 2025
saveRDS(df_sxh_2025, "df_sxh_2025.RDS")

#Đọc kết quả chạy của năm 2025
df_sxh_2025 <- readRDS("df_sxh_2025.RDS")

out_f_sxh_2026 <- run_algo(df_sxh, 
                      ref_years = c(2016, 2017, 2020, 2023, 2024, 2026), 
                      method = "farrington")

out_c_sxh_2026 <- run_algo(df_sxh, 
                      ref_years = c(2016, 2017, 2020, 2023, 2024, 2026), 
                      method = "cusum")

sxh_res_combined_2026 <- data.frame(
  week = 1:52,
  farrington_ub = as.numeric(out_f_sxh_2026$result$upperbound),
  outbreak_f    = as.numeric(out_f_sxh_2026$result$alarm),
  
  cusum_ub      = as.numeric(out_c_sxh_2026$result$upperbound),
  outbreak_c    = as.numeric(out_c_sxh_2026$result$alarm)
)

df_sxh_2026 <- out_f_sxh$df %>%
  filter(year == 2026) %>%
  left_join(sxh_res_combined_2026, by = "week")

df_sxh_2025_2026 <- bind_rows(df_sxh_2025, df_sxh_2026)


df_sxh_combined <- extract_week(df_tail = df_sxh_2025, df_head = df_sxh_2026)
seasonal_val <- remake(df_sxh, c(2016, 2017, 2020, 2023, 2024))$seasonal

chart_sxh_combined <- plot_algo_combined(
  df_combined = df_sxh_combined,
  seasonal = seasonal_val,
  ref_years = c(2016, 2017, 2020, 2023, 2024),
  ten_benh = "Số ca sốt xuất huyết"
)

print(chart_sxh_combined)
ggsave("chart_sxh_combined.svg", plot = chart_sxh_combined, dpi = 300, height = 6, width = 9, bg = "white" )
