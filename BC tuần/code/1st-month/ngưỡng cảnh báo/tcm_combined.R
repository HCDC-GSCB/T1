#TCM - 2025
df_tcm <- load_data("https://docs.google.com/spreadsheets/d/1ouVcS4B-sU07j4BT2VjHhxY13f2JUlqSeCY541vH6vA",
                    sheet = "TCM_3KV")

out_f_tcm_2025  <- run_algo(df_tcm , 
                       ref_years = c(2017, 2019, 2020, 2022, 2024, 2025), 
                       method = "farrington")

out_c_tcm_2025  <- run_algo(df_tcm , 
                       ref_years = c(2017, 2019, 2020, 2022, 2024, 2025), 
                       method = "cusum")



res_combined_2025 <- data.frame(
  week = 1:52,
  farrington_ub = as.numeric(out_f_tcm_2025$result$upperbound),
  outbreak_f    = as.numeric(out_f_tcm_2025$result$alarm),
  
  cusum_ub      = as.numeric(out_c_tcm_2025$result$upperbound),
  outbreak_c    = as.numeric(out_c_tcm_2025$result$alarm)
)

df_tcm_2025 <- out_f_tcm_2025$df %>%
  filter(year == 2025) %>%
  left_join(res_combined_2025, by = "week")
#TCM - 2026
out_f_tcm_2026  <- run_algo(df_tcm , 
                            ref_years = c(2017, 2019, 2020, 2022, 2024, 2026), 
                            method = "farrington")

out_c_tcm_2026  <- run_algo(df_tcm , 
                            ref_years = c(2017, 2019, 2020, 2022, 2024, 2026), 
                            method = "cusum")

res_combined_2026 <- data.frame(
  week = 1:52,
  farrington_ub = as.numeric(out_f_tcm_2026$result$upperbound),
  outbreak_f    = as.numeric(out_f_tcm_2026$result$alarm),
  
  cusum_ub      = as.numeric(out_c_tcm_2026$result$upperbound),
  outbreak_c    = as.numeric(out_c_tcm_2026$result$alarm)
)

df_tcm_2026 <- out_f_tcm_2026$df %>%
  filter(year == 2026) %>%
  left_join(res_combined_2026, by = "week")

df_tcm_combined <- extract_week(df_tail = df_tcm_2025, df_head = df_tcm_2026)
seasonal_val <- remake(df_tcm, c(2017, 2019, 2020, 2022, 2024))$seasonal

chart_tcm_combined <- plot_algo_combined(
  df_combined = df_tcm_combined,
  seasonal = seasonal_val,
  ref_years = c(2017, 2019, 2020, 2022, 2024),
  ten_benh = "Số ca tay chân miệng"

)

print(chart_tcm_combined)
ggsave("chart_tcm_combined.svg", plot = chart_tcm_combined, dpi = 300, height = 6, width = 9, bg = "white" )
