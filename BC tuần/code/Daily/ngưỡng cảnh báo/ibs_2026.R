######### SOT XUAT HUYET ##########

## SXH 3KV 
df_sxh <- load_data("https://docs.google.com/spreadsheets/d/1tkoFRYLPNrojiAFzdbpT2aZGkIjuaIn7EcDSRJcPPHY",
                    sheet = "SXH_3KV")

  out_f_sxh <- run_algo(df_sxh, 
                        ref_years = c(2016, 2017, 2020, 2023, 2024, 2026), 
                        method = "farrington")
  
  out_c_sxh <- run_algo(df_sxh, 
                        ref_years = c(2016, 2017, 2020, 2023, 2024, 2026), 
                        method = "cusum")
  
  sxh_3kv <- plot_algo_dual(out_f_sxh$df, out_f_sxh$result, out_c_sxh$result, 
                            year_target = 2026, week_target = 52, 
                            seasonal = remake(df_sxh, c(2016, 2017, 2020, 2023, 2024))$seasonal,
                            ref_years = c(2016, 2017, 2020, 2023, 2024),
                            disease_label = "Số ca Sốt xuất huyết")

ggsave("sxh_3kv.svg", plot = sxh_3kv, dpi = 300, height = 6, width = 9, bg = "white" )

######### TAY CHAN MIENG ##########

## TCM 3KV
df_tcm <- load_data("https://docs.google.com/spreadsheets/d/1ouVcS4B-sU07j4BT2VjHhxY13f2JUlqSeCY541vH6vA",
                sheet = "TCM_3KV")

out_f_tcm  <- run_algo(df_tcm , 
                  ref_years = c(2017, 2019, 2020, 2022, 2024, 2026), 
                  method = "farrington")

out_c_tcm  <- run_algo(df_tcm , 
                  ref_years = c(2017, 2019, 2020, 2022, 2024, 2026), 
                  method = "cusum")

tcm_3kv <- plot_algo_dual(out_f_tcm$df, out_f_tcm$result, out_c_tcm$result, 
                          year_target = 2026, week_target = 52,
                          seasonal = remake(df_tcm, c(2017, 2019, 2020, 2022, 2024))$seasonal,
                          ref_years = c(2017, 2019, 2020, 2022, 2024),
                          disease_label = "Số ca Tay chân miệng")

ggsave("tcm_3kv.svg", plot = tcm_3kv, dpi = 300, height = 6, width = 9, bg = "white" )

# PHỤ LỤC 1 #########
library(writexl)

# Hàm phụ trợ để thêm mũi tên dựa trên giá trị
format_pct <- function(val) {
  if (is.na(val)) return("-")
  icon <- ifelse(val > 0, "↑", ifelse(val < 0, "↓", ""))
  return(paste0(icon, abs(round(val, 1)), "%"))
}

summary_epidemic <- function(df, res_f, res_c, remake_obj, target_year, target_week) {
  
  # 1. Số ca trong tuần và cộng dồn
  current_cases <- df %>% filter(year == target_year, week == target_week) %>% pull(cases)
  cum_current <- df %>% filter(year == target_year, week <= target_week) %>% summarise(s = sum(cases, na.rm=T)) %>% pull(s)
  
  # 2. Số cùng kỳ năm trước
  cum_last_year <- df %>% filter(year == (target_year - 1), week <= target_week) %>% summarise(s = sum(cases, na.rm=T)) %>% pull(s)
  pct_vs_last_year <- (cum_current - cum_last_year) / cum_last_year * 100
  
  # 3. So với tuần trước
  last_week_val <- df %>% filter((year == target_year & week == target_week - 1) | 
                                   (year == target_year - 1 & target_week == 1 & week == 52)) %>% pull(cases)
  pct_vs_last_week <- (current_cases - last_week_val) / last_week_val * 100
  
  # 4. So với TB 4 tuần trước
  idx_target <- which(df$year == target_year & df$week == target_week)
  avg_4weeks <- mean(df$cases[(idx_target-4):(idx_target-1)], na.rm = TRUE)
  pct_vs_avg4w <- (current_cases - avg_4weeks) / avg_4weeks * 100
  
  # 5. So với các ngưỡng cảnh báo
  pct_vs_seasonal <- (current_cases - remake_obj$seasonal) / remake_obj$seasonal * 100
  is_cusum <- ifelse(res_c$alarm[target_week] == 1, "+", "-")
  
  far_limit <- res_f$upperbound[target_week]
  pct_vs_farrington <- (current_cases - far_limit) / far_limit * 100
  
  cdc_limit <- df %>% filter(year == target_year, week == target_week) %>% pull(cdc)
  pct_vs_cdc <- (current_cases - cdc_limit) / cdc_limit * 100
  
  # Trả về kết quả với mũi tên điều chỉnh
  res <- c(
    format(cum_current, big.mark="."),
    format_pct(pct_vs_last_year),
    format(current_cases, big.mark="."),
    format_pct(pct_vs_last_week),
    format_pct(pct_vs_avg4w),
    format_pct(pct_vs_seasonal),
    is_cusum,
    format_pct(pct_vs_farrington),
    format_pct(pct_vs_cdc)
  )
  return(res)
}


# Tính toán cho SXH và TCM
sxh_metrics <- summary_epidemic(out_f_sxh$df, out_f_sxh$result, out_c_sxh$result, 
                                remake(df_sxh, c(2016, 2017, 2020, 2023, 2024)), 2026, 4)

tcm_metrics <- summary_epidemic(out_f_tcm$df, out_f_tcm$result, out_c_tcm$result, 
                                remake(df_tcm, c(2017, 2019, 2020, 2022, 2024)), 2026, 4)

# Tạo bảng
summary_table <- data.frame(
  "Chi_tieu" = c(
    "Số ca cộng dồn đến tuần 4/2026",
    "- Số cùng kỳ năm trước",
    "Số ca trong tuần",
    "- So với tuần trước",
    "- So với TB 4 tuần trước",
    "So với ngưỡng mùa",
    "Vượt ngưỡng CUSUM",
    "So với ngưỡng Farrington",
    "So với ngưỡng Mean+2SD"
  ),
  "Sot_xuat_huyet" = sxh_metrics,
  "Tay_chan_mieng" = tcm_metrics,
  stringsAsFactors = FALSE
)

# Xuất ra file Excel
write_xlsx(summary_table, "PL1_T4_2026.xlsx")

# In kiểm tra
print(summary_table)
