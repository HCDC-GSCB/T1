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
                           seasonal = remake(df_sxh, c(2016, 2017,2020, 2023, 2024))$seasonal,
                           ref_years = c(2016, 2017,2020, 2023, 2024))

ggsave("sxh_3kv.svg", plot = sxh_3kv, dpi = 400, height = 6, width = 9, bg = "white" )

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

tcm_3kv <- plot_algo_dual(out_f_tcm $df, out_f_tcm $result, out_c_tcm $result, 
                         year_target = 2026, week_target = 52,
                         seasonal = remake(df_tcm , c(2017, 2019, 2020, 2022, 2024))$seasonal,
                         ref_years = c(2017, 2019, 2020, 2022, 2024))

ggsave("tcm_3kv.svg", plot = tcm_3kv, dpi = 400, height = 6, width = 9, bg = "white" )

# PHỤ LỤC 1 ##########
library(tidyverse)
library(writexl)

calc_report_metrics <- function(df_raw, out_f, out_c, hist_years, w_idx, y_curr) {
  
  # Helper format
  fmt_pct <- function(n) {
    if(length(n)==0 || is.na(n) || !is.numeric(n) || is.infinite(n)) return("-")
    paste0(ifelse(n>0, "↑", ifelse(n<0, "↓", "")), format(round(abs(n)*100, 1), decimal.mark=","), "%")
  }
  fmt_num <- function(n) format(round(n), big.mark=".", decimal.mark=",", scientific=FALSE)
  
  # --- BƯỚC 1: XỬ LÝ DỮ LIỆU THÔ CHO ĐỒNG BỘ VỚI MÔ HÌNH ---
  # (Gộp tuần 53 vào 52 giống hệt hàm remake)
  df_processed <- df_raw %>%
    mutate(week = ifelse(week == 53, 52, week)) %>%
    group_by(year, week) %>%
    summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
  
  # Lấy dữ liệu năm hiện tại và năm trước từ df_processed (đã xử lý)
  # Lưu ý: Không lấy từ out_f$df nữa để đảm bảo mọi con số đều từ 1 nguồn chuẩn
  df_curr <- df_processed %>% filter(year == y_curr)
  df_prev <- df_processed %>% filter(year == y_curr - 1)
  
  # --- BƯỚC 2: TÍNH CÁC CHỈ SỐ CƠ BẢN ---
  a <- df_curr %>% filter(week == w_idx) %>% pull(cases) 
  if(length(a) == 0) return(NULL) 
  
  b <- df_curr %>% filter(week == w_idx - 1) %>% pull(cases) 
  c_val <- df_curr %>% filter(week > (w_idx - 4) & week < w_idx) %>% summarize(m=mean(cases, na.rm=T)) %>% pull(m)
  d <- df_curr %>% filter(week <= w_idx) %>% summarize(s=sum(cases, na.rm=T)) %>% pull(s) 
  e <- df_prev %>% filter(week <= w_idx) %>% summarize(s=sum(cases, na.rm=T)) %>% pull(s) 
  
  # --- BƯỚC 3: LẤY NGƯỠNG TỪ KẾT QUẢ MÔ HÌNH ---
  # Riêng các ngưỡng này vẫn phải lấy từ output của function run_algo
  f <- out_f$result$upperbound[w_idx] # Farrington (Lưu ý: dùng ngoặc vuông đơn [] nếu là vector)
  # Kiểm tra lại cấu trúc upperbound, nếu là matrix thì dùng [w_idx, 1]
  if(is.matrix(out_f$result$upperbound)) f <- out_f$result$upperbound[w_idx, 1]
  
  k <- out_c$result$upperbound[w_idx] # CUSUM
  if(is.matrix(out_c$result$upperbound)) k <- out_c$result$upperbound[w_idx, 1]
  
  # Lấy ngưỡng CDC (Mean+2SD) từ out_f$df vì nó đã tính sẵn ở đó
  g <- out_f$df %>% filter(year == y_curr, week == w_idx) %>% pull(cdc) 
  
  # --- BƯỚC 4: TÍNH NGƯỠNG MÙA (TRUNG VỊ) ---
  # Sử dụng df_processed (đã gộp tuần 53->52) để tính toán
  required_years <- tibble(year = hist_years)
  
  actual_data <- df_processed %>%
    filter(year %in% hist_years, week == w_idx) %>%
    select(year, cases)
  
  h <- required_years %>%
    left_join(actual_data, by = "year") %>%
    mutate(cases = replace_na(cases, 0)) %>% 
    pull(cases) %>%
    median()
  
  # --- BƯỚC 5: TRẢ VỀ KẾT QUẢ ---
  list(
    cum_str  = fmt_num(d), diff_cum = fmt_pct((d - e) / e),
    week_str = fmt_num(a), diff_pre = fmt_pct((a - b) / b), diff_4wk = fmt_pct((a - c_val) / c_val),
    diff_sea = fmt_pct((a - h) / h), 
    cusum    = ifelse(!is.na(k) & a > k, "+", "-"),
    diff_far = fmt_pct((a - f) / f),
    diff_cdc = fmt_pct((a - g) / g)
  )
}

# ==============================================================================
# CHẠY LẠI PHẦN TẠO BẢNG
# ==============================================================================

# Cấu hình
RP_WEEK <- 1
RP_YEAR <- 2026

# Chạy SXH
res_sxh <- calc_report_metrics(
  df_raw     = df_sxh,        
  out_f      = out_f_sxh,     
  out_c      = out_c_sxh,     
  hist_years = c(2016, 2017,2020, 2023, 2024), 
  w_idx      = RP_WEEK, y_curr = RP_YEAR
)

# Chạy TCM
res_tcm <- calc_report_metrics(
  df_raw     = df_tcm,        
  out_f      = out_f_tcm,     
  out_c      = out_c_tcm,     
  hist_years = c(2017, 2019, 2020, 2022, 2024), 
  w_idx      = RP_WEEK, y_curr = RP_YEAR
)

# Tạo và xuất bảng
if(!is.null(res_sxh) && !is.null(res_tcm)) {
  final_df <- tibble(
    `Chỉ tiêu` = c(
      paste0("Số ca cộng dồn đến tuần ", RP_WEEK, " năm nay"), "   - So cùng kỳ năm trước",
      "Số ca trong tuần", "   - So với tuần trước", "   - So với TB 4 tuần trước",
      "So với ngưỡng cảnh báo dịch", "   - So với ngưỡng mùa", "   - Vượt ngưỡng CUSUM", 
      "   - So với ngưỡng Farrington", "   - So với ngưỡng Mean+2SD"
    ),
    `Sốt xuất huyết` = c(
      res_sxh$cum_str, res_sxh$diff_cum, res_sxh$week_str, res_sxh$diff_pre, res_sxh$diff_4wk,
      "", res_sxh$diff_sea, res_sxh$cusum, res_sxh$diff_far, res_sxh$diff_cdc
    ),
    `Tay chân miệng` = c(
      res_tcm$cum_str, res_tcm$diff_cum, res_tcm$week_str, res_tcm$diff_pre, res_tcm$diff_4wk,
      "", res_tcm$diff_sea, res_tcm$cusum, res_tcm$diff_far, res_tcm$diff_cdc
    )
  )
  print(final_df)
  write_xlsx(final_df, "ket_qua_bao_cao_fixed.xlsx")
}
# season SXH
df_2025 <- out_f_sxh$df %>% filter(year==2025)
a <- df_2025[52,"cases"] %>% pull() # Current week
h <- remake(df_sxh, c(2016, 2017, 2018, 2020, 2023))$seasonal # SXH
(a-h)/h # Current vs Seasonal

# season TCM
df_2025 <- out_f_tcm$df %>% filter(year==2025)
a <- df_2025[52,"cases"] %>% pull() # Current week
h <- remake(df_tcm, c(2017, 2019, 2020, 2022, 2024))$seasonal # TCM
(a-h)/h # Current vs Seasonal
