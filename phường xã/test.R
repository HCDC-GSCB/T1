pacman::p_load(tidyverse, readxl, surveillance, stringi, gt, 
               cowplot, scales, dplyr, ggplot2, sf, gtsummary,
               googlesheets4, googledrive, cartogram)
# PHẦN 1: SETUP & LOAD DATA 
NAM_BAO_CAO  <- 2026
TUAN_BAO_CAO <- 5

YEARS_TCM <- c( 2017, 2019, 2020, 2022, 2024, NAM_BAO_CAO) 
YEARS_SXH <- c( 2016, 2017,2020, 2023, 2024, NAM_BAO_CAO)

standardize_name_func <- function(x) {
  x %>% iconv(from="UTF-8", to="UTF-8", sub="") %>% 
    stringi::stri_trans_nfkc() %>% str_to_lower() %>% 
    str_replace_all("\\s+", " ") %>% str_squish()
}

load_data <- function(url, sheet) {
  gs4_deauth()
  read_sheet(url, sheet = sheet)
}

df_danso <- load_data("https://docs.google.com/spreadsheets/d/1ZlfExROncZcCpm8LzGlaw8wim9T_0eNNhpAsyB836kA/edit?gid=438665137#gid=438665137", sheet = "Sheet1") %>% 
  rename(Phuong = ward, DanSo = danso) %>% 
  mutate(phuong_clean = standardize_name_func(Phuong))

# PHẦN 2: LIỆU FULL 52 TUẦN
# --- Function chạy thuật toán (Không đổi) ---
run_algo_core <- function(d_ward, t_years) {
  if(nrow(d_ward) != t_years * 52) return(NULL) 
  
  stsObj <- sts(d_ward$cases, start = c(1, 1), frequency = 52)
  disP   <- sts2disProg(stsObj)
  start_idx <- (t_years - 1) * 52 + 1
  range_idx <- start_idx:(t_years * 52) 
  
  r_far <- tryCatch(algo.farrington(disP, control = list(range=range_idx, b=5, w=1, reweight=TRUE, verbose=FALSE)), error = function(e) NULL)
  r_cus <- tryCatch(algo.cusum(disP, control = list(range=range_idx, k=1.04, h=2.26)), error = function(e) NULL)
  
  tibble(
    ward_clean = unique(d_ward$ward_clean),
    Tuan       = 1:52,                    
    SoCa       = d_ward$cases[range_idx], 
    Flag_Far   = if(!is.null(r_far)) as.vector(r_far$alarm) else FALSE,
    Flag_Cus   = if(!is.null(r_cus)) as.vector(r_cus$alarm) else FALSE
  )
}

# --- Function xử lý FULL YEAR 
process_full_year_data <- function(df_raw, df_pop, target_years_list) {
  
  # 1. Chuẩn hóa & Lọc năm
  df_input <- df_raw %>%
    filter(year %in% target_years_list) %>% 
    mutate(ward_clean = standardize_name_func(ward)) %>% 
    arrange(ward_clean, year, week)
  
  total_years <- length(unique(df_input$year))
  
  # 2. Chạy thuật toán (Trả về 52 tuần cho mỗi phường)
  df_algo_results <- df_input %>%
    pull(ward_clean) %>% unique() %>%
    map_dfr(~run_algo_core(filter(df_input, ward_clean == .x), total_years))
  
  # 3. Tính toán chỉ số & Logic Hotspot (Tính cho TOÀN BỘ 52 tuần)
  df_processed <- df_algo_results %>%
    left_join(df_pop, by = c("ward_clean" = "phuong_clean")) %>%
    arrange(ward_clean, Tuan) %>% 
    group_by(ward_clean) %>%
    mutate(
      TyLe = (SoCa / DanSo) * 100000,
      TichLuy = cumsum(SoCa),
      
      SoSanh_Tuan = case_when(
        lag(SoCa) == 0 & SoCa == 0 ~ 0,
        lag(SoCa) == 0 & SoCa > 0  ~ NA_real_,
        TRUE ~ (SoCa - lag(SoCa))/lag(SoCa)
      ),
      
      TB_4T = (lag(SoCa,1) + lag(SoCa,2) + lag(SoCa,3) + lag(SoCa,4)) / 4,
      SoSanh_4T = case_when(
        TB_4T == 0 & SoCa == 0 ~ 0,
        TB_4T == 0 & SoCa > 0  ~ NA_real_,
        TRUE ~ (SoCa - TB_4T)/TB_4T
      ),
      
      # Logic 3 tuần: Tính sẵn ở đây cho tất cả các tuần
      Is_Hotspot_Far = (Flag_Far & lag(Flag_Far, 1) & lag(Flag_Far, 2)),
      Is_Hotspot_Cus = (Flag_Cus & lag(Flag_Cus, 1) & lag(Flag_Cus, 2))
    ) %>%
    ungroup()
  
  # TRẢ VỀ DATA FULL (Không filter)
  return(df_processed)
}

# PHẦN 3:  BÁO CÁO
render_gt_report <- function(df_full_year, disease_name, report_week, filename) {
  
  # BƯỚC QUAN TRỌNG: Lọc tuần báo cáo tại đây
  df_view <- df_full_year %>%
    filter(Tuan == report_week) %>%  # <--- CHỈ LẤY TUẦN BÁO CÁO
    mutate(
      F_Display = ifelse(Is_Hotspot_Far, "(+)", "(-)"),
      C_Display = ifelse(Is_Hotspot_Cus, "(+)", "(-)")
    ) %>%
    arrange(desc(Is_Hotspot_Far & Is_Hotspot_Cus), desc(Is_Hotspot_Far), desc(SoCa)) %>%
    select(Phuong, SoCa, TyLe, TichLuy, SoSanh_Tuan, SoSanh_4T, F_Display, C_Display)
  
  # Tạo bảng GT
  tbl <- df_view %>% 
    gt() %>%
    tab_spanner(label = paste("Số liệu", disease_name), columns = c(SoCa, TyLe, TichLuy)) %>%
    tab_spanner(label = "So sánh", columns = c(SoSanh_Tuan, SoSanh_4T)) %>%
    tab_spanner(label = "Cảnh báo 3 tuần", columns = c(F_Display, C_Display)) %>%
    cols_label(Phuong="Phường/Xã", SoCa=paste("Tuần", report_week), TyLe="Tỷ lệ/100k",
               SoSanh_Tuan="vs Tuần trước", SoSanh_4T="vs TB 4 tuần", F_Display="Farrington", C_Display="CUSUM") %>%
    fmt_number(TyLe, decimals = 2) %>%
    sub_missing(columns = contains("SoSanh"), missing_text = "-") %>%
    fmt_percent(contains("SoSanh"), decimals = 1, force_sign = TRUE) %>%
    cols_align("center", -Phuong) %>%
    
    # Tô màu đỏ đậm cho (+)
    tab_style(style = cell_text(color = "red", weight = "bold"), 
              locations = cells_body(columns = c(F_Display, C_Display), rows = (F_Display == "(+)" | C_Display == "(+)"))) %>%
    # Tô màu xám cho (-)
    tab_style(style = cell_text(color = "gray"), 
              locations = cells_body(columns = c(F_Display, C_Display), rows = (F_Display == "(-)" | C_Display == "(-)")))
  
  gtsave(tbl, filename)
  return(tbl)
}

# XUẤT BÁO CÁO #######
# --- 1. TCM ---
# Nhập data
df_tcm_raw <- load_data("https://docs.google.com/spreadsheets/d/1H8E1Ou7HplqMPS09-ctHmtHM-1e18tUPFO0FTDguons/edit?gid=1402026268#gid=1402026268", sheet = "all")
# Xuất dữ liệu đầy đủ
df_final_tcm <- process_full_year_data(df_tcm_raw, df_danso, YEARS_TCM) 
# Tạo bảng
gt_tcm <- render_gt_report(df_final_tcm, "TCM", TUAN_BAO_CAO, paste0("BaoCao_TCM_Tuan_", TUAN_BAO_CAO, ".docx"))
print(gt_tcm)

# --- 2. SXH ---
# Nhập data
df_sxh_raw <- load_data("https://docs.google.com/spreadsheets/d/1Qg5zNehb86sRHaDWRdVrQPz_s9R0g0GbEV9lOajSBkw/edit?usp=sharing", sheet = "all")
# Xuất dữ liệu đầy đủ
df_final_sxh <- process_full_year_data(df_sxh_raw, df_danso, YEARS_SXH)
# Tạo bảng
gt_sxh <- render_gt_report(df_final_sxh, "SXH", TUAN_BAO_CAO, paste0("BaoCao_SXH_Tuan_", TUAN_BAO_CAO, ".docx"))
print(gt_sxh)

# MA TRẬN ===========================================
export_matrix <- function(df_full, disease_name) {
  
  # 1. TÍNH TOÁN & SẮP XẾP
  # Tính tổng ca để xếp hạng phường
  ward_rank <- df_full %>%
    group_by(Phuong) %>%
    summarise(Total = sum(SoCa, na.rm = TRUE)) %>%
    arrange(desc(Total)) %>% 
    pull(Phuong)
  
  # --- QUAN TRỌNG: Lấy giá trị cao nhất của TOÀN BỘ dữ liệu ---
  # Việc này giúp thang màu cố định từ 0 đến max_val cho tất cả các trang
  max_val <- max(df_full$SoCa, na.rm = TRUE)
  
  # 2. CHIA TRANG (43 phường/trang)
  chunks <- split(ward_rank, ceiling(seq_along(ward_rank) / 43))
  
  # 3. VẼ & XUẤT FILE
  iwalk(chunks, function(wards_batch, page_idx) {
    
    # Lọc data cho trang hiện tại
    df_plot <- df_full %>% 
      filter(Phuong %in% wards_batch) %>%
      mutate(Phuong = factor(Phuong, levels = rev(wards_batch)))
    
    # Vẽ
    p <- ggplot(df_plot, aes(x = Tuan, y = Phuong, fill = SoCa)) +
      geom_tile(color = "white", linewidth = 0.05) +
      
      # --- SỬA ĐỔI TẠI ĐÂY: Thêm limits ---
      scale_fill_distiller(
        palette = "Spectral", 
        direction = -1, 
        na.value = "grey98", 
        name = "Số ca",
        limits = c(0, max_val) # Cố định thang màu từ 0 đến max toàn thành phố
      ) +
      
      scale_x_continuous(expand = c(0,0), breaks = seq(5, 52, 5), position = "top") +
      labs(
        title = paste0("BẢN ĐỒ NHIỆT ", disease_name, " - TOÀN THÀNH PHỐ (Trang ", page_idx, ")"),
        subtitle = "Sắp xếp: Tổng số ca tích lũy từ Cao đến Thấp",
        x = "Tuần", y = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        panel.grid = element_blank()
      )
    
    # Lưu file
    ggsave(paste0("Ranking_", disease_name, "_Page", page_idx, ".svg"), 
           p, width = 8, height = 11, dpi = 400)
  })
}

# Xuất matrix
export_matrix(df_final_tcm, "TCM")
export_matrix(df_final_sxh, "SXH")


# BẢN ĐỒ  =======================================================================
shp_path <- "TPHCM_XA_2025_JUL_AP" 
hcm_map <- st_read(shp_path, quiet = TRUE) 

hcm_map_clean <- hcm_map %>%
  mutate(
    ward_clean_map = standardize_name_func(tenXa) # Giả định hàm này đã có sẵn
  ) %>%
  # FIX LỖI TÊN XÃ/PHƯỜNG (Bám sát code gốc)
  mutate(
    ward_clean_map = case_when(
      str_detect(ward_clean_map, "thanh an") ~ "thanh an",
      str_detect(ward_clean_map, "long hoa") ~ "long hoa", 
      str_detect(ward_clean_map, "phuoc hoa") ~ "phuoc hoa",
      TRUE ~ ward_clean_map 
    )
  )
# --- BƯỚC 2: KHAI BÁO FUNCTION VẼ BẢN ĐỒ ---

# --- FUNCTION VẼ BẢN ĐỒ ĐÃ FIX LỖI MÀU CÔN ĐẢO ---

ve_ban_do_dich <- function(df_input, TUAN_BAO_CAO, ten_benh_title, ten_file_xuat) {
  
  # 1. CHUẨN BỊ DỮ LIỆU
  data_map_source <- df_input %>%
    filter(Tuan == TUAN_BAO_CAO) %>%
    select(ward_clean, SoCa, Phuong, STT)
  
  # 2. GHÉP DỮ LIỆU
  map_final <- hcm_map_clean %>%
    left_join(data_map_source, by = c("ward_clean_map" = "ward_clean")) %>%
    mutate(SoCa = replace_na(SoCa, 0)) # Điền 0 vào chỗ trống
  
  # --- [MỚI] TÍNH MAX SO CA TOÀN THÀNH PHỐ ĐỂ ĐỒNG BỘ MÀU ---
  max_case <- max(map_final$SoCa, na.rm = TRUE)
  # Nếu toàn thành phố là 0 ca thì gán tạm max là 1 để tránh lỗi scale, ngược lại giữ nguyên
  max_case_limit <- ifelse(max_case == 0, 1, max_case) 
  
  # 3. TÁCH BẢN ĐỒ
  map_main <- map_final %>% filter(ward_clean_map != "đặc khu côn đảo")
  map_condao <- map_final %>% filter(ward_clean_map == "đặc khu côn đảo")
  
  # 4. VẼ BẢN ĐỒ CHÍNH
  p_main <- ggplot(map_main) +
    geom_sf(aes(fill = SoCa), color = "grey30", size = 0.05) +
    geom_sf_text(aes(label = STT), size = 2.5, color = "black", check_overlap = TRUE) +
    # [FIX]: Thêm limits để cố định thang màu
    scale_fill_distiller(palette = "Spectral", direction = -1, name = "Số ca", 
                         limits = c(0, max_case_limit)) + 
    labs(
      title = paste("BẢN ĐỒ", ten_benh_title, "TP.HCM - TUẦN", TUAN_BAO_CAO),
      subtitle = "Đơn vị: Xã/Phường"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # 5. VẼ BẢN ĐỒ PHỤ (CÔN ĐẢO)
  if(nrow(map_condao) > 0) {
    p_inset <- ggplot(map_condao) +
      geom_sf(aes(fill = SoCa), color = "grey30", size = 0.05) +
      geom_sf_text(aes(label = STT), size = 3, color = "black") +
      # [FIX]: Thêm limits y hệt bản đồ chính
      scale_fill_distiller(palette = "Spectral", direction = -1, guide = "none",
                           limits = c(0, max_case_limit)) + 
      labs(title = "Côn Đảo") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      )
    
    # Ghép 2 bản đồ
    final_map_plot <- ggdraw() +
      draw_plot(p_main) +
      draw_plot(p_inset, x = 0.65, y = 0.55, width = 0.1, height = 0.1)
  } else {
    final_map_plot <- p_main
  }
  
  # 6. XUẤT FILE
  full_file_name <- paste0(ten_file_xuat, "_Tuan", TUAN_BAO_CAO, ".svg")
  print(paste("Đang xuất file:", full_file_name))
  ggsave(full_file_name, final_map_plot, width = 14, height = 8, dpi = 500)
  
  return(final_map_plot)
}


# XUẤT BẢN ĐỒ
# 1. Vẽ cho Tay Chân Miệng
ve_ban_do_dich(
  df_input = df_final_tcm, 
  TUAN_BAO_CAO = TUAN_BAO_CAO, 
  ten_benh_title = "TAY CHÂN MIỆNG", 
  ten_file_xuat = "BanDo_TCM"
)

# 2. Vẽ cho Sốt Xuất Huyết
ve_ban_do_dich(
  df_input = df_final_sxh, 
  TUAN_BAO_CAO = TUAN_BAO_CAO, 
  ten_benh_title = "SỐT XUẤT HUYẾT", 
  ten_file_xuat = "BanDo_SXH"
)

# Catogram =============
ve_ban_do_cartogram <- function(df_input, TUAN_BAO_CAO, ten_benh_title, ten_file_xuat) {
  
  message("--- BẮT ĐẦU XỬ LÝ CARTOGRAM CHO TUẦN ", TUAN_BAO_CAO, " ---")
  
  # 1. CHUẨN BỊ DỮ LIỆU
  data_map_source <- df_input %>%
    filter(Tuan == TUAN_BAO_CAO) %>%
    select(ward_clean, SoCa, STT)
  
  # Ghép dữ liệu
  map_final <- hcm_map_clean %>%
    left_join(data_map_source, by = c("ward_clean_map" = "ward_clean")) %>%
    mutate(SoCa = replace_na(SoCa, 0))
  
  # Tính Max Scale (để đồng bộ màu giữa 2 bản đồ)
  max_case <- max(map_final$SoCa, na.rm = TRUE)
  max_case_limit <- ifelse(max_case == 0, 1, max_case)
  
  # 2. TÁCH DỮ LIỆU & CHUYỂN HỆ TỌA ĐỘ (BẮT BUỘC CHO CARTOGRAM)
  # Chuyển sang UTM Zone 48N (code 32648) để tính diện tích đúng
  map_main <- map_final %>% 
    filter(ward_clean_map != "đặc khu côn đảo") %>%
    st_transform(crs = 32648)
  
  map_condao <- map_final %>% 
    filter(ward_clean_map == "đặc khu côn đảo") 
  # Côn Đảo giữ nguyên hoặc chuyển CRS tuỳ ý, ở đây giữ nguyên để vẽ inset đơn giản
  
  # 3. TÍNH TOÁN BIẾN HÌNH (CHỈ ÁP DỤNG CHO ĐẤT LIỀN)
  # Xử lý số ca = 0 thành 0.1 để thuật toán chạy được
  map_main_ready <- map_main %>%
    mutate(SoCa_Carto = ifelse(SoCa <= 0, 0.1, SoCa))
  
  message("Đang biến hình bản đồ đất liền... (Bước này tốn tài nguyên)")
  # itermax = 5 để chạy nhanh, muốn đẹp hơn tăng lên 10-15
  hcm_cartogram <- cartogram_cont(map_main_ready, "SoCa_Carto", itermax = 5)
  
  # 4. VẼ BẢN ĐỒ CHÍNH (CARTOGRAM)
  p_carto_main <- ggplot(hcm_cartogram) +
    geom_sf(aes(fill = SoCa), color = "white", size = 0.1) +
    geom_sf_text(aes(label = STT), size = 2.5, color = "black", check_overlap = TRUE) + 
    scale_fill_distiller(palette = "Spectral", direction = -1, 
                         limits = c(0, max_case_limit), 
                         name = "Số ca") +
    labs(
      title = paste("CARTOGRAM", ten_benh_title, "TP.HCM - TUẦN", TUAN_BAO_CAO),
      subtitle = "Diện tích phường tỷ lệ thuận với Số ca mắc"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # 5. VẼ BẢN ĐỒ PHỤ CÔN ĐẢO (GIỮ NGUYÊN HÌNH DẠNG)
  # Lưu ý: Côn Đảo đứng một mình nên không chạy cartogram được, ta vẽ thường nhưng ốp cùng scale màu
  if(nrow(map_condao) > 0) {
    p_inset <- ggplot(map_condao) +
      geom_sf(aes(fill = SoCa), color = "grey30", size = 0.1) +
      geom_sf_text(aes(label = STT), size = 3, color = "black") +
      scale_fill_distiller(palette = "Spectral", direction = -1, guide = "none",
                           limits = c(0, max_case_limit)) +
      labs(title = "Côn Đảo") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      )
    
    # Ghép map
    final_plot <- ggdraw() +
      draw_plot(p_carto_main) +
      draw_plot(p_inset, x = 0.65, y = 0.55, width = 0.1, height = 0.1)
  } else {
    final_plot <- p_carto_main
  }
  
  # 6. XUẤT FILE
  file_name <- paste0(ten_file_xuat, "_Cartogram_Tuan", TUAN_BAO_CAO, ".svg")
  message("Đang lưu file: ", file_name)
  ggsave(file_name, final_plot, width = 14, height = 8, dpi = 500)
  
  return(final_plot)
}

# Vẽ Cartogram 
ve_ban_do_cartogram(
  df_input = df_final_sxh,
  TUAN_BAO_CAO = TUAN_BAO_CAO, 
  ten_benh_title = "SỐT XUẤT HUYẾT",
  ten_file_xuat = "BanDo_SXH"
)

ve_ban_do_cartogram(
  df_input = df_final_tcm,
  TUAN_BAO_CAO = TUAN_BAO_CAO, 
  ten_benh_title = "TAY CHÂN MIỆNG",
  ten_file_xuat = "BanDo_TCM"
)
