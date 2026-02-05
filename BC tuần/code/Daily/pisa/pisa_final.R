library(tidyverse)
library(readxl)
library(scales)
library(surveillance)
library(googlesheets4)
library(googledrive)
library(patchwork)
# FUNCTIONS ######

# --- Hàm phụ trợ: Dịch chuyển vòng tròn (cho biểu đồ độ nặng) ---
circular_shift <- function(x, shift) {
  n <- length(x)
  if (shift == 0) return(x)
  if (shift > 0) return(c(tail(x, shift), head(x, n - shift)))
  return(c(tail(x, n + shift), head(x, -shift)))
}

# --- Hàm 1: Tính ngưỡng Độ lây truyền (Transmission Thresholds) ---
calc_transmission_thresholds <- function(df, hist_years) {
  # Chuyển dữ liệu lịch sử sang dạng dài
  df_hist <- df %>%
    select(all_of(hist_years)) %>% 
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Cases") %>%
    mutate(Cases = replace_na(Cases, 0))
  
  # Tính các chỉ số thống kê
  seasonal_median <- median(df_hist$Cases)
  
  stats_peak <- df_hist %>%
    group_by(Year) %>%
    summarise(Peak = max(Cases), .groups = "drop") %>%
    summarise(Mean_Peak = mean(Peak), SD_Peak = sd(Peak))
  
  # Trả về danh sách các giá trị ngưỡng
  list(
    Seasonal = seasonal_median,
    Moderate = stats_peak$Mean_Peak + (stats_peak$SD_Peak * 0.53),
    High     = stats_peak$Mean_Peak + (stats_peak$SD_Peak * 1.65),
    Extra    = stats_peak$Mean_Peak + (stats_peak$SD_Peak * 2.24)
  )
}

# --- Hàm 2: Vẽ biểu đồ Độ lây truyền (Đã sửa đổi) ---
plot_transmission <- function(df_curr, thresholds, current_year_col, disease_name = "") {
  
  # Tạo nhãn cho trục Y và Legend dựa trên tên bệnh
  y_label_text <- paste("Số ca bệnh", disease_name)
  legend_label_text <- paste("Số mắc mới", disease_name)
  
  # Chuẩn bị dữ liệu năm hiện tại
  df_plot <- df_curr %>%
    select(Week, Value = all_of(current_year_col)) %>%
    mutate(Value = replace_na(Value, 0))
  
  # Xác định giới hạn trục Y
  y_max <- max(max(df_plot$Value, na.rm = TRUE), thresholds$Extra) * 1.2
  
  # Vẽ biểu đồ
  p <- ggplot() +
    # Lớp nền (Thresholds)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = thresholds$Seasonal, fill = "#00a049", alpha = 0.6) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = thresholds$Seasonal, ymax = thresholds$Moderate, fill = "#fddc10", alpha = 0.6) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = thresholds$Moderate, ymax = thresholds$High, fill = "#FF8C00", alpha = 0.6) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = thresholds$High, ymax = thresholds$Extra, fill = "#FF0001", alpha = 0.6) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = thresholds$Extra, ymax = y_max, fill = "#9932CC", alpha = 0.6) +
    
    # Lớp dữ liệu hiện tại (Sử dụng biến legend_label_text)
    geom_col(data = df_plot, aes(x = Week, y = Value, fill = legend_label_text), color = "black", alpha = 1, width = 1) +
    
    # Nhãn ngưỡng
    annotate("text", x = 54, y = thresholds$Seasonal/2, label = "Dưới\nngưỡng", color = "#006409", fontface = "bold", hjust = 0, size=6) +
    annotate("text", x = 54, y = (thresholds$Seasonal + thresholds$Moderate)/2, label = "Thấp", color = "#8B8009", fontface = "bold", hjust = 0, size=6) +
    annotate("text", x = 54, y = (thresholds$Moderate + thresholds$High)/2, label = "Vừa", color = "#CC5509", fontface = "bold", hjust = 0, size=6) +
    annotate("text", x = 54, y = (thresholds$High + thresholds$Extra)/2, label = "Cao", color = "red", fontface = "bold", hjust = 0, size=6) +
    annotate("text", x = 54, y = (thresholds$Extra + y_max)/2, label = "Rất cao", color = "#9932a9", fontface = "bold", hjust = 0, size=6) +
    
    # Trang trí
    scale_x_continuous(breaks = seq(1, 52, by = 4), expand = c(0, 0.4), limits = c(0, 58)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, y_max)) +
    # Sử dụng setNames để gán màu cho tên legend mới
    scale_fill_manual(name = "", values = setNames("#eaf5f8", legend_label_text)) +
    labs(x = "Tuần", y = y_label_text) + # Sử dụng biến y_label_text
    theme_classic(base_size = 20, base_family = "serif") +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black", size=20),
      legend.position = "bottom",
      legend.text = element_text(size = 20)
    )
  
  return(p)
}

# --- Hàm 3: Xử lý dữ liệu Nội trú  ---
process_inpatient_data <- function(df_inpatient, hist_years, current_year_col) {
  # 1. Xử lý lịch sử (Alignment)
  df_hist <- df_inpatient %>% 
    select(Week, all_of(hist_years)) %>% 
    mutate(across(all_of(hist_years), ~replace_na(., 0)))
  
  peak_info <- df_hist %>%
    pivot_longer(cols = all_of(hist_years), names_to = "Year", values_to = "Cases") %>%
    group_by(Year) %>% slice_max(Cases, n = 1, with_ties = FALSE) %>% ungroup()
  
  ref_peak <- peak_info %>% slice_max(Cases, n = 1, with_ties = FALSE)
  target_week <- ref_peak$Week
  
  df_aligned <- df_hist %>% select(Week)
  for (year in hist_years) {
    curr_peak <- peak_info$Week[peak_info$Year == year]
    df_aligned[[year]] <- circular_shift(df_hist[[year]], target_week - curr_peak)
  }
  
  # 2. Tính ngưỡng trung bình & SD
  df_final <- df_aligned %>%
    rowwise() %>%
    mutate(
      Mean = mean(c_across(all_of(hist_years))),
      SD = sd(c_across(all_of(hist_years))),
      Limit_Low = Mean,
      Limit_Mod = Mean + SD,
      Limit_High = Mean + 3*SD
    ) %>% ungroup()
  
  # 3. Ghép dữ liệu hiện tại (Chỉ nội trú)
  df_curr_inpatient <- df_inpatient %>% 
    select(Week, Inpatient = all_of(current_year_col)) %>% 
    replace_na(list(Inpatient = 0))
  
  df_final <- df_final %>% 
    left_join(df_curr_inpatient, by = "Week")
  
  return(df_final)
}
# --- Hàm 4: Vẽ biểu đồ Nội trú (Đã tối ưu dynamic legend) ---
plot_inpatient <- function(df_processed, disease_name = "", y_lab = "Số ca nội trú") {
  
  last_row <- tail(df_processed, 1)
  
  # Tạo nhãn legend động
  # VD: Nếu disease_name = "Sốt xuất huyết" -> "Số ca Sốt xuất huyết nội trú"
  legend_label <- if(disease_name != "") paste("Số ca", disease_name, "nội trú") else "Số ca nội trú"
  
  # Tạo vector màu động khớp với nhãn
  # Đây là trick để ggplot hiểu đúng mapping màu khi tên thay đổi
  fill_colors <- setNames("#ADD8E6", legend_label)
  
  # Tính toán y_max
  y_max <- max(c(df_processed$Limit_High, df_processed$Inpatient), na.rm = TRUE) * 1.15
  
  ggplot(df_processed, aes(x = Week)) +
    # Cột nội trú: Sử dụng legend_label thay vì string cứng
    geom_col(aes(y = Inpatient, fill = legend_label), color = "black", alpha = 0.4, width = 1) +
    
    # Các đường ngưỡng
    geom_line(aes(y = Limit_Low, color = "Trung bình"), linewidth = 1) +
    geom_line(aes(y = Limit_Mod, color = "Trung bình + 1SD"), linewidth = 1) +
    geom_line(aes(y = Limit_High, color = "Trung bình + 3SD"), linewidth = 1) +
    
    # Nhãn ngưỡng (Giữ nguyên)
    annotate("text", x = 53, y = last_row$Limit_Low/2, label = "Thấp", color = "#fddc10", fontface = "bold", hjust = 0) +
    annotate("text", x = 53, y = (last_row$Limit_Low + last_row$Limit_Mod)/2, label = "Vừa", color = "#FF8C00", fontface = "bold", hjust = 0) +
    annotate("text", x = 53, y = (last_row$Limit_Mod + last_row$Limit_High)/2, label = "Cao", color = "red", fontface = "bold", hjust = 0) +
    annotate("text", x = 53, y = last_row$Limit_High * 1.05, label = "Rất cao", color = "#800080", fontface = "bold", hjust = 0) +
    
    # Scales & Theme
    scale_y_continuous(
      name = y_lab, 
      expand = c(0, 0), 
      limits = c(0, y_max)
    ) +
    scale_x_continuous(breaks = seq(1, 52, by = 4), expand = expansion(mult = c(0.02, 0.15))) +
    
    # Truyền vector màu động vào đây
    scale_fill_manual(name = "", values = fill_colors) +
    
    scale_color_manual(name = "", 
                       values = c("Trung bình"="#FF8C00", 
                                  "Trung bình + 1SD"="red", 
                                  "Trung bình + 3SD"="#800080")) +
    labs(x = "Tuần") +
    theme_classic(base_size = 20, base_family = "serif") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.text = element_text(size = 20)
    )
}


load_data <- function(url, sheet) {
  gs4_deauth()
  df <- read_sheet(url, sheet = sheet)
  return(df)
}
# RUN TCM #########

# --- CẤU HÌNH CHUNG ---
file_path_tcm <- "https://docs.google.com/spreadsheets/d/1jSAuGFUkcHBL5iie999qn7ClW2dTYB3AOSOaRwOBCGY/edit?gid=552422139#gid=552422139"
years_hist_tcm <- c("2024", "2022", "2020", "2019", "2017")
curr_year_col_tcm <- "2026"

# 1. BIỂU ĐỒ ĐỘ LÂY TRUYỀN 
df_ca_tcm <- load_data(url = file_path_tcm, sheet = "ca")
thresholds_trans_tcm <- calc_transmission_thresholds(df_ca_tcm, years_hist_tcm)
p_trans_tcm <- plot_transmission(df_curr = df_ca_tcm, 
                             thresholds = thresholds_trans_tcm, 
                             current_year_col = curr_year_col_tcm,
                             disease_name = "tay chân miệng")
print(p_trans_tcm)
ggsave("hfmd-t-tp.svg", p_trans_tcm, width = 14, height = 7, dpi = 300)

# 2. BIỂU ĐỒ NỘI TRÚ 

df_noi_tcm <- load_data(file_path_tcm, sheet = "noi")
df_inpatient_ready_tcm <- process_inpatient_data(df_inpatient = df_noi_tcm, 
                                             hist_years = years_hist_tcm, 
                                             current_year_col = curr_year_col_tcm)


p_inp_tcm <- plot_inpatient(df_inpatient_ready_tcm, disease_name = "tay chân miệng")

print(p_inp_tcm)
ggsave("hfmd-s-tp.svg", p_inp_tcm, width = 14, height = 7, dpi = 300)

# RUN SXH #########
# --- CẤU HÌNH CHUNG ---
file_path_sxh <- "https://docs.google.com/spreadsheets/d/13_o7NAlfBjckO6PspzITbWhlkWbfp4eKkqYCa7Dvvgg/edit?usp=sharing"
years_hist_sxh <- c("2024", "2023", "2020", "2017", "2016")
curr_year_col_sxh <- "2026"

# 1. BIỂU ĐỒ ĐỘ LÂY TRUYỀN
df_ca_sxh <- load_data(file_path_sxh, sheet = "ca")
thresholds_trans_sxh <- calc_transmission_thresholds(df_ca_sxh, years_hist_sxh)
p_trans_sxh <- plot_transmission(df_curr = df_ca_sxh, 
                             thresholds = thresholds_trans_sxh, 
                             current_year_col = curr_year_col_sxh,
                             disease_name = "sốt xuất huyết")
print(p_trans_sxh)
ggsave("dengue-t-tp.svg", p_trans_sxh, width = 14, height = 7, dpi = 300)

# 2. BIỂU ĐỒ NỘI TRÚ 

df_noi_sxh <- load_data(file_path_sxh, sheet = "noi")

df_inpatient_ready_sxh <- process_inpatient_data(df_inpatient = df_noi_sxh, 
                                             hist_years = years_hist_sxh, 
                                             current_year_col = curr_year_col_sxh)

p_inp_sxh <- plot_inpatient(df_inpatient_ready_sxh, disease_name = "sốt xuất huyết")

print(p_inp_sxh)
ggsave("dengue-s-tp.svg", p_inp_sxh, width = 14, height = 7, dpi = 300)


# CA NẶNG ################
process_severe_simple <- function(df, hist_years, current_year_col) {
  
  # Bước 1: Tính toán Trung bình và SD từ các năm lịch sử theo từng tuần
  df_stats <- df %>%
    select(Week, all_of(hist_years)) %>%
    pivot_longer(cols = -Week, names_to = "Year", values_to = "Cases") %>%
    mutate(Cases = replace_na(Cases, 0)) %>% # Điền 0 nếu thiếu số liệu
    group_by(Week) %>%
    summarise(
      Mean = mean(Cases, na.rm = TRUE),
      SD = sd(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Threshold = Mean + 2 * SD) # Ngưỡng = Trung bình + 2SD
  
  # Bước 2: Lấy dữ liệu năm hiện tại
  df_curr <- df %>%
    select(Week, Current = all_of(current_year_col)) %>%
    replace_na(list(Current = 0))
  
  # Bước 3: Ghép lại
  df_final <- df_stats %>%
    left_join(df_curr, by = "Week")
  
  return(df_final)
}

# ==============================================================================
# 2. HÀM VẼ BIỂU ĐỒ ĐƠN GIẢN
plot_severe_simple <- function(df_data, disease_name = "", y_lab = "Số ca nặng") {
  
  # 1. Tạo nhãn legend động
  # VD: "Số ca Tay chân miệng nặng"
  legend_label <- if(disease_name != "") paste("Số ca", disease_name, "nặng") else "Số ca nặng"
  
  # 2. Tạo mapping màu động
  # Gán màu #ADD8E6 cho nhãn vừa tạo
  fill_colors <- setNames("#ADD8E6", legend_label)
  
  # Tính y_max
  y_max <- max(c(df_data$Current, df_data$Threshold), na.rm = TRUE) * 1.2
  if(y_max == 0) y_max <- 5
  
  ggplot(df_data, aes(x = Week)) +
    # --- Thay đổi: dùng biến legend_label cho fill ---
    geom_col(aes(y = Current, fill = legend_label), width = 0.8, alpha = 0.7) +
    
    # Đường ngưỡng (Giữ nguyên)
    geom_line(aes(y = Threshold, color = "Ngưỡng (TB + 2SD)"), linewidth = 1) +
    
    # Trang trí
    scale_y_continuous(expand = c(0, 0), limits = c(0, y_max)) +
    scale_x_continuous(breaks = seq(1, 52, by = 4), expand = expansion(mult = c(0.02, 0.02))) +
    
    # --- Thay đổi: truyền vector màu động vào values ---
    scale_fill_manual(name = "", values = fill_colors) +
    
    scale_color_manual(name = "", values = c("Ngưỡng (TB + 2SD)" = "#000000")) + 
    
    labs(x = "Tuần", y = y_lab) +
    theme_classic(base_size = 14, base_family = "serif") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    )
}

# 3. THỰC THI (CHẠY CHO CẢ 2 BỆNH)
# --- CẤU HÌNH ---
curr_year_col <- "2026"

# --- A. TAY CHÂN MIỆNG (HFMD) ---
file_path_hfmd <- "https://docs.google.com/spreadsheets/d/1jSAuGFUkcHBL5iie999qn7ClW2dTYB3AOSOaRwOBCGY/edit?gid=552422139#gid=552422139"
years_hist_hfmd <- c("2024", "2022", "2020", "2019", "2017")

df_nang_hfmd <- load_data(file_path_hfmd, sheet = "nang")
df_ready_hfmd <- process_severe_simple(df_nang_hfmd, years_hist_hfmd, curr_year_col)

p_hfmd <- plot_severe_simple(df_ready_hfmd, 
                             disease_name = "tay chân miệng", 
                             y_lab = "Số ca nặng (>= độ 2b)")

print(p_hfmd)
ggsave("hfmd_severe_simple.svg", p_hfmd, width = 10, height = 6)


# --- B. SỐT XUẤT HUYẾT (DENGUE) ---
file_path_dengue <- "https://docs.google.com/spreadsheets/d/13_o7NAlfBjckO6PspzITbWhlkWbfp4eKkqYCa7Dvvgg/edit?usp=sharing"
years_hist_dengue <- c("2024", "2023", "2020", "2017", "2016")

df_nang_dengue <- load_data(file_path_dengue, sheet = "nang")
df_ready_dengue <- process_severe_simple(df_nang_dengue, years_hist_dengue, curr_year_col)

p_dengue <- plot_severe_simple(df_ready_dengue, 
                               disease_name = "sốt xuất huyết", 
                               y_lab = "Số ca nặng SXH")
print(p_dengue)
ggsave("dengue_severe_simple.svg", p_dengue, width = 10, height = 6)
