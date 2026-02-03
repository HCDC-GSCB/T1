library(readxl)
library(tidyverse)
library(surveillance)
library(googlesheets4)
library(googledrive)
library(patchwork)

##Load data
load_data <- function(url, sheet) {
  gs4_deauth()
  df <- read_sheet(url, sheet = sheet)
  return(df)
}

## Calculate Mean+2SD and seasonal
remake <- function(df, ref_years) {
  df <- df %>%
    mutate(
      week = ifelse(week == 53, 52, week)
    ) %>%
    group_by(year, week) %>%
    summarise(
      cases = if (all(is.na(cases))) NA_integer_
      else sum(cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year, week)
  
  seasonal <- df %>% 
    filter(year %in% ref_years) %>% 
    summarise(seasonal = median(cases, na.rm = TRUE)) %>% 
    pull(seasonal)
  
  df_cdc <- df %>% 
    filter(year %in% ref_years) %>% 
    group_by(week) %>% 
    summarise(
      mean = mean(cases, na.rm = TRUE),
      sd = sd(cases, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(cdc = mean + 2 * sd)
  
  df <- df %>% 
    left_join(df_cdc, by = "week") %>% 
    mutate(outbreak_cdc = ifelse(!is.na(cdc) & cases >= cdc, 1, 0))
  
  return(list(data = df, seasonal = seasonal))
}

## Run Farrington or CUSUM
run_algo <- function(df, ref_years, method = c("farrington", "cusum"),
                     start_year = 2020, start_week = 1, range_weeks = 261:312,
                     cusum_k = 1.04, cusum_h = 2.26) {
  
  method <- match.arg(method)
  
  remake_out <- remake(df, ref_years)
  df <- remake_out$data
  
  dff <- df %>% filter(year %in% ref_years)
  
  stsObj <- with(dff, sts(observed = cases,
                          state = outbreak_cdc,
                          start = c(start_year, start_week),
                          frequency = 52))
  disProgObj <- sts2disProg(stsObj)
  
  if (method == "farrington") {
    control <- list(b = 5, w = 1, range = range_weeks, reweight = TRUE, verbose = FALSE)
    res <- algo.farrington(disProgObj, control = control)
  } else if (method == "cusum") {
    control <- list(range = range_weeks, k = cusum_k, h = cusum_h)
    res <- algo.cusum(disProgObj, control = control)
  }
  
  return(list(df = df, result = res))
}

## Theme
theme_an <- function() {
  theme_classic() +
    theme(
      axis.text.x = element_text(family = "Times New Roman", size = 11, color = "black"),
      axis.title.x = element_text(family = "Times New Roman", size = 13, color = "black", face = "bold"),
      axis.text.y = element_text(family = "Times New Roman", size = 11, color = "black"),
      axis.title.y = element_text(family = "Times New Roman", size = 13, color = "black", face = "bold"),
      plot.title = element_text(family = "Times New Roman", size = 16, color = "black", face = "bold", hjust = 0.5),
      legend.text = element_text(family = "Times New Roman", size = 12, color = "black"),
      legend.title = element_text(family = "Times New Roman", size = 13, color = "black", face = "bold"),
      legend.position = "bottom",
      panel.grid = element_blank()
    )
}

##Extract week
extract_week <- function(df_tail, df_head){
  df_tail <- df_tail %>% 
    filter(year==2025) %>% 
    filter(week > 44)
  
  df_head <- df_head %>% 
    filter(week < 45)
  df_combined <- bind_rows(df_tail, df_head) %>% 
    mutate(
      time_index = 1:n(),
      label = paste0(week)
    )
  return(df_combined)
}

##Plot
plot_algo_combined <- function(df_combined, seasonal, ref_years, ten_benh = "Sốt xuất huyết") {
  
  idx <- seq(1, nrow(df_combined), by = 3)
  
  # 1. Xử lý trục tung: Làm tròn y_max lên bội số của 500 để chia vạch đẹp hơn
  y_max_raw <- max(c(df_combined$cdc, df_combined$farrington_ub, 
                     df_combined$cusum_ub, df_combined$cases, seasonal), na.rm = TRUE)
  y_max <- ceiling(y_max_raw / 500) * 500 
  
  # 2. Tạo cột giả lập tên bệnh để đưa vào chú thích (Legend)
  df_combined$Legend_Label <- ten_benh
  
  p <- ggplot(data = df_combined) + 
    # Cập nhật: Đưa fill vào trong aes() và gán bằng tên bệnh
    geom_col(aes(x = time_index, y = cases, fill = Legend_Label),
             color = "black", width = 1, alpha = 0.4) +
    
    # Các đường ngưỡng (giữ nguyên)
    geom_line(aes(x = time_index, y = farrington_ub), color = "#E41A1C", size = 1) +
    geom_line(aes(x = time_index, y = cdc, color = "Mean+2SD"), size = 1) +
    geom_hline(aes(yintercept = seasonal, color = "Ngưỡng mùa"), 
               linetype = "dashed", size = 1) +
    
    # Các điểm cảnh báo (giữ nguyên)
    geom_point(data = filter(df_combined, outbreak_f == 1),
               aes(x = time_index, y = cases/2, color = "Farrington"),
               size = 3, shape = 17, stroke = 1.2) +
    geom_point(data = filter(df_combined, outbreak_c == 1),
               aes(x = time_index, y = cases/4, color = "CUSUM"),
               size = 3, shape = 17, stroke = 1.2) +
    geom_point(data = filter(df_combined, outbreak_cdc == 1),
               aes(x = time_index, y = cases), color = "#4DAF4A", 
               size = 3, shape = 17, stroke = 1.2) +
    
    # Trục X
    scale_x_continuous(breaks = df_combined$time_index[idx], labels = df_combined$label[idx]) +
    
    # Cập nhật: Trục Y chia nhỏ mỗi 500 đơn vị
    scale_y_continuous(limits = c(0, y_max), expand = c(0,0),
                       breaks = seq(0, y_max, by = 500)) +
    
    labs(x = "Tuần", y = "Số ca bệnh",
         caption = paste("Năm lịch sử:", paste(ref_years, collapse = ", "))) +
    
    # Cập nhật: Thêm scale_fill_manual cho cột
    scale_fill_manual(name = "", values = "steelblue") +
    
    scale_color_manual(name = "",
                       values = c("Farrington" = "#E41A1C", "CUSUM" = "orange", 
                                  "Mean+2SD" = "#4DAF4A", "Ngưỡng mùa" = "black")) +
    theme_an() + 
    theme(plot.caption = element_text(hjust = 0, family = "Times New Roman", size = 10, color = "black"))
  
  return(p)
}
