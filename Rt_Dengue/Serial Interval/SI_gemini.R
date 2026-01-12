# Cài đặt và gọi các thư viện cần thiết
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr") # Để làm việc với dữ liệu gọn gàng
library(ggplot2)
library(dplyr)
library(tidyr)

# ---- HÀM TÍNH W_U (từ câu trả lời trước) ----
calculate_wu <- function(t, s) {
  if (length(s) != 4) stop("Vector 's' phải chứa đúng 4 giá trị tốc độ.")
  prod_s <- prod(s)
  terms <- sapply(1:4, function(j) {
    s_j <- s[j]
    numerator <- prod_s * exp(-s_j * t)
    denominator <- prod(s[-j] - s_j)
    return(numerator / denominator)
  })
  return(sum(terms))
}

# ---- BƯỚC 1: ĐỊNH NGHĨA KHOẢNG GIÁ TRỊ CỦA CÁC THAM SỐ ----
# Dựa trên bảng bạn cung cấp
param_ranges <- data.frame(
  parameter = c("mu_m", "theta_m", "mu_h", "theta_h", "alpha_h", "c_m"),
  min = c(0.000, 0.067, 0.0000033, 0.100, 0.143, 0.0),
  max = c(0.200, 0.500, 0.000034, 0.330, 0.500, 1.0)
)
print("Khoảng giá trị của các tham số:")
print(param_ranges)

# ---- BƯỚC 2: THIẾT LẬP MÔ PHỎNG ----
N <- 2000 # Số lần mô phỏng, bạn có thể tăng lên để kết quả mịn hơn
time_points <- seq(0, 50, by = 0.5) # Các điểm thời gian để tính toán

# Tạo một ma trận để lưu kết quả của tất cả các đường cong
# Mỗi hàng là một lần mô phỏng, mỗi cột là một điểm thời gian
results_matrix <- matrix(NA, nrow = N, ncol = length(time_points))


# ---- BƯỚC 3: CHẠY VÒNG LẶP MÔ PHỎNG ----
# Tạo một thanh tiến trình để theo dõi
pb <- txtProgressBar(min = 0, max = N, style = 3)

for (i in 1:N) {
  # a. Lấy mẫu ngẫu nhiên các tham số từ phân phối đều (uniform distribution)
  sampled_params <- runif(1, min = param_ranges$min[1], max = param_ranges$max[1]) # mu_m
  names(sampled_params) <- "mu_m"
  sampled_params["theta_m"] <- runif(1, min = param_ranges$min[2], max = param_ranges$max[2])
  sampled_params["mu_h"]    <- runif(1, min = param_ranges$min[3], max = param_ranges$max[3])
  sampled_params["theta_h"] <- runif(1, min = param_ranges$min[4], max = param_ranges$max[4])
  sampled_params["alpha_h"] <- runif(1, min = param_ranges$min[5], max = param_ranges$max[5])
  sampled_params["c_m"]     <- runif(1, min = param_ranges$min[6], max = param_ranges$max[6])
  
  # b. Tính toán 4 tốc độ s
  s1 <- sampled_params["theta_m"] + sampled_params["mu_m"] + sampled_params["c_m"]
  s2 <- sampled_params["mu_m"] + sampled_params["c_m"]
  s3 <- sampled_params["theta_h"] + sampled_params["mu_h"]
  s4 <- sampled_params["alpha_h"] + sampled_params["mu_h"]
  s_rates <- c(s1, s2, s3, s4)
  
  # c. Tính toán toàn bộ đường cong w_u cho bộ tham số này
  wu_curve <- sapply(time_points, calculate_wu, s = s_rates)
  
  # d. Lưu kết quả vào ma trận
  results_matrix[i, ] <- wu_curve
  
  # Cập nhật thanh tiến trình
  setTxtProgressBar(pb, i)
}
close(pb)


# ---- BƯỚC 4: TỔNG HỢP KẾT QUẢ ----
# Tính toán trung vị, và khoảng tin cậy 95% (phân vị 2.5% và 97.5%) cho mỗi cột (mỗi điểm thời gian)
summary_stats <- apply(results_matrix, 2, function(col) {
  quantile(col, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
})

# Chuyển kết quả thành một dataframe dễ sử dụng
summary_df <- data.frame(
  Time = time_points,
  LowerCI = summary_stats[1, ], # Khoảng tin cậy dưới (2.5%)
  Median = summary_stats[2, ],    # Trung vị (50%)
  UpperCI = summary_stats[3, ]  # Khoảng tin cậy trên (97.5%)
)

# ---- BƯỚC 5: VẼ ĐỒ THỊ KẾT QUẢ ----
ggplot(summary_df, aes(x = Time)) +
  # Vẽ dải màu thể hiện khoảng tin cậy 95%
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "skyblue", alpha = 0.5) +
  # Vẽ đường trung vị
  geom_line(aes(y = Median), color = "navy", size = 1) +
  labs(
    title = "Phân phối Khoảng thời gian thế hệ với sự không chắc chắn của tham số",
    subtitle = "Đường liền nét là trung vị, vùng màu là khoảng tin cậy 95% từ 2000 mô phỏng",
    x = "Thời gian (ngày)",
    y = "Mật độ xác suất"
  ) +
  theme_minimal()

summary(summary_df)

# 1. Tìm ước tính điểm cho thời gian phổ biến nhất (đỉnh của đường trung vị)
peak_row <- summary_df[which.max(summary_df$Median), ]
mode_generation_interval <- peak_row$Time

# In kết quả
print(paste("Thời gian thế hệ phổ biến nhất (mode) ước tính là:", round(mode_generation_interval, 1), "ngày."))

# 2. Tìm khoảng tin cậy 95% cho thời gian phổ biến nhất

# Tìm mode của đường giới hạn dưới (LowerCI)
lower_ci_peak_time <- summary_df$Time[which.max(summary_df$LowerCI)]

# Tìm mode của đường giới hạn trên (UpperCI)
upper_ci_peak_time <- summary_df$Time[which.max(summary_df$UpperCI)]

# In kết quả
print(paste("Do sự không chắc chắn của tham số, thời gian thế hệ phổ biến nhất có khả năng (95%) nằm trong khoảng từ",
            round(lower_ci_peak_time, 1), "đến", round(upper_ci_peak_time, 1), "ngày."))
