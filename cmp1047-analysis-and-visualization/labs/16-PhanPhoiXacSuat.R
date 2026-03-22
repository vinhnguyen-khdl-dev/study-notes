# ------------------------------------------------------------------------------
# Bài tập 1: Binomial
# ------------------------------------------------------------------------------

# Một học sinh làm bài trắc nghiệm 20 câu, mỗi câu 4 đáp án.
# Học sinh đoán ngẫu nhiên.

# 1. Xác suất trả lời đúng 5 câu?
# 2. Xác suất trả lời đúng ít nhất 10 câu?
# 3. Số câu đúng kỳ vọng?

n <- 20
p <- 0.25  # 1/4
cat("Xac suat tra loi dung 5 cau: ", dbinom(5, n, p), "\n")
cat("Xac suat tra loi dung it nhat 10 cau: ", 1 - pbinom(9, n, p), "\n")
cat("So cau dung ky vong: ", n * p, "\n")


# ------------------------------------------------------------------------------
# Bài tập 2: Poisson
# ------------------------------------------------------------------------------

# Một cửa hàng nhận trung bình 12 khách hàng/giờ.

# 1. Xác suất có đúng 10 khách trong 1 giờ?
# 2. Xác suất có nhiều hơn 15 khách?
# 3. Xác suất có từ 10-15 khách?

lambda <- 12

cat("Xac suat co dung 10 khach: ", dpois(10, lambda), "\n")
cat("Xac suat co nhieu hon 15 khach: ", 1 - ppois(15, lambda), "\n")
cat("Xac suat co tu 10 den 15 khach: ", ppois(15, lambda) - ppois(9, lambda), "\n\n")

# ------------------------------------------------------------------------------
# Bài tập 3: Normal
# ------------------------------------------------------------------------------

# Điểm thi có phân phối N(75, 10²).

# 1. Xác suất sinh viên đạt trên 85 điểm?
# 2. Xác suất đạt từ 65-85 điểm?
# 3. Điểm tối thiểu để vào top 20%?
# 4. Tính Z-score của điểm 90
mu <- 75
sigma <- 10

cat("Xac suat tren 85 diem: ", 1 - pnorm(85, mu, sigma), "\n")
cat("Xac suat tu 65 den 85 diem: ", pnorm(85, mu, sigma) - pnorm(65, mu, sigma), "\n")
cat("Diem toi thieu vao top 20%: ", qnorm(0.8, mu, sigma), "\n")
cat("Z-score cua diem 90: ", (90 - mu) / sigma, "\n\n")

# ------------------------------------------------------------------------------
# Bài tập 4: CLT
# ------------------------------------------------------------------------------

# Thời gian làm bài có μ = 45 phút, σ = 8 phút.
# Lấy mẫu 36 sinh viên.

# 1. Xác suất trung bình thời gian > 47 phút?
# 2. Xác suất trung bình thời gian < 43 phút?
mu <- 45
sigma <- 8
n <- 36

sigma_xbar <- sigma / sqrt(n)

cat("Xac suat trung binh > 47: ", 1 - pnorm(47, mu, sigma_xbar), "\n")
cat("Xac suat trung binh < 43: ", pnorm(43, mu, sigma_xbar), "\n")
