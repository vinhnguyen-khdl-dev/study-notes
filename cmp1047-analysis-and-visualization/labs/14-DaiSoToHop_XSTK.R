# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Phân biệt P, A, C
# ------------------------------------------------------------------------------

# 1. Hoán vị
cat("1. Sắp xếp 6 quyển sách:", factorial(6), "\n")

# 2. Tổ hợp
cat("2. Chọn 3 học sinh từ 15:", choose(15, 3), "\n")

# 3. Chỉnh hợp
cat("3. Chọn 3 học sinh có vai trò:", factorial(15) / factorial(12), "\n")

# 4. Số có 4 chữ số khác nhau
# Lưu ý: chữ số đầu không được là 0
ways_4digit <- 9 * 9 * 8 * 7
cat("4. Số có 4 chữ số khác nhau:", ways_4digit, "\n")


# ------------------------------------------------------------------------------
# Bài tập 2: Bài toán đếm
# ------------------------------------------------------------------------------

# 1. Chọn đội hình
ways <- choose(2, 1) * choose(6, 4) * choose(5, 4) * choose(2, 2)
cat("Số cách chọn đội hình:", ways, "\n")

# 2. Chia 12 học sinh thành 3 nhóm (không phân biệt nhóm)
ways <- choose(12, 4) * choose(8, 4) * choose(4, 4) / factorial(3)
cat("Số cách chia nhóm:", ways, "\n")


# ------------------------------------------------------------------------------
# Bài tập 3: Xác suất
# ------------------------------------------------------------------------------

# 1. Bài lá bài
prob_ge1ace <- 1 - choose(48, 3) / choose(52, 3)
prob_2ace   <- choose(4, 2) * choose(48, 1) / choose(52, 3)
prob_3ace   <- choose(4, 3) / choose(52, 3)

cat("P(≥1 át):", prob_ge1ace, "\n")
cat("P(2 át):", prob_2ace, "\n")
cat("P(3 át):", prob_3ace, "\n")


# 2. Bi đỏ/xanh
prob_3red <- choose(6, 3) / choose(10, 3)
prob_2red <- choose(6, 2) * choose(4, 1) / choose(10, 3)
prob_ge1red <- 1 - choose(4, 3) / choose(10, 3)

cat("P(3 đỏ):", prob_3red, "\n")
cat("P(2 đỏ):", prob_2red, "\n")
cat("P(≥1 đỏ):", prob_ge1red, "\n")


# 3. Sinh viên
prob_4girl <- choose(10, 4) / choose(25, 4)
prob_2girl <- choose(10, 2) * choose(15, 2) / choose(25, 4)
prob_ge1girl <- 1 - choose(15, 4) / choose(25, 4)

cat("P(4 nữ):", prob_4girl, "\n")
cat("P(2 nữ):", prob_2girl, "\n")
cat("P(≥1 nữ):", prob_ge1girl, "\n")


# ------------------------------------------------------------------------------
# Bài tập 4: Ứng dụng thực tế
# ------------------------------------------------------------------------------

# 1. Xổ số
total <- choose(45, 6)
prob_jackpot <- 1 / total
prob_2nd_prize <- choose(6, 5) * choose(39, 1) / total

cat("Tổng số tổ hợp:", total, "\n")
cat("P(Jackpot):", prob_jackpot, "\n")
cat("P(Giải 2):", prob_2nd_prize, "\n")


# 2. Mật khẩu
total_with_dups <- 10^6
total_no_dups <- factorial(10) / factorial(4)

cat("Mật khẩu (có lặp):", total_with_dups, "\n")
cat("Mật khẩu (không lặp):", total_no_dups, "\n")