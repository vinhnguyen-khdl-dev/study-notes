# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Bar Chart
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu sau:
subjects <- c("Toán", "Lý", "Hóa", "Văn", "Anh")
scores <- c(8, 7.5, 9, 8.5, 7)

# Yêu cầu:
# 1. Vẽ bar chart cơ bản
# 2. Thêm tiêu đề "Điểm thi của bạn"
# 3. Tô màu khác nhau cho mỗi môn
# 4. Thêm giá trị điểm lên đầu mỗi cột
# 5. Vẽ bar chart ngang
barplot(scores,
        names.arg = subjects,
        main="Diem thi cua ban",
        xlab="Mon",
        ylab="Diem",
        col = rainbow(length(scores)),
        horiz = TRUE,
        xlim = c(0, 10)
        )


# ------------------------------------------------------------------------------
# Bài tập 2: Histogram
# ------------------------------------------------------------------------------

# Tạo dữ liệu: Điểm thi của 100 sinh viên
set.seed(2024)
exam_scores <- rnorm(100, mean = 70, sd = 10)

# Yêu cầu:
# 1. Vẽ histogram với 10 bins
# 2. Thêm tiêu đề và nhãn trục phù hợp
# 3. Tô màu xanh lam
# 4. Thêm đường thẳng đứng màu đỏ tại vị trí điểm trung bình
# 5. Vẽ histogram khác với 20 bins, so sánh sự khác biệt
par(mfrow = c(1, 2))

hist(exam_scores,
     main = "Phan phoi diem thi (10 bins)",
     col = "skyblue",
     breaks = 10,
     )

abline(v = mean(exam_scores),
       col = "red",
       lty = 2)

hist(exam_scores,
     main = "Phan phoi diem thi (20 bins)",
     col = "skyblue",
     breaks = 20,
)

abline(v = mean(exam_scores),
       col = "red",
       lty = 2)


# ------------------------------------------------------------------------------
# Bài tập 3: Box Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu iris

# Yêu cầu:
# 1. Vẽ box plot so sánh Petal.Length giữa 3 loài
# 2. Tô màu khác nhau cho mỗi loài
# 3. Thêm tiêu đề phù hợp
# 4. Nhìn vào biểu đồ và trả lời:
#    - Loài nào có petal dài nhất?
#    - Loài nào có độ biến thiên lớn nhất?
#    - Có outliers không? Ở loài nào?
boxplot(Petal.Length ~ Species,
        data = iris,
        xlab = "Species",
        ylab = "Petal",
        col = c("pink", "skyblue", "lightgreen"))
# ------------------------------------------------------------------------------
# Bài tập 4: Scatter Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars

# Yêu cầu:
# 1. Vẽ scatter plot giữa hp (horsepower) và mpg
# 2. Tô màu các điểm theo số cy-lanh (cyl)
# 3. Thêm đường hồi quy tuyến tính
# 4. Thêm legend giải thích màu
# 5. Nhận xét về mối quan hệ giữa hp và mpg
# dữ liệu
data(mtcars)

# tạo màu theo cyl
colors <- c("red", "blue", "darkgreen")
cyl_factor <- factor(mtcars$cyl)

plot(mtcars$hp, mtcars$mpg,
     col = colors[cyl_factor],
     pch = 19,
     xlab = "Horsepower (hp)",
     ylab = "Miles per gallon (mpg)",
     main = "Relationship between hp and mpg")

# đường hồi quy
abline(lm(mpg ~ hp, data = mtcars),
       col = "black",
       lwd = 2)

# legend
legend("topright",
       legend = levels(cyl_factor),
       col = colors,
       pch = 19,
       title = "Cylinders")
# ------------------------------------------------------------------------------
# Bài tập 5: Nhiều biểu đồ
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars

# Yêu cầu:
# Tạo một figure với 4 biểu đồ (2x2) để phân tích biến hp:
# 1. Histogram của hp
# 2. Box plot của hp
# 3. Box plot so sánh hp theo cyl
# 4. Scatter plot hp vs mpg
par(mfrow = c(2,2))

# 1 Histogram hp
hist(mtcars$hp,
     col = "skyblue",
     main = "Histogram of hp",
     xlab = "Horsepower")

# 2 Boxplot hp
boxplot(mtcars$hp,
        col = "pink",
        main = "Boxplot of hp")

# 3 Boxplot hp theo cyl
boxplot(hp ~ cyl,
        data = mtcars,
        col = c("lightgreen","lightblue","pink"),
        main = "hp by cyl",
        xlab = "Cylinders",
        ylab = "Horsepower")

# 4 Scatter hp vs mpg
plot(mtcars$hp, mtcars$mpg,
     col = "purple",
     pch = 19,
     main = "hp vs mpg",
     xlab = "hp",
     ylab = "mpg")
# ------------------------------------------------------------------------------
# Bài tập 6: Tổng hợp
# ------------------------------------------------------------------------------

# Tạo dữ liệu bán hàng của 4 quý
Q1 <- c(100, 120, 110, 130)
Q2 <- c(150, 140, 160, 155)
Q3 <- c(180, 170, 190, 185)
Q4 <- c(200, 210, 195, 220)
products <- c("Sản phẩm A", "Sản phẩm B", "Sản phẩm C", "Sản phẩm D")
sales <- rbind(Q1, Q2, Q3, Q4)
colnames(sales) <- products
# Yêu cầu:
# 1. Vẽ grouped bar chart so sánh doanh thu 4 quý
# 2. Vẽ line plot cho từng sản phẩm qua 4 quý
# 3. Tính tổng doanh thu mỗi quý, vẽ bar chart
# 4. Tạo figure 2x2 hiển thị:
#    - Grouped bar chart
#    - Line plot tất cả sản phẩm
#    - Pie chart tổng doanh thu mỗi quý
#    - Bar chart tổng doanh thu mỗi sản phẩm

par(mfrow = c(2,2))

# grouped bar
barplot(sales,
        beside = TRUE,
        col = rainbow(4),
        legend = rownames(sales),
        main = "Sales by Quarter")

# line plot
matplot(t(sales),
        type="l",
        lty=1,
        col=1:4,
        xaxt="n",
        main="Product Trend",
        xlab="Quarter",
        ylab="Sales")

axis(1, at=1:4, labels=c("Q1","Q2","Q3","Q4"))

# pie chart
pie(rowSums(sales),
    col = rainbow(4),
    main = "Revenue by Quarter")

# bar chart sản phẩm
product_total <- colSums(sales)

barplot(product_total,
        col = "lightgreen",
        main = "Revenue by Product")