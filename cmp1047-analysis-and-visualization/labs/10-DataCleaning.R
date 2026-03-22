# Đọc dữ liệu chất lượng nước
df <- read.csv("data/graphics - water quality/water_potability.csv")

# Xem nhanh dữ liệu
View(df)

# Kiểm tra cấu trúc và thống kê cơ bản
str(df)
summary(df)

# Hiển thị các dòng có NA
View(df[is.na(df),])

# Lấy danh sách các cột có giá trị thiếu
cols <- names(which(colSums(is.na(df)) > 0))
cols

# Vẽ histogram cho các cột có NA
par(mfrow=c(1, 3))

for (col in cols){
  hist(df[[col]],
       main=col,
       col="skyblue",)
}

# Điền giá trị thiếu bằng mean của từng cột
for (col in cols){
  df[is.na(df[,col]), col] <- mean(df[[col]], na.rm=TRUE)
}

# Chuyển Potability sang logical (TRUE/FALSE)
df$Potability <- as.logical(df$Potability)

# Kiểm tra dữ liệu trùng lặp
sum(duplicated(df))

# Xem các dòng bị trùng (nếu có)
df[duplicated(df), ]

# Xóa dòng trùng lặp
df <- df[!duplicated(df), ]

# Kiểm tra lại sau khi xử lý NA
summary(df)

# Vẽ boxplot để kiểm tra outlier
par(mfrow=c(3, 3))

cols <- names(df)[1:9]
for (col in cols){
  boxplot(df[[col]], 
          main=col,
          col="skyblue",
          horizontal=TRUE)
}


# Hàm xử lý outlier theo IQR
handle_outlier <- function(x){
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  
  # Cắt giá trị nhỏ hơn lower
  x[x < lower] <- lower
  
  # Cắt giá trị lớn hơn upper
  x[x > upper] <- upper
  
  return(x)
}

# Áp dụng xử lý outlier cho các cột numeric
for (col in cols){
  if (is.numeric(df[[col]])) {
    df[[col]] <- handle_outlier(df[[col]])
  }
}

# Vẽ lại boxplot sau khi xử lý outlier
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
for (col in cols){
  if (is.numeric(df[[col]])) {
    boxplot(df[[col]], 
            main = col,
            col = "skyblue",
            horizontal = TRUE)
  }
}

# Lưu dữ liệu sau khi làm sạch
write.csv("data/graphics - water quality/water_potability_cleaned.csv")
