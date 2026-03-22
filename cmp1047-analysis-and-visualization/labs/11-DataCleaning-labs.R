# Đường dẫn tới file dữ liệu
file_path <- "~/Documents/study-notes/cmp1047-analysis-and-visualization/labs/data/clients.csv"

# Đọc file CSV vào dataframe
df <- read.csv(file_path)

# Mở bảng dữ liệu để xem nhanh
View(df)

# Xóa cột thừa 'X' (thường xuất hiện khi export từ R / Excel)
df$X <- NULL

# Lấy số lượng cột hiện tại
size <- ncol(df)

# Tìm các dòng bị thiếu giá trị ở cột Response
idx <- which(is.na(df$Response))

# Các dòng này bị lệch cột nên dịch dữ liệu sang phải
# copy từ cột 5 -> size-1 sang 6 -> size

df[idx, 6:size] <- df[idx, 5:(size-1)]

# Sau khi dịch thì cột 5 sẽ bị trống
df[idx, 5] <- NA

# Một số dòng có Marital_Status = "Cycle" thực chất bị lệch dữ liệu
# kiểm tra thêm điều kiện Recency có thể parse được thành ngày
idx <- which(df$Marital_Status == "Cycle" &
               !is.na(as.Date(df$Recency, format = "%d-%m-%Y")))

# Dịch dữ liệu sang trái 1 cột
df[idx, 4:(size-1)] <- df[idx, 5:size]

# Cột cuối cùng sẽ bị dư nên set NA
df[idx, size] <- NA

# Với các dòng còn lại có Marital_Status = "Cycle"
# giá trị bị đặt nhầm vào cột Income
idx <- which(df$Marital_Status == "Cycle")

df[idx, "Marital_Status"] <- df[idx, "Income"]
df[idx, "Income"] <- NA

# Kiểm tra số lượng NA ở từng cột
colSums(is.na(df))

# Chuyển một số cột về dạng integer
# vì lúc đọc CSV có thể bị đọc thành character
for(x in c("Income", "Kidhome", "Teenhome", "Recency", "MntWines")){
  df[, x] <- as.integer(df[, x])
}

# Chuyển cột ngày về kiểu Date
df$Dt_Customer <- as.Date(df$Dt_Customer, format="%d-%m-%Y")

# Điền giá trị thiếu bằng median (ít bị ảnh hưởng bởi outlier)
df$Year_Birth[is.na(df$Year_Birth)] <- median(df$Year_Birth, na.rm=TRUE)
df$Income[is.na(df$Income)] <- median(df$Income, na.rm=TRUE)
df$MntWines[is.na(df$MntWines)] <- median(df$MntWines, na.rm=TRUE)

# Hàm lấy mode (giá trị xuất hiện nhiều nhất)
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Với Response dùng mode thay vì median
# vì đây là biến phân loại

df$Response[is.na(df$Response)] <- get_mode(df$Response)

# Kiểm tra lại NA sau khi xử lý
colSums(is.na(df))

# Xem thống kê tổng quan sau khi làm sạch
summary(df)
