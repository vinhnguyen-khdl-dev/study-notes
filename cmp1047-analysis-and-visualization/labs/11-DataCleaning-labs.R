### LẤY ĐIỂM THỰC HÀNH
### Các bài tập ######################################################################
# 1. Đọc mô tả dữ liệu phân tích tính cách khách hàng và tải 
# vào R (file clients.csv) với tên biến là "clients".
file_path <- "~/Documents/study-notes/cmp1047-analysis-and-visualization/labs/data/clients.csv"
data <- read.csv(file_path)
View(data)
# 2. Xem qua cấu trúc dữ liệu và kiểm tra các lớp (classes) đã được gán 
# cho các biến trong bộ dữ liệu.
str(data)
summary(data)
# 3. Kiểm tra xem có giá trị nào bị thiếu trong bộ dữ liệu không.
# a) Những biến nào có chứa giá trị bị thiếu?
missing_cols <- names(which(colSums(is.na(data)) > 0))
# b) Điền các giá trị bị thiếu bằng giá trị trung bình hoặc trung vị của biến đó.
# Trước khi điền, hãy xem xét bản chất của biến. Nếu là số nguyên (ví dụ: năm sinh),
# thì hãy điền giá trị phù hợp với bản chất của biến (chúng ta không muốn năm sinh là 1995.832, phải không? ;)).
str(data[,missing_cols])
summary(data[,missing_cols])

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data$Year_Birth[is.na(data$Year_Birth)] <- get_mode(data$Year_Birth)
data$Response[is.na(data$Response)] <- get_mode(data$Response)
data$MntWines[is.na(data$MntWines)] <- median(data$MntWines)
colSums(is.na(data))
# c) Bạn sử dụng đoạn mã nào để điền các giá trị bị thiếu của Year_Birth (nếu có)?

# 4. a) Kiểm tra xem tất cả các giá trị bị thiếu đã được điền đầy đủ chưa. Nếu chưa, lặp lại bước 3.
# b) Bạn sẽ dùng đoạn mã nào để hiển thị tất cả các dòng vẫn còn chứa dữ liệu bị thiếu?
data[!complete.cases(data),]
# 5. a) Xem xét những biến nào nên chuyển đổi thành kiểu "factor"?
# Gợi ý: Đây thường là các biến văn bản có một số giá trị cụ thể và lặp lại.
# Chúng cũng có thể là các biến được biểu diễn bằng số nhưng không mang "ý nghĩa số học"
# - ví dụ: biến "education" với các giá trị 2, 3, 4 thực chất đại diện cho các cấp độ
# giáo dục liên tiếp (ý nghĩa logic) thay vì số năm học tập chính xác (ý nghĩa số học).
str(data)
data$X <- NULL

unique((data$Education))
data$Education[data$Education %in% c("Basic", "2n")] <- "Undergraduate"
data$Education[data$Education %in% c("PhD", "Master")] <- "Postgraduate"
data$Education[data$Education == "Graduation"] <- "Graduate"

data$Education <- factor(data$Education,
                         levels=c("Postgraduate", "Graduate", "Undergraduate"))
is.ordered(data$Education)
unique(data$Marital_Status)
data$Marital_Status[data$Marital_Status %in% c("Married", "Together")] <- "Together"
data$Marital_Status[!data$Marital_Status %in% c("Married", "Together")] <- "Single"

data$Marital_Status <- factor(data$Marital_Status,
                              levels=unique(data$Marital_Status))

data$Teenhome <- as.integer(data$Teenhome)
data$Recency <- as.integer(data$Recency)
data$Income <- as.integer(data$Income)
data$Dt_Customer <- as.Date(data$Dt_Customer)

summary(data)

missing_cols <- names(which(colSums(is.na(data)) > 0))
data[!complete.cases(data), missing_cols]

data$Teenhome[is.na(data$Teenhome)] <- get_mode(data$Teenhome)

for (x in c("Income", "Recency", "MntWines")){
  data[[x]][is.na(data[[x]])] <- median(data[[x]], na.rm = TRUE)
}

colSums(is.na(data))

# b) Bạn sẽ dùng đoạn mã ngắn nhất nào để chuyển đổi biến Marital_Status?

# 6. a) Xem xét biến nào trong số các biến đã xác định ở trên nên được
# chuyển đổi thành kiểu 'ordered factor' (biến phân loại có thứ tự).
# Gợi ý: Biến kiểu 'ordered factor' nên chứa các mức có thứ tự logic
# - ví dụ: biến 'education' với các giá trị 'primary', 'secondary'
# và 'tertiary'. Trong trường hợp này, việc giữ thứ tự các mức là quan trọng.
# Một ví dụ điển hình khác của biến ordered factor là các câu trả lời
# khảo sát sử dụng thang đo Likert (https://en.wikipedia.org/wiki/Likert_scale).
# b) Bạn sẽ dùng đoạn mã nào để chuyển đổi biến Education? Giả sử rằng
# 2n nghĩa là giáo dục trung học và graduation tương đương với bảo vệ bằng cử nhân.

# 7. Chuyển đổi các biến đã xác định trong bước 5 và 6 thành các lớp thích hợp.

# 8. Lưu kết quả để tham khảo sau này! Sử dụng file RData với tên "clientsInR".
write.csv(data, "~/Documents/clientsInR.csv", row.names = FALSE)