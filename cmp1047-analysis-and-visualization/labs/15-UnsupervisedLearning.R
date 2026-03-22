# --------------------------------------------------------------------------
# Mục tiêu học tập
# Sau khi hoàn thành bài học này, sinh viên có thể:
# - Hiểu Unsupervised Learning và vai trò của Clustering
# - Nắm vững thuật toán K-Means và cách hoạt động từng bước
# - Biết cách chọn K tối ưu (Elbow Method và Silhouette Method)
# - Hiểu và áp dụng Hierarchical Clustering
# - Sử dụng DBSCAN cho dữ liệu phức tạp
# - Đánh giá chất lượng clustering
# - Áp dụng vào bài toán thực tế
# --------------------------------------------------------------------------

# ## 2.1 Unsupervised Learning là gì?

# ### 2.1.1 Định nghĩa
# Unsupervised Learning (Học không giám sát) là học từ dữ liệu KHÔNG có nhãn.
# Đặc điểm:
# - Chỉ có input (X), không có output (y)
# - Mục tiêu: Tìm cấu trúc ẩn trong dữ liệu
# - Không có “đáp án đúng” để so sánh

# ### 2.1.2 So sánh với Supervised Learning
# | Khía cạnh    | Supervised Learning           | Unsupervised Learning        |
# |--------------|-------------------------------|------------------------------|
# | Dữ liệu      | Có nhãn (X, y)                | Không nhãn (X)               |
# | Mục tiêu     | Dự đoán y từ X                | Tìm patterns, cấu trúc       |
# | Ví dụ        | Phân loại spam, Dự đoán giá   | Phân nhóm khách hàng         |
# | Đánh giá     | Accuracy, F1, RMSE            | Silhouette, Elbow, Inertia   |
# | Độ khó       | Dễ đánh giá (có ground truth) | Khó đánh giá (không có nhãn) |

# ### 2.1.3 Các loại Unsupervised Learning
# 1. Clustering (Phân cụm): K-Means, Hierarchical, DBSCAN
# 2. Dimensionality Reduction (Giảm chiều): PCA, t-SNE, UMAP
# 3. Association Rules (Luật kết hợp): Apriori, FP-Growth
# 4. Anomaly Detection (Phát hiện bất thường)

# --------------------------------------------------------------------------
# ## 2.2 Clustering là gì?

# ### 2.2.1 Định nghĩa
# Clustering (Phân cụm) là nhóm các đối tượng tương tự vào cùng một cụm.
# Mục tiêu:
# - Các đối tượng trong cùng cụm có độ tương đồng cao
# - Các đối tượng khác cụm có độ khác biệt cao

# ### 2.2.2 Ví dụ trực quan
set.seed(123)

# Tạo 3 nhóm dữ liệu rõ ràng
group1 <- data.frame(x = rnorm(50, 2, 0.5), y = rnorm(50, 2, 0.5))
group2 <- data.frame(x = rnorm(50, 8, 0.6), y = rnorm(50, 3, 0.6))
group3 <- data.frame(x = rnorm(50, 5, 0.5), y = rnorm(50, 7, 0.5))

all_data <- rbind(group1, group2, group3)
true_labels <- c(rep(1, 50), rep(2, 50), rep(3, 50))

par(mfrow = c(1, 2))

# Trước clustering
plot(all_data$x, all_data$y, pch = 19, col = "gray", cex = 1.2,
     xlab = "Feature 1", ylab = "Feature 2",
     main = "TRƯỚC clustering\n(Không có nhãn)")

# Sau clustering
plot(all_data$x, all_data$y, pch = 19, cex = 1.2,
     col = c("red", "blue", "green")[true_labels],
     xlab = "Feature 1", ylab = "Feature 2",
     main = "SAU clustering\n(Máy tự tìm 3 nhóm)")

legend("topright", legend = c("Cụm 1", "Cụm 2", "Cụm 3"),
       col = c("red", "blue", "green"), pch = 19, cex = 0.9)

par(mfrow = c(1, 1))

# ### 2.2.3 Ví dụ cụ thể: Phân khúc khách hàng
set.seed(42)

# Tạo dữ liệu khách hàng
customers <- data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8)),
  Spending = c(rnorm(70, 20, 5), rnorm(60, 80, 12), rnorm(70, 40, 8))
)

# K-Means
km <- kmeans(customers, centers = 3, nstart = 25)
customers$Cluster <- km$cluster

# Visualization
par(mfrow = c(1, 2))

plot(customers$Age, customers$Income,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Thu nhập (triệu/tháng)",
     main = "Age vs Income")
points(km$centers[, 1:2], pch = 4, cex = 3, lwd = 3)

plot(customers$Income, customers$Spending,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Thu nhập", ylab = "Chi tiêu",
     main = "Income vs Spending")
points(km$centers[, 2:3], pch = 4, cex = 3, lwd = 3)

par(mfrow = c(1, 1))

# Phân tích 3 nhóm khách hàng:
# Thống kê từng cụm
cluster_summary <- data.frame(
  Cum = 1:3,
  So_luong = as.numeric(table(customers$Cluster)),
  Tuoi_TB = tapply(customers$Age, customers$Cluster, mean),
  Thu_nhap_TB = tapply(customers$Income, customers$Cluster, mean),
  Chi_tieu_TB = tapply(customers$Spending, customers$Cluster, mean)
)

# Làm tròn
cluster_summary[, 3:5] <- round(cluster_summary[, 3:5], 1)
print(cluster_summary)

# --------------------------------------------------------------------------
# ## 2.3 K-Means Clustering

# ### 2.3.1 Hiểu thuật toán K-Means từ đầu
# K-Means là thuật toán phân cụm dựa trên khoảng cách.
# - K: Số cụm (phải chọn trước)
# - Means: Trung bình (centroid là điểm trung bình)

# ### 2.3.2 Các bước thuật toán chi tiết
# Bước 1: Khởi tạo (Initialization)
set.seed(42)

# Tạo dữ liệu mẫu
data_points <- data.frame(
  x = c(rnorm(30, 2, 0.5), rnorm(30, 8, 0.6), rnorm(30, 5, 0.5)),
  y = c(rnorm(30, 2, 0.5), rnorm(30, 3, 0.6), rnorm(30, 7, 0.5))
)

# [Đoạn mã minh họa các cách khởi tạo Centroids]
par(mfrow = c(1, 3))

# Cách 1: Random
set.seed(123)
random_idx <- sample(1:nrow(data_points), 3)
plot(data_points$x, data_points$y, pch = 19, col = "gray", cex = 1.2,
     main = "Cách 1: Random", xlab = "X", ylab = "Y")
points(data_points$x[random_idx], data_points$y[random_idx], 
       pch = 8, cex = 3, lwd = 3, col = "red")

# Cách 2: K-Means++ (mô phỏng)
c1_idx <- sample(1:nrow(data_points), 1)
dist_to_c1 <- sqrt((data_points$x - data_points$x[c1_idx])^2 + (data_points$y - data_points$y[c1_idx])^2)
c2_idx <- which.max(dist_to_c1)
dist_to_c2 <- sqrt((data_points$x - data_points$x[c2_idx])^2 + (data_points$y - data_points$y[c2_idx])^2)
min_dist <- pmin(dist_to_c1, dist_to_c2)
c3_idx <- which.max(min_dist)
kmpp_idx <- c(c1_idx, c2_idx, c3_idx)

plot(data_points$x, data_points$y, pch = 19, col = "gray", cex = 1.2,
     main = "Cách 2: K-Means++", xlab = "X", ylab = "Y")
points(data_points$x[kmpp_idx], data_points$y[kmpp_idx], 
       pch = 8, cex = 3, lwd = 3, col = "blue")

# Cách 3: Random Partition
set.seed(456)
random_clusters <- sample(1:3, nrow(data_points), replace = TRUE)
centers_rp <- aggregate(data_points, by = list(random_clusters), mean)[, -1]

plot(data_points$x, data_points$y, pch = 19, col = "gray", cex = 1.2,
     main = "Cách 3: Random Partition", xlab = "X", ylab = "Y")
points(centers_rp$x, centers_rp$y, pch = 8, cex = 3, lwd = 3, col = "green")

par(mfrow = c(1, 1))

# Bước 2: Assignment (Gán cụm) - Tính khoảng cách Euclidean
# Công thức: d(x, c) = sqrt(sum((xi - ci)^2))
point <- c(x = 5, y = 5)
centroids <- rbind(c(2, 2), c(8, 3), c(5, 7))
distances <- apply(centroids, 1, function(c) sqrt(sum((point - c)^2)))
print(distances)

# Bước 3: Update (Cập nhật centroids)
# Centroid mới = trung bình tọa độ của các điểm trong cụm
cluster1_points <- data.frame(x = c(2.1, 2.5, 1.8), y = c(2.3, 1.9, 2.1))
new_centroid <- colMeans(cluster1_points)
print(new_centroid)

# ### 2.3.5 Tại sao cần nstart = 25?
# K-Means nhạy cảm với khởi tạo. Cần chạy nhiều lần để tránh local minimum.
km_nstart1 <- kmeans(data_points, centers = 3, nstart = 1)
km_nstart25 <- kmeans(data_points, centers = 3, nstart = 25)

# --------------------------------------------------------------------------
# ## 2.4 Chọn số cụm K tối ưu

# ### 2.4.2 Elbow Method
# Ý tưởng: Tìm điểm "khuỷu tay" nơi WSS giảm chậm lại.
wss_values <- sapply(1:10, function(k) {
  kmeans(customers[, 1:2], centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss_values, type = "b", pch = 19, col = "blue", 
     xlab = "Số cụm K", ylab = "WSS", main = "Elbow Method")
points(3, wss_values[3], col = "red", pch = 19, cex = 2)