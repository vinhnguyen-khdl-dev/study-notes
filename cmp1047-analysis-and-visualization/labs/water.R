library(ggplot2)
library(caret)

# 1. ĐỌC DỮ LIỆU & XỬ LÝ MISSING (Giữ nguyên logic của bạn)
df <- read.csv("data/graphics - water quality/water_potability.csv")
for (x in names(df)[colSums(is.na(df)) > 0]){
  df[is.na(df[[x]]), x] <- median(df[[x]], na.rm = TRUE)
}

# 2. FEATURE ENGINEERING (Toàn bộ các biến bạn đã tạo)
df$ph_level <- cut(df$ph, breaks = c(-Inf, 6.5, 8.5, Inf), labels = c("acidic", "neutral", "alkaline"))
df <- cbind(df, model.matrix(~ ph_level - 1, data = df))
df$ph_level <- NULL
df$ph_deviation <- abs(df$ph - 7)
df$pollution_index <- df$Solids + df$Chloramines + df$Sulfate + df$Trihalomethanes
df$chlorine_ratio <- df$Chloramines / df$Solids
df$organic_ratio  <- df$Organic_carbon / df$Solids
df$ph_chloramine    <- df$ph * df$Chloramines
df$hardness_sulfate <- df$Hardness * df$Sulfate
df$high_solids       <- ifelse(df$Solids > 30000, 1, 0)
df$high_chloramines  <- ifelse(df$Chloramines > 8, 1, 0)

# 3. VẼ HEATMAP TƯƠNG QUAN BẰNG GGPLOT2 (Không cần corrplot)
# Tính tương quan và chuyển về định dạng dọc (long format) để ggplot vẽ được
cor_data <- as.data.frame(as.table(cor(df)))

ggplot(cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Heatmap tương quan các biến", fill = "Correlation")

# 4. CHIA DỮ LIỆU & CHUẨN HÓA (CENTER & SCALE)
set.seed(123)
# Biến mục tiêu phải là factor cho các hàm của caret
df$Potability <- as.factor(df$Potability)

trainIndex <- createDataPartition(df$Potability, p = 0.8, list = FALSE)
train_raw <- df[trainIndex, ]
test_raw  <- df[-trainIndex, ]

# Chuẩn hóa dữ liệu (Center & Scale giúp mô hình Logistic chạy ổn định hơn)
# LƯU Ý: preProcess chỉ tính toán trên các cột số, bỏ qua cột Potability
scaler <- preProcess(train_raw[, -which(names(train_raw) == "Potability")], method = c("center", "scale"))
train_data <- predict(scaler, train_raw)
test_data  <- predict(scaler, test_raw)

# 5. HUẤN LUYỆN MÔ HÌNH VỚI TOÀN BỘ BIẾN (Tận dụng dấu ".")
model <- glm(Potability ~ ., data = train_data, family = binomial)

# 6. DỰ ĐOÁN VỚI NGƯỠNG ĐIỀU CHỈNH (THRESHOLD = 0.4)
# Để khắc phục việc chỉ đoán đúng 6 ca nhóm 1, ta hạ ngưỡng xuống 0.4
prob <- predict(model, newdata = test_data, type = "response")
pred <- as.factor(ifelse(prob > 0.4, 1, 0))
actual <- test_data$Potability

# 7. CONFUSION MATRIX & TRỰC QUAN
cm <- confusionMatrix(pred, actual, positive = "1")
print(cm)

cm_df <- as.data.frame(cm$table)
ggplot(cm_df, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() + 
  geom_text(aes(label = Freq), color = "white", size = 8) +
  scale_fill_gradient(low = "#3399FF", high = "#FF3333") +
  theme_minimal() +
  labs(title = "Ma trận nhầm lẫn (Ngưỡng dự đoán: 0.4)", 
       subtitle = paste("Accuracy:", round(cm$overall['Accuracy'], 3)),
       x = "Dự đoán", y = "Thực tế")


