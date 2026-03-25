library(tidyverse)
library(caret)
library(pROC)

wine <- read.csv("~/Documents/R_Intro/labs/dataset/wines.csv", stringsAsFactors = TRUE)
View(wine)

str(wine)
summary(wine)

# Holdout
set.seed(42)

idx <- createDataPartition(wine$type, p=0.8, list=FALSE)

train <- wine[idx, ]
test <- wine[-idx, ]

cat("Kích thước tập train:", nrow(train), "\n")
cat("Kích thước tập test: ", nrow(test),  "\n")

# Kiểm tra tỷ lệ lớp trong từng tập
prop.table(table(train$type))
prop.table(table(test$type))

model_lr <- glm(type ~ .,
                data=train,
                family=binomial(link = "logit"))

pred_prob <- predict(model_lr, newdata=test, type="response")

print(levels(test$type))
# Tính AUC
roc_obj  <- roc(test$type, pred_prob, levels = c("red", "white"))
cat("AUC (Holdout):", round(auc(roc_obj), 4), "\n")

set.seed(NULL)

auc_scores <- numeric(20)

for (i in 1:20){
  idx <- createDataPartition(wine$type, p=0.8, list=FALSE)
  
  train <- wine[idx, ]
  test <- wine[-idx, ]
  
  m <- glm(type ~ .,
           data=train,
           family=binomial(link = "logit"))
  pred_prob <- predict(m, test, type="response")
  roc_obj  <- roc(test$type, pred_prob, levels = c("red", "white"))
  auc_scores[i] <- round(auc(roc_obj), 4)
}
cat("AUC min:", round(min(auc_scores),  4), "\n")
cat("AUC max:", round(max(auc_scores),  4), "\n")
cat("AUC sd: ", round(sd(auc_scores),   4), "\n")

# Trực quan hóa
hist(auc_scores, main = "Phân phối AUC — Holdout (20 lần)",
     xlab = "AUC", col = "steelblue", border = "white")

# 10-Fold Cross Validation
ctrl_10fold <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(123)
model_cv <- train(
  type ~ .,
  data      = wine,
  method    = "glm",
  family    = binomial,
  trControl = ctrl_10fold,
  metric    = "ROC"
)

print(model_cv)
cat("AUC trung bình (10-Fold CV):",
    round(model_cv$results$ROC, 4), "\n")

cat("AUC SD (10-Fold CV):",
    round(model_cv$results$ROCSD, 4), "\n")

# Vòng lặp thủ công

set.seed(123)

k <- 10
folds <- createFolds(wine$type, k = k, list = TRUE)

auc_scores <- numeric(k)

for (i in 1:k) {
  test_idx  <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(wine)), test_idx)
  
  train <- wine[train_idx, ]
  test  <- wine[test_idx, ]
  
  # train model
  m <- glm(type ~ .,
           data = train,
           family = binomial)
  
  # predict
  prob <- predict(m, test, type = "response")
  
  # AUC
  roc_obj <- pROC::roc(test$type, prob, levels = c("red","white"))
  auc_scores[i] <- as.numeric(pROC::auc(roc_obj))
}

cat("AUC mean:", round(mean(auc_scores), 4), "\n")
cat("AUC sd:  ", round(sd(auc_scores), 4), "\n")