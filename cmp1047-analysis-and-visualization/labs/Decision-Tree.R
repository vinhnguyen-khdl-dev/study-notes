# =============================
# Install packages (chạy 1 lần)
# =============================
packages <- c("tidyverse","rpart","caret","pROC")

install_if_missing <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))

# =============================
# Load libraries
# =============================
library(tidyverse)
library(rpart)
library(caret)
library(pROC)


# =============================
# TITANIC — Classification Tree
# =============================
titanic <- read.csv("dataset/titanic.csv", stringsAsFactors = TRUE)

titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(
    survived = factor(survived,
                      levels = c(0, 1),
                      labels = c("No", "Yes"))
  ) %>%
  drop_na()

set.seed(42)
train_idx <- createDataPartition(titanic_clean$survived, p = 0.8, list = FALSE)

tit_train <- titanic_clean[train_idx, ]
tit_test  <- titanic_clean[-train_idx, ]

tree_default <- rpart(
  survived ~ .,
  data   = tit_train,
  method = "class"
)

# plot tree
plot(tree_default, uniform = TRUE, margin = 0.1)
text(tree_default, use.n = TRUE, all = TRUE, cex = 0.7)

pred_class <- predict(tree_default, tit_test, type = "class")
pred_prob  <- predict(tree_default, tit_test, type = "prob")[, "Yes"]

confusionMatrix(pred_class, tit_test$survived, positive = "Yes")

roc_tree <- roc(tit_test$survived, pred_prob, levels = c("No","Yes"))
auc(roc_tree)


# =============================
# DEPTH ANALYSIS
# =============================
depths <- 1:10
acc_train <- numeric(length(depths))
acc_test  <- numeric(length(depths))

for(i in seq_along(depths)){
  
  tree_d <- rpart(
    survived ~ .,
    data = tit_train,
    method = "class",
    control = rpart.control(maxdepth = depths[i], cp = 0)
  )
  
  acc_train[i] <- mean(
    predict(tree_d, tit_train, type="class") == tit_train$survived
  )
  
  acc_test[i] <- mean(
    predict(tree_d, tit_test, type="class") == tit_test$survived
  )
}

data.frame(depth = depths,
           Train = acc_train,
           Test  = acc_test)


# =============================
# PRUNING
# =============================
tree_full <- rpart(
  survived ~ .,
  data = tit_train,
  method = "class",
  control = rpart.control(cp = 0)
)

plotcp(tree_full)

best_cp <- tree_full$cptable[
  which.min(tree_full$cptable[,"xerror"]), "CP"
]

tree_pruned <- prune(tree_full, cp = best_cp)

plot(tree_pruned, uniform = TRUE)
text(tree_pruned, use.n = TRUE, all = TRUE, cex = 0.7)


# =============================
# CARET 10-FOLD CV
# =============================
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

cp_grid <- expand.grid(
  cp = c(0, 0.001, 0.005, 0.01, 0.02, 0.05)
)

set.seed(42)
tree_caret <- train(
  survived ~ .,
  data = tit_train,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = cp_grid,
  metric = "ROC"
)

print(tree_caret)
plot(tree_caret)


# =============================
# WINE — REGRESSION TREE
# =============================
wines <- read.csv("dataset/wines.csv", stringsAsFactors = TRUE)

wines_reg <- wines %>% select(-type)

set.seed(42)
idx <- sample(1:nrow(wines_reg), 0.8*nrow(wines_reg))

w_train <- wines_reg[idx, ]
w_test  <- wines_reg[-idx, ]

tree_reg <- rpart(
  quality ~ .,
  data = w_train,
  method = "anova"
)

plot(tree_reg, uniform = TRUE)
text(tree_reg, use.n = TRUE, all = TRUE, cex = 0.7)

pred_reg <- predict(tree_reg, w_test)

rmse_tree <- sqrt(mean((w_test$quality - pred_reg)^2))
mae_tree  <- mean(abs(w_test$quality - pred_reg))
r2_tree   <- 1 - sum((w_test$quality - pred_reg)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

rmse_tree
mae_tree
r2_tree


# =============================
# GERMAN CREDIT
# =============================
german <- read.csv("dataset/german1.csv")

german$target <- factor(
  german$target,
  levels = c(1,2),
  labels = c("Good","Bad")
)

set.seed(42)
g_idx <- createDataPartition(german$target, p=0.8, list=FALSE)

g_train <- german[g_idx, ]
g_test  <- german[-g_idx, ]

ctrl_compare <- trainControl(
  method="cv",
  number=10,
  classProbs=TRUE,
  summaryFunction=twoClassSummary
)

set.seed(42)
m_lr <- train(
  target ~ .,
  data = g_train,
  method = "glm",
  family = "binomial",
  trControl = ctrl_compare,
  metric = "ROC"
)

set.seed(42)
m_tree <- train(
  target ~ .,
  data = g_train,
  method = "rpart",
  trControl = ctrl_compare,
  metric = "ROC"
)

resamples(list(
  LogisticRegression = m_lr,
  DecisionTree = m_tree
))