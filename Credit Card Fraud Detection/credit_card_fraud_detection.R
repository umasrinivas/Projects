
library(caret)      # for sampling
library(caTools)    # for train/test split
library(dplyr)      # for data manipulation
library(stringr)    # for data manipulation
library(ggplot2)    # for data visualization
library(corrplot)   # for correlation
library(Rtsne)      # for tsne plotting
library(DMwR)       # for smote implementation
library(ROSE)       # for ROSE sampling
library(rpart)      # for decision tree model
library(Rborist)    # for random forest model
library(xgboost)    # for xgboost model


setwd("E:/R/Credit Card Fraud Detection")

# load data
data <- read.csv("./data/creditcard.csv")


## Basic Exploration

head(data)
str(data)
summary(data)

# check for missing values
colSums(is.na(data))

# check class
table(data$Class)
prop.table(table(data$Class))

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# distribution of class labels
ggplot(data = data, aes(x = factor(Class), 
                        y = prop.table(stat(count)), 
                        fill = factor(Class),
                        label = scales::percent(prop.table(stat(count)), accuracy = 0.1))) +
    geom_bar(position = "dodge") + 
    geom_text(stat = 'count',
              position = position_dodge(0.90), 
              vjust = -0.5, 
              size = 3) + 
    scale_x_discrete(labels = c("no fraud", "fraud")) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = 'Class', y = 'Percentage') +
    ggtitle("Distribution of class labels") +
    common_theme
    
# distribution of time of transaction by class

ggplot(data = data, aes(x = Time, fill = factor(Class))) +
    geom_histogram(bins = 100) +
    facet_grid(Class~., scales = "free_y") +
    labs(x = "Time in seconds since first transaction", y = "No. of transactions") +
    ggtitle("Distribution of time of transaction by class") +
    common_theme

# distribution of variable 'Amount' by Class

ggplot(data = data, aes(x = factor(Class), y = Amount)) +
    geom_boxplot() +
    labs(x = "Class", y = "Amount") +
    ggtitle("Distribution of transaction amount by class") +
    common_theme

# correlation
correlations <- cor(data[,-1], method = "pearson")
corrplot(correlations, number.cex = 0.9, method = "circle", 
         type = "full", tl.cex = 0.8, tl.col = "black")


# Visualization of transactions using t-SNE
# Use 10% data to compute t-SNE

tsne_subset <- 1:as.integer(0.1*nrow(data))
tsne <- Rtsne(data[tsne_subset,-c(1,31)], perplexity = 20, theta = 0.5,
              pca = FALSE, max_iter = 500, verbose = FALSE, check_duplicates = FALSE)

classes <- as.factor(data$Class[tsne_subset])
tsne_mat <- as.data.frame(tsne$Y)

ggplot(tsne_mat, aes(x = V1, y = V2)) + 
    geom_point(aes(color = classes)) +
    theme_minimal() +
    ggtitle("t-SNE visualization of transactions") +
    scale_color_manual(values = c("#E69F00", "#56B4E9")) +
    common_theme



## Modeling Approach

# SMOTE - synthetic minority oversampling technique
# ROSE - random over-sampling examples

# Data preparation

# remove 'Time' variable
data <- data[,-1]
data$Class <- as.factor(data$Class)
levels(data$Class) <- c("Not Fraud", "Fraud")

# scale numeric variables
data[,-30] <- scale(data[,-30])
head(data)

# split data into train and test sets
set.seed(123)
split <- sample.split(data$Class, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

table(train$Class)

# under sampling
set.seed(1951)
down_train <- downSample(x = train[,-ncol(train)], y = train$Class)
table(down_train$Class)

# over sampling
set.seed(1951)
up_train <- upSample(x = train[,-ncol(train)], y = train$Class)
table(up_train$Class)

# Synthetic data generation

# SMOTE
set.seed(1951)
smote_train <- SMOTE(Class~., train)
table(smote_train$Class)

# ROSE
set.seed(1951)
rose_train <- ROSE(Class~., train)$data
table(rose_train$Class)



# CART model performance on imbalanced data
set.seed(1591)
orig_fit <- rpart(Class~., data = train)

# evaluate model performance on test set
pred_orig <- predict(orig_fit, newdata = test, method = "class")

roc.curve(test$Class, pred_orig[,2], plotit = TRUE)    # AUC : 0.912



# down sampled model
set.seed(1591)
down_fit <- rpart(Class~., data = down_train)
pred_down <- predict(down_fit, newdata = test)

roc.curve(test$Class, pred_down[,2], plotit = FALSE)   # AUC : 0.942


# up sampled model
set.seed(1591)
up_fit <- rpart(Class~., data = up_train)
pred_up <- predict(up_fit, newdata = test)

roc.curve(test$Class, pred_up[,2], plotit = FALSE)     # AUC : 0.943


# SMOTE
set.seed(1591)
smote_fit <- rpart(Class~., data = smote_train)
pred_smote <- predict(smote_fit, newdata = test)

roc.curve(test$Class, pred_smote[,2], plotit = FALSE)  # AUC : 0.934


# ROSE
set.seed(1591)
rose_fit <- rpart(Class~., data = rose_train)
pred_rose <- predict(rose_fit, newdata = test)

roc.curve(test$Class, pred_rose[,2], plotit = FALSE)   # AUC : 0.942



# logistic regression (GLM) fit

glm_fit <- glm(Class~., data = up_train, family = "binomial")
pred_glm <- predict(glm_fit, newdata = test, type = "response")

roc.curve(test$Class, pred_glm, plotit = TRUE)         # AUC : 0.971


# Random Forest fit

x <- up_train[,-30]
y <- up_train[,30]

rf_fit <- Rborist(x, y, nTree = 1000, minNode = 20, maxLeaf = 13)
pred_rf <- predict(rf_fit, newdata = test[,-30], ctgCensus = "prob")

roc.curve(test$Class, pred_rf$prob[,2], plotit = TRUE) # AUC : 0.973


# XGB fit

# convert class labels from factor to numeric
labels <- up_train$Class
y <- recode(labels, "Not Fraud" = 0, "Fraud" = 1)

xgb_fit <- xgboost(data = data.matrix(up_train[,-30]), 
                   label = y, 
                   eta = 0.1, 
                   gamma = 0.1, 
                   max_depth = 10, 
                   nrounds = 300, 
                   objective = "binary:logistic",
                   colsample_bytree = 0.6,
                   verbose = 0,
                   nthread = 7,
                   seed = 42)
pred_xgb <- predict(xgb_fit, data.matrix(test[,-30]))

roc.curve(test$Class, pred_xgb, plotit = TRUE)        # AUC : 0.974


# important features
names <- dimnames(data.matrix(up_train[,-30]))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb_fit)

# graph
xgb.plot.importance(importance_matrix[1:10,])












