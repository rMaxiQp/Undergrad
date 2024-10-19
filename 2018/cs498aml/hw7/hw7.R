# change directory
setwd('hw7/')

# imports
library(glmnet)
# part 1 #
# UC Irvine hosts a dataset of blog posts at https://archive.ics.uci.edu/ml/datasets/
# BlogFeedback. There are 280 independent features which measure various
# properties of the blog post. The dependent variable is the number of comments
# that the blog post received in the 24 hours after a base time. The zip
# file that you download will have training data in blogData train.csv, and
# test data in a variety of files named blogData test-*.csv.
# (a) Predict the dependent variable using all features, a generalized linear
# model (I’d use a Poisson model, because these are count variables), and
# the Lasso. For this exercise, you really should use glmnet in R. Produce
# a plot of the cross-validated deviance of the model against the regularization
# variable (cv.glmnet and plot will do this for you). Use only the
# data in blogData train.csv.
# (b) Your cross-validated plot of deviance likely doesn’t mean all that much to
# you, because the deviance of a Poisson model takes a bit of getting used
# to. Choose a value of the regularization constant that yields a strong
# model, at least by the deviance criterion. Now produce a scatter plot
# of true values vs predicted values for data in blogData train.csv. How
# well does this regression work? keep in mind that you are looking at
# predictions on the training set.
# (c) Choose a value of the regularization constant that yields a strong model,
# at least by the deviance criterion. Now produce a scatter plot of true
# values vs predicted values for data in blogData test-*.csv. How well
# does this regression work?
# (d) Why is this regression difficult?
  

# end of part 1 #

# part 2 #

# Build a logistic regression of the label (normal vs tumor) against the
# expression levels for those genes. There are a total of 62 tissue samples, so
# this is a wide regression. For this exercise, you really should use glmnet in R.
# Produce a plot of the classification error of the model against the regularization
# variable (cv.glmnet – look at the type.measure argument – and plot will
#          do this for you). Compare the prediction of this model with the baseline of
# predicting the most common class.
matrix <- read.csv('part2/I2000.txt', header = FALSE, sep = " ")
labels <- read.csv('part2/tissues.txt',header = FALSE)
data <- t(matrix)

# binomialize labels
bi1 <- sign(labels)

# plot model
glmmod <- cv.glmnet(as.matrix(data), as.matrix(bi1), family="binomial", type.measure = 'class')
plot(glmmod)
title("Missclassification Error VS. Regularization Variable", line = 2.5)

# second prediction
bi2 <- predict(glmmod, data, s='lambda.min', type="class")
cat('accuracy rate:', sum(bi1 ==bi2)/dim(bi1)[1])
# end of part 2 #


# part 3 #
# (a) We will predict the gender of a mouse from the body properties and the
# behavior. The variables you want are columns 4 through 41 of the dataset
# (or bw to visit time d3 d5; you shouldn’t use the id of the mouse). Read
# the description; I’ve omitted the later behavioral measurements because
# there are many N/A’s. Drop rows with N/A’s (there are relatively few).
# How accurately can you predict gender using these measurements, using
# a logistic regression and the lasso? For this exercise, you really should
# use glmnet in R. Produce a plot of the classification error of the model
# against the regularization variable. Compare the prediction of this
# model with the baseline of predicting the most common gender for all
# mice.

raw_data_full <- read.csv('part3/Crusio1.csv')
raw_data <- raw_data_full[c(1, 2, 4:41)]
data <- na.omit(raw_data)
train_data <- data[c(3:40)]
train_label <- as.matrix(data[c(2)])

# binarilize label
train_label[train_label == 'f'] <- 0
train_label[train_label == 'm'] <- 1

# plot model
glmmod <- cv.glmnet(as.matrix(train_data), as.matrix(train_label), family="binomial", type.measure = 'class')
plot(glmmod)
# (b) This exercise is considerably more elaborate than the previous, because
# multinomial logistic regression does not like classes with few examples.
# You should drop strains with fewer than 10 rows. How accurately can
# you predict strain using these measurements, using multinomial logistic
# regression and the lasso? For this exercise, you really should use glmnet
# in R. Produce a plot of the classification error of the model against the
# regularization variable. Compare the prediction of this model
# with the baseline of predicting a strain at random.

#which((train_name_full %in% name_needed) == TRUE),]

train_name_full <- as.matrix(data[c(1)])
name_set <- unique(train_name_full)
name_needed <- c()
for (i in 1 : nrow(name_set)) {
  if (sum(train_name_full == name_set[i]) >= 10) {
    name_needed <- c(name_needed, name_set[i])
  }
}
name_needed <- as.matrix(name_needed)

data_than_10 <- na.omit(raw_data[raw_data$strain %in% name_needed,])

train_name <- as.matrix(data_than_10[c(1)])
train_data <- as.matrix(data_than_10[-c(1, 2)])

glmmod <- cv.glmnet(train_data, train_name, family="multinomial", type.measure = 'class')
plot(glmmod)
# end of part 3 #