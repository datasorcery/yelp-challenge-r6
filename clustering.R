# Load libraries
#library(caret)

# Load data
load(file="./rdata/checkin.RData")
load(file="./rdata/business.RData")
load(file="./rdata/tip.RData")
load(file="./rdata/user.RData")
load(file="./rdata/review.RData")

# Random sampling of users 
sample_users <- user[sample(1:nrow(user), 500, replace=FALSE),] 

# Remove nested data
nested_cols <- c('votes','friends','compliments','elite')
sample_users <- sample_users[!(names(user) %in% nested_cols)]

# Cluster users
kmeans(sample_users, 10)