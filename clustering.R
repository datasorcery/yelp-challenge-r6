# Load libraries
#library(caret)
#library(clValid)
#library(plyr)
library(lubridate)

# Load data
# load(file="./rdata/checkin.RData")
# load(file="./rdata/business.RData")
# load(file="./rdata/tip.RData")
load(file="./rdata/review.RData")
load(file="./rdata/user.RData")
save(df_users, size, file = './rdata/analysis.RData')

# SETUP
set.seed(666)
s_number <- 50000 # number of random samples
clusters_no <- 15
rescale <- FALSE
alpha <- 0.05

# Do users analysis
system.time({
# Random sampling of users and business
if (s_number > 0 ) {
    df_users <- user[sample(1:nrow(user), s_number, replace=FALSE),] 
} else df_users <- user

# Transform nested data in columns
df_users$votes.funny <- df_users$votes$funny
df_users$votes.useful <- df_users$votes$useful
df_users$votes.cool <- df_users$votes$cool
df_users$compliments.profile <- df_users$compliments$profile
df_users$compliments.cute <- df_users$compliments$cute
df_users$compliments.funny <- df_users$compliments$funny
df_users$compliments.plain <- df_users$compliments$plain
df_users$compliments.writer <- df_users$compliments$writer
df_users$compliments.note <- df_users$compliments$note
df_users$compliments.photos <- df_users$compliments$photos
df_users$compliments.hot <- df_users$compliments$hot
df_users$compliments.cool <- df_users$compliments$cool
df_users$compliments.more <- df_users$compliments$more
df_users$compliments.list <- df_users$compliments$list
df_users$reviews <- df_users$review_count

# Count how many friends a user have
df_users <- cbind(df_users, friends_no = sapply(df_users$friends, FUN = length))

# Get how long a user is yelping
# Convert time to real time
df_users$yelping_since <- ymd(paste(df_users$yelping_since,'-01', sep=''))

# To understand the logic look this stackoverflow:
# http://stackoverflow.com/questions/1995933/number-of-months-between-two-dates/1996404#1996404
# Function to get time difference in months
y_months <- function(d) {
    length(seq(from = d,to = now(), by='month')) - 1
}
# Calculate for each row
df_users$yelping_months <- sapply(df_users$yelping_since, FUN = y_months)

# Remove nested and unused data
nested_cols <- c('votes','compliments','type','friends','name',
                 'yelping_since','elite','review_count')
df_users <- df_users[!(names(df_users) %in% nested_cols)]

# Transform NAs in ZERO
df_users[is.na(df_users)] <- 0

# Rescale data?
if (rescale) {
    mtx_user <- scale(df_users[!(names(df_users) %in% c('user_id'))])
} else mtx_user <- df_users[!(names(df_users) %in% c('user_id'))]

# Cluster users
cl_users <- kmeans(mtx_user, clusters_no,
                   iter.max = 1000, nstart = 5)
size <- cl_users$size   # Store clusters size for first run.
cl_labels <- cl_users$cluster  # Store cluster membership info

# Re-cluster for cluster stability analysis
for (i in 1:4) {
    cl_users <- kmeans(mtx_user, clusters_no,
                       iter.max = 1000, nstart = 5)
    size <- cbind(size,cl_users$size)
    cl_labels <- cbind(cl_labels,cl_users$cluster)
}

# Name columns to help analysis
size <- data.frame(size)
colnames(size) <- c('C1','C2','C3','C4','C5')
colnames(cl_labels) <- c('C1','C2','C3','C4','C5')

# Apply classification to the original DF
df_users <- cbind(df_users, cl_labels)

# Find places that some person in one group
# Select users from one group
df_users[df_users$C1 == 1, ]$user_id

}) # End of users analysis

# Start business analysis
# Find the average rate by group, only need some fields
mtx_business <- review[c('business_id','user_id','stars')]

# if working with a user sample, select only the places for witch there is
# users reviews
mtx_business <- mtx_business[(mtx_business$user_id %in% unique(df_users$user_id)),]

# Function to find the group from user_id
user_group <- function(u) {
    df_users[df_users$user_id == u, ]$C1
}

# Atribute review to correct group
system.time({
    mtx_business$group <- sapply(mtx_business$user_id, FUN = user_group)
})

# Calculate average, standard deviation and count
system.time({
resumo <- with(mtx_business,
               aggregate(stars ~ business_id + group,
                         FUN = function(x) c(MN = mean(x), SD = sd(x), COUNT = length(x))))
})

# Select only the business with 20 or more review for each group
resumo <- resumo[resumo$stars[,3] >= 20, ]

# Find business that have reviews for at least 2 groups
resumo <- resumo[duplicated(resumo$business_id), ]

# Calculate standard error
resumo$SR <- resumo$stars[,2] / resumo$stars[,3]

# Calculate T score for two sided test
system.time({
    resumo$TS <- sapply(resumo$stars[,3], 
                              FUN = function(x) qt(1-alpha/2,df = x))
})

# Test if diferent groups lead to diferent averages
g <- c('zTCCbg7mGslxACL5KlAPIQ')
g <- unique(resumo$business_id)
hyp_tests <- 0

for (b in g) {
    print(resumo[resumo$business_id == 'zTCCbg7mGslxACL5KlAPIQ',])
}

# cleanup USERS
rm(df_users,nested_cols,s_number,y_months,cl_users,mtx_user,size,
   cl_labels,i,clusters_no,rescale)

# cleanup BUSINESS
rm(busines_avg,user_group,mtx_business,resumo)


# Business things
# df_business <- business[sample(1:nrow(business), s_number, replace = FALSE),]
#nested_b_cols <- c('hours','categories','neighborhoods','attributes')
#df_business <- df_business[!(names(business) %in% nested_b_cols)]