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
s_number <- -1 # number of random samples. Use -1 for the full dataset
clusters_no <- 15
rescale <- FALSE
alpha <- 0.2
min_review <- 30 # minimal number of reviews to consider in analysis per group

# Do users analysis (6min for full dataset)
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
system.time({
    # To find the average rate by group, we only need some fields
    mtx_business <- review[c('business_id','user_id','stars')]
    
    # Certify that the review are from our current users (beware users sampling)
    mtx_business <- mtx_business[(mtx_business$user_id %in% unique(df_users$user_id)),]
    
#     # Function to find the group from user_id
#     user_group <- function(u) {
#         df_users[df_users$user_id == u, ]$C1
#     }
#     
#     # Atribute review to correct group
#     system.time({
#         mtx_business$group <- sapply(mtx_business$user_id, FUN = user_group)
#     })
#     
    # Atribute review to correct group
    mtx_business <- merge(mtx_business, df_users[c('user_id','C1')], by='user_id')
    colnames(mtx_business) <- c('user_id','business_id','stars','group')
    mtx_business <- mtx_business[c('business_id','user_id','stars','group')]
    
    # Calculate average, standard deviation and count
    resumo <- with(mtx_business,
                   aggregate(stars ~ business_id + group,
                             FUN = function(x) c(MN = mean(x), SD = sd(x), COUNT = length(x))))
    
    # Select only the business with min_review or more for each group
    resumo <- resumo[resumo$stars[,3] >= min_review, ]
    
    # Sort business by business_id and group
    resumo <- resumo[order(resumo$business_id, resumo$group), ]
    
    # Business that have at least two ocurrencies
    multiple <- unique(resumo[duplicated(resumo$business_id), ]$business_id)
    
    # Subset only business existing in *multiple* reviews
    resumo <- resumo[(resumo$business_id %in% multiple), ]
    
    # Calculate T score for two sided test
    resumo$TS <- sapply(resumo$stars[,3], 
                        FUN = function(x) qt(1-alpha/2,df = x))
    
    # Get list of unique business
    g <- unique(resumo$business_id)
#     hyp_tests <- data.frame(business_id = character(), 
#                             positive = integer(), 
#                             negative = integer(), 
#                             stringsAsFactors = F)
    hyp_tests <- list()
    
    c <- 0 # Counter
    # Test if diferent groups lead to diferent averages
    # Do hypotesis test for all business
    for (b in g) {
        # Get the list of business with the same ID and reset counters
        c <- c + 1
        l <- resumo[resumo$business_id == b, ]
        
        pos <- 0
        neg <- 0
        
        # Get the relevant mean and T statistic
        mu <- l[1,]$stars[1,1]
        TS <- l[1,]$TS
        
        # We start in the second business in the list
        i <- 2
        for (i in 2:length(l$business_id)) {
            c <- c + 1
            Z <- abs(l[i,]$stars[1] - mu) / (l[i,]$stars[2]/sqrt(l[i,]$stars[3]))
            if (Z > TS) { pos <- pos + 1 } else { neg <- neg + 1 }
        }
        
        # Assign to analysis DF
        v <- data.frame(business_id = b, 
                        positive = pos, 
                        negative = neg,
                        stringsAsFactors = F)
        hyp_tests <- rbind(hyp_tests, v)
        
        # Cleanup
        rm(l,pos,neg,mu,TS,i,Z,v)
    }
    rm(b,g)
    
    # Rename column names to simplify understanding... :-)
    colnames(hyp_tests) <- c('business_id', 'positive', 'negative')
    
    # Conclusions
    total_pos <- sum(hyp_tests$positive)
    total_neg <- sum(hyp_tests$negative)
    
})

# cleanup USERS
rm(df_users,nested_cols,s_number,y_months,cl_users,mtx_user,size,
   cl_labels,i,clusters_no,rescale)

# cleanup BUSINESS
rm(busines_avg,user_group,mtx_business,resumo)


# Business things
# df_business <- business[sample(1:nrow(business), s_number, replace = FALSE),]
#nested_b_cols <- c('hours','categories','neighborhoods','attributes')
#df_business <- df_business[!(names(business) %in% nested_b_cols)]