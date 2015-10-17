# Cross Tabulation
users_fan <- aggregate(user$fans ~ user_id, data = user, sum)
users_fan$class <- "Fan <= 1"

users_funny <- aggregate(user$compliments$funny ~ user_id, data = user, sum)
users_funny$class <- "Funny <= 1"

users_fan[users_fan$`user$fans` > 1,]$class <- 'Fan > 1'
users_funny[users_funny$`user$compliments$funny` > 1,]$class <- 'Funny > 1'