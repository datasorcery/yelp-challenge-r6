# setup
library(jsonlite)

# CONSTANTS
DATA_DIR <- "./data/"
TIP_FILENAME <- paste(DATA_DIR,"yelp_academic_dataset_tip.json",sep="")
USER_FILENAME <- paste(DATA_DIR,"yelp_academic_dataset_user.json",sep="")
REVIEW_FILENAME <- paste(DATA_DIR,"yelp_academic_dataset_review.json",sep="")
CHECKIN_FILENAME <- paste(DATA_DIR,"yelp_academic_dataset_checkin.json",sep="")
BUSINESS_FILENAME <- paste(DATA_DIR,"yelp_academic_dataset_business.json",sep="")

# processing the json files
tip <- stream_in(file(TIP_FILENAME, "r"))
user <- stream_in(file(USER_FILENAME, "r"))
review <- stream_in(file(REVIEW_FILENAME, "r"))
checkin <- stream_in(file(CHECKIN_FILENAME, "r"))
business <- stream_in(file(BUSINESS_FILENAME, "r"))

# save as RData object
dir.create("rdata")
save(tip, file="./rdata/tip.RData")
save(user, file="./rdata/user.RData")
save(review, file="./rdata/review.RData")
save(checkin, file="./rdata/checkin.RData")
save(business, file="./rdata/business.RData")

