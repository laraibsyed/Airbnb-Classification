library(stringr)
date_cols = c("first_review", "host_since", "last_review", "last_scraped")
listings[date_cols] = lapply(listings[date_cols], function(x) {
  as.Date(str_sub(x, 1, 10), format = "%m/%d/%Y")
})

listings$host_verifications_cleaned = str_remove_all(listings$host_verifications, "[',]")
listings$host_verifications_cleaned = str_remove_all(listings$host_verifications, "[\\[\\]'\",\\s]")
listings$host_verifications_cleaned = NULL

listings$verification_count = str_count(listings$host_verifications, ",") + 1
listings$verification_count = ifelse(is.na(listings$host_verifications) |listings$host_verifications == "" | listings$host_verifications == "[]" | 
                                       listings$host_verifications == "none", 0, listings$verification_count)
listings$host_verifications = NULL
listings$amenities_count = str_count(listings$amenities, ",") + 1
listings$amenities_count = ifelse(is.na(listings$amenities) | listings$amenities == "" |listings$amenities == "[]" | listings$amenities == "{}" | 
                                    listings$amenities_count < 1,0, listings$amenities_count)
listings$amenities = NULL

listings$price = as.numeric(gsub("[\\$,]", "", listings$price))
listings$host_response_time = factor(listings$host_response_time, levels = c("a few days or more", "within a day", "within a few hours", "within an hour"), ordered=TRUE)

rate_cols = c("host_acceptance_rate", "host_response_rate")
listings[rate_cols] <- lapply(listings[rate_cols], function(x) {
  as.numeric(gsub("%", "", x)) / 100
})

listings$room_type = as.factor(listings$room_type)

listings$bathrooms_cleaned = gsub("half-bath", "0.5", listings$bathrooms_text, ignore.case = TRUE)
listings$number_of_baths = as.numeric(gsub("^(\\d+\\.?\\d*).*", "\\1", listings$bathrooms_cleaned))
listings$bathrooms_cleaned <- NULL

listings$bath_privacy_level = "Unspecified"
listings$bath_privacy_level[str_detect(listings$bathrooms_text, pattern = "shared")] = "Shared"
listings$bath_privacy_level[str_detect(listings$bathrooms_text, pattern = "private") | listings$room_type == "Entire home/apt"] = "Private"
listings$bath_privacy_level = factor(listings$bath_privacy_level, levels = c("Shared", "Unspecified", "Private"), ordered = TRUE)

cols_to_drop = c(
  "neighbourhood_group_cleansed",
  "bathrooms",
  "bathrooms_text",
  "calendar_updated",
  "calendar_last_scraped",
  "license"
)
listings[cols_to_drop] = NULL

listings$building_type = "Other"
listings$building_type[grepl("hotel|bed and breakfast", listings$property_type, ignore.case = TRUE)] = "Hotel/B&B"
listings$building_type[grepl("house|home|villa|townhouse", listings$property_type, ignore.case = TRUE)] = "House/Villa"
listings$building_type[grepl("apartment|condo|flat|loft|rental unit|serviced apartment", listings$property_type, ignore.case = TRUE)] = "Apartment/Condo/Unit"
listings$building_type = as.factor(listings$building_type)
listings$property_type = NULL

listings$booked_30 = 30 - listings$availability_30

imd_agg = aggregate(imd_data$`Index of Multiple Deprivation (IMD) Rank`, by = list(LTLA = imd_data$`Local Authority District name (2019)`), FUN = mean, na.rm = TRUE)
colnames(imd_agg)[2] = "Mean_IMD_Rank"
listings = merge(listings, imd_agg, by.x = "neighbourhood_cleansed", by.y = "LTLA", all.x = TRUE)

prosperity_labels = c("Low Prosperity", "Mid-Low Prosperity", "Mid-High Prosperity", "High Prosperity")
listings$neighbourhood_prosperity_tier = cut(listings$Mean_IMD_Rank, breaks = quantile(listings$Mean_IMD_Rank, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), labels = prosperity_labels, include.lowest = TRUE)
listings$neighbourhood_prosperity_tier[is.na(listings$neighbourhood_prosperity_tier)] = "Mid-Low Prosperity"
listings$neighbourhood_prosperity_tier <- factor(listings$neighbourhood_prosperity_tier, levels = prosperity_labels, ordered = TRUE)

listings$EMR = listings$price * listings$booked_30
emr_cap = quantile(listings$EMR, 0.99, na.rm = TRUE)
listings$EMR = ifelse(listings$EMR > emr_cap, emr_cap, listings$EMR)
listings$EMR[listings$EMR <= 0] = 1

library(geosphere)
library(dplyr)

borough_centers = summarise(group_by(listings, neighbourhood_cleansed, neighbourhood_prosperity_tier),
                            avg_lat = mean(latitude, na.rm = TRUE),
                            avg_long = mean(longitude, na.rm = TRUE),
                            .groups = 'drop')

distance_matrix = distm(borough_centers[, c("avg_long", "avg_lat")], fun = distHaversine)

rownames(distance_matrix) = borough_centers$neighbourhood_cleansed
colnames(distance_matrix) = borough_centers$neighbourhood_cleansed


distance_threshold_meters = 3000
dist_matrix_attractions = distm(listings[, c("longitude", "latitude")], attractions[, c("longitude", "latitude")], fun = distHaversine)
listings$min_dist_to_attraction = apply(dist_matrix_attractions, 1, min)
listings$is_near_tourist_attraction = ifelse(listings$min_dist_to_attraction <= distance_threshold_meters, 1, 0)
listings$is_near_tourist_attraction = as.factor(listings$is_near_tourist_attraction)
rm(dist_matrix_attractions)

library(sf)
tube_ne = st_as_sf(tube_stations, coords = c("X", "Y"), crs = 27700)
tube_latlong = st_transform(tube_ne, crs = 4326)
new_coords = st_coordinates(tube_latlong)

tube_stations$Longitude = new_coords[, 1]
tube_stations$Latitude = new_coords[, 2]

print(summary(tube_stations$Latitude))
print(summary(tube_stations$Longitude))

tube_threshold_meters = 1000
dist_matrix_tube = distm(listings[, c("longitude", "latitude")], tube_stations[, c("Longitude", "Latitude")], fun = distHaversine)
listings$min_dist_to_tube = apply(dist_matrix_tube, 1, min)
listings$is_near_tube = ifelse(listings$min_dist_to_tube <= tube_threshold_meters, 1, 0)
listings$is_near_tube = as.factor(listings$is_near_tube)
rm(dist_matrix_tube)

listings$experience_days = as.numeric(difftime(listings$last_scraped, listings$host_since, units = "days"))
listings$host_experience_years = listings$experience_days / 365.25

listings$host_experience_years[listings$host_experience_years < 0] = 0

listings$experience_days = NULL

listings$description_length = nchar(as.character(listings$description))
listings$description_length[is.na(listings$description_length)] = 0

listings$neighbourhood_description_length = ifelse(is.na(listings$neighborhood_overview), 0, nchar(as.character(listings$neighborhood_overview)))

listings$host_tier = cut(listings$calculated_host_listings_count,breaks = c(0, 1, 5, Inf), labels = c("Single Listing", "Small Portfolio", "Professional Manager"), include.lowest = TRUE)
listings$host_tier = as.factor(listings$host_tier)

borough_pairs = as.data.frame(as.table(distance_matrix))
names(borough_pairs) = c("Origin", "Target", "Distance")

borough_pairs = borough_pairs[borough_pairs$Origin != borough_pairs$Target, ]

borough_pairs = merge(borough_pairs, 
                      borough_centers[, c("neighbourhood_cleansed", "neighbourhood_prosperity_tier")], 
                      by.x = "Origin", by.y = "neighbourhood_cleansed")

names(borough_pairs)[names(borough_pairs) == "neighbourhood_prosperity_tier"] = "Origin_Tier"

borough_pairs = merge(borough_pairs, 
                      borough_centers[, c("neighbourhood_cleansed", "neighbourhood_prosperity_tier")], 
                      by.x = "Target", by.y = "neighbourhood_cleansed")
names(borough_pairs)[names(borough_pairs) == "neighbourhood_prosperity_tier"] = "Target_Tier"

valid_pairs = borough_pairs[borough_pairs$Origin_Tier == borough_pairs$Target_Tier, ]
valid_pairs = valid_pairs[order(valid_pairs$Origin, valid_pairs$Distance), ]

fallback_lookup_table = valid_pairs[!duplicated(valid_pairs$Origin), c("Origin", "Target")]
names(fallback_lookup_table) = c("neighbourhood_cleansed", "fallback_neighbourhood")

listings = merge(listings, fallback_lookup_table, by = "neighbourhood_cleansed", all.x = TRUE)

min_listings = 15 

peer_group_medians = aggregate(
  EMR ~ neighbourhood_cleansed + neighbourhood_prosperity_tier + room_type + accommodates, 
  data = listings, 
  FUN = function(x) if(length(x) >= min_listings) median(x) else NA
)
colnames(peer_group_medians)[5] = "Local_Median_EMR"

fallback_group_medians = aggregate(
  EMR ~ neighbourhood_cleansed + neighbourhood_prosperity_tier + room_type + accommodates, 
  data = listings, 
  FUN = function(x) if(length(x) >= min_listings) median(x) else NA
)
colnames(fallback_group_medians)[1] = "fallback_neighbourhood"
colnames(fallback_group_medians)[5] = "Fallback_Median_EMR"

listings = merge(listings, peer_group_medians, all.x = TRUE)

listings = merge(listings, fallback_group_medians, 
                 by = c("fallback_neighbourhood", "neighbourhood_prosperity_tier", "room_type", "accommodates"), 
                 all.x = TRUE)

listings$Final_Benchmark = ifelse(
  !is.na(listings$Local_Median_EMR), 
  listings$Local_Median_EMR, 
  listings$Fallback_Median_EMR
)

tier_averages = aggregate(EMR ~ neighbourhood_prosperity_tier, data = listings, FUN = median)
colnames(tier_averages)[2] = "Tier_Median_EMR"
listings = merge(listings, tier_averages, by = "neighbourhood_prosperity_tier", all.x = TRUE)

listings$Final_Benchmark[is.na(listings$Final_Benchmark)] = listings$Tier_Median_EMR[is.na(listings$Final_Benchmark)]

listings$is_good_listing = ifelse(listings$EMR >= listings$Final_Benchmark, "Good Listing", "Bad Listing")
listings$is_good_listing = as.factor(listings$is_good_listing)

recent_crime = c("202211", "202212", "202301", "202302", "202303", "202304", 
                 "202305", "202306", "202307", "202308", "202309", "202310")

crime_subset = crime_stats[crime_stats$MajorText == "ROBBERY" | 
                             crime_stats$MajorText == "THEFT" | 
                             crime_stats$MajorText == "VIOLENCE AGAINST THE PERSON" | 
                             crime_stats$MajorText == "BURGLARY" |
                             crime_stats$MajorText == "PUBLIC ORDER OFFENCES", ]

crime_subset$Annual_Count = rowSums(crime_subset[, recent_crime], na.rm = TRUE)

borough_safety = aggregate(Annual_Count ~ BoroughName, data = crime_subset, FUN = sum)

names(borough_safety)[names(borough_safety) == "Annual_Count"] = "Total_Street_Crime"

min_crime = min(borough_safety$Total_Street_Crime)
max_crime = max(borough_safety$Total_Street_Crime)

borough_safety$Safety_Index = (max_crime - borough_safety$Total_Street_Crime) / (max_crime - min_crime)

listings = merge(listings, borough_safety[, c("BoroughName", "Safety_Index")], by.x = "neighbourhood_cleansed", by.y = "BoroughName", all.x = TRUE)

listings$Safety_Index[is.na(listings$Safety_Index)] = 0.5

density_table = aggregate(
  x = list(competitor_count = listings$id), 
  by = list(neighbourhood_cleansed = listings$neighbourhood_cleansed, 
            room_type = listings$room_type), 
  FUN = length
)

listings = merge(listings, 
                        density_table, 
                        by = c("neighbourhood_cleansed", "room_type"), 
                        all.x = TRUE)

listings$days_since_last_review = as.numeric(difftime(listings$last_scraped, listings$last_review, units = "days"))

max_staleness = max(listings$days_since_last_review, na.rm = TRUE)
listings$days_since_last_review[is.na(listings$days_since_last_review)] = max_staleness

listings$is_stale = ifelse(listings$days_since_last_review > 365, 1, 0) 
listings$is_stale = as.factor(listings$is_stale)

listings$is_short_stay_policy = ifelse(listings$minimum_nights >= 2 & listings$minimum_nights <= 7, 1, 0)

listings$is_short_stay_policy = as.factor(listings$is_short_stay_policy)

business_keywords = "workspace|office|desk|wfh|work from home|monitor|fast wifi|fiber|fibre|business|remote work"

listings$is_business_ready_text = ifelse(grepl(business_keywords, 
                                               paste(listings$name, listings$description), 
                                               ignore.case = TRUE), 1, 0)

listings$is_business_ready_text = as.factor(listings$is_business_ready_text)

family_keywords = "crib|cot|high chair|baby|stair gate|family|kids|children|nursery|playroom"

listings$is_family_text = ifelse(grepl(family_keywords, 
                                       paste(listings$name, listings$description), 
                                       ignore.case = TRUE), 1, 0)

listings$is_family_friendly = as.factor(listings$is_family_text)

couples_keywords = "romantic|couple|honeymoon|anniversary|getaway|secluded|intimate|lover"

listings$is_couples_text = ifelse(grepl(couples_keywords, 
                                        paste(listings$name, listings$description), 
                                        ignore.case = TRUE), 1, 0)

listings$is_couples_friendly = as.factor(listings$is_couples_text)

modern_keywords = "modern|contemporary|renovated|refurbished|brand new|high spec|luxury|minimalist|sleek|architect|designer|newly built"

listings$is_modern_aesthetic = ifelse(grepl(modern_keywords, 
                                            paste(listings$name, listings$description), 
                                            ignore.case = TRUE), 1, 0)

listings$is_modern_aesthetic = as.factor(listings$is_modern_aesthetic)

historical_keywords = "victorian|edwardian|georgian|period|historic|vintage|character|high ceiling|sash window|fireplace|brick|original features|19th century"

listings$is_historical_aesthetic = ifelse(grepl(historical_keywords, 
                                                paste(listings$name, listings$description), 
                                                ignore.case = TRUE), 1, 0)

listings$is_historical_aesthetic = as.factor(listings$is_historical_aesthetic)

view_keywords = "view|skyline|river|panoramic|balcony|terrace|rooftop|shards|canary wharf|cityscape"

listings$has_scenic_view = ifelse(grepl(view_keywords, 
                                        paste(listings$name, listings$description), 
                                        ignore.case = TRUE), 1, 0)

listings$has_scenic_view = as.factor(listings$has_scenic_view)

library(ggplot2)
library(dplyr)

borough_counts = count(listings, neighbourhood_cleansed, sort = TRUE, name = "Count")

contrast_data = rbind(head(borough_counts, 5), tail(borough_counts, 5))

ggplot(contrast_data, aes(x = reorder(neighbourhood_cleansed, Count), y = Count)) +
  geom_col(fill = ifelse(contrast_data$Count > 1000, "#FF5A5F", "gray")) +
  coord_flip() +
  labs(
    title = "Why I Hypothesized Market Saturation Matters",
    subtitle = "The extreme gap between London's busiest and quietest boroughs",
    x = "Borough",
    y = "Number of Competitors (Listings)"
  ) +
  theme_minimal()


ggplot(listings, aes(x = longitude, y = latitude)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "#e6f0ff", high = "#FF5A5F") +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Market Saturation Heatmap",
    subtitle = "Red zones indicate areas with extreme competitor density",
    fill = "Density Level"
  )

listings$recency_group = cut(listings$days_since_last_review, 
                              breaks = c(-1, 30, 90, 180, 365, 1095, 1460, 10000),
                              labels = c("< 1 Month", "1-3 Months", "3-6 Months", "6-12 Months", "1-3 Years", "3-4 Years", "4+ Years"))

listings_subset = listings[!is.na(listings$recency_group), ]

success_by_recency = aggregate(
  x = (listings_subset$is_good_listing == "Good Listing"), 
  by = list(recency_group = listings_subset$recency_group), 
  FUN = mean 
)
colnames(success_by_recency) = c("recency_group", "Success_Rate")

ggplot(success_by_recency, aes(x = recency_group, y = Success_Rate)) +
  geom_col(fill = "#00A699") +
  geom_text(aes(label = scales::percent(Success_Rate, accuracy = 1)), vjust = -0.5) +
  labs(
    title = "Why I Hypothesized 'Listing Staleness'",
    subtitle = "Probability of being a 'Good Listing' drops as reviews get older",
    x = "Time Since Last Review",
    y = "Success Rate (% Good Listings)"
  ) +
  theme_minimal()

modern_success = aggregate(
  x = (listings$is_good_listing == "Good Listing"), 
  by = list(is_modern = listings$is_modern_aesthetic), 
  FUN = mean
)

modern_success$Label = ifelse(modern_success$is_modern == 1, "Modern/Renovated", "Standard/Historic")

ggplot(modern_success, aes(x = Label, y = x)) +
  geom_col(fill = "#FF5A5F") +
  geom_text(aes(label = scales::percent(x, accuracy = 1)), vjust = -0.5) +
  labs(
    title = "Hypothesis Check: Modern is better",
    subtitle = "Success rate of listings with modern/renovated keywords",
    x = "Listing Description Aesthetic",
    y = "Success Rate (% Good Listings)"
  ) +
  theme_minimal()

superhost_success = aggregate(
  x = (listings$is_good_listing == "Good Listing"), 
  by = list(is_superhost = listings$host_is_superhost), 
  FUN = mean
)

superhost_success$Label = ifelse(superhost_success$is_superhost == 1, "Superhost", "Standard Host")

ggplot(superhost_success, aes(x = Label, y = x)) +
  geom_col(fill = "#FF5A5F") +
  geom_text(aes(label = scales::percent(x, accuracy = 1)), vjust = -0.5) +
  labs(
    title = "Hypothesis Check: Superhosts are more trusted",
    subtitle = "Success rate of Superhosts vs. Standard Hosts",
    x = "Host Status",
    y = "Success Rate (% Good Listings)"
  ) +
  theme_minimal()


good_listings = listings[listings$is_good_listing == "Good Listing", ]

ggplot() +
  stat_density_2d(data = good_listings, aes(x = longitude, y = latitude, fill = ..level..), 
                  geom = "polygon", alpha = 0.4) +
  scale_fill_gradient(low = "#e0f3f8", high = "#00A699") + 
  geom_point(data = tube_stations, aes(x = Longitude, y = Latitude), 
             color = "black", size = 0.8, alpha = 0.6) +

  geom_point(data = attractions, aes(x = longitude, y = latitude), 
             color = "#FF5A5F", shape = 17, size = 3) +
  coord_quickmap() +

  theme_void() +

  labs(
    title = "Hypothesis Check: Connectivity vs. Landmarks",
    subtitle = "Success (Teal) follows the Tube Network (Black Dots) vs Attractions (Red Triangles)",
    fill = "Success Density"
  ) +
  theme(legend.position = "bottom")

listings_clean = listings

cols_to_drop = c(
  "id", "listing_url", "scrape_id", "last_scraped", "source", 
  "name", "description", "neighborhood_overview", "picture_url", 
  "host_id", "host_url", "host_name", "host_location", "host_about", 
  "host_thumbnail_url", "host_picture_url", "host_neighbourhood", "host_since",
  "latitude", "longitude", "neighbourhood", "fallback_neighbourhood",
  "price", "EMR", "booked_30", "has_availability",
  "availability_30", "availability_60", "availability_90", "availability_365",
  "number_of_reviews_ltm", "number_of_reviews_l30d", # Outcome metrics
  "Local_Median_EMR", "Fallback_Median_EMR", "Final_Benchmark", "Tier_Median_EMR",
  "Mean_IMD_Rank", "host_listings_count", "host_total_listings_count",
  "first_review", "last_review", "days_since_last_review",
  "minimum_minimum_nights", "maximum_minimum_nights", "minimum_maximum_nights", 
  "maximum_maximum_nights", "minimum_nights_avg_ntm", "maximum_nights_avg_ntm",
  "calculated_host_listings_count", "calculated_host_listings_count_entire_homes",
  "calculated_host_listings_count_private_rooms", "calculated_host_listings_count_shared_rooms",
  "competitor_count.y",
  "min_dist_to_attraction", "min_dist_to_tube",
 "is_family_text", "is_couples_text","recency_group", "is_stale")

listings_clean[cols_to_drop] = NULL

listings_clean$host_response_time = as.character(listings_clean$host_response_time)
listings_clean$host_response_time[is.na(listings_clean$host_response_time)] = "Unknown"

listings_clean$host_response_time = factor(
  listings_clean$host_response_time,
  levels = c("Unknown", "a few days or more", "within a day", "within a few hours", "within an hour"),
  ordered = TRUE)

listings_clean$host_response_rate_missing = ifelse(is.na(listings_clean$host_response_rate), 1, 0)
listings_clean$host_acceptance_rate_missing = ifelse(is.na(listings_clean$host_acceptance_rate), 1, 0)

listings_clean$host_response_rate_missing = as.factor(listings_clean$host_response_rate_missing)
listings_clean$host_acceptance_rate_missing = as.factor(listings_clean$host_acceptance_rate_missing)

mean_resp = mean(listings_clean$host_response_rate, na.rm = TRUE)
mean_acc = mean(listings_clean$host_acceptance_rate, na.rm = TRUE)

listings_clean$host_response_rate[is.na(listings_clean$host_response_rate)] = mean_resp
listings_clean$host_acceptance_rate[is.na(listings_clean$host_acceptance_rate)] = mean_acc

binary_cols = c("host_is_superhost", "host_has_profile_pic", "host_identity_verified")

for(col in binary_cols) {
  listings_clean[[col]] = as.numeric(listings_clean[[col]])
  listings_clean[[col]][is.na(listings_clean[[col]])] = 0
}

median_bedrooms = median(listings_clean$bedrooms, na.rm = TRUE)
listings_clean$bedrooms[is.na(listings_clean$bedrooms)] = median_bedrooms

median_beds = median(listings_clean$beds, na.rm = TRUE)
listings_clean$beds[is.na(listings_clean$beds)] = median_beds

review_cols = c(
  "number_of_reviews",
  "review_scores_rating",
  "review_scores_accuracy",
  "review_scores_cleanliness",
  "review_scores_checkin",
  "review_scores_communication",
  "review_scores_location",
  "review_scores_value",
  "reviews_per_month")

for (col in review_cols){
  med_val = median(listings_clean[[col]], na.rm = TRUE)
  listings_clean[[col]][is.na(listings_clean[[col]])] = med_val
}

listings_clean$number_of_baths[is.na(listings_clean$number_of_baths)] = median(listings_clean$number_of_baths, na.rm = TRUE)

median_exp = median(listings_clean$host_experience_years, na.rm = TRUE)
listings_clean$host_experience_years[is.na(listings_clean$host_experience_years)] = median_exp

outlier_cols = c(
  "maximum_nights", 
  "minimum_nights", 
  "beds", 
  "bedrooms", 
  "reviews_per_month"
)

for (outlier in outlier_cols){
  cap_value = quantile(listings_clean[[outlier]], 0.99, na.rm = TRUE)
  listings_clean[[outlier]] = ifelse(listings_clean[[outlier]] > cap_value, cap_value, listings_clean[[outlier]])
}

set.seed(123)
train_size = floor(0.7 * nrow(listings_clean))
train_rows = sample(seq_len(nrow(listings_clean)), size = train_size)

train = listings_clean[train_rows, ]
test = listings_clean[-train_rows, ]

cols_to_remove_due_to_constancy = c("host_experience_years") 

train = train[ , !(names(train) %in% cols_to_remove_due_to_constancy)]
test = test[ , !(names(test) %in% cols_to_remove_due_to_constancy)]

logistic_regression = glm(is_good_listing ~ ., data = train, family = binomial(link="logit"))
summary(logistic_regression)

probability = predict(logistic_regression, test, type = "response")
classification_glm = ifelse(probability > 0.6, "Good Listing", "Bad Listing")

conf_matrix = table(Predicted = classification_glm, Actual = test$is_good_listing)
print(conf_matrix)

TP = conf_matrix["Good Listing", "Good Listing"]
TN = conf_matrix["Bad Listing", "Bad Listing"]
FP = conf_matrix["Good Listing", "Bad Listing"]
FN = conf_matrix["Bad Listing", "Good Listing"]

accuracy_glm = (TP + TN) / sum(conf_matrix)
precision_glm = TP / (TP + FP)
recall_glm = TP / (TP + FN)
f1_score_glm = 2 * ((precision_glm * recall_glm) / (precision_glm + recall_glm))

print(paste("V1 Accuracy: ", round(accuracy_glm, 4)))
print(paste("V1 F1 Score: ", round(f1_score_glm, 4)))

library(corrplot)

numeric_vars = train[sapply(train, is.numeric)]
cor_matrix = cor(numeric_vars, use = "complete.obs")

corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.cex = 0.6, 
         tl.srt = 45,
         diag = FALSE)

cols_to_drop_v2 = c(
  "number_of_baths",
  "bedrooms",
  "beds",
  "number_of_reviews",
  "review_scores_accuracy",
  "review_scores_cleanliness",
  "review_scores_checkin",
  "review_scores_communication",
  "review_scores_value",
  "Safety_Index",                
  "neighbourhood_prosperity_tier", 
  "host_response_rate_missing",
  "host_total_listings_count"
)

train_v2 = train[ , !(names(train) %in% cols_to_drop_v2)]
test_v2 = test[ , !(names(test) %in% cols_to_drop_v2)]

logistic_regression_v2 = glm(is_good_listing ~ ., data = train_v2, family = binomial(link="logit"))
summary(logistic_regression_v2)

probability_v2 = predict(logistic_regression_v2, test_v2, type = "response")
classification_glm_v2 = ifelse(probability_v2 > 0.5, "Good Listing", "Bad Listing")

conf_matrix_v2 = table(Predicted = classification_glm_v2, Actual = test_v2$is_good_listing)
print(conf_matrix_v2)

TP_2 = conf_matrix_v2["Good Listing", "Good Listing"]
TN_2 = conf_matrix_v2["Bad Listing", "Bad Listing"]
FP_2 = conf_matrix_v2["Good Listing", "Bad Listing"]
FN_2 = conf_matrix_v2["Bad Listing", "Good Listing"]

accuracy_v2 = (TP_2 + TN_2) / sum(conf_matrix_v2)
precision_v2 = TP_2 / (TP_2 + FP_2)
recall_v2 = TP_2 / (TP_2 + FN_2)
f1_score_v2 = 2 * ((precision_v2 * recall_v2) / (precision_v2 + recall_v2))

print(paste("V2 Accuracy: ", round(accuracy_v2, 4)))
print(paste("V2 F1 Score: ", round(f1_score_v2, 4)))

train_v3 = listings_clean[train_rows, ]
test_v3 = listings_clean[-train_rows, ]

cols_to_drop_v3 = c(
  "host_experience_years",
  "Safety_Index",                   
  "neighbourhood_prosperity_tier",  
  "host_response_rate_missing",
  "number_of_baths", 
  "bedrooms",        
  "beds",            
  "host_total_listings_count" 
)

train_v3 = train_v3[ , !(names(train_v3) %in% cols_to_drop_v3)]
test_v3 = test_v3[ , !(names(test_v3) %in% cols_to_drop_v3)]

logistic_regression_v3 = glm(is_good_listing ~ ., data = train_v3, family = binomial(link="logit"))

probability_v3 = predict(logistic_regression_v3, test_v3, type = "response")
classification_glm_v3 = ifelse(probability_v3 > 0.5, "Good Listing", "Bad Listing")

conf_matrix_v3 = table(Predicted = classification_glm_v3, Actual = test_v3$is_good_listing)
print(conf_matrix_v3)

TP_3 = conf_matrix_v3["Good Listing", "Good Listing"]
TN_3 = conf_matrix_v3["Bad Listing", "Bad Listing"]
FP_3 = conf_matrix_v3["Good Listing", "Bad Listing"]
FN_3 = conf_matrix_v3["Bad Listing", "Good Listing"]

accuracy_v3 = (TP_3 + TN_3) / sum(conf_matrix_v3)
precision_v3 = TP_3 / (TP_3 + FP_3)
recall_v3 = TP_3 / (TP_3 + FN_3)
f1_score_v3 = 2 * ((precision_v3 * recall_v3) / (precision_v3 + recall_v3))

print(paste("V3 Accuracy: ", round(accuracy_v3, 4)))
print(paste("V3 F1 Score: ", round(f1_score_v3, 4)))

odds_ratios = exp(coef(logistic_regression_v3)) 
print(round(odds_ratios, 2))


library(rpart)
library(rpart.plot)

tree_v1 = rpart(is_good_listing ~ ., data = train_v3, method = "class", cp = 0.001)

rpart.plot(tree_v1, extra = 106, main = "Tree V1 (Simple)")

preds_v1 = predict(tree_v1, newdata = test, type = "class")

conf_matrix_tree_v1 = table(Predicted = preds_v1, Actual = test$is_good_listing)
print(conf_matrix_tree_v1)

TP_t1 = conf_matrix_tree_v1["Good Listing", "Good Listing"]
TN_t1 = conf_matrix_tree_v1["Bad Listing", "Bad Listing"]
FP_t1 = conf_matrix_tree_v1["Good Listing", "Bad Listing"]
FN_t1 = conf_matrix_tree_v1["Bad Listing", "Good Listing"]

acc_t1 = (TP_t1 + TN_t1) / sum(conf_matrix_tree_v1)
prec_t1 = TP_t1 / (TP_t1 + FP_t1)
rec_t1  = TP_t1 / (TP_t1 + FN_t1)
f1_t1   = 2 * ((prec_t1 * rec_t1) / (prec_t1 + rec_t1))

print(paste("Tree V1 Accuracy: ", round(acc_t1, 4)))
print(paste("Tree V1 F1 Score: ", round(f1_t1, 4)))

tree_v2 = rpart(is_good_listing ~ ., data = train, method = "class", cp = 0.007)

preds_v2 = predict(tree_v2, newdata = test, type = "class")

conf_matrix_tree_v2 = table(Predicted = preds_v2, Actual = test$is_good_listing)
print(conf_matrix_tree_v2)

TP_t2 = conf_matrix_tree_v2["Good Listing", "Good Listing"]
TN_t2 = conf_matrix_tree_v2["Bad Listing", "Bad Listing"]
FP_t2 = conf_matrix_tree_v2["Good Listing", "Bad Listing"]
FN_t2 = conf_matrix_tree_v2["Bad Listing", "Good Listing"]

acc_t2 = (TP_t2 + TN_t2) / sum(conf_matrix_tree_v2)
prec_t2 = TP_t2 / (TP_t2 + FP_t2)
rec_t2  = TP_t2 / (TP_t2 + FN_t2)
f1_t2   = 2 * ((prec_t2 * rec_t2) / (prec_t2 + rec_t2))

print(paste("Tree V2 Accuracy: ", round(acc_t2, 4)))
print(paste("Tree V2 F1 Score: ", round(f1_t2, 4)))
rpart.plot(tree_v2, extra = 106, main = "Tree V2 (Complex)")

printcp(tree_v2)
plotcp(tree_v2)
