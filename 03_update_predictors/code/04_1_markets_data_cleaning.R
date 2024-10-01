
#.........................................................................................
### Cleaning market price data
#.........................................................................................
# Function to process markets data -------------------------------------------
f_process_markets_data <- function(path) {
  # Identify raw data file names
  mfiles <- list.files(path, full.names = TRUE)
  
  # Read each file into a list
  mdat <- lapply(mfiles, read.csv)
  
  # Check consistency of column names
  mnames <- lapply(mdat, colnames)
  lapply(mnames, function(x) {
    which(!unique(unlist(mnames)) %in% x)
  })
  mnames[[1]][83]
  mnames[[9]][33]
  
  # Fix inconsistent column names
  mdat[[9]] <- mdat[[9]] |> dplyr::rename(WaterDrum = WaterDrum200L)
  mdat[[10]] <- mdat[[10]] |> dplyr::rename(WaterDrum = WaterDrum200L)
  
  # Row bind all datasets
  mdat <- do.call(rbind, mdat)
  
  return(mdat)
}


#...................................
## Select columns of interest and change column names

# Get and process markets data
mdat <- f_process_markets_data("03_update_predictors/data/markets_fsnau/") 

# Metadata on FSNAU markets
markets_fsnau <- rio::import("03_update_predictors/data/market_fsnau_type.xlsx") 

# Equivalence between cereal Kg and Kcal
markets_kcal_equivalents <- rio::import("03_update_predictors/data/market_kcal_equivalents.xlsx")

analysis_strata <- mast::som_admin2
#analysis_strata <- rio::import('00_overall_data/som_admin2.xlsx')

# Keep only some columns
cols_geotime <- c("District", "Market", "Year", "Month")
cols_prices <- c("WhiteSorghum1kg", "RedSorghum1kg", "WhiteMaize1kg", "YellowMaize1kg",
                 "WheatFlour1kg", "ImportedRedRice1kg", "GoatLocalQuality", "DailyLaborRate", "WaterDrum")

mdat <- mdat[, c(cols_geotime, cols_prices)]

# dplyr::rename some columns for downstream consistency
mdat <- mdat |> dplyr::rename(
  district_fsnau = District, market = Market,
  y = Year, m = Month
)
cols_geotime <- c("district_fsnau", "market", "y", "m")

#...................................
## Change formats and check for duplicates / missing records

mdat <- mdat |>
  # Replace month and year with their corresponding numeric values
  dplyr::mutate(
    m = match(m, month.name),
    y = as.numeric(y)
  ) |>
  
  # Replace "-" values with NA in all price columns
  dplyr::mutate(dplyr::across(
    .cols = all_of(cols_prices),
    .fns = ~ dplyr::na_if(., "-")
  )) |>
  
  # Convert character values to numeric in price columns
  dplyr::mutate(dplyr::across(
    .cols = tidyselect::all_of(cols_prices),
    .fns = function (x) {as.numeric(x)}
  )) |>
  
  # Remove rows where the 'district_fsnau' column is an empty string
  dplyr::filter(district_fsnau != "") |>
  
  # Remove leading and trailing whitespace from the 'market' column
  dplyr::mutate(market = trimws(market)) |>
  
  # Remove duplicate rows (2022 is duplicated)
  unique() |>
  
  # Sort data based on market, year and month
  dplyr::arrange(market, y, m)

# Deal with remaining duplicates
# any duplicates left? yes, for 2022, because duplicates differ in terms of NAs
table(duplicated(mdat[, c("market", "y", "m")]) )
# View(subset(mdat[order(mdat$market, mdat$y, mdat$m), ], y == 2022) )

# keep the version of 2022 with least NA values
mdat$n_na <- apply(mdat, 1, function (x) {sum(is.na(x))} )
mdat <- mdat[order(mdat$market, mdat$y, mdat$m, mdat$n_na), ]
mdat <- mdat[! duplicated(mdat[, c("market", "y", "m")]), ]
table(duplicated(mdat[, c("market", "y", "m")]) )
# View(subset(mdat[order(mdat$market, mdat$y, mdat$m), ], y == 2022) )

#...................................
## Identify and deal with outliers

# Dataset of ranges and quantiles by year, for each price variable
outliers <- data.frame(expand.grid(cols_prices, sort(unique(mdat$y))) )
colnames(outliers) <- c("commodity", "y")
outliers[, c("min", "2.5% centile", "median", "97.5% centile", "max")] <- NA

for (i in 1:nrow(outliers) ) {
  x <- subset(mdat, y == outliers[i, "y"])
  x <- na.omit(x[, as.character(outliers[i, "commodity"])])
  outliers[i, c("min", "2.5% centile", "median", "97.5% centile", "max")] <-
    c(min(x), quantile(x, c(0.025, 0.050, 0.975)), max(x))
}
outliers <- outliers[order(outliers$commodity, outliers$y), ]
# View(outliers)

# Identify yearly price series where there may be spurious values, based on ratio of min/max to quantiles
outliers$flagmin <- ifelse((outliers$min / outliers$`2.5% centile`) < 0.2, TRUE, FALSE)
outliers$flagmax <- ifelse((outliers$max / outliers$`97.5% centile`) > 5, TRUE, FALSE)
# View(subset(outliers, flagmin == TRUE | flagmax == TRUE))

# Revise allowed min and max to within above thresholds
outliers$min_ok <- outliers$`2.5% centile` * 0.2
outliers$max_ok <- outliers$`97.5% centile` * 5

# Set any values not within the thresholds to NA
for (i in 1:nrow(outliers)) {
  x <- as.character(outliers[i, "commodity"])
  mdat[which(mdat$y == outliers[i, "y"] & ! dplyr::between(mdat[, x],
                                                    outliers[i, "min_ok"], outliers[i, "max_ok"]) ), x] <- NA
}

#...................................
## Merge with market metadata

# Check that all markets and FSNAU districts are featured in the metadata
base::setdiff(unique(markets_fsnau$market), unique(mdat$market))

# Check that there are no duplicate market names
table(duplicated(markets_fsnau$market) )

# Merge
mdat <- merge(mdat, markets_fsnau, by = c("district_fsnau", "market"), all.x = TRUE) 
mdat <- subset(mdat, select = - n_na)

# Market names to sentence case
mdat$market <- stringr::str_to_title(mdat$market)


#.........................................................................................
### Generating terms of trade indicators by district and month
#.........................................................................................

#...................................
## Generate market time series

# Identify start and end of time series
y_start <- min(mdat$y)
y_end <- max(mdat$y)
m_start <- min(mdat[which(mdat$y == y_start), "m"])
m_end <- max(mdat[which(mdat$y == y_end), "m"])

# Create a time unit variable tm (from month 1 to month T of period)
tm <- seq(1, (( y_end - y_start ) * 12 + m_end - m_start + 1 ), 1)

# Create a time series of market-year-months
ts <- expand.grid(sort(unique(mdat$market)), tm)
colnames(ts) <- c("market", "tm")

# Work out corresponding year and month as well as date values
ts[, "y"] <- floor( (ts[, "tm"] + m_start - 2) / 12) + y_start
ts[, "m"] <- (ts[, "tm"] + m_start - 1) - (ts[, "y"] - y_start) * 12
ts[, "date"] <- lubridate::ymd(paste(ts$y, ts$m, "15", sep = "-"))

# Sort time series
ts <- ts[order(ts[, "market"], ts[, "tm"]), ]

# Merge with market data to ensure each market-year-month is included
mdat <- merge(ts, mdat, by = c("market", "y", "m"), all.x = TRUE)

#...................................
## Visualise completeness of each commodity for each market, and apply completeness threshold

# Generate completeness database
complete <- mdat[, c("market", "y", "m", "date", cols_prices)]
complete[, cols_prices] <- apply(complete[, cols_prices], c(1, 2), is.na)
complete[, cols_prices] <- ! complete[, cols_prices]

# Aggregate by market and year
complete <- aggregate(complete[, cols_prices], by = complete[, c("market", "y")], FUN = mean, na.rm = TRUE)

# Reshape long
complete <- data.table::melt(complete, id.vars = c("market", "y"), measure.vars = cols_prices)
colnames(complete) <- c("market", "year", "commodity", "completeness")

# Apply to each market-commodity pair the completeness threshold of 75% for each year
complete$pass <- ifelse(complete$completeness >= 0.75, TRUE, FALSE)

# For which markets and commodities is the threshold met for each year since 2013?
complete_2013 <- subset(complete, year >= 2013)
complete_2013 <- aggregate(complete_2013$pass, by = complete_2013[, c("market", "commodity")], FUN = mean)
colnames(complete_2013) <- c("market", "commodity", "pass_fraction")
complete_2013$pass_all_y <- ifelse(complete_2013$pass_fraction == 1, TRUE, FALSE)

# Plot completeness across years
# plot <- ggplot(data = complete_2013, aes(x = commodity, y = market, fill = pass_all_y)) +
#   geom_tile(alpha = 0.5, colour = "white") +
#   theme_bw() +
#   scale_fill_manual(values = palette_cb[c(7,4)]) +
#   geom_text(aes(label = round(pass_fraction, digits = 2) ), color = "grey20", size = 3) +
#   labs(fill = ">= 75% complete for every year since 2013") +
#   theme(legend.position = "top", panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1))
# 
# # plot
# ggsave(paste(dir_path, "plot/output/predictors/som_market_price_completeness.png", sep=""),
#        dpi = "print", units = "cm", width = 20, height = 45)

# Set price values to NA for market-commodity time series that do not pass the completeness test
for (i in 1:nrow(complete_2013)) {
  if (complete_2013[i, "pass_all_y"] == FALSE) {
    mdat[which(mdat$market == as.character(complete_2013[i, "market"]) ),
         as.character(complete_2013[i, "commodity"])] <- NA
  }
}


#...................................
## Compute terms of trade for each market

# Identify staple cereal columns
cols_staple <- cols_prices[! cols_prices %in% c("GoatLocalQuality", "DailyLaborRate", "WaterDrum")]

# Compute Kg of each cereal that a daily wage buys
mdat[, paste("tot_w_", cols_staple, sep = "")] <- mdat[, "DailyLaborRate"] / mdat[, cols_staple]

# Compute Kg of each cereal that a medium quality goat buys
mdat[, paste("tot_g_", cols_staple, sep="")] <- mdat[, "GoatLocalQuality"] / mdat[, cols_staple]

# Transform to Kcal equivalents
for (i in cols_staple) {
  x <- as.numeric(markets_kcal_equivalents[which(markets_kcal_equivalents$item == i), "milling_recovery"]) *
    as.numeric(markets_kcal_equivalents[which(markets_kcal_equivalents$item == i), "kcal_1kg"])
  mdat[, paste("tot_w_", i, sep = "")] <- mdat[, paste("tot_w_", i, sep = "")] * x
  mdat[, paste("tot_g_", i, sep = "")] <- mdat[, paste("tot_g_", i, sep = "")] * x
}

# Weighted mean of available cereals
# (weights are the actual ToT values for each cereal, hence the squaring - see methods notes)
mdat[, "tot_w"] <- rowSums(mdat[, grep("tot_w_", colnames(mdat))] ^ 2, na.rm = TRUE) /
  rowSums(mdat[, grep("tot_w_", colnames(mdat))], na.rm = TRUE)
mdat[, "tot_g"] <- rowSums(mdat[, grep("tot_g_", colnames(mdat))] ^ 2, na.rm = TRUE) /
  rowSums(mdat[, grep("tot_g_", colnames(mdat))], na.rm = TRUE)

#...................................
## Compute weighted mean ToT and water price for each district

# Attribute to each market a weight of 3 for main markets and 1 for rural markets
mdat[, "market_wt"] <- ifelse(mdat[, "market_type"] == "main", 3, 1)

# Compute weighted market quantities
mdat[, "tot_w_part"] <- mdat[, "tot_w"] * mdat[, "market_wt"]
mdat[, "tot_g_part"] <- mdat[, "tot_g"] * mdat[, "market_wt"]
mdat[, "water_price_part"] <- mdat[, "WaterDrum"] * mdat[, "market_wt"]

# Aggregate to district and take weighted means
mdat_dis <- aggregate(mdat[ , c("tot_w_part", "tot_g_part", "water_price_part", "market_wt")],
                      by = mdat[, c("district", "m", "y")], FUN = sum, na.rm = TRUE )
mdat_dis[, "tot_wage_cereal"] <- mdat_dis[, "tot_w_part"] / mdat_dis[, "market_wt"]
mdat_dis[, "tot_goat_cereal"] <- mdat_dis[, "tot_g_part"] / mdat_dis[, "market_wt"]
mdat_dis[, "water_price"] <- mdat_dis[, "water_price_part"] / mdat_dis[, "market_wt"]

# Replace 0 values with NA (these result from rows with all missing ToT/water price values)
for (i in c("tot_wage_cereal", "tot_goat_cereal", "water_price")) {
  mdat_dis[which(mdat_dis[, i] == 0), i] <- NA
}

# Remove unnecessary columns
mdat_dis <- mdat_dis[, ! colnames(mdat_dis) %in% c("tot_w_part", "tot_g_part", "water_price_part", "market_wt")]

# save the unprocessed data for 
# later missingness plotting
mdat_dis |> 
  saveRDS("03_update_predictors/output/som_tot_preprocess.rds")

#.........................................................................................
### Performing imputation, smoothing and lagging
#.........................................................................................

#...................................
## Fill in the time series to feature all districts and regions

# Create a time unit variable tm (from month 1 to month T of period)
tm <- seq(1, (( y_end - y_start ) * 12 + m_end - m_start + 1 ), 1)

# Create a time series of district-year-months
ts <- expand.grid(sort(unique(analysis_strata$district)), tm)
colnames(ts) <- c("district", "tm")

# Work out corresponding year and month as well as date values
ts[, "y"] <- floor( (ts[, "tm"] + m_start - 2) / 12) + y_start
ts[, "m"] <- (ts[, "tm"] + m_start - 1) - (ts[, "y"] - y_start) * 12
ts[, "date"] <- lubridate::ymd(paste(ts$y, ts$m, "15", sep = "-"))

# Sort time series
ts <- ts[order(ts[, "district"], ts[, "tm"]), ]

# Merge with ToT / water price data to ensure each district-year-month is included
ts <- merge(ts, mdat_dis, by = c("district", "y", "m"), all.x = TRUE)

# Add region (admin1)
ts <- merge(ts, analysis_strata, by = "district", all.x = TRUE)

#...................................
## Figure out which districts don't have any ToTw/g data from 2013 onwards

# Generate completeness database
cols_tot <- c("tot_wage_cereal", "tot_goat_cereal")
complete <- ts[, c("district", "y", "m", cols_tot)]
complete[, cols_tot] <- apply(complete[, cols_tot], c(1, 2), is.na)
complete[, cols_tot] <- ! complete[, cols_tot]

# Aggregate by district and year
complete <- aggregate(complete[, cols_tot], by = complete[, c("district", "y")], FUN = mean, na.rm = TRUE)

# Reshape long
complete <- data.table::melt(complete, id.vars = c("district", "y"), measure.vars = cols_tot)
colnames(complete) <- c("district", "year", "which_tot", "completeness")

# Apply to each district-ToT pair the completeness threshold of 75% for each year
complete$pass <- ifelse(complete$completeness >= 0.75, TRUE, FALSE)

# To what extent is the threshold met for each year since 2013?
complete_2013 <- subset(complete, year >= 2013)
complete_2013 <- aggregate(complete_2013$pass, by = complete_2013[, c("district", "which_tot")], FUN = mean)
colnames(complete_2013) <- c("district", "which_tot", "pass_fraction")

# table of completeness since 2013
table(complete_2013$pass_fraction)
# only 0 or 1, which makes imputation more straightforward;
# however, if 0 < pass fraction < 1 will need to think about something different

# Which districts require imputation
# for ToT (wage)
dist_imp_tot_w <- subset(complete_2013, which_tot == "tot_wage_cereal" & pass_fraction == 0)$district

# for ToT (goat)
dist_imp_tot_g <- subset(complete_2013, which_tot == "tot_goat_cereal" & pass_fraction == 0)$district

#...................................
## Manually impute ToT values from 2013 in districts without ToT values

# Sort dataset
ts <- ts[order(ts$district, ts$y, ts$m), ]



ts
# For ToT (wage)
for (i in as.character(dist_imp_tot_w) ) {
  
  # which region the district is in
  region_i <- analysis_strata[which(analysis_strata$district == i), "region"]
  
  # mean of values from other districts in the region
  region_values <- ts[which(ts$y >= 2013 & ts$region == region_i ), c("y", "m", "tot_wage_cereal")]
  region_values <- aggregate(region_values$tot_wage_cereal, by = region_values[, c("y", "m")], FUN = mean, na.rm = TRUE)
  region_values <- region_values[order(region_values$y, region_values$m), ]
  
  # mean of values from districts outside the region
  non_region_values <- ts[which(ts$y >= 2013 & ts$region != region_i ), c("y", "m", "tot_wage_cereal")]
  non_region_values <- aggregate(non_region_values$tot_wage_cereal, by = non_region_values[, c("y", "m")], FUN = mean, na.rm = TRUE)
  non_region_values <- non_region_values[order(non_region_values$y, non_region_values$m), ]
  
  # weighted mean of within-region (weight = 0.7) and outside-region (weight = 0.3) values, former is not NA
  if (all(is.na(region_values$x))) {x <- non_region_values$x} else
  {x <- region_values$x * 0.7 + non_region_values$x * 0.3}
  
  # attribute weighted mean to district
  ts[which(ts$y >= 2013 & ts$district == i ), "tot_wage_cereal"] <- x
}

# For ToT (goat)
for (i in as.character(dist_imp_tot_g) ) {
  
  # which region the district is in
  region_i <- analysis_strata[which(analysis_strata$district == i), "region"]
  
  # mean of values from other districts in the region
  region_values <- ts[which(ts$y >= 2013 & ts$region == region_i ), c("y", "m", "tot_goat_cereal")]
  region_values <- aggregate(region_values$tot_goat_cereal, by = region_values[, c("y", "m")], FUN = mean, na.rm = TRUE)
  region_values <- region_values[order(region_values$y, region_values$m), ]
  
  # mean of values from districts outside the region
  non_region_values <- ts[which(ts$y >= 2013 & ts$region != region_i ), c("y", "m", "tot_goat_cereal")]
  non_region_values <- aggregate(non_region_values$tot_goat_cereal, by = non_region_values[, c("y", "m")], FUN = mean, na.rm = TRUE)
  non_region_values <- non_region_values[order(non_region_values$y, non_region_values$m), ]
  
  # weighted mean of within-region (weight = 0.7) and outside-region (weight = 0.3) values, former is not NA
  if (all(is.na(region_values$x))) {x <- non_region_values$x} else
  {x <- region_values$x * 0.7 + non_region_values$x * 0.3}
  
  # attribute weighted mean to district
  ts[which(ts$y >= 2013 & ts$district == i ), "tot_goat_cereal"] <- x
}


#...................................
## Smooth and plot ToT time series (only from 2013 onwards)

# Initialise smoothed columns
ts[, paste(cols_tot, "_smooth", sep = "")] <- NA

# For each district...
for (i in unique(ts$district)) {
  
  # select data
  ts_i <- ts[which(ts$y >= 2013 & ts$district == i ), c("tm", cols_tot)]
  
  # smooth and attribute ToT (wage) values
  x <- predict(smooth.spline(as.matrix(na.omit(ts_i[, c("tm", 'tot_wage_cereal')]) ), spar = 0.3), ts_i[, "tm"] )$y
  ts[which(ts$y >= 2013 & ts$district == i ), "tot_wage_cereal_smooth"] <- x
  
  # smooth and attribute ToT (goat) values
  x <- predict(smooth.spline(as.matrix(na.omit(ts_i[, c("tm", 'tot_goat_cereal')]) ), spar = 0.3), ts_i[, "tm"] )$y
  ts[which(ts$y >= 2013 & ts$district == i ), "tot_goat_cereal_smooth"] <- x
}

markets_data_clean <- ts 
