
# The first stage, will consist in performing a data manipulation

# 1.Data Manipulation

  ## 1.1 File Opening and defining our Data frames

  library(dplyr)
  library(ggplot2)
  

  sales_df <- tryCatch(
    {
      read.csv('C:/test_input/sales_data.csv', sep = ";")
    },
    error = function(e) {
      # If an error occurs, try to read from the second path
      read.csv('C:\\test_input\\sales_data.csv', sep = ";")
    }
  )
  
  store_coordinates <- tryCatch(
    {
      read.csv('C:/test_input/store_master.csv', sep = ";")
    },
    error = function(e) {
      read.csv('C:\\test_input\\store_master.csv', sep = ";")
    }
  )

## 1.2 Visualize the structure and formats of the Data

  names(sales_df)
  for(i in 1:ncol(sales_df)) {
    print(class(sales_df[2,i]))
  }
  
  
  names(store_coordinates)
  for(i in 1:ncol(store_coordinates)) {
    print(class(store_coordinates[2,i]))
  }
  

# 1.3 A few things came up when looking at the data that require attention:

  ## 1.3.1 The format of the 'Unit Price' is being read as a character, due to the comma, so it needs to be subbed by a period.

  sales_df$unit_price <- as.numeric(gsub(",", ".", sales_df$unit_price))

  ## 1.3.2 The Data is not sorted, to make it a bit more comprehensive, it should be sorted in an ascending manner on the Date Attribute.
  
  sales_df <- arrange(sales_df,date)  
  sales_df$monetary_value <- sales_df$qty * sales_df$unit_price

  # Coordinates might be a long shot to try without more socio-economic context of the area, might be useful for holidays - seasonality/special, seasons - seasonality/weather
  store_coordinates <- arrange(store_coordinates,store)
  
  ## Tried using the coordinates, however not working through this method / package, might require API.
  # store_coordinates$geo_location <- revgeo(longitude=store_coordinates$longitude, latitude=store_coordinates$latitude, provider='photon', output='frame')
  
  # We could make a better in depth adaptation of the forecasting if the output is analyzed under the following items :
    ## Macro economic indicators/indexes.
    ## The industry that these stores belong to.

  # Get the unique categories, for further usage if required
  
  category_df <- sales_df[!duplicated(sales_df$item),c("item","item_category")]
  
  # 2 Understand the Data that it's available
  
  ## 2.1 Digging into the nature and structure of the data, some conclusions can be drawn.
  
  ## Some Variables, have different nature, understanding this nature is key to develop a plan in how to analyze the data
  
  # Store: Finite, discrete, great to acquire a list of the unique Stores, as each store intrinsically will have a unique behavior,
  ## For the store, we have the coordinates on another Data frame, which we can use, depending on the season we can determine behavior of customers, as seasons are linked with geolocation and dates.
  
  store_volumes <- aggregate(qty ~ store, data=sales_df,sum)
  store_volumes <- arrange(store_volumes,qty)
  
  item_volumes <- aggregate(qty ~ item, data=sales_df,sum)  
  item_volumes <- arrange(item_volumes,qty)
  
  item_popularity <- as.data.frame(table(sales_df$item))
  colnames(item_popularity) <- c("item","item_unique_transactions")
  
  item_monetary_value_vol <- aggregate(monetary_value ~ item, data=sales_df,sum)
  
  # Determining the 10 products that would be interesting to analyze
  
  ## Products that are most relevant to the stores, depending on the strategy intended (Impulse sales to penetrate new markets [Low Monetary Value/High Sales Volume], Profitabilty [High Monetary Value/Normal-High Sales Volume], etc)
  ## In order to see which items are most relevant to each store, it would be good to determine the top item by Sales Volume, by Moneraty Volume (qty*price_unit), depending on the strategy, the ponderation on each to take further decisions would be different and a last
  
  ## First and Last sell date for each product, the diference in time from these 2 dates, to see if discontinued, new product, etc.
  
  sales_dates <- aggregate(date ~ item, data = sales_df, FUN = function(x) c(min = min(x), max = max(x)))
  sales_dates$Time_Diff <- as.Date(sales_dates$date[,"max"]) - as.Date(sales_dates$date[,"min"])
  sales_dates <- arrange(sales_dates,desc(Time_Diff))
  
  avg_time_diff <- mean(sales_dates$Time_Diff, na.rm = TRUE)
  
  ## There is a lot of context missing for this analysis, for example, the sales that are stated on the data, are for batches of products, in which these "stores" purchase from a supplier (maybe we being the supplier), in this case, the purchase maybe it is being made planning ahead the sales in the upcoming future for this specific "store", Lead times would be an intrinsic variable they would be taking in consideration to order this from the supplier (us or the client), so in order to make a valid estimation, we need to consider this variable as a factor that would be part of our client's behaviour as a top layer variable to include before the layer in which we determine the behavior consumer of these products..
  
  ## In the table of sales_dates we can see that there are several products discontinued or unknown context which not enough information is provided, so those will be discarded first.
  
  ## For this analysis, the focus will be on products that are ACTIVE, by discarding all those that have not gotten a sale 1 week prior the last date of the dataset.
  
  date_of_the_last_sell <- as.Date(max(sales_dates$date[,"max"],na.rm=TRUE))
  sales_dates <- subset(sales_dates,date[,"max"]>= (date_of_the_last_sell-7))
  
  ## In order for the Forecasting Time-Series Method I would like to use, for better results I would include products with Low CV (Coefficient of Variability) over time with the sales, meaning stability and also with a above average mean Volume in sales.
  
  mean_qty <- aggregate(qty ~ item,data=sales_df,FUN = mean, na.rm=TRUE)
  std_qty <- aggregate(qty ~ item,data=sales_df,FUN = sd, na.rm=TRUE)
  qty_statistics <- merge(mean_qty,std_qty,by="item")
  qty_statistics$CV <- qty_statistics$qty.y/qty_statistics$qty.x
  
  items_dataframe <- merge(qty_statistics,sales_dates,by="item")
  
  items_dataframe <- merge(items_dataframe,item_monetary_value_vol,by="item")
  items_dataframe <- merge(items_dataframe,item_popularity,by="item")
  
  items_dataframe <- merge(items_dataframe,category_df,by="item",all.x = TRUE)
  items_dataframe <- arrange(items_dataframe,desc(monetary_value))
  
  
  # Calculations of Averages
  
  avg_mean_monetary_val <- mean(items_dataframe$monetary_value, na.rm = TRUE)
  avg_mean_sales <- mean(items_dataframe$qty.x, na.rm = TRUE)
  avg_item_transactions <- mean(items_dataframe$item_unique_transactions,na.rm = TRUE)
  
  if (items_dataframe$qty.x[1]>= avg_mean_sales) {
    list_special_items <- list(items_dataframe$item[1]) # Special Condition 1
  }
  items_dataframe <- arrange(items_dataframe,desc(item_unique_transactions))
  list_special_items <- c(list_special_items,items_dataframe$item[1]) # Special Condition 2
  items_dataframe <- arrange(items_dataframe,desc(monetary_value))
  
  # Creation of filtered df
  
  low_cv_above_mean_df <- subset(items_dataframe,CV<=0.5 & qty.x > avg_mean_sales )
  low_cv_above_mean_df <- arrange(low_cv_above_mean_df,CV)
  
  
  
 
  ## The final selection for the items occurs here
  if ( items_dataframe$CV[1] >= 0.5 & items_dataframe$qty.x[1]>= avg_mean_sales) {
    list_of_10_items <- list(head(low_cv_above_mean_df$item,8))
    list_of_10_items <- c(list_of_10_items,list_special_items)
    print("The highest Valid* Monetary Valued item was added")
  } else {
    list_of_10_items <- head(low_cv_above_mean_df$item,10)
    if (items_dataframe$item[1] %in% low_cv_above_mean_df$item[1:10] ) {
      print("The highest Valid* Monetary Valued item was included in the top 10 CV")
    } else {
      print("The highest Valid* Monetary Valued item was Not* included in the top 10 CV")
    }
    
  }
  
  # Summary of Conditions:
  # 1- Sold at least a week prior to the end of the data
  # 2- Relative Low CV
  # 3- Sales Volume above sample's avg
  # 4- Special: added the highest valid* Monetary Valued item, as it has higher probability to impact on the dataset revenue as a whole, and also it is a interesting one to add.
  ## 4.1 - Special 2: added the item with the highest unique transactions, as it may be synomym for popularity among distinct regions, which is also a product of high importance.
  
  # I decided these conditions, without much context to be the most focused objetively on the behavior of the market with these items, and how this market behavior differentiates them between another.
  # The category I decided to not use, as I have no context or data of how the categorization was made, if it was empirical or clustered by it's variables (ie. Lead Times, Rotation, Importance "Z" product, etc), and given the time I could not think of a way to provide a substantial insight to the data via unique categories.
  
  list_of_10_items<- unlist(list_of_10_items)
  print(list_of_10_items)
  
  
  # In order to proceed objectively with the analysis, the season variable needs to be defined and added, however I tried integrating the region from the coordinates given, was unsuccessful, given the time and my abilities using these libraries at the short moment.
  
  # Ideas for improving the analysis: As we have qtys and unique item ids, we can find correlations in given timeframes within these items. (Complex)
  # Clustering of the Items, maybe add a column for seasonality (by time frames and geolocation of the store ie, store 43 has winter within march and september due to the geolocation, winter is 1) in numeric format, to replace the dates when making clusters.
  # Correlation between variables, for this you need to be careful to use specific time frames to do this analysis (Complex)
  # Easy things to get: volumes sold per item per store (Maybe popularity as 1 observation might be considered a unique ticket)
  # A good method to forecast would be the Holt-Winters given the sizes of the samples, and Number of Observations
  
  
  # 3. Forecasting Model
  
  ## 3.1 The Forecasting model I would like to use for this, is the Holt-Winters one, as it include seasonality and trends, as a triple exponential smoothed.
  ## The Size of the sample adapts to this model, also the volumes, however as the stores are most likely located in different regions,so we will assume that the seasonality for each is different, so a different model or set of constants (alpha, beta and gamma) needs to be used in all of combinations in where these products are sold.
  
  models_list<-list()
  forecast_df <- data.frame(store_var = character(),item_var = character(),date = as.Date(character()),forecast_qty = numeric())
  forecast_list <- list()

  
  for (item_var in list_of_10_items) {
    for (store_var in unique(sales_df$store)) {
      
      ### A df to store the data from the items and matching stores.
      
      store_item_df <- subset(sales_df,store==store_var & item==item_var)
      
      # To avoid a time series error.
      if (nrow(store_item_df) < 730) {
        next
      }
      
      store_item_df_time_series <- ts(store_item_df$qty, start=c(2017, 3, 15), frequency=365)
      hw_model <- HoltWinters(store_item_df_time_series)
      models_list[[paste(store_var, item_var, sep="_")]] <- hw_model
      forecast <- predict(hw_model, n.ahead = 30, prediction.interval = TRUE)
      forecast_qty = forecast[,1]
      forecast_list <- c(item_var,store_var,forecast_qty)
      forecast_results <- data.frame(store = store_var,item = item_var,date = seq(as.Date("2019-03-30"), by="day", length.out=30),forecast_qty = forecast[,1])
      print("forecast_results passed")
      forecast_results$forecast_qty <- as.numeric(forecast_results$forecast_qty)
      forecast_df$forecast_qty <- as.numeric(forecast_df$forecast_qty)
      forecast_df <- rbind(forecast_df, forecast_results)
      
      print("Success adding in forecast_df")
    }
  }
  
  # 4 Data Visualization of Results 
  ## Merging the results into the actual database and only keeping what is needed.
  
  sales_subset<-sales_df$item %in% list_of_10_items
  sales_subset <- subset(sales_df, date >= as.Date("2019-03-25") & date <= as.Date("2019-03-30"))
  sales_subset <- sales_subset[, -c(5,6,7)]
  colnames(sales_subset)[colnames(sales_subset) == "qty"] <- "forecast_qty"
  
  sales_subset <- sales_subset[, c("store", "item", "date","forecast_qty")]
  sales_subset$date <- as.Date(sales_subset$date)
  
  MAPE <- function(actual, forecast) {
    return(mean(abs((actual - forecast) / actual)) * 100)
  }
  actual_values <- sales_subset$qty 
  forecasted_values <- forecast_df$forecast_qty  
  mape_calculation <- MAPE(actual_values, forecasted_values)
  print(mape_calculation)
  

  sales_forecast <- bind_rows(sales_subset, forecast_df)
  sales_forecast$item <- factor(sales_forecast$item, levels = list_of_10_items)
  sales_forecast$date <- as.Date(sales_forecast$date)
  sales_forecast$type <- ifelse(sales_forecast$date <= as.Date("2019-04-01"), "actual", "forecast")
  
  start_date <- as.Date("2019-03-25")
  end_date <- as.Date("2019-04-30")
  ggplot(sales_forecast, aes(x = date, y = forecast_qty, color = item, linetype = type, width = 15, height = 8)) +
    geom_line() +
    geom_point() +
    labs(title = "Sales and Forecast", x = "Date", y = "Quantity") +
    scale_color_discrete(name = "Item") +
    scale_linetype_discrete(name = "Type") +
    theme_minimal() +
    xlim(start_date, end_date) +
    scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Final Result
  
  sales_forecast_without_na <- na.omit(sales_forecast)
  final_result_df <- aggregate(sales_forecast_without_na$forecast_qty ~ sales_forecast_without_na$date + sales_forecast_without_na$item, FUN = sum)
  final_result_df <- arrange(final_result_df, final_result_df$date)
  print(final_result_df)
  
# In conclusion, the forecast seems to have some sense, clearly it shows some major errors in some periods (forecasted qtys below 0, however only 5 entries like this)
  ## Overall, the handling of R was a bit uneasy at first, made a lot of research to advance and the packages of the Holt Winters which is the method I wanted to use given my expertise on it and previous usage (built by myself on other language), I could not figure out 100% on how to adapt it to the dataset to avoid these errors.
  ## The final results show mistakes that need to be investigated.
  ## I believe there is a lot more information to be extracted and conclusions to be drawn from this dataset, which would lead to really interesting reccomendations and actions points to take to the stakeholders from the Companies involved in this, as there are insights to improve the whole supply chain.
  ## Future adjustments: Error detection, get rid of empty values, transform the dates into a weekly timeframes and not daily, detect "anomalies" in data that can be discarded to improve the quality of the output.
  ## If a inventory model is requested, then a cluster analysis would be required, Pearson's correlation, among other statistical indicators to determine the level of service (z) in which the companies would to operate.
  ## Thank you for the Opportunity.
  
  print("In conclusion, the forecast seems to have some sense, clearly it shows some major errors in some periods (forecasted qtys below 0, however only 5 entries like this). Overall, the handling of R was a bit uneasy at first, made a lot of research to advance and the packages of the Holt Winters which is the method I wanted to use given my expertise on it and previous usage (built by myself on other language), I could not figure out 100% on how to adapt it to the dataset to avoid these errors.

The final results show mistakes that need to be investigated. I believe there is a lot more information to be extracted and conclusions to be drawn from this dataset, which would lead to really interesting recommendations and actions points to take to the stakeholders from the Companies involved in this, as there are insights to improve the whole supply chain.

Future adjustments include error detection, getting rid of empty values, transforming the dates into a weekly timeframes and not daily, and detecting \"anomalies\" in data that can be discarded to improve the quality of the output. If an inventory model is requested, then a cluster analysis would be required, along with Pearson's correlation, among other statistical indicators to determine the level of service (z) in which the companies would to operate.

Thank you for the Opportunity :)")

