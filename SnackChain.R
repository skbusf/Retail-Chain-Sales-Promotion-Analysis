setwd("C:/Users/batch_2kjxgc7/Desktop/USF/Courses/SDM/Assignments/Snack Chain")
library(readxl)
library(dplyr)
library(lme4)
library(PerformanceAnalytics)

df_stores = read_xlsx("SnackChain.xlsx", sheet = "stores")
str(df_stores)

df_products = read_xlsx("SnackChain.xlsx", sheet = "products")
str(df_products)

df_transactions = read_xlsx("SnackChain.xlsx", sheet = "transactions")
str(df_transactions)

df3 = merge(df_transactions,df_stores, by.x = c("STORE_NUM") , by.y = c("STORE_ID"))
str(df3)

df = merge(df3,df_products, by.x = c("UPC") , by.y = c("UPC"))


dim(df)

colnames(df) = tolower(colnames(df))
str(df)

colSums(is.na(df))
df = df[!is.na(df$price),]
df = df[!is.na(df$base_price),]

unique(df$category)

df = subset(df, category != "ORAL HYGIENE PRODUCTS")
dim(df)
table(df$category, df$sub_category)
length(unique(df$upc))
length(unique(df$store_num))

df$circular = ifelse(df$feature == 1, "yes", "no") 
df$feature = NULL

df$price_reduction =ifelse(df$tpr_only == 1, "yes", "no") 
df$tpr_only = NULL

df$special_display = ifelse(df$display == 1, "yes", "no") 
df$display = NULL

df$store_name = NULL

df$parking = NULL

df$product_size = NULL

df$manufacturer = NULL

df$sub_category = NULL


# listed_date = as.Date(df$week_end_date, "%m/%d/%Y")
# 
# df$date = strftime(listed_date,"%d")
# 
# df$dayofweek = strftime(listed_date,"%A")
# df$typeofday = ifelse(df$dayofweek == "Saturday" | df$dayofweek == "Sunday", "weekend", "weekday")
# 
# df$date = as.numeric(df$date)
# 
# df$typeofDate = ifelse(df$date > 5 & df$date < 25,  "mid_month", "end_and_start")
# df$date = NULL
# df$dayofweek = NULL
df$week_end_date = NULL

#View(df)

df$msa = as.factor(df$msa)
df$store_num = as.factor(df$store_num)

df$category = as.factor(df$category)
df$category = relevel(df$category, "FROZEN PIZZA")

df$segment = as.factor(df$segment)
df$segment = relevel(df$segment, "VALUE")

str(df)

attach(df)

#visualizations

hist(df$spend)
hist(df$units)
hist(df$hhs)

p_df = df[, c(3:8, 14,15)]

cor(p_df)
chart.Correlation(cor(p_df))

#spend models
spend = glm(round(spend) ~ price_reduction + circular + special_display + size + 
             price + segment + category + city + special_display*category + price_reduction*category + 
             circular*category + special_display*segment + price_reduction*segment + circular*segment
             + description + price * description, data = df, family = poisson(link = log))

summary(spend)
library(AER)
dispersiontest(spend)

library(MASS)
spend_nb = glm.nb(round(spend) ~ price_reduction + circular + special_display + size + 
              price + segment + category + city + special_display*category + price_reduction*category + 
              circular*category + special_display*segment + price_reduction*segment + circular*segment
            + description + price * description, data = df)

dwtest(spend_nb)
summary(spend_nb)

#units models
units = glm(units ~ price_reduction + circular + special_display + size + 
              price + segment + category + state + city + special_display*category + price_reduction*category + 
              circular*category + special_display*segment + price_reduction*segment + circular*segment
              + description + price * description, data = df, family = poisson(link = log))

summary(units)  
dispersiontest(units)


units_nb = glm.nb(units ~ price_reduction + circular + special_display + size + 
                 price + segment + category + state + city + special_display*category + price_reduction*category + 
                 circular*category + special_display*segment + price_reduction*segment + circular*segment
               + description + price * description, data = df)

summary(units_nb)

dwtest(units_nb)

 
#hhs models 
hhs = glm(hhs ~ price_reduction + circular + special_display + size + 
              price + segment + category + state + city + special_display*category + price_reduction*category + 
              circular*category + special_display*segment + price_reduction*segment + circular*segment
              + description + price * description, data = df, family = poisson(link = log))

summary(hhs)
dispersiontest(hhs)

hhs_nb = glm.nb(hhs ~ price_reduction + circular + special_display + size + 
               price + segment + category + state + city + special_display*category + price_reduction*category + 
               circular*category + special_display*segment + price_reduction*segment + circular*segment
             + description + price * description, data = df)
summary(hhs_nb)

dwtest(hhs_nb)

library(stargazer)
stargazer(spend_nb, units_nb, hhs_nb, type="text", single.row=TRUE)

#Question 3, 4
products = unique(df$description)

df_pd = data.frame(matrix(ncol = 3, nrow = 0))
str(df_pd)
for(i in products){
  df_temp = subset(df ,description == i)
  round(mean(df_temp$price),3)
  round(mean(df_temp$units), 0)
  df_temp1 = data.frame(unique(df_temp$description), round(mean(df_temp$price),3), round(mean(df_temp$units), 0))
  df_pd <- rbind(df_pd,df_temp1)
}
colnames(df_pd) = c("product_name", "mean_price", "mean_qty")
View(df_pd)

df_pd1 = data.frame(matrix(ncol = 2, nrow = 0))
for(p in products){
  df_temp = subset(df, description == p)
  model = glm(units ~ price, data = df_temp, family = poisson(link = log))
  df_temp1 = data.frame(p, summary(model)$coefficients[2,1])
  df_pd1 <- rbind(df_pd1,df_temp1)
}
colnames(df_pd1) = c("product_name", "beta_coeff")

df_final_pd = merge(df_pd,df_pd1, by.x = c("product_name") , by.y = c("product_name"))

df_final_pd$price_elasticity = df_final_pd$beta_coeff * (df_final_pd$mean_price/df_final_pd$mean_qty) 

View(df_final_pd)
write.csv(df_final_pd, "C:/Users/batch_2kjxgc7/Desktop/USF/Courses/SDM/Assignments/Snack Chain/price_elasticities.csv", row.names = FALSE)

# glm(units ~ price + price_reduction + state + city + size + special_display+ 
#       segment + special_display*segment + price_reduction*segment, data = df_temp, family = poisson(link = log))
