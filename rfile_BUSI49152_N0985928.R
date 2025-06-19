#library 
install.packages("readxl")
install.packages("dplyr")
install.packages("lme4")
install.packages("ggplot2")
install.packages("rpart")
install.packages("caret")
install.packages("knitr")
install.packages("gtsummary")
install.packages("kableExtra")
install.packages("cluster")
install.packages("ggplot")
install.packages("gridExtra")
webshot::install_phantomjs()
install.packages("forecast")
install.packages("stringr")
install.packages("autoplotly")



# clean environment if you need 

rm(missing_indices)
rm(sales_decomp)
rm(sales_ts)
rm(product_names)


#Descriptive Part 

# SALE ANALYSIS

# Create "Total Sales By Year" 
total_sales_by_year <- colSums(N0985928_BUSI49152[, c("Salesin_2012", "Salesin_2011", "Salesin_2010", "Salesin_2009", "Salesin_2008", "Salesin_2007", "Salesin_2006")])
# Graph
barplot(total_sales_by_year, main="Total Sales by Year", xlab="Year", ylab="Total Sale", col="lightblue", ylim=c(0, max(total_sales_by_year)*1.1))
barplot <- barplot(total_sales_by_year, main="Total Sales by Year", xlab="Year", ylab="Total Sale", names.arg=years, col="lightblue")
barplot <- barplot(total_sales_by_year, main="Total Sales by Year", xlab="Year", ylab="Total Sale", names.arg=clean_years, col="lightblue", ylim=c(0, max(total_sales_by_year)*1.1))
clean_years <- gsub("Salesin_", "", names(total_sales_by_year))
#Value Tags
text(x = barplot, y = total_sales_by_year, labels = total_sales_by_year, pos = 3, cex = 0.8, col = "black")




# Max and Min Sales by Product

# Calculate total sales by product
total_sales_by_product <- aggregate(. ~ Product + Description + Colour, data = N0985928_BUSI49152[, c("Product", "Description", "Colour", "Salesin_2012", "Salesin_2011", "Salesin_2010", "Salesin_2009", "Salesin_2008", "Salesin_2007", "Salesin_2006")], sum)
total_sales_by_product$total_sales <- rowSums(total_sales_by_product[, 4:ncol(total_sales_by_product)])
# Find the highest and lowest selling products
highest_sales <- max(total_sales_by_product$total_sales, na.rm = TRUE)
highest_selling_products <- total_sales_by_product[total_sales_by_product$total_sales == highest_sales, c("Product", "Description", "Colour", "total_sales")]
lowest_sales <- min(total_sales_by_product$total_sales, na.rm = TRUE)
lowest_selling_products <- total_sales_by_product[total_sales_by_product$total_sales == lowest_sales, c("Product", "Description", "Colour", "total_sales")]
# Print the results
print(highest_selling_products)
print(lowest_selling_products)


library(ggplot2)
library(grid)

# Create a data frame for highest selling products
highest_selling_df <- data.frame(Product = highest_selling_products$Product,
                                 Sales = highest_selling_products$total_sales,
                                 Type = rep("Highest Selling", nrow(highest_selling_products)))

# Create a data frame for lowest selling products
lowest_selling_df <- data.frame(Product = lowest_selling_products$Product,
                                Sales = lowest_selling_products$total_sales,
                                Type = rep("Lowest Selling", nrow(lowest_selling_products)))

# Combine the data frames
products_df <- rbind(highest_selling_df, lowest_selling_df)

# Create a bar plot
ggplot(products_df, aes(x = Product, y = Sales, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Highest and Lowest Selling Products by Product Code",
       x = "Product",
       y = "Total Sales") +
  scale_fill_manual(values = c("blue", "red")) +
  geom_text(aes(label = Sales), vjust = -0.5) +
  theme_bw() +
  guides(fill=guide_legend(title=NULL))

# Highest selling product table
highest_selling_table <- data.frame(Product = highest_selling_products$Product,
                                    Description = highest_selling_products$Description,
                                    Sales = highest_selling_products$total_sales,
                                    Type = "Highest Selling")

# Lowest selling product table
lowest_selling_table <- data.frame(Product = lowest_selling_products$Product,
                                   Description = lowest_selling_products$Description,
                                   Sales = lowest_selling_products$total_sales,
                                   Type = "Lowest Selling")

# Combine the two tables
products_df <- rbind(highest_selling_table, lowest_selling_table)

# Create the plot
ggplot(products_df, aes(x = Product, y = Sales, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Sales), vjust = -0.5) +
  theme(legend.position = "none") +
  labs(x = "Product", y = "Total Sales", title = "Highest and Lowest Selling Products")

# Highest selling product graph
highest_graph <- ggplot(highest_selling_products, aes(x = Description, y = total_sales, fill = Product)) +
  geom_bar(stat = "identity") +
  ggtitle("Highest Selling Product") +
  labs(x = "Product Description", y = "Total Sales") +
  geom_text(aes(label = total_sales), vjust = -0.5)

# Print highest selling product graph
print(highest_graph)

# Lowest selling product graph
lowest_graph <- ggplot(lowest_selling_products, aes(x = Description, y = total_sales, fill = Product)) +
  geom_bar(stat = "identity") +
  ggtitle("Lowest Selling Product") +
  labs(x = "Product Description", y = "Total Sales") +
  geom_text(aes(label = total_sales), vjust = -0.5)

# Print lowest selling product graph
print(lowest_graph)

# Display both graphs side by side
grid.arrange(highest_graph, lowest_graph, ncol=2)

#end of the min max sale part#


#Colour Analysis 

total_sales_by_color <- aggregate(. ~ Colour, data=N0985928_BUSI49152[, c("Colour", "Salesin_2012", "Salesin_2011", "Salesin_2010", "Salesin_2009", "Salesin_2008", "Salesin_2007", "Salesin_2006")], sum)

par(xaxt='n')
barplot_values <- barplot(rowSums(sorted_total_sales_by_color[, 2:8]), 
                          main="Colour Popularity in Sales", 
                          xlab="Colour", 
                          ylab="Total Sale", 
                          col="lightblue",
                          ylim=c(0, max(rowSums(sorted_total_sales_by_color[, 2:8])) * 1.1),
                          yaxs="i",
                          axes = FALSE)

# Add vertical labels
text(x = barplot_values, y = -0.05 * max(rowSums(sorted_total_sales_by_color[, 2:8])), 
     labels = sorted_total_sales_by_color$Colour, 
     srt = 90, adj = c(1, 0.5), xpd = TRUE)

# Add Y-axis without X-axis labels
axis(2, 
     at=seq(0, max(rowSums(sorted_total_sales_by_color[, 2:8])) * 1.1, by=1000), 
     labels=format(seq(0, max(rowSums(sorted_total_sales_by_color[, 2:8])) * 1.1, by=1000), scientific=FALSE))

# Remove X-axis labels (numbers)
axis(1, at=barplot_values, labels=rep("", length(barplot_values)))

box() # Draw a box around the plot

text(x = barplot_values - 0.1, 
     y = rowSums(sorted_total_sales_by_color[, 2:8]), 
     labels = format(rowSums(sorted_total_sales_by_color[, 2:8]), scientific=FALSE),
     pos = 3, cex=0.8)

# Remove default X-axis labels
par(xaxt='n')


# Plotting
par(mar=c(10, 5, 3, 1))
par(plot.margin=c(2,2,2,2))
barplot_values <- barplot(rowSums(sorted_total_sales_by_color[, 2:8]), 
                          main="Colour Popularity in Sales", 
                          xlab="", 
                          ylab="Total Sale", 
                          col="lightblue",
                          ylim=c(0, max(rowSums(sorted_total_sales_by_color[, 2:8])) * 1.1),
                          yaxs="i",
                          axes = FALSE)

# Add vertical labels
text(x = barplot_values, y = -0.05 * max(rowSums(sorted_total_sales_by_color[, 2:8])), 
     labels = sorted_total_sales_by_color$Colour, 
     srt = 90, adj = c(1, 0.5), xpd = TRUE)

# Add Y-axis without X-axis labels
axis(2, 
     at=seq(0, max(rowSums(sorted_total_sales_by_color[, 2:8])) * 1.1, by=1000), 
     labels=format(seq(0, max(rowSums(sorted_total_sales_by_color[, 2:8])) * 1.1, by=1000), scientific=FALSE))

# Remove X-axis labels (numbers)
axis(1, at=barplot_values, labels=rep("", length(barplot_values)))

box() # Draw a box around the plot

text(x = barplot_values - 0.1, 
     y = rowSums(sorted_total_sales_by_color[, 2:8]), 
     labels = format(rowSums(sorted_total_sales_by_color[, 2:8]), scientific=FALSE),
     pos = 3, cex=0.8)

# Remove default X-axis labels
par(xaxt='n')

par(cex.main=10, font.main=2) # main title size and font style

title(xlab="", line=3)
mtext("Colour", side = 1, line = 8, font=2, cex.lab=1.5)


par(mfrow=c(1,1), mar=c(5, 4, 1, 1), ps=10, cex=0.7)



#end of the colour-sale analysis#


#Profit Margin Analysis 
library(tidyverse)
library(dplyr)

names(N0985928_BUSI49152)[4] <- "Powder_weight_kg"
names(N0985928_BUSI49152)[5] <- "Selling_Price_Pound"

sales_data <- select(N0985928_BUSI49152, Product, Description, Colour, Powder_weight_kg, Selling_Price_Pound, 
                     Salesin_2012, Salesin_2011, Salesin_2010)


profit_data <- sales_data %>% 
  mutate(Total_Sales = Salesin_2012 + Salesin_2011 + Salesin_2010,
         Unit_Cost = Selling_Price_Pound / Powder_weight_kg,
         Total_Cost = Unit_Cost * Total_Sales,
         Profit = Selling_Price_Pound - Total_Cost)

#max profit margin
profit_data %>% 
  arrange(desc(Profit)) %>% 
  head(1)

#min profit margin
profit_data %>% 
  arrange(Profit) %>% 
  head(1)

profit_data %>%
  arrange(Profit) %>%
  filter(Profit == min(Profit)) %>%
  select(Colour, Powder_weight_kg, Selling_Price_Pound, Total_Sales, Total_Cost, Profit)


profit_data %>%
  arrange(Profit) %>%
  select(Product, Description, Colour, Powder_weight_kg, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  head(1)




library(ggplot2)

lowest_profit <- profit_data %>%
  arrange(Profit) %>%
  select(Product, Description, Colour, Powder_weight_kg, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  head(1)

profit_data <- sales_data %>%
  mutate(Total_Sales = Salesin_2012 + Salesin_2011 + Salesin_2010,
         Unit_Cost = Selling_Price_Pound / Powder_weight_kg,
         Total_Cost = Unit_Cost * Total_Sales,
         Profit = Selling_Price_Pound - Total_Cost) %>%
  select(Product, Description, Colour, Powder_weight_kg, Selling_Price_Pound, Total_Sales, Total_Cost, Profit)



top10_profit_products <- profit_data %>%
  arrange(desc(Profit)) %>%
  select(Description, Colour, Powder_weight_kg, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  head(10)


library(tidyr)
library(dplyr)
library(knitr)

top10_profit_products %>%
  select(Description, Colour, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  gather(key = "variable", value = "value", -Description, -Colour) %>%
  unite(col = "desc_col", Description, Colour, sep = " - ") %>%
  spread(key = "variable", value = "value") %>%
  select(desc_col, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  kable()

library(ggplot2)

top10_profit_products %>%
  select(Description, Colour, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  gather(key = "variable", value = "value", -Description, -Colour) %>%
  unite(col = "description", Description, Colour, sep = " - ") %>%
  spread(key = "variable", value = "value") %>%
  select(desc_col, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  ggplot(aes(x = desription, y = Profit, fill = desc_col)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Most Profitable Products", y = "Profit (Pound)", x = "")

top10_profit_products %>%
  select(Description, Colour, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  gather(key = "variable", value = "value", -Description, -Colour) %>%
  unite(col = "description", Description, Colour, sep = " - ") %>%
  spread(key = "variable", value = "value") %>%
  select(description, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  gather(key = "variable", value = "value", -description) %>%
  ggplot(aes(x = description, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Most Profitable Products", y = "") +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73", "#F0E442"), 
                    labels = c("Selling Price", "Total Sales", "Total Cost", "Profit")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.title = element_blank())

top10_profit_products %>%
  select(Description, Colour, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  gather(key = "variable", value = "value", -Description, -Colour) %>%
  unite(col = "description", Description, Colour, sep = " - ") %>%
  spread(key = "variable", value = "value") %>%
  select(description, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  gather(key = "variable", value = "value", -description) %>%
  ggplot(aes(x = description, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = " 10 Most Profitable Products by Sales, Costs, and Profit", y = "Profit (Pound)") +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73", "#F0E442"), 
                    labels = c("Selling Price", "Total Sales", "Total Cost", "Profit")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.title = element_blank())


library(ggplot2)

library(ggplot2)
top10_profit_products %>%
  select(Description, Colour, Selling_Price_Pound, Total_Sales, Total_Cost, Profit) %>%
  unite(col = "Product", Description, Colour, sep = " - ") %>%
  arrange(desc(Profit)) %>%
  mutate(Product = factor(Product, levels = Product)) %>%
  ggplot(aes(x = Product, y = Profit, fill = Product)) +
  geom_col() +
  geom_text(aes(label = round(Profit, 2)), vjust = -0.5) + # Sayılar burada ekleniyor
  scale_fill_manual(values = palette()[1:10], name = "Product") +
  labs(title = "10 most profitable products in 2006-2012", y = "Profit (Pound)", x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


############


#Due Date Performance


library(dplyr)
library(ggplot2)

DueDatePerformance <- N0985928_BUSI49152 %>%
  select(Product, `Due Date Performance/%`)

DueDatePerformance$`Due Date Performance/%` <- ifelse(DueDatePerformance$`Due Date Performance/%` == "", NA, DueDatePerformance$`Due Date Performance/%`)


DueDatePerformance <- DueDatePerformance[complete.cases(DueDatePerformance),]


AvgDueDatePerformance <- DueDatePerformance %>%
  summarise(Avg_Performance = mean(`Due Date Performance/%`, na.rm = TRUE))


ggplot(DueDatePerformance, aes(x = `Due Date Performance/%`)) +
  geom_histogram(fill = "blue", bins = 20) +
  labs(title = "Due Date Performance Distribution",
       x = "Due Date Performance (%)",
       y = "Frequency") +
  geom_vline(data = AvgDueDatePerformance, aes(xintercept = Avg_Performance, color = "red"), linetype = "dashed") +
  scale_color_manual(name = "", values = "red") +
  theme_minimal()


AvgDueDatePerformance$Avg_Performance
library(repr)
options(repr.plot.width=5, repr.plot.height=4)

# Histogram 

ggplot(DueDatePerformance, aes(x = `Due Date Performance/%`)) +
  geom_histogram(fill = "blue", bins = 20) +
  labs(title = "Delivery Performance Distribution",
       x = "Due Date Performance (%)",
       y = "Frequency") +
  geom_vline(data = AvgDueDatePerformance, aes(xintercept = Avg_Performance), linetype = "dashed", color = "red", size = 1.2) +
  annotate("text", x = 95, y = 25, label = "Average delivery performance", color = "red", size = 5, fontface = "bold") +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))


#Predictive Part 



#linear regression model1

summary(N0985928_BUSI49152_)
head(N0985928_BUSI49152_)
#check correlations
cor_matrix <- cor(N0985928_BUSI49152_[, c("Powder_weight_kg", "Powder_Cost", "Selling_Price_Pound", "Profit", "Total_of_Sales", "Due Date Performance")], use="pairwise.complete.obs")
print(cor_matrix)
print(cor_matrix_yearly)
#profit-sale cor.
yearly_profit_columns <- c("Profit_in2012", "Profit_in2011", "Profit_in2010", "Profit_in2009", "Profit_in2008", "Profit_in2007", "Profit_in2006")
yearly_sales_columns <- c("Sales_in2012", "Sales_in2011", "Sales_in2010", "Sales_in2009", "Sales_in2008", "Sales_in2007", "Sales_in_2006")
cor_matrix_yearly <- cor(N0985928_BUSI49152_[, c(yearly_profit_columns, yearly_sales_columns)], use="pairwise.complete.obs")
print(cor_matrix_yearly)
# Install and load the corrplot package
install.packages("corrplot")
library(corrplot)

library(corrplot)

# Correlation matrix
cor_matrix_yearly <- cor(df)

# Create correlation matrix plot
corrplot(cor_matrix_yearly, method = "circle", type = "lower", order = "hclust", tl.cex = 0.8, tl.col = "black")






         




summary(N0985928_BUSI49152_)
head(N0985928_BUSI49152_)

model <- lm(Profit_in2011 ~ Sales_in2011, data = N0985928_BUSI49152_)
summary(model)
print(model)


model2 <- lm(Profit ~ Selling_Price_Pound + Powder_Cost + Sales_in2012 + Profit_in2012 + Total_of_Sales + Product Family, data = N0985928_BUSI49152_)

# Boşluklu sütun ismini düzeltme
colnames(N0985928_BUSI49152_)[24] <- "Product_Family"

# Product_Family değişkenini faktöre dönüştürme
N0985928_BUSI49152_$Product_Family <- as.factor(N0985928_BUSI49152_$Product_Family)

# Lineer regresyon modeli oluşturma
model2 <- lm(Profit ~ Selling_Price_Pound + Powder_Cost + Sales_in2012 + Profit_in2012 + Total_of_Sales + Product_Family, data = N0985928_BUSI49152_)

# Model özetini görüntüleme
summary(model2)

model5 <- lm(Profit ~ Product_Family + Powder_Cost + Total_of_Sales, data = N0985928_BUSI49152_)
summary(model5)


summary(N0985928_BUSI49152_)


model_sales <- lm(Salesin_2012 ~ Colour, data = N0985928_BUSI49152_)
summary(model_sales)

model_profit <- lm(Profitin_2012 ~ Colour, data = N0985928_BUSI49152_)
summary(model_profit)

















model2012 <- lm(Profitin_2012 ~ Colour + Selling_Price_Pound + Salesin_2012, data=N0985928_BUSI49152)

summary(model2012)
library(ggplot2)


coefs <- data.frame(coef(summary(model2012)))
coefs <- coefs[-1,] # Intercept'i kaldırın
coefs$Variable <- rownames(coefs)
colnames(coefs)[1:2] <- c("Estimate", "Std.Error")

coefs$conf.low <- coefs$Estimate - 1.96 * coefs$Std.Error
coefs$conf.high <- coefs$Estimate + 1.96 * coefs$Std.Error

ggplot(coefs, aes(x=Variable, y=Estimate)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x="Independent Variables", y="Coefficients and Confidence Intervals") +
  ggtitle("2012 Sales Prediction Model - All Coefficients and Confidence Intervals")


coefs <- coef(summary(model2012))
coefs_all <- data.frame(Terms = row.names(coefs), coefs)
colnames(coefs_all) <- c("Terms", "Estimate", "Std.Error", "t.value", "Pr(>|t|)")
coefs_all$lower <- coefs_all$Estimate - (1.96 * coefs_all$Std.Error)
coefs_all$upper <- coefs_all$Estimate + (1.96 * coefs_all$Std.Error)


library(ggplot2)

coef_plot <- ggplot(coefs_all, aes(x = reorder(Terms, -Estimate), y = Estimate)) +
  geom_point(color = "steelblue", size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  labs(x = "Independent Variables", y = "Coefficient Estimates", title = "Coefficient Estimates and Confidence Intervals") 

# Coefficient values as labels
coefs_all$label <- round(coefs_all$Estimate, 2)
coef_plot <- coef_plot + geom_text(aes(label = label), hjust = -0.5, vjust = 0.5, size = 4)

library(ggplot2)

ggplot(coefs_all[-1,], aes(x = reorder(Terms, Estimate), y = Estimate, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  xlab("Terms") + ylab("Estimate") +
  ggtitle("Coefficient Estimates and Confidence Intervals") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  geom_text(aes(label = round(Estimate, 2)), hjust = -0.2, vjust = 0.5)































#classification tree

library(ISLR)
library(rpart)
library(rpart.plot)



# Decision Tree 
library(dplyr)
library(tidyr)
library(rpart)
library(stringr)


# Dummy 

library(dplyr)
library(dplyr)
N0985928_BUSI49152_ <- N0985928_BUSI49152_ %>%
  mutate(Product = as.factor(Product),
         Description = as.factor(Description),
         Colour = as.factor(Colour)) %>%
  mutate(across(c(Product, Description, Colour), as.character)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate_all(~str_replace_all(., " ", "_")) %>%
  select(-c(Total_Powder_Weight_kg, Powder_Cost, Selling_Price_Pound)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(where(is.factor) | where(is.numeric)) %>%
  mutate(across(where(is.numeric), as.numeric))



str






# Decision tree model
library(rpart)
dt_model <- rpart(Profit_Margin ~ ., data = N0985928_BUSI49152_)
print(dt_model)
install.packages("rpart.plot")
library(rpart.plot)
prp(dt_model)
prp(dt_model, extra=1)
printcp(dt_model)
plotcp(dt_model)
plot(dt_model, uniform = TRUE,cex = 0.2)
text(dt_model, use.n = TRUE, all = TRUE, cex = 0.7)
rpart.plot(dt_model,extr=101)
summary(dt_model)

predicted_values <- predict(dt_model, newdata = N0985928_BUSI49152_)
print(predicted_values)
importance <- dt_model$variable.importance
print(importance)
best_split <- which.max(dt_model$cptable[, "xstd"])
best_cp <- dt_model$cptable[best_split, "CP"]
print(paste("Best CP value:", best_cp))
pruned_model <- prune(dt_model, cp = best_cp)
rpart.plot(pruned_model)




library(lattice)
library(ggplot2)
install.packages("lattice")
library(caret)
set.seed(42)
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(Profit_Margin ~ ., data = N0985928_BUSI49152_, method = "rpart", trControl = train_control)
print(model_cv)
packageVersion("caret")



models <- getModelInfo()
models_rpart <- grep("rpart", names(models), value = TRUE)
print(models_rpart)

set.seed(42)
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(Profit_Margin ~ ., data = N0985928_BUSI49152_, method = "rpart", trControl = train_control)



install.packages("rpart")
library(rpart.plot)

model_rpart <- rpart(Profit_Margin ~ ., data = N0985928_BUSI49152_)
summary(model_rpart)
rpart.plot(model_rpart)
print(model_rpart)
set.seed(42)
train_control <- trainControl(method = "cv", number = 10)


fit <- rpart(Profit_Margin ~ ., data = N0985928_BUSI49152_)
predictions <- predict(fit, newdata1 = N0985928_BUSI49152_)
print(predictions)





#Prescriptive
dim(N0985928_BUSI49152)
head(N0985928_BUSI49152)
str(N0985928_BUSI49152)

library(rpart)



summary(N0985928_BUSI49152_)
head(N0985928_BUSI49152_)









summary(N0985928_BUSI49152_)
head(N0985928_BUSI49152_)








