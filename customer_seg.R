# packages

library(RColorBrewer)
library(ggridges)
library(cowplot)
library(ggplot2)
library(dplyr)
library(plotly)
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
library(clValid)
library(magrittr)
library(clustertend)
library(caret)
library(party)
library(randomForest)
library(e1071)
library(neuralnet)
library(rpart.plot)

# loading dataset

setwd("C:/Users/giese/Desktop/outros_projetos/customer_segmetation")
getwd()


df_data <- read.csv("data.csv", header = TRUE, sep = ",")

str(df_data)

summary(df_data)

#### Data munging ####

df_data$InvoiceNo <- NULL
df_data$StockCode <- NULL

df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

df_data <- na.omit(df_data)

# data engineering

df_data <- df_data %>% 
  mutate(InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y'), CustomerID=as.factor(CustomerID),)

df_data <- df_data %>% 
  mutate(dolar_total = Quantity*UnitPrice)

str(df_data)

#### Exploratory analysis ####

c(unique(df_data["Country"]))

c(unique(df_data["Quantity"]))

# time series plot

a <- ggplot(data = df_data) +
  geom_line(aes(x = InvoiceDate, y = dolar_total), 
            alpha = 0.6,
            size = 0.6) +
  scale_x_date(date_breaks = "1 month") +
  theme_minimal() +
  labs(x = "Date", 
       y = "sales" ,
       title = "sales between december 2010 & december 2011")

a <- ggplotly(a)
a

# frequency of country sales

freq_country <- data.frame(cbind(Frequency = table(df_data$Country), Percent = prop.table(table(df_data$Country)) * 100))
freq_country
str(freq_country)

# frequency plot

options(repr.plot.width=15, repr.plot.height=6)
b <- ggplot(data = freq_country, mapping = aes(x = Frequency, y = row.names(freq_country))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_country), color = row.names(freq_country)), alpha = .7, size = 1.1) +
  ylab("") +
  ggtitle("Number of sales by country") 
b <- ggplotly(b)
b

#### Pre processing ####

# creating a copy of the dataframe for clustering

df_data2 <- as.data.frame(df_data)

#

df_data2$Description <- NULL
df_data2$InvoiceDate <- NULL
df_data2$CustomerID <- NULL
df_data2$Country <- NULL

# reducing data size 

df_data3 <- sample_n(df_data2, 5000)

# standardizing variables

df_data3_scaled = scale(df_data3)
head(df_data3_scaled)

#### Cluster ####

set.seed(123)
hopkins(df_data3_scaled, n = nrow(df_data3_scaled)-1)

# option 1

num_clusters_opt1 <- NbClust(df_data3_scaled,  
                             distance = "euclidean",
                             min.nc = 2, 
                             max.nc = 15, 
                             method = "kmeans",
                             index = "silhouette")

num_clusters_opt1$Best.nc
num_clusters_opt1$All.index

# option 2

num_clusters_opt2 <- NbClust(df_data3_scaled,  
                             distance = "euclidean", 
                             min.nc = 2, 
                             max.nc = 15, 
                             method = "kmeans",
                             index = "all")

# creating cluster model

model <- kmeans(df_data3_scaled, 2)
print(model)

# cluster information

model$size
model$centers

# putting cluster groups 

model$cluster
df_data3$Cluster <- model$cluster
View(df_data3)

# plot of the cluster

plot(df_data3_scaled, col = model$cluster, pch = 15)

# creating a better version of the plot

cluster_viz <- eclust(df_data3_scaled, "kmeans", k = 2, nstart = 25, graph = FALSE)

# 2nd plot

fviz_cluster(cluster_viz, geom = "point", ellipse.type = "norm")

#### Regression ####

#

indexes <- sample(1:nrow(df_data2), size = 0.7 * nrow(df_data2))
train.data <- df_data2[indexes,]
test.data <- df_data2[-indexes,]
class(train.data)
class(test.data)

str(train.data)


# Creating linear regression machine learning model 95%


model <- lm(dolar_total ~. , data = train.data)

# prevision for the training data

prevision1 <- predict(model, test.data)

summary(model)

# making plot of real and predictive data


print(data.frame(test.data$dolar_total, prevision1))

x = 1:length(test.data$dolar_total)

plot(x, test.data$dolar_total, col = "red", type = "l", lwd=2,
     main = "Product sales real and predictive data (linear regression)")
lines(x, prevision1, col = "blue", lwd=2)
legend("topleft",  legend = c("real data", "predictive data"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

# decision tree model

model <- rpart(dolar_total ~., 
                       data = train.data)

rpart.plot(model)

pred_model <- predict(model, test.data)

printcp(model)

summary(model)

# making plot of real and predictive data


print(data.frame(test.data$dolar_total, pred_model))

x = 1:length(test.data$dolar_total)

plot(x, test.data$dolar_total, col = "red", type = "l", lwd=2,
     main = "Product sales real and predictive data (decision tree)")
lines(x, pred_model, col = "blue", lwd=2)
legend("topleft",  legend = c("real data", "predictive data"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

# Randomforest model

model <- randomForest(dolar_total ~., data = train.data)

print(model)
plot(model)

summary(model)

pred_model <- predict(model, test.data)

# making plot of real and predictive data


print(data.frame(test.data$dolar_total, pred_model))

x = 1:length(test.data$dolar_total)

plot(x, test.data$dolar_total, col = "red", type = "l", lwd=2,
     main = "Product sales real and predictive data (random forest)")
lines(x, pred_model, col = "blue", lwd=2)
legend("topleft",  legend = c("real data", "predictive data"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

# svm model

model <- svm(dolar_total ~ ., data=train.data)

pred_model <- predict(model, test.data)

# making plot of real and predictive data


print(data.frame(test.data$dolar_total, pred_model))

x = 1:length(test.data$dolar_total)

plot(x, test.data$dolar_total, col = "red", type = "l", lwd=2,
     main = "Product sales real and predictive data (svm model)")
lines(x, pred_model, col = "blue", lwd=2)
legend("topleft",  legend = c("real data", "predictive data"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
