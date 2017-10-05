# edWisor- Data Science Project
# Kaggle - Instacart Market Basket Analysis


rm(list=ls(all=TRUE))

# Set current working directory
setwd("C:/MyFiles/Kshiti/Edwisor/PROJECT")

# Install packages
install.packages(c("outliers","readr","ggplot2","dplyr","scales"))

# Exploratory data Analysis


# Read dataset csv files
library(readr)

order_products_prior= read_csv("C:/MyFiles/Kshiti/Edwisor/PROJECT/order_products__prior.csv")
order_products_train = read_csv("C:/MyFiles/Kshiti/Edwisor/PROJECT/order_products__train.csv")
products = read_csv("C:/MyFiles/Kshiti/Edwisor/PROJECT/products.csv")
orders = read_csv("C:/MyFiles/Kshiti/Edwisor/PROJECT/orders.csv")
departments = read_csv("C:/MyFiles/Kshiti/Edwisor/PROJECT/departments.csv")
aisles = read_csv("C:/MyFiles/Kshiti/Edwisor/PROJECT/aisles.csv")

#---------------------------------------------------------------------------------------------------------------------------


# Display structure of datasets

str(order_products_prior)
str(order_products_train)
str(products)
str(orders)
str(departments)
str(aisles)

# Understand the data type

 orders$eval_set=as.factor(orders$eval_set)
 products$product_name = as.factor(products$product_name)
 departments$department = as.factor(departments$department)
 aisles$aisle=as.factor(aisles$aisle)

# Data treatment

# Missing value analysis
 
 apply(orders,2, function(x)sum(is.na(x)))
# orders$days_since_prior_order has 206209 missing values
# Imputing missing values not required since missing values mean that some users did not reorder some products

# Outlier Analysis
 
  library(outliers)
 outlier(orders$days_since_prior_order, opposite=FALSE)
 
 #Grubbs test for outliers
 grubbs.test(orders$days_since_prior_order, type = 10) 
 # outliers: day 30 - No extreme outliers found
 
#---------------------------------------------------------------------------------------------------------------------
 
# Predictive data analysis

# Visualisation
library(ggplot2)
library(dplyr) 
library(scales)

# Hour of day
 ggplot(data=orders,aes(x=order_hour_of_day)) + 
   geom_area(stat="count",fill="goldenrod2",colour = "red",size=1.2)+
   ggtitle(' No of orders at each hour')+ theme_bw()+
   xlab('Order hour of the day') + ylab('No of orders') + 
   scale_x_continuous(breaks=pretty_breaks(n=24))+
   scale_y_continuous(breaks=pretty_breaks(n=7)) 
 
 # Day of Week
 ggplot(data=orders,aes(x=order_dow))+
   geom_histogram(stat="count",fill="grey",size=1.2, colour = "pink") + 
   ggtitle('No of orders per day')+ theme_bw()+
   xlab('Days of Week') + ylab('No of orders')+ 
   scale_x_continuous(breaks=pretty_breaks(n=7))+
   scale_y_continuous(breaks=pretty_breaks(n=7))
 
 # Days taken to reorder
 ggplot(data=orders,aes(x=days_since_prior_order))+
   geom_density(stat="density",fill="steelblue1",colour = "goldenrod2",size=1.05) + 
   ggtitle('Days taken to reorder') + theme_bw() +
   xlab('Days since previous order') + ylab('No of orders')  
 scale_x_continuous(breaks=pretty_breaks(n=15))+
   scale_y_continuous(breaks=pretty_breaks(n=7))
 
 
# Retrieve test data from orders 
 test = orders[orders$eval_set=="test",]
 
# Retrieve prior users with prior and test by their user_id
 
 # Create temporary dataframes by left join with products
 products_aisles = left_join(products, aisles, by = "aisle_id")
 products_dept = left_join(products, departments, by = "department_id")
 
 products_aisles_dept = left_join(products_aisles,products_dept, by = "product_id",copy=FALSE)
 
 # Remove redundant columns
 products_aisles_dept = products_aisles_dept[,c(1,2,3,4,9,5)]
 
 # Rename the columns
 colnames(products_aisles_dept) = c("product_id","product_name","aisle_id", "department_id","department","aisle")
 
 # Remove the temporary dataframes
 rm(products_aisles, products_dept)
 
 # Rearrange and group by order_id
 orders_train = left_join(order_products_train, orders, by = "order_id")
 orders_prior = left_join(order_products_prior, orders, by = "order_id")
 
 # Rearrange and group by product_id, remove redundant columns
 orders_train = left_join(orders_train, products_aisles_dept,by = 'product_id')
 orders_train = orders_train[,c(-11, -14, -15)]
 orders_prior = left_join(orders_prior, products_aisles_dept,by = 'product_id')
 orders_prior = orders_prior[,c(-11, -14, -15)]
 
 #row bind train and prior data with all order columns
 orders_train_prior = rbind(orders_train,orders_prior)
 head(orders_train_prior,10)
 
 #Pick the users for train data
 test_users = orders_train_prior[orders_train_prior$user_id %in% test$user_id,]
 
 
# Merging products with order_products_prior 
 order_products_prior_pid = left_join(order_products_prior, products_aisles_dept, by = "product_id",copy=FALSE)
 
 
# Top 15 products
 # Count and sort the products by product_id
 Top15products <- order_products_prior_pid %>% 
   group_by(product_id) %>% 
   summarize(count = n()) %>% 
   top_n(15, wt = count) %>%
   left_join(select(products,product_id,product_name),by="product_id") %>%
   arrange(desc(count))
   Top15products
 
 Top15products %>% 
   ggplot(aes(x=reorder(product_name,-count), y=count))+ theme_bw()+
   geom_bar(stat="identity",fill="antiquewhite1",colour = "steelblue2")+
   theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("Top 15 products")

 
# Top 5 departments
 # Count and sort the department by department names
 Top5departs <- order_products_prior_pid %>% 
   group_by(department) %>% 
   summarize(count = n()) %>% 
   top_n(5, wt = count) %>%
   left_join(select(departments,department_id,department),by="department") %>%
   arrange(desc(count))
   Top5departs
 
 Top5departs %>% 
   ggplot(aes(x=reorder(department,-count), y=count))+ theme_bw()+
   geom_bar(stat="identity",fill="darkolivegreen1",colour = "pink")+
   theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("Top 5 departments")
 
# Top 5 aisles
 # Count and sort the aisles by aisle names
 Top5aisles <- order_products_prior_pid %>% 
   group_by(aisle) %>% 
   summarize(count = n()) %>% 
   top_n(5, wt = count) %>%
   left_join(select(aisles,aisle_id,aisle),by="aisle") %>%
   arrange(desc(count))
   Top5aisles
 
 Top5aisles %>% 
   ggplot(aes(x=reorder(aisle,-count), y=count))+ theme_bw()+
   geom_bar(stat="identity",fill="coral2",colour = "cyan")+
   theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("Top 5 aisles")

 
#--------------------------------------------------------------------------------------------------------

# Recommendation
 
 # calcuating maximum order number
 
  order_items_number <- order_products_prior %>%
  group_by(order_id) %>%
  summarise(number_of_items = n())

 user_max_order_number <- orders %>%
  group_by(user_id) %>%
   summarise(max_order_number = max(order_number))

order_x <- orders %>%
  inner_join(order_items_number) %>%
  inner_join(user_max_order_number)


# calcuating average number of items in basket

order_latest_4 <- order_x %>%
  filter(order_number + 4 >= max_order_number)

 user_average_items <- order_latest_4 %>%
  group_by(user_id) %>%
   summarise(avg_n_of_items = mean(number_of_items))

order_product_latest_4 <- order_latest_4 %>%
  inner_join(order_products_prior)

# Finding out favoured product that is reordered most

user_favor_product <- order_product_latest_4 %>%
  group_by(user_id, product_id) %>%
  summarise(favor = sum((reordered*9+1)*(order_number+5-max_order_number)))

user_product_rank <- user_favor_product %>%
  arrange(user_id,favor) %>%
  group_by(user_id) %>%
  mutate(rank = rank(desc(favor)))

user_next_basket <- user_product_rank %>%
  inner_join(user_average_items) %>%
  filter (rank <= avg_n_of_items)


# Retrive users who reordered the favoured products
# And retrive users who would reorder the favoured products

sample_submission_user <- sample_submission %>%
  inner_join(orders) %>%
  select(user_id,order_id)

submission_user_next_basket <- user_next_basket %>%
  inner_join(sample_submission_user) %>%
  select(user_id, order_id, product_id)

submission_order_buy <- submission_user_next_basket %>%
  group_by(order_id) %>%
  summarise(products = paste(product_id, collapse=' '))

submission_order <- sample_submission_user %>%
  left_join(submission_order_buy) %>%
  select(order_id,products)


# Store results in the submission file

write.table(submission_order, file="./submission.csv", sep=",",
            row.names = FALSE, quote = FALSE, na="NA")

submission = read_csv("C:/MyFiles/Kshiti/Edwisor/PROJECT/submission.csv")
View(submission)

#--------------------------------------------------------------------------------------------
