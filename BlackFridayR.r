
library(tidyverse)
library(ggrepel)
library(lubridate)
library(scales)
library(highcharter)
library(ggpubr)

b <- read_csv("BlackFriday.csv")

b_cat <- b%>% 
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  filter(!is.na(category))

b %>% group_by(User_ID) %>% filter(row_number(User_ID) == 1) %>% group_by(Age, Occupation) %>% summarize(occupationCount = n()) %>% ggplot(aes(x = Occupation, y = occupationCount)) + geom_area(fill = "royalblue") + facet_grid(Age~.)

student_customers_count <- b_cat %>% group_by(User_ID, Occupation, City_Category, category) %>%
filter(Occupation %in% c(4, 10)) %>% summarise(total_items_bought = n()) %>%
group_by(City_Category, Occupation, category) %>% summarise(total_items_bought = sum(total_items_bought)) %>%
ungroup() %>% mutate(Occupation = ifelse(Occupation == 4, "4 - Supposedly University/College", "10 - supposedly School"),
Occupation = factor(Occupation)) %>% arrange(Occupation, City_Category, category)

student_customers_count %>% ggplot(aes(x = category, y = total_items_bought, fill = Occupation)) +
geom_bar(stat = "identity", position = position_dodge()) + facet_wrap(City_Category~.) +
ggtitle("Total items bought by occupation 4 and 10 across categories and cities")

b_cat %>%
  group_by(category) %>%
  summarise(spent_per_category = sum(Purchase)) -> spent_per_category

spent_per_category %>%
  ggplot(aes(x = category, y = spent_per_category/1000000000)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  ylab("Total purchases per categories (in billions, *10^9)") +
  ggtitle("Most purchased categories")


b_cat %>%
  group_by(category) %>%
  summarise(total_sales_per_category = n()) %>%
  left_join(spent_per_category, by = "category") %>%
  mutate(spent_sales_ratio = spent_per_category/total_sales_per_category) %>%
  ggplot() +
  geom_bar(stat = "identity", aes(x = category, y = spent_sales_ratio), fill = "royalblue") +
  ggtitle("Purchase/sales ratio across categories")

b %>%
  select(Purchase, City_Category) %>%
  ggplot() +
  geom_histogram(aes(x = Purchase), bins = 24, fill = "royalblue") +
  facet_wrap(City_Category~.) +
  theme(axis.text.x.bottom = element_text(angle = 90))


plot_linetype <- list(15000, "dashed", "orange", 2)

aplot <- b_cat %>%
  filter(Purchase > 6999 & Purchase < 9000) %>%
  group_by(category) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  geom_hline(yintercept = plot_linetype[[1]], linetype = plot_linetype[[2]],
             color = plot_linetype[[3]], size = plot_linetype[[4]]) +
  ggtitle("Sales count in the 7-9K range")

bplot <- b_cat %>%
  filter(Purchase > 14999 & Purchase < 17000) %>%
  group_by(category, Age) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  geom_hline(yintercept = plot_linetype[[1]], linetype = plot_linetype[[2]],
             color = plot_linetype[[3]], size = plot_linetype[[4]]) +
  ggtitle("Sales count in the 15-17K range")

#Customers and number of sales vs cities
#There is much less number of customers in A and B than in city C
customer_count_per_city <- b %>%  
  group_by(User_ID) %>%
  filter(row_number(User_ID) == 1) %>%
  group_by(City_Category, Age) %>%
  summarise(customers_count = n()) %>%
  arrange(City_Category, Age)

#Even though city A has less sales than city C, it has more sales in the 26-35 age range, whereas city C has twice as much sales in the 46-55+ age category
#That signifies that the customers in city A tend to be younger than those in city C
citySales <- b %>%
  group_by(City_Category)%>%
  summarise(sold_by_city = n(), 
            percentage_by_city = round(n()/nrow(b)*100,0))

ggarrange(
  customer_count_per_city %>%
    ggplot(aes(x = City_Category, y = customers_count, fill = Age)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle("Number of customers across cities"))
  

#Average number of bought items across cities
  #Three very different patterns
  b %>%
    group_by(User_ID, City_Category, Age) %>%
    summarise(bought_items = n()) %>%
    group_by(City_Category, Age) %>%
    summarise(items_bought_per_city = sum(bought_items)) %>%
    arrange(City_Category, Age) %>%
    cbind(customers_count = customer_count_per_city$customers_count) %>%
    mutate(average_items_per_person_in_city = items_bought_per_city/customers_count) %>%
    ggplot(aes(x = City_Category, y = average_items_per_person_in_city, fill = Age)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle("Average number of bought items across cities")
  
  


b %>%
    group_by(City_Category, Age) %>%
    summarise(sales = n()) %>%
    left_join(citySales, by = "City_Category") %>%
    mutate(percentage = round(sales/sold_by_city*100, 0),
           percentage = ifelse(percentage < 3, "", as.character(paste0(percentage, "%")))) %>%
    select(-percentage_by_city) %>%
    ggplot(aes(x = City_Category, y = sales, fill = Age)) +
    geom_bar(stat="identity", position = position_dodge()) +
    ggtitle("Number of sales across age groups in the three cities")
  

  b %>%
    group_by(City_Category) %>%
    summarise(sales = n(), 
              percentage_by_city = paste0(round(n()/nrow(b)*100,0), "%")) %>%
    ggplot(aes(x = City_Category, y = sales)) +
    geom_bar(stat="identity", fill = "royalblue") +
    geom_text(aes(label = percentage_by_city),
              vjust=1.6, 
              color="white",
              size=8) +
    ggtitle("Number of sales in the three cities")

b %>%  
  group_by(User_ID) %>%
  filter(row_number(User_ID) == 1) %>%
  group_by(Age, Stay_In_Current_City_Years, City_Category) %>%
  summarise(customer_count = n()) %>%
  ggplot(aes(Age, customer_count, fill = Stay_In_Current_City_Years)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(City_Category~.) +
  theme(axis.text.x.bottom = element_text(angle = 90))

library(ggplot2)


ggarrange(
  b %>%
    group_by(Age, Gender, City_Category) %>%
    summarise(count = n(),
              percentage = round(n()/nrow(b)*100, 1)) %>%
    ggplot(aes(x = Age, y = percentage, fill = Gender)) +
    geom_bar(stat = "identity") +
    facet_wrap(City_Category~.) +
    ylab("Percentage of total purchases") +
    ggtitle("Age and gender profile of customers across cities") +
    theme(axis.text.x.bottom = element_text(angle = 90)))

  b %>%
    group_by(Age, Gender, City_Category) %>%
    summarise(count = n()) %>%
    spread(Gender, count) %>%
    mutate(men_women_ratio = M/F) %>%
    ungroup() %>%
    select(-M, -F) %>%
    ggplot(aes(x = Age, y = men_women_ratio)) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "royalblue") +
    facet_wrap(City_Category~.) +
    ggtitle("M/F customers ratio across cities") +
    theme(axis.text.x.bottom = element_text(angle = 90))

b_users <- b %>%
  group_by(User_ID, Age, Gender, Marital_Status, Occupation, City_Category) %>%
  summarise(av_purchase = mean(Purchase),
            sum_purchase = sum(Purchase),
            products_bought = n())

ggarrange(
  b_users %>%
    ggplot(aes(Age, y = products_bought, fill = Gender)) +
    geom_boxplot(aes(middle = mean(products_bought))) +
    facet_wrap(City_Category~.),
  
  b_users %>%
    ggplot(aes(Age, y = av_purchase, fill = Gender)) +
    geom_boxplot(aes(middle = mean(av_purchase))) +
    facet_wrap(City_Category~.),
  
  b_users %>%
    ggplot(aes(Age, y = sum_purchase, fill = Gender)) +
    geom_boxplot(aes(middle = mean(sum_purchase))) +
    facet_wrap(City_Category~.),
  nrow = 3, common.legend = TRUE)
