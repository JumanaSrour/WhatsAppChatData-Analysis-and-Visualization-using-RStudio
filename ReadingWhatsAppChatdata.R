library(rwhatsapp)

##read the WhatsApp chat data
v_Chats <- rwa_read("WhatsAppData.txt")

##Display the data
head(v_Chats, 5)

##Display the structure
str(v_Chats)

##Massage the Data
library(dplyr)
library(lubridate)

v_Chats <- v_Chats %>%
  mutate(message_date = date(time)) %>%
  mutate(message_month = month(time, label = TRUE)) %>%
  mutate(message_month = factor(message_month)) %>%
  mutate(message_weekday_number = mday(message_date)) %>%
  mutate(message_weekday_name = weekdays(message_date)) %>%
  mutate(message_weekday_name = factor(message_weekday_name)) %>%
  mutate(message_hour = hour(time)) %>%
  filter(!is.na(author))

head(v_Chats, 10)
  
library(ggplot2)
library(ggthemes)
library(viridis)

v_Chats %>%
  group_by(message_month) %>%
  count(message_date) %>%
  ggplot(aes(x = reorder(message_date), y = n, fill = message_month)) +
  geom_bar(stat =  "identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Date", y = "Number of Messages", fill = "Month - ") +
  ggtitle("Messages per Day", "Frequency of Message/Day \nGrouped by Month")
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         legend.key.size = unit(0.3, "cm"),
         legend.key.width = unit(0.3, "cm"),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title = element_text(size = 9),
         plot.title = element_text(size = 18),
         plot.subtitle = element_text(size = 10))
  
### plot messages per weekday
v_Chats %>%
  group_by(message_month,message_weekday_number, message_weekday_name) %>%
  count() %>%
  ggplot(aes(x = reorder(message_weekday_name, -message_weekday_number),
  #ggplot(aes(x = message_weekday_name,
             y = n, 
             fill = message_month)) +
  geom_bar(stat =  "identity") + 
  scale_x_discrete(labels = c("Saturday" = "Sat",
                              "Sunday" = "Sun",
                              "Monday" = "Mon",
                              "Tuesday" = "Tue",
                              "Wednesday" = "Wed",
                              "Thursday" = "Thu",
                              "Friday" = "Fri",)) + 

  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Date", y = "Number of Messages", fill = "Month - ") +
  coord_flip() + 
  ggtitle("Messages per Week Day", "Frequency of Message/Weekd Day \nGrouped by Month")
  theme_minimal() +
  theme( legend.title = element_text(color = "blue", size = 9),
         legend.text = element_text(colour = "red", size = 7),
         legend.position = "bottom",
         legend.key.size = unit(0.3, "cm"),
         legend.key.width = unit(0.3, "cm"),
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7),
         axis.title = element_text(size = 9),
         plot.title = element_text(size = 18),
         plot.subtitle = element_text(size = 10))

v_temp <- v_Chats %>%
          group_by(message_month) %>%
          count(message_date)

v_temp[which(v_temp$n == max(v_temp$n)),] $message_date