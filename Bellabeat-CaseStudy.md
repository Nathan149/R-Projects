

## BellaBeat
Urška Sršen and Sando Mur founded Bella beat, a high-tech company that manufactures health-focused smart products.Sršen used her background as an artist to develop beautifully designed technology that informs and inspires women around the world. Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to empower women with
knowledge about their own health and habits. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.  

### Business Task
Analyze smart device usage data, and trends in order to gain insight on how customers use Bella beat smart devices, how these trends apply to Bellabeat customers and how this can influence Bella beat marketing strategy.  

#### About the data
For this project, the [fitBit Fitness Tracker Data](https://www.kaggle.com/arashnic/fitbit) made available through [Mobius](https://www.kaggle.com/arashnic) was used. This Kaggle data set
contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.  

#### Installing and Loading Packages  
```{r}
    #install.packages('tidyverse')
    library(tidyverse)
    library(lubridate)
```  
#### Importing Datasets  
```{r}
  daily_activity <- read.csv("daily_activity.csv")
  daily_sleep <- read.csv("Sleep_daily.csv")
``` 
Viewing table column information  
```{r}
      head(daily_activity)
      head(daily_sleep)
```
### Cleaning and Transformation
#### Formatting date and time columns  
For the daily sleep dataframe, all of the sleep activity started at about the same time, on various days, hence it's safe to format the sleep day column as a date column alone.
```{r}
      
        #daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, "%d/%m/%Y")
      
        daily_sleep$SleepDay <- as.Date(daily_sleep$SleepDay, "%m/%d/%Y") 
```  
#### Remove empty rows
```{r}
      daily_activity <- daily_activity[rowSums(is.na(daily_activity)) != ncol(daily_activity),]
      daily_sleep <- daily_sleep[rowSums(is.na(daily_sleep)) != ncol(daily_sleep),]
```   
#### Understanding some summary statistics
How many unique users are in each data frame, for the daily activity and the daily sleep  
```{r}
    n_distinct(daily_activity)
    n_distinct(daily_sleep)
```
We see from  the result above, there are more users for the daily activity data frame than the daily sleep data frame, next we'll find out the number of entries per user to best determine how much information we have on every user. We'll create additional tables...   
```{r}
      user_activity_count <- aggregate(data.frame(daily_count = daily_activity$Id),
                              list(Id = daily_activity$Id),
                              length)
      sleep_user_count <- aggregate(data.frame(sleep_count = daily_sleep$Id),
                              list(Id = daily_sleep$Id),
                              length)
      head(user_activity_count)
      head(sleep_user_count)
```  
### Analysis  
From the data set above, there are users with only two entries  for their sleep activity and so much more entries for their daily activity, comparing such statistics against each other would not provide proper analysis considering there are no records for some sleep days to match the records for activity days, for better analysis, we'll select users with at least 20 entries in both data frames. to do this we'll classify users in the daily activity and daily sleep data frames according to the number of entries as activity levels and sleep activity levels respectively.    
 - Between 25 and 31 classify as Excellent,  
 - Between 20 and 25,Very Good  
 - Between 15 and 20, Good
 - Between 10 and 15, Fair
 - less than 10, Poor

```{r}
      user_activity_count <- user_activity_count %>% 
     mutate(activity_levels = case_when(
         daily_count <= 10 ~ "Poor",
         daily_count > 10 & daily_count <= 15 ~ "Fair",
         daily_count > 15 & daily_count <= 20 ~ "Good",
         daily_count > 20 & daily_count <= 25 ~ "Very Good",
         daily_count > 25 & daily_count <= 31 ~ "Excellent",
         TRUE ~ "other"
     ))
       sleep_user_count <- sleep_user_count %>% 
          mutate(sleep_levels = case_when(
                  sleep_count <= 10 ~ "Poor",
                  sleep_count > 10 & sleep_count <= 15 ~ "Fair",
                  sleep_count > 15 & sleep_count <= 20 ~ "Good",
                  sleep_count > 20 & sleep_count <= 25 ~ "Very Good",
                  sleep_count > 25 & sleep_count <= 31 ~ "Excellent",
                  TRUE ~ "Excellent"
       ))
head(user_activity_count)
head(sleep_user_count)
```

```{r}  
# Creating a plot to show the activity levels for the users in both data frames    
sleep_user_count$sleep_levels <- as.character(sleep_user_count$sleep_levels)
sleep_user_count$sleep_levels <- factor(sleep_user_count$sleep_levels, levels = unique(sleep_user_count$sleep_levels))
sleep_user_count$sleep_levels <- factor(sleep_user_count$sleep_levels, levels = c("Excellent","Very Good", "Good", "Fair", "Poor"))
       
    sleep_user_count %>%
    count(sleep_levels) %>% 
    mutate(Percentage = n/sum(n)) %>% 
    ggplot(aes(x = sleep_levels,
               y = Percentage)) + 
    geom_col(aes(fill = sleep_levels), position = "dodge") + 
    geom_text(aes(label = scales::percent(Percentage),
                  y = Percentage,
                  group = sleep_levels),
              position = position_dodge(width = 0.9),
              vjust = 1.5) + 
    labs(title = "Sleep user activity", x = "Sleep Levels")
    
    
    user_activity_count %>%
    count(activity_levels) %>% 
    mutate(Percentage = n/sum(n)) %>% 
    ggplot(aes(x = activity_levels,
               y = Percentage)) + 
    geom_col(aes(fill = activity_levels), position = "dodge") + 
    geom_text(aes(label = scales::percent(Percentage),
                  y = Percentage,
                  group = activity_levels),
              position = position_dodge(width = 0.9),
              vjust = 1.5) + 
    labs(title = "User activity", x = "Activity Levels") 
```     

  From the the plots above, the activity levels and sleep levels both indicate the level of activity carried out by users in the daily activity and daily sleep data frames respectively, this is indicated by the number of records per user across a 31 day period. We see from the sleep user plot, that a total of about 50 percent of users have entries between 20 and 31 days, while in the user activity the majority of users have entries everyday across the 31 day period, this suggest that most users turn off their smart devices before they go to bed. To carry out a better sleep and user activity analysis, we would merge the daily activity and sleep activity data sets, but with only users that have at least a 20 day sleep activity recorded, so we can better understand how their daily activity affect sleep.  

#### Summary analysis for the daily activity data set
```{r}
   daily_activity %>% 
      select(TotalSteps,
             TotalDistance,
             VeryActiveMinutes,
             FairlyActiveMinutes,
             LightlyActiveMinutes,
             SedentaryMinutes) %>% 
      summary()
```  
#### About this Summary  
  From this summary the average number of steps taken daily is 7638, which a lot less compared to the maximum number of steps taken by a participant which is 36019, its is also, good to note that some records hold 0 steps per day, it is also good to note that age can affect number of steps taken per user, the older the user the less steps taken, another point to note is that there a so much more sedentary users than active users, the average active minutes is 21.16 minutes, as opposed to the average sedentary minutes which is 991.2, it is safe to say that most users spend most of their time sitting or resting. The number of minutes spent across the day improves with fairly and lightly active minutes.  
  
#### Merging data Sets
We merge the daily sleep and daily activity data sets, with Id and activity date columns for users with at least a 20 day sleep record over 31 days.
```{r}
#Filtering Id's with the most sleep count from the sleep_user_count data set
  x <- filter(sleep_user_count, sleep_user_count$sleep_count >= 20)
  
#Filtering daily activity with the list of Id's from the x data frame
y_daily_activity <- filter(daily_activity, Id %in% x$Id)

#Filter daily sleep with list of Id's from the x data frame
y_daily_sleep <- filter(daily_sleep, Id %in% x$Id)

#Merge daily activity and sleep activity by column Id and data,
#change column name to date for both column
y_daily_activity <- y_daily_activity %>% 
    rename(Date = ActivityDate)
y_daily_sleep <- y_daily_sleep %>% 
    rename(Date = SleepDay)
combined_data <- merge(y_daily_sleep, y_daily_activity, by = c("Id","Date"))
```    

### Visualisation and Findings
```{r}
ggplot(data = combined_data, aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) + 
  geom_point() + geom_smooth() + 
  labs(title = "Sleep Time and Time in Bed")
```   
  
  This is a simple linear plot, the more time spent in bed, the more hours of sleep.

Next we'll look at the daily activity data frame, to find out in general how total steps per day affects calories burned.
```{r}
ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) + 
  geom_point() + geom_smooth() + 
  labs(title = "Total Steps on calories", x = "Total Steps", y = "Calories")
```
  
  This plot clearly shows that for most users the more steps taken the more calories burned, as with any physical activity

Just for Emphasis we show a plot of all the various activity minutes and the calories burned
```{r}
ggplot(combined_data, aes(Calories, col = "Minutes")) + 
    geom_point(aes(y = VeryActiveMinutes, color = "red")) +
    geom_point(aes(y = LightlyActiveMinutes, color = "orange")) +
    geom_point(aes(y = SedentaryMinutes, color = "green")) + 
    labs(title = "Activity minutes and Calories", x = "Calories", y = "Minutes") + 
    scale_color_manual( labels = c("Sedentary","Lightly Active", "Very Active"),
                        values = c("Red", "Orange", "Blue"))
```
  
  This plot doesn't indicate any relationship between levels of active minutes and calories burned, in fact users that spend most of their time sitting or inactive sleep burn as much calories as the very active users, nevertheless this plot emphasizes the fact time most user spend the bulk of their time inactive or sitting.

Again we plot Activity minutes against sleep Time...
```{r}
ggplot(combined_data, aes(TotalMinutesAsleep, col = Minutes)) + 
    geom_point(aes(y = VeryActiveMinutes, color = "red")) +
    geom_point(aes(y = LightlyActiveMinutes, color = "orange")) +
    geom_point(aes(y = SedentaryMinutes, color = "green")) + 
  labs(title = "Activity minutes and Sleep Time", x = "Minutes Asleep", y = "Minutes Active") + 
    scale_color_manual( labels = c("Sedentary","Lightly Active", "Very Active"),
                        values = c("Red", "Orange", "Blue"))
```
  
  This plot doesn't exactly change for the sleep time,  still no exact relationship with levels of active minutes and sleep time.

#### Summary and Recommendations  
The Bella beat technology has been designed to inform and inspire women all over the world, and is focused on smart health products for women. From the fitbit data set, we understand that.  

 1. Most users spend most of there time inactive or sitting in offices.
 2. Most users turn off their devices before they go to bed, hence collecting accurate sleep Information might be difficult.
 3. The more time you spend in bed the more sleep you get.
 4. On the average most users are getting a good amount of sleep between 6 -10 hours daily,hence some other factors will account for stress levels and not just their       level of physical activity.  
 
 This data set does not account for the different ages of the users, which is a factor to consider in the kind of physical activity they carry out daily, there are various kinds of stress that can affect sleep time and activity levels.  

Some Recommendations..   

 1. Bella beat product should be targeted at the more sedentary users, individuals that spend most of their time in offices or generally inactive.
 2. Users should be encouraged to use their devices even while they sleep, to better understand their sleep habits, user devices can be made to remove any form of         discomfort while sleeping.  
 3. An on boarding session that sensitizes users on the benefits of the devices and keeping track of their activities as well as maintaining a healthy                     lifestyle, would encourage users to stay active and use the products.  
 
#### Thanks for reading, your comments would be appreciated.
