install.packages("dplyr")
library(dplyr)
install.packages("highcharter")
library(highcharter)
install.packages("vcd")
library(grid)
library(vcd)
install.packages("ggplot2")
library(ggplot2)
Data <- as.data.frame(read.csv("/Users/Shwetha Hara/Desktop/Data Analyst Take Home Exercise/Data Analyst Take Home Exercise/tubitv_sample_dataset.csv"))
Data$session_start_ts <- as.POSIXct(Data$session_start_ts,format = "%m/%d/%Y")
Data <- Data %>% filter(!(movie_total_viewtime < 0 | series_total_viewtime < 0))
View(Data)
#----------------------------------------------------------- Basic Analysis -------------------------------------------------------------------------------

Sessions <- Data[c(1,3,4,5,6,7,8)] %>%
  group_by(userid) %>%
  summarise_each(funs(sum)) 
View(Sessions)

#categories of lengths of Session visits
Sessions$duration <- 
  ifelse(((Sessions$movie_total_viewtime > 0 & Sessions$movie_total_viewtime <= 10)|(Sessions$series_total_viewtime >0 & Sessions$series_total_viewtime <= 10)),"0-10 sec",
         ifelse(((Sessions$movie_total_viewtime > 10 & Sessions$movie_total_viewtime <= 30)|(Sessions$series_total_viewtime > 10 & Sessions$series_total_viewtime <= 30)), "11-30 sec",
                ifelse(((Sessions$movie_total_viewtime > 30 & Sessions$movie_total_viewtime <= 60)|(Sessions$series_total_viewtime > 30 & Sessions$series_total_viewtime <= 60)), "31-60 sec",
                       ifelse(((Sessions$movie_total_viewtime > 60 & Sessions$movie_total_viewtime <= 180)|(Sessions$series_total_viewtime > 60 & Sessions$series_total_viewtime <= 180)), "61-180 sec", 
                              ifelse(((Sessions$movie_total_viewtime > 180 & Sessions$movie_total_viewtime <= 600)|(Sessions$series_total_viewtime >180 & Sessions$series_total_viewtime <= 600)), "181-600 sec",
                                     ifelse(((Sessions$movie_total_viewtime > 600 & Sessions$movie_total_viewtime <= 1800)|(Sessions$series_total_viewtime >600 & Sessions$series_total_viewtime <= 1800)), "601-1800 sec","1801+ sec"))))))

# Creating a column that tells if a user casted or not casted
Sessions$casted <- ifelse(((Sessions$movie_cast_time > 1) | (Sessions$series_cast_time > 1)), "1","0")

#New user vs Returning user
user_group <- as.data.frame(table(Data$userid))
user_group$type <- ifelse(user_group$Freq > 1, "Returning user", "New user")
View(user_group)
user_perc <- data.frame(table(user_group$type))
colnames(user_perc) <- c("UserType", "Freq")
user_perc$perc <- round(user_perc$Freq/sum(user_perc$Freq)*100)
user_perc

#pie chart for New vs Returning user
pie3D(user_perc$Freq,labels = user_perc$perc, main = "Percentage of New users vs Returning users", col = c('seagreen1','seagreen4'), explode = 0.1)
legend("bottomright", c("New user","Returning user"), cex = 0.8, fill = c('seagreen1','seagreen4'))

#Sessions vs casting 
install.packages("forcats")
require(forcats)
ggplot(Sessions)+aes(x = duration, fill = casted)+geom_bar(position = 'dodge')
ggplot(Sessions, aes (fct_infreq(duration)), fill = casted)+geom_bar(position = 'dodge')


#-----------------------------------------------------------Time Series Analysis--------------------------------------------------------------------------
Data$session_start_ts <- as.Date(Data$session_start_ts)
Data[is.na(Data)] <- 0
View(Data)
TimeLine <- Data[2:6] %>% 
                group_by(session_start_ts) %>%
                mutate_all(funs(round(. / (60*60), 2))) %>%
                summarise_each(funs(sum)) %>%
                mutate(movie_percentage_cast = round((movie_cast_time/movie_total_viewtime) *100, 2) ) %>%
                mutate(series_percentage_cast = round((series_cast_time/series_total_viewtime) *100, 2) )

View(TimeLine)
#Total Movie Time Vs. Total Cast Time
highchart() %>% 
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$movie_total_viewtime, name = "Total Movie View Time") %>%
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$series_total_viewtime, name = "Total Series View Time", color = "#c2b2af" ) %>%
  hc_xAxis(title = list(text = "Time Line")) %>%
  hc_yAxis(title = list(text = "Hours"))

#Movie Cast Time Vs. Series Cast Time
highchart() %>% 
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$movie_cast_time, name = "Movie Cast Time") %>%
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$series_cast_time, name = "Series Cast Time", color = "#c2b2af" ) %>%
  hc_xAxis(title = list(text = "Time Line")) %>%
  hc_yAxis(title = list(text = "Hours"))

#View Time Vs. Cast Time over the two months for movies
highchart() %>% 
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$movie_total_viewtime, name = "Movie View Time") %>%
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$movie_cast_time, name = "Movie Cast Time", color = "#c2b2af" ) %>%
  hc_xAxis(title = list(text = "Time Line")) %>%
  hc_yAxis(title = list(text = "Hours"))

#Percentage of movie cast
highchart() %>%
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$movie_percentage_cast, name = "Percentage Cast for Movies", color = "#618f56") %>%
  hc_xAxis(title = list(text = "Time Line")) %>%
  hc_yAxis(title = list(text = "Percentage Cast"))

#View Time Vs. Cast Time over the two months for series
highchart() %>% 
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$series_total_viewtime, name = "Series View Time") %>%
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$series_cast_time, name = "Series Cast Time", color = "#c2b2af" ) %>%
  hc_xAxis(title = list(text = "Time Line")) %>%
  hc_yAxis(title = list(text = "Hours"))

#Percentage of Series cast
highchart() %>%
  hc_add_series_times_values(TimeLine$session_start_ts, TimeLine$series_percentage_cast, name = "Percentage Cast for Series", color = "#618f56") %>%
  hc_xAxis(title = list(text = "Time Line")) %>%
  hc_yAxis(title = list(text = "Percentage Cast"))


highchart() %>%
  hc_add_series_times_values(Data$session_start_ts, Data$movie_total_starts, name = "Total movies started ", color = "#618f56") %>%
  hc_xAxis(title = list(text = "Movie View Time")) %>%
  hc_yAxis(title = list(text = "Number of Movies"))
#-------------------------------------------------------------------------------------------------------------------------------------

hchart(TimeLine, "scatter", hcaes(x = movie_total_viewtime, y = movie_total_starts))
getwd()

View(TimeLine)
