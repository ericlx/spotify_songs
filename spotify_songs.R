# Subject: PMBA6093 Analytics for Managers
# Title: Individual Assignment
# By: Eric LX
# Date: 14 Jul, 2024


# Requirements
# Your report should be an executive summary with a maximum length of three pages
# Please explain the key numbers in your analysis in plain language and
# provide proper visualizations to support your findings.


# Import necessary library
library(ggplot2)
library(dplyr)
library(nortest)
library(broom)


# Import data from "spotify_songs.csv" as "SS"
SS <- read.csv("~/Library/CloudStorage/OneDrive-个人/ERIC/02 Personal Development/03 Master Related/HKUMBA/01Modules/Module07_Analytics_for_Managers/Individual_Assignment/spotify_songs.csv")
colnames(SS)


# Question 1a. How many songs are there in total in the dataset?
total_rows <- nrow(SS)
total_rows
total_unique_songs <- length(unique(SS$track_id))
total_unique_songs

# Question 1b. How many distinct playlists are there in the dataset?
total_playlists <- length(unique(SS$playlist_name))
total_playlists

# Question 1c. How many distinct artists are there in the dataset?
total_artists <- length(unique(SS$track_artist))
total_artists


# Question 2a. Plot a histogram of the overall track popularity.
# 需要美化
ggplot(SS, aes(x=track_popularity)) +    
  geom_histogram(binwidth=1, fill="#1EB955", color="#1E7855") +    
  geom_vline(xintercept=mean_popularity, color="#929292", linetype="dashed", linewidth=0.5) +    
  geom_vline(xintercept=median_popularity, color="#929292", linetype="solid", linewidth=0.5) +    
  labs(title="Figure 1: Histogram of Track Popularity", x="Popularity", y="Count",    
       caption=paste("Dashed line: Mean = 42.82\nSolid line: Median = 45.00")) +    
  theme_minimal() +  
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3)) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = rel(1.5)), 
        axis.title = element_text(size = rel(1.1)),  
        axis.text = element_text(size = rel(1.1)),  
        plot.caption = element_text(size = rel(1.1)))

ad_result <- ad.test(SS$track_popularity)
ad_result

# Question 2b. Plot a histogram of the average track popularity by playlist genre.
# 计算平均和置信区间  
genre_popularity <- SS %>%  
  group_by(playlist_genre) %>%  
  summarise(  
    mean_popularity = mean(track_popularity, na.rm = TRUE),  
    se = sd(track_popularity, na.rm = TRUE) / sqrt(n()),  
    lower = mean_popularity - 1.96 * se,  
    upper = mean_popularity + 1.96 * se)  

# 添加用于显示的标签数据列  
genre_popularity <- genre_popularity %>%  
  mutate(  
    label_mean = paste0("Mean: ", round(mean_popularity, 2)),  
    label_lower = paste0("CI: [", round(lower, 2), ",\n", round(upper, 2), "]")  
  )  

# 绘制图形  
ggplot(genre_popularity, aes(x = reorder(playlist_genre, -mean_popularity), y = mean_popularity)) +  
  geom_bar(stat = "identity", fill="#1EB955", color = "#1E7855", width = 0.75) +  
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, color = "black") +  # 绘制置信区间  
  geom_text(aes(label = label_mean), vjust = -1.5, color = "black", family = "Times New Roman") +  # 在条形图顶部显示平均值  
  geom_text(aes(y = lower, label = label_lower), vjust = 1.2, color = "black", hjust = 0.5, family = "Times New Roman") +  # 在条形图下方显示置信区间  
  labs(x = "Playlist Genre", y = "Average Track Popularity", title = "Figure 2: Average Track Popularity by Playlist Genre with 95% CI") +  
  scale_y_continuous(limits = c(0, 60)) +
  theme_minimal() +  
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3)) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = rel(1.5)), 
        axis.title = element_text(size = rel(1.1)),  
        axis.text = element_text(size = rel(1.1)))


# Question 3. Based on "track_album_release_date", 
# plot the average popularity of each genre over years.
# 计算genre_popularity_by_year
genre_popularity_by_year <- SS %>%  
  # 确保没有缺失的年份或流派信息（如果有，你可能需要先处理它们）  
  filter(!is.na(track_album_release_year), !is.na(playlist_genre)) %>%  
  # 按流派和年份分组  
  group_by(playlist_genre, track_album_release_year) %>%  
  # 计算每个组的平均流行度  
  summarise(mean_popularity = mean(track_popularity, na.rm = TRUE),  
            .groups = 'drop')

# 绘制图表  
ggplot(genre_popularity_by_year, aes(x = track_album_release_year, y = mean_popularity, color = playlist_genre)) +  
  geom_line() +  # 绘制线条图  
  # geom_point() +  # 绘制点图，以便更清楚地看到每个数据点  
  labs(  
    x = "Year",  
    y = "Average Popularity",  
    title = "Figure 3: Average Popularity of Each Genre Over Years",  
    color = "Genre"  
  ) +  
  theme_minimal() +  
  theme(legend.position = "right")  +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3)) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = rel(1.5)), 
        axis.title = element_text(size = rel(1.1)),  
        axis.text = element_text(size = rel(1.1)))


# Question 4. Identify and discuss the features 
# that make a song more "danceable."



# Question 5. Perform a multiple linear regression to predict 
# track popularity using numeric features and the genre of 
# the playlist and answer the following questions

# 5a. As an agent of a label company looking for young talents and 
# their songs, identify the top three important features in a song that 
# make it more likely



# 5b. Assess the goodness of fit of your model. If the model fits the data well,
# explain why you think it's possible to "quantify art." 
# If not, discuss what component(s) might be missing. to be popular.


# 5c. Evaluate whether your regression suffers from multicollinearity.
