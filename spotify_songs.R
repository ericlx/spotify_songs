# Subject: Analysis of Spotify Songs
# By: Eric LX
# Date: 12 Jul, 2024


# Requirements
# Your report should be an executive summary with a maximum length of three pages
# Please explain the key numbers in your analysis in plain language and
# provide proper visualizations to support your findings.


# Import necessary library
library(car)
library(broom)
library(dplyr)
library(ggplot2)
library(nortest)
library(corrplot)
library(reshape2)


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
mean_popularity <- mean(SS$track_popularity)
median_popularity <- median(SS$track_popularity)

ggplot(SS, aes(x=track_popularity)) +    
  geom_histogram(binwidth=1, fill="#1EB955", color="#1E7855") +    
  geom_vline(xintercept=mean_popularity, color="#929292", linetype="dashed", linewidth=0.5) +    
  geom_vline(xintercept=median_popularity, color="#929292", linetype="solid", linewidth=0.5) +    
  labs(title="Figure 1: Histogram of Track Popularity", x="Popularity", y="Frequency of Observations",    
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

# Calculating the mean and confidence interval
genre_popularity <- SS %>%  
  group_by(playlist_genre) %>%  
  summarise(  
    mean_popularity = mean(track_popularity, na.rm = TRUE),  
    se = sd(track_popularity, na.rm = TRUE) / sqrt(n()),  
    lower = mean_popularity - 1.96 * se,  
    upper = mean_popularity + 1.96 * se)  

# To show the mean and CI in the figure
genre_popularity <- genre_popularity %>%  
  mutate(  
    label_mean = paste0("Mean: ", round(mean_popularity, 2)),  
    label_lower = paste0("CI: [", round(lower, 2), ",\n", round(upper, 2), "]"))  

# Plot
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
        plot.title = element_text(hjust = 1.25,size = rel(1.5)), 
        axis.title = element_text(size = rel(1.1)),  
        axis.text = element_text(size = rel(1.1)))


# Question 3. Based on "track_album_release_date", 
# plot the average popularity of each genre over years.

# Calculate genre_popularity_by_year
genre_popularity_by_year <- SS %>%  
  # To ensure no missing values in year or genre
  filter(!is.na(track_album_release_year), !is.na(playlist_genre)) %>%  
  # Group by year and genre
  group_by(playlist_genre, track_album_release_year) %>%  
  # Calculate the mean for each genre
  summarise(mean_popularity = mean(track_popularity, na.rm = TRUE), .groups = 'drop')

# Plot
ggplot(genre_popularity_by_year, aes(x = track_album_release_year, y = mean_popularity, color = playlist_genre)) +  
  geom_line() +  # Plot the lines 
  # geom_point() +  # Plot the points
  labs(x = "Track Album Release Year", y = "Average Popularity",  
    title = "Figure 3: Average Popularity of Each Genre Over Years",
    color = "Genre") +  
  theme_minimal() +  
  theme(legend.position = "right")  +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3)) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = rel(1.5)), 
        axis.title = element_text(size = rel(1.1)),  
        axis.text = element_text(size = rel(1.1)))


# Question 4. Identify and discuss the features that make a song more "danceable".

# Calculate the correlation matrix
cor_matrix <- cor(SS[, c("danceability", "energy", "key", "loudness",   
                         "speechiness", "acousticness", "instrumentalness",   
                         "liveness", "valence", "tempo", "duration")],   
                  use = "complete.obs")  

# Transfer the matrix into melt  
cor_melt <- melt(cor_matrix, varnames = c("Var1", "Var2"), value.name = "Correlation")  
cor_melt$Correlation <- round(cor_melt$Correlation, 4)
cor_melt$Correlation

# Filter all abs(correlation coefficient > 0.2)
cor_melt_significant <- cor_melt[abs(cor_melt$Correlation) > 0,]
cor_melt_significant$Correlation <- round(cor_melt_significant$Correlation, 2)

# Plot the correlation matrix with ggplot2  
p <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = Correlation)) +  
  geom_tile() +  
  scale_fill_gradient2(low = "#0FA0D2", mid = "#FAFAFA", high = "#1EB955", midpoint = 0,   
                       limit = c(-1, 1), space = "Lab", name = "Correlation\n") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # rotating the x-axis
        axis.title = element_blank()) + # remove the axis title
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + 
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = rel(1.5)), 
        axis.text = element_text(size = rel(1.1))) +
  labs(title = "Figure 4: Correlation Matrix of Spotify Songs")

p + geom_text(data = cor_melt_significant, aes(label = Correlation), color = "black", family = "Times New Roman")

# Regression analysis of danceability
reg_danceability <- lm(danceability ~ energy + speechiness + liveness + 
                         valence + tempo + duration + factor(mode), data = SS)

# Regression analysis of danceabiltiy ()
reg_danceability <- lm(data = SS, danceability ~ 
                         factor(mode)
                         # + energy 
                         # + speechiness 
                         # + liveness 
                         # + valence 
                         # + tempo 
                         # + duration 
                         # + key 
                         # + loudness
                         # + acousticness
                         # + instrumentalness
                         # + factor(playlist_subgenre)
                       )

summary(reg_danceability)


# Question 5. Perform a multiple linear regression to predict 
# track popularity using numeric features and the genre of 
# the playlist and answer the following questions

# Prepare the scaled independent variables
SS_scaled <- SS

SS_scaled$danceability <- scale(SS$danceability)
SS_scaled$energy <- scale(SS$energy)
SS_scaled$key <- scale(SS$key)
SS_scaled$loudness <- scale(SS$loudness)
SS_scaled$speechiness <- scale(SS$speechiness)
SS_scaled$acousticness <- scale(SS$acousticness)
SS_scaled$instrumentalness <- scale(SS$instrumentalness)
SS_scaled$liveness <- scale(SS$liveness)
SS_scaled$valence <- scale(SS$valence)
SS_scaled$tempo <- scale(SS$tempo)
SS_scaled$duration <- scale(SS$duration)

# 5a. As an agent of a label company looking for young talents and 
# their songs, identify the top three important features in a song that 
# make it more likely to be popular.

# Regression with original independent variables
reg_popularity <- lm(data = SS, track_popularity ~ danceability + energy +
                       key + loudness + speechiness + acousticness + 
                       instrumentalness + liveness + valence + tempo +
                       duration + factor(playlist_genre))

summary(reg_popularity)

# Regression with standardized independent variables
reg2_popularity <- lm(data = SS_scaled, track_popularity ~ danceability + energy +
                        key + loudness + speechiness + acousticness + 
                        instrumentalness + liveness + valence + tempo +
                        duration + factor(playlist_genre))

summary(reg2_popularity)

# 5b. Assess the goodness of fit of your model. If the model fits the data well,
# explain why you think it's possible to "quantify art." 
# If not, discuss what component(s) might be missing. to be popular.

# Analyze the adjusted R-squared
summary(reg_popularity)

# 5c. Evaluate whether your regression suffers from multicollinearity.

# Test for multicolinearity
vif(reg_popularity)
