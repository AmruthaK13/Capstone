#Libraries needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "https://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(RWeka)) install.packages("RWeka", repos = "http://cran.us.r-project.org")
if(!require(qdap)) install.packages("qdap", repos = "https://cran.us.r-project.org")
if(!require(tm)) install.packages("tm", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "https://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "https://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
#Loading the dataset
Spotify_Telugu <- read_excel("Documents/Right now/Spotify Telugu.xlsx")
#Summary statistics
head(Spotify_Telugu)
summary(Spotify_Telugu)
#Correlation between variables
Spotify_Telugu_num <- Spotify_Telugu[,-(1:3)]
MCor <- cor(Spotify_Telugu_num)
MCor
corrplot(MCor, method = "ellipse")
#Data Visualization
ggplot(Spotify_Telugu, aes(x = Danceability)) + geom_histogram(bins = 25) + theme_minimal()
ggplot(Spotify_Telugu, aes(x = Loudness)) + geom_histogram(bins = 25) + theme_minimal()
ggplot(Spotify_Telugu, aes(x = Speechiness)) + geom_histogram(bins = 25) + theme_minimal()
ggplot(Spotify_Telugu, aes(x = Valence)) + geom_histogram(bins = 25) + theme_minimal()
#Artists with more than 2 songs on the list
A1 <- group_by(Spotify_Telugu, Artists)
A2 <- dplyr::summarise(A1, count=n())
A2 <- arrange(A2, desc(count))
A3 <- filter(A2, count>1)
AP1 <- ggplot(A3, aes(x = reorder(Artists, count), y = count)) +
  geom_bar(aes(y = count, fill = Artists), stat = "identity") +
  labs(x = "Artists", y = "Number of Songs",
       title = "Artists with more than 2 songs") + theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1))
AP1
#Top Artists
Top_Artists <- Spotify_Telugu %>%
  group_by(Artists) %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 1) %>%
  arrange(desc(n_apperance))
Top_Artists$Artists <- factor(Top_Artists$Artists, levels = Top_Artists$Artists [order(Top_Artists$n_apperance)])
head(Top_Artists,10)
ggplot(Top_Artists, aes(x = Artists, y = n_apperance)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.6) +
  labs(title = "Popular Telugu Artists of 2019", x = "Artists", y = "Number of Apperances in the list") +
  theme(plot.title = element_text(size=15, hjust=-3, face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust=2, size = 3, color = 'white') +
  coord_flip()
#Popular Albums
Top_Albums <- Spotify_Telugu %>%
  group_by(Album) %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 1) %>%
  arrange(desc(n_apperance))
Top_Albums$Album <- factor(Top_Albums$Album, levels = Top_Albums$Album [order(Top_Albums$n_apperance)])
head(Top_Albums, 10)
ggplot(Top_Albums, aes(x = Album, y = n_apperance)) +
  geom_bar(stat = "identity" , fill = "red", width = 0.6) +
  labs(title = "Popular Telugu Albums of 2019", x = "Albums", y = "Number of Apperances in the list") +
  theme(plot.title = element_text(size=15, hjust=-3, face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust=2, size = 3, color = 'white') +
  coord_flip()
#Consolidating all the numerical values of features 
Spotify_Telugu_num_norm <- sapply(Spotify_Telugu_num, scale)
summary(Spotify_Telugu_num_norm)
#Common keys among the songs
Spotify_Telugu$Key <- as.character(Spotify_Telugu$Key)
Spotify_Telugu$Key <- revalue(Spotify_Telugu$Key, c("0" = "C","1" = "C♯,D♭", "2" = "D", "3" = "D♯,E♭","4" = "E", "5" = "F", "6" = "F♯,G♭", "7" = "G", "8" = "G♯,A♭", "9" = "A", "10" = "A♯,B♭", "11" = "B"))
song_keys <- Spotify_Telugu %>%
  group_by(Key) %>%
  summarise(n_key = n()) %>%
  arrange(desc(n_key))
song_keys$Key <- factor(song_keys$Key, levels = song_keys$Key[order(song_keys$n_key)])
#Plot the keys
ggplot(song_keys, aes(x = reorder(Key, -n_key), y = n_key, fill = reorder(Key, -n_key))) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of keys of the top telugu songs", x = "Keys", y = "Count of Keys on the Top songs") +
  geom_text(aes(label=n_key), position = position_stack(vjust=0.8)) +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"), axis.title = element_text(size=12)) +
  theme(legend.position = "none")
#Density of the correlation 
correlated_density <- ggplot(Spotify_Telugu) +
  geom_density(aes(Danceability, fill = "Danceability", alpha = 0.1)) +
  geom_density(aes(Valence, fill = "Valence", alpha = 0.1)) +
  geom_density(aes(Loudness, fill = "Loudness", alpha = 0.1)) +
  geom_density(aes(Speechiness, fill = "Speechiness", alpha = 0.1)) +
  scale_x_continuous(name = "Danceability, Valence, Speechiness, Loudness") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Danceability, Valence, Speechiness, Loudness") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette="Accent")
correlated_density
#Logistic regression
Spotify_Telugu$Artists <- as.factor(Spotify_Telugu$Artists)
Spotify_Telugu <- Spotify_Telugu %>%
  select(Artists, Danceability, Valence, Speechiness, Loudness)
mp_siz = floor(0.80*nrow(Spotify_Telugu))
set.seed(123)
train_data = sample(seq_len(nrow(Spotify_Telugu)), size = smp_siz)
train = Spotify_Telugu[train_data,]
test = Spotify_Telugu[-train_data,]
test$Artists <- as.factor(test$Artists)
train$Artists <- as.factor(train$Artists)
logit_Spotify <- glm(Artists ~ Danceability + Valence , data = train, family = "binomial")
summary(logit_Spotify)
attach(test)
pdata <- predict(logit_Spotify, newdata = test, type = "response")
pdata = ifelse(pdata > .5, 1, 0)
table(pdata, test$Artists)
#Average model Danceability
mu <- mean(train$Danceability)
mu
basic_rmse_dancebility <- RMSE(test$Danceability, mu)
basic_rmse_dancebility

#Average model Loudness
basic_rmse_loudness <- RMSE(test$Loudness, mu)
basic_rmse_loudness

#Average model Valence
basic_rmse_valence <- RMSE(test$Valence, mu)
basic_rmse_valence

#Average model Speechiness
basic_rmse_speechiness <- RMSE(test$Speechiness, mu)
basic_rmse_speechiness












