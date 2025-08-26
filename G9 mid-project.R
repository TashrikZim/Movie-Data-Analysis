
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("corrplot")

library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)

url <- "https://drive.google.com/uc?id=1DgQL_uai85Y3kEdHhrVfvY2zMB3-NtzD"
movie_data <- read.csv(url)

head(movie_data)
str(movie_data)
summary(movie_data)


colSums(is.na(movie_data))
movie_clean <- na.omit(movie_data)
cat("Total NA values after cleaning:", sum(is.na(movie_clean)), "\n")
movie_clean <- movie_clean[!duplicated(movie_clean), ]


get_mode <- function(v) {
  v <- v[!is.na(v)]
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
}


mean_budget  <- mean(movie_clean$budget, na.rm=TRUE)
median_budget<- median(movie_clean$budget, na.rm=TRUE)
mode_budget  <- get_mode(movie_clean$budget)
sd_budget    <- sd(movie_clean$budget, na.rm=TRUE)
iqr_budget   <- IQR(movie_clean$budget, na.rm=TRUE)

cat("Budget - Mean:", mean_budget, " Median:", median_budget, 
    " Mode:", mode_budget, " SD:", sd_budget, " IQR:", iqr_budget, "\n")


mean_gross   <- mean(movie_clean$gross, na.rm=TRUE)
median_gross <- median(movie_clean$gross, na.rm=TRUE)
mode_gross   <- get_mode(movie_clean$gross)
sd_gross     <- sd(movie_clean$gross, na.rm=TRUE)
iqr_gross    <- IQR(movie_clean$gross, na.rm=TRUE)

cat("Gross - Mean:", mean_gross, " Median:", median_gross, 
    " Mode:", mode_gross, " SD:", sd_gross, " IQR:", iqr_gross, "\n")


movie_clean <- movie_clean %>% mutate(profit = gross - budget)
mean_profit  <- mean(movie_clean$profit, na.rm=TRUE)
median_profit<- median(movie_clean$profit, na.rm=TRUE)
mode_profit  <- get_mode(movie_clean$profit)
sd_profit    <- sd(movie_clean$profit, na.rm=TRUE)
iqr_profit   <- IQR(movie_clean$profit, na.rm=TRUE)

cat("Profit - Mean:", mean_profit, " Median:", median_profit, 
    " Mode:", mode_profit, " SD:", sd_profit, " IQR:", iqr_profit, "\n")


mean_imdb    <- mean(movie_clean$imdb_score, na.rm=TRUE)
median_imdb  <- median(movie_clean$imdb_score, na.rm=TRUE)
mode_imdb    <- get_mode(movie_clean$imdb_score)
sd_imdb      <- sd(movie_clean$imdb_score, na.rm=TRUE)
iqr_imdb     <- IQR(movie_clean$imdb_score, na.rm=TRUE)

cat("IMDb Score - Mean:", mean_imdb, " Median:", median_imdb, 
    " Mode:", mode_imdb, " SD:", sd_imdb, " IQR:", iqr_imdb, "\n")


mean_duration <- mean(movie_clean$duration, na.rm=TRUE)
median_duration<-median(movie_clean$duration, na.rm=TRUE)
mode_duration <- get_mode(movie_clean$duration)
sd_duration   <- sd(movie_clean$duration, na.rm=TRUE)
iqr_duration  <- IQR(movie_clean$duration, na.rm=TRUE)

cat("Duration - Mean:", mean_duration, " Median:", median_duration, 
    " Mode:", mode_duration, " SD:", sd_duration, " IQR:", iqr_duration, "\n")



movie_clean$primary_genre <- sapply(strsplit(as.character(movie_clean$genres), "\\|"), `[`, 1)

genre_counts <- sort(table(movie_clean$primary_genre), decreasing = TRUE)

genre_df <- data.frame(Genre = names(genre_counts), Count = as.numeric(genre_counts))

top15_genres <- head(genre_df, 15)
library(ggplot2)
ggplot(top15_genres, aes(x = reorder(Genre, Count), y = Count)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 15 Movie Genres", x = "Genre", y = "Number of Movies")

movie_data %>%
  group_by(content_rating) %>%
  summarise(
    count = n(),
    mean_score = mean(imdb_score, na.rm = TRUE),
    sd_score = sd(imdb_score, na.rm = TRUE)
  )





ggplot(movie_clean, aes(x = imdb_score)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of IMDb Scores", x = "IMDb Score", y = "Frequency")


ggplot(movie_clean, aes(x = budget)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of Movie Budgets (Log Scale)", x = "Budget", y = "Count")


pairs(movie_data[, c("imdb_score", "duration", "gross", "budget")],
      main = "Scatterplot Matrix of Movie Data",
      col = as.factor(movie_data$content_rating))


ggplot(movie_data, aes(x = budget, y = gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Budget vs Gross Revenue", x = "Budget", y = "Gross Revenue")


ggplot(movie_clean, aes(x = imdb_score, y = profit)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "IMDb Score vs Profit", x = "IMDb Score", y = "Profit")


ggplot(movie_data, aes(y = gross)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Gross Revenue", y = "Gross Revenue")




num_cols <- movie_data[, sapply(movie_data, is.numeric)]
num_cols <- num_cols[, colSums(!is.na(num_cols)) > 0]
corr_matrix <- cor(num_cols, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)



movies_filtered <- movie_clean %>% 
  filter(gross > 1e8)

movies_selected <- movies_filtered %>% 
  select(movie_title, imdb_score, budget, gross)

movies_mutated <- movies_selected %>%
  mutate(profit = gross - budget)

movies_scaled <- movies_selected %>%
  mutate(across(c(imdb_score, budget, gross), ~ scale(.)[,1]))
head(movies_scaled)



model_simple <- lm(gross ~ budget, data = movie_clean)
plot(movie_clean$budget, movie_clean$gross,
     main = "Simple Linear Regression: Budget vs Gross",
     xlab = "Budget", ylab = "Gross")
abline(model_simple, col = "blue", lwd = 2)



