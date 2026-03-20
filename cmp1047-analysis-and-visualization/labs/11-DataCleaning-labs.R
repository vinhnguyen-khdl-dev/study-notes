file_path <- "~/Documents/study-notes/cmp1047-analysis-and-visualization/labs/data/clients.csv"
df <- read.csv(file_path)
View(df)

df$X <- NULL

size <- ncol(df)
idx <- which(is.na(df$Response))
df[idx, 6:size] <- df[idx, 5:(size-1)]
df[idx, 5] <- NA

idx <- which(df$Marital_Status == "Cycle" &
               !is.na(as.Date(df$Recency, format = "%d-%m-%Y")))
df[idx, 4:(size-1)] <- df[idx, 5:size]
df[idx, size] <- NA

idx <- which(df$Marital_Status == "Cycle")
df[idx, "Marital_Status"] <- df[idx, "Income"]
df[idx, "Income"] <- NA

colSums(is.na(df))

for(x in c("Income", "Kidhome", "Teenhome", "Recency", "MntWines")){
  df[, x] <- as.integer(df[, x])
}

df$Dt_Customer <- as.Date(df$Dt_Customer, format="%d-%m-%Y")

df$Year_Birth[is.na(df$Year_Birth)] <- median(df$Year_Birth, na.rm=TRUE)
df$Income[is.na(df$Income)] <- median(df$Income, na.rm=TRUE)
df$MntWines[is.na(df$MntWines)] <- median(df$MntWines, na.rm=TRUE)

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df$Response[is.na(df$Response)] <- get_mode(df$Response)

colSums(is.na(df))
summary(df)