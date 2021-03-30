td <- tempdir()
tf <- tempfile(tmpdir = td)

download.file("https://github.com/clarkdatalabs/soms/raw/master/ShakespearePlaysPlus.zip", 
              tf)

fname <- unzip(tf, list = TRUE)$Name[1]
unzip(tf, exdir = td, overwrite = TRUE)
fpath <- file.path(td, fname)
remove(fname)
unlink(tf)

genres <- list.dirs(fpath, full.names = FALSE, recursive = FALSE)

library("readr")
play_table <- data.frame(play = character(), genre = character(), text = character())

for (genre in genres) {
  plays <- list.files(file.path(fpath, genre), pattern = ".txt")
  for (play in plays) {
    play.text <- read_file(file.path(fpath, genre, play), locale(encoding = "UTF-16"))
    play.name <- gsub(".txt", "", play)
    play_table <- rbind(play_table, data.frame(play = play.name, genre = genre, 
                                               text = play.text))
  }
}
remove(genre, plays, play, play.name, play.text)
unlink(td, recursive = FALSE)

# Forming & Cleaning a Corpus
library("tm")

play.vec <- VectorSource(play_table[, "text"])
play.corpus <- Corpus(play.vec)
substr(play.corpus[[1]]$content, 1, 400)

striptags <- function(x) gsub(x, pattern = "<[^>]*>", replacement = "")
play.corpus <- tm_map(play.corpus, content_transformer(striptags))
substr(play.corpus[[1]]$content, 1, 400)

library("SnowballC")
play.corpus <- tm_map(play.corpus, removePunctuation)
play.corpus <- tm_map(play.corpus, removeNumbers)
play.corpus <- tm_map(play.corpus, content_transformer(tolower))
play.corpus <- tm_map(play.corpus, removeWords, stopwords("english"))
play.corpus <- tm_map(play.corpus, stemDocument)
play.corpus <- tm_map(play.corpus, stripWhitespace)
play.corpus <- tm_map(play.corpus, removePunctuation)

substr(play.corpus[[1]]$content, 1, 400)

# Document Term Matrix
play.DTM <- DocumentTermMatrix(play.corpus)
# Restrict to Shared Words
play.DTM.modified <- removeSparseTerms(play.DTM, 0.8)
ncol(play.DTM.modified)
# Normalize Play Vectors
play.DTM.modified <- t(apply(play.DTM.modified, 1, function(x) x/sqrt(sum(x^2))))

# Making a SOM
library(kohonen)

play.SOM <- som(play.DTM.modified, grid = somgrid(6, 6, "hexagonal"), toroidal = FALSE)
plot(play.SOM)

palette.3 <- c("firebrick1", "darkolivegreen3", "dodgerblue2")

plot(play.SOM,
     type = "mapping",
     col = palette.3[as.factor(play_table$genre)],
     bgcol = "lightgray",
     labels = play_table$play)

# Labeling Selected Plays
plays.to.plot <- c("Romeo And Juliet", 
                   "Julius Caesar", 
                   "King Lear", 
                   "Macbeth", 
                   "Othello, the Moor of Venice", 
                   "The Tragedy of King Richard II", 
                   "The Tragedy of King Richard III", 
                   "The Tempest", 
                   "The Taming of the Shrew")
play.labels <- c()
X <- as.vector(play_table$play)
for (i in 1:nrow(play_table)){
  if (X[i] %in% plays.to.plot){play.labels[i] <- X[i]}
  else{play.labels[i]<-"x"}
}

# Visualizing Cell Distance
code.distances <- function(SOM) {
  N <- nrow(SOM$codes)
  # calculate all pairwise distances
  pairwise.distances = c()
  for (i in 1:N) {
    list <- matrix()
    for (j in 1:N) {
      list[j] <- dist(rbind(SOM$codes[i, ], SOM$codes[j, ]))
    }
    pairwise.distances <- cbind(pairwise.distances, list)
  }
  
  ## distance coefficient function, to be applied to geometric GRID distances
  d.coef <- function(x) {
    if (x == 0) {
      return(0)
    } else {
      return(1/(x^4))
    }
  }
  distance.coefficients <- apply(kohonen::unit.distances(SOM$grid, SOM$toroidal), 
                                 c(1, 2), d.coef)
  
  # calculate scaled sum of distances
  A <- distance.coefficients * pairwise.distances
  scaled.dist.sums <- (colSums(A) - min(colSums(A)))/(max(colSums(A)) - min(colSums(A)))
  
  # clean up variables
  remove(i, j)
  
  return(scaled.dist.sums)
}

# SOM Revised Plot

plot(play.SOM, 
     type = "mapping", 
     col = palette.3[as.factor(play_table$genre)],
     bgcol = hsv(h=0,s=0,v=code.distances(play.SOM)), 
     labels = play.labels)
