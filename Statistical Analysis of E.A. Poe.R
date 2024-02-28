library(gutenbergr)
library(tidytext)
library(dplyr)

my_mirror <- "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/"
df <- gutenberg_metadata

unique(df$author)[startsWith(unique(df$author), "Po")]
gutenberg_works(author ==  "Poe, Edgar Allan")
Poe <- gutenberg_download(25525, mirror = my_mirror)

words_Poe <- unnest_tokens(Poe, words, text)
count_words <- (count(words_Poe, words, sort = TRUE))
barplot(count_words$n[1:10], names.arg = count_words$words[1:10])

bigrams_Poe <- unnest_tokens(Poe, words, text, token = "ngrams", n=2)
count_bigrams <- (count(bigrams_Poe, words, sort = TRUE))
count_bigrams <- count_bigrams[-1,]
barplot(count_bigrams$n[1:10], names.arg = count_bigrams$words[1:10])

bigram <- "with grape"
first_word <- "with"
second_word <- "grape"
count_bigram <- count_bigrams[startsWith(count_bigrams$words, bigram),]
count_first_word <- count_bigrams[startsWith(count_bigrams$words, first_word),]
count_second_word <- count_bigrams[endsWith(count_bigrams$words, second_word),]

bigram_together <- count_bigram$n
first_word_only <- sum(count_first_word$n) - bigram_together 
second_word_only <- sum(count_second_word$n) - bigram_together 
no_bigram <- sum(count_bigrams$n) - bigram_together - first_word_only - second_word_only

freq_bigram <- matrix(c(bigram_together, first_word_only, second_word_only, no_bigram), ncol=2, byrow=T)
mosaicplot(freq_bigram)
chisq.test(freq_bigram)

freq_bigram

mt <- count_bigrams[startsWith(count_bigrams$words, "with grape"),]
m <- count_bigrams[startsWith(count_bigrams$words, "with"),]
t <- count_bigrams[endsWith(count_bigrams$words, "grape"),]

more_than <- mt$n
more_other <- sum(m$n) - more_than
other_than <- sum(t$n) - more_than
other_other_mt <- sum(count_bigrams$n) - more_than - more_other - other_than

freq_mt <- matrix(c(more_than, more_other, other_than, other_other_mt), ncol=2, byrow=T)
mosaicplot(freq_mt)
chisq.test(freq_mt)

entropy <- c()
for (i in 0:413)
{
  entr <- words_Poe[(i*1000+1):(i*1000+1000),2]
  char <- unnest_tokens(entr, token, words, token = "characters")
  df.char <- as.data.frame(count(char, token, sort=TRUE))
  df.char$relfreq <- df.char$n/sum(df.char$n)
  df.char$ent <- df.char$relfreq*log2(df.char$relfreq)
  entropy <- c(entropy, -sum(df.char$ent))
}
plot(entropy)

t.test(entropy)

Poe1 <- words_Poe[1:103559, 2]
Poe2 <- words_Poe[103560:207118, 2]
Poe3 <- words_Poe[207119:310677, 2]
Poe4 <- words_Poe[310678:414235, 2]

Poe1_count_words <- (count(Poe1, words, sort = TRUE))
Poe2_count_words <- (count(Poe2, words, sort = TRUE))
Poe3_count_words <- (count(Poe3, words, sort = TRUE))
Poe4_count_words <- (count(Poe4, words, sort = TRUE))

line <- 1178
sentence <- unnest_tokens(Poe[line, 2], words, text)

x <- c()
for (i in sentence$words) 
{x1 <- Poe1_count_words[Poe1_count_words$words == i,]$n
 x2 <- Poe2_count_words[Poe2_count_words$words == i,]$n
 x3 <- Poe3_count_words[Poe3_count_words$words == i,]$n
 x4 <- Poe4_count_words[Poe4_count_words$words == i,]$n
 xsum <- (x1+x2+x3+x4)
 x <- c(x, x1/xsum, x2/xsum, x3/xsum, x4/xsum)
}

relfreq_matr <- matrix(x, ncol=4, byrow=TRUE)
relfreq_matr
     
NBC <- c()
for (i in 1:4) 
{
  NBC <- c(NBC, (prod(relfreq_matr[,i])*(1/4)))
}

NBC
barplot(NBC, names.arg = c("1st part", "2nd part", "3rd part", "4th part"))

if (line < (49213/4)) {
  print("The sentence is in the 1st part")
} else if (line < (49213/2)) {
  print("The sentence is in the 2nd part")
} else if (line < (49213*(3/4))) {
  print("The sentence is in the 3rd part")
} else {
  print("The sentence is in the 4th part")
}

