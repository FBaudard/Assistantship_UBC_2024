
install.packages("stm")
library(stm)
library(stringi)
library(dplyr)

set.seed(1234)

#Reading and cleaning data

dat <- read.csv("C:/Users/bauda/OneDrive/Documents/UBC/RA/lca_dat_french2.csv")

# benefits
data <- dat[ ,c("X", "gender_bin", "risks_ai_trade")] |>
  filter(!is.na(gender_bin))
data$text <- stri_trans_tolower(data$risks_ai_trade)
data$text <- iconv(data$text, to = "UTF-8")
data$text <- gsub("'", "", data$text)
data$text <- gsub("<.+>", "", data$text)

#The text processor functions removes stop words, removes lowercase and tokenizes it
#Then the prepdocument extracts the metadata (gender), documents (the answers), and the vocabulary (the stem words processed by the textProcessor function)
processed <- textProcessor(data$text, metadata = data) 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#In case of sparse and numerous data, it can be useful to remove some words to keep the final clusters more coherent. The plotRemoved function allows us to modelize the impact of thresholds on the final number of words: a threshold of 3 means that the words present in less than 3 documents are removed. As our data is small, it is better not to weed out any. I therefore chose 1 as the lower threshold in the prepDocuments function. 
plotRemoved(processed$documents, lower.thresh = seq(1, 10, by = 1))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 1)


#searchK allows to run different models with different numbers of K topics to compare their statistics. I set gender_bin as the covariate.
storage <- searchK(out$documents, out$vocab, K = seq(5, 15, by = 1), prevalence = ~ gender_bin, data = meta)


plot(storage)


# The diagnostic highlights that 5Ks has the highest semantic coherence (-210) yet its residuals are slightly more important than other models
# On the other hand, 7Ks shows the second highest semantic coherence (-220) and the least amount of residuals
# I think you should look closely at the topics to settle on the number of K


#7K
stm_model <- stm(documents = out$documents, vocab = out$vocab, K = 7, prevalence = ~ gender_bin, max.em.its = 300, data = out$meta, init.type = "Spectral")

labelTopics(stm_model)

#5K

stm_model <- stm(documents = out$documents, vocab = out$vocab, K = 5, prevalence = ~ gender_bin, max.em.its = 300, data = out$meta, init.type = "Spectral")

labelTopics(stm_model)

#This snipet studies the effect of gender on the prevalence of each topic 
out$meta$gender_bin <- as.factor(out$meta$gender_bin)
prep <- estimateEffect(1:7 ~ gender_bin, stm_model, meta = out$meta, uncertainty = "Global")


#shows the summaries of the estimated effects of gender on each topics
summary(prep, topics = 1:7)


plot(stm_model, type = "summary", xlim = c(0, 0.3))


plot(prep, 
     covariate = "gender_bin", 
     topics = seq(1, 7 , by = 1), 
     model = stm_model, 
     method = "difference", 
     cov.value1 = "Man", 
     cov.value2 = "Woman", 
     xlab = "Men ... Women", 
     main = "Effect of Gender affiliation", 
     xlim = c(-0.1, 0.1)
)

#Adding a content argument to the model allows the word to define a topic to change between the gender to study how each gender words the topics.
#By taking into account how each gender speaks of each topics, the model renders different clusters and the estimated effects are different.
stm_content <- stm(documents = out$documents, vocab = out$vocab, K = 7, prevalence = ~ gender_bin, content = ~ gender_bin, max.em.its = 300, data = out$meta, init.type = "Spectral")

#This code studies how gender influences the repartition of vocab inside a topic
for (i in 1:7) {
  plot(stm_content, type = "perspectives", topics = i)
}
