
# Setting working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Clearing global environment and removing objects
rm(list=ls())

# Loading relevant libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "quanteda", # for QTA
         "quanteda.textstats", # more Quanteda!
         "quanteda.textplots", # even more Quanteda!
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem", # an alternative method for lemmatizing
         "lubridate" # working with dates
), pkgTest)


data_backup <- df

# Let's also tidy the body_text column before we transform into a corpus
df$Text <- str_replace(df$Text, "\u2022.+$", "")

# Creating a quanteda corpus for use
budget_corpus <- corpus(df, 
                     docid_field = "Filename", 
                     text_field = "Text")
budget_corpus$year <- as.Date(as.character(budget_corpus$year), format = "%Y")

budget_corpus$year <- format(budget_corpus$year, "%Y")
budget_corpus
# Summarising the corpus, using the first 5 documents
budget_corpus_sum <- summary(budget_corpus)
head(budget_corpus_sum)

# Corpus statistics
# We can also plot these simultaneously using lubridate
budget_corpus_sum$ttr <- budget_corpus_sum$Types / budget_corpus_sum$Tokens
mean(budget_corpus_sum$ttr)

mean(budget_corpus_sum$ttr[budget_corpus_sum$INC_rule == 1], na.rm = TRUE) #INC
mean(budget_corpus_sum$ttr[budget_corpus_sum$INC_rule == 0], na.rm = TRUE) #Non-INC

#The ratio shows the lexical diversity of the text
# We can plot this over time as well:
budget_corpus_sum$year <- as.Date(paste0(budget_corpus_sum$year, "-01-01"))

ggplot(data = budget_corpus_sum) +
  geom_point(aes(x = year, y = ttr, color = factor(INC_rule)), size = 2) +
  geom_smooth(aes(x = year, y = ttr, color = factor(INC_rule)), method = "loess", size = 1) +
  labs(title = "Scatter Plot with Smoothed Line by INC_rule") +
  scale_color_manual(values = c("red", "blue"), labels = c("Not INC Ruled", "INC Ruled"))

# Readability
# "Readability" is a qualitative metric. A common scale is Flesch-Kincaid, 
# which estimates readability in terms of school grade (year).

# Let's compare by government type
budget_corpus_sum$fk <- textstat_readability(budget_corpus, measure = "Flesch.Kincaid")

budget_corpus_sum %>%
  filter(grepl("1|0", 
               INC_rule, ignore.case = TRUE)) %>%
  group_by(grp = str_extract(INC_rule, "1|0")) %>%
  summarise(av = mean(fk$Flesch.Kincaid)) %>%
  ggplot(aes(x = reorder(grp, -av), y = av)) +
  geom_col() +
  ggtitle(label = "FK Readability by government type") +
  xlab(label = NULL) +
  ylab("Mean Flesch-Kincaid")

# Creating the tokens list and the dfm
budget_tok <- quanteda::tokens(budget_corpus, 
                           include_docvars = TRUE,
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_separators = TRUE,
                           remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

# The tokens_remove() function allows us to apply the stop_list to our toks object
super_stop <- c("r", "year", "crores", "crore", "government", "budget", "lakhs", 
               "per", "cent", "also", "rate", "increase", "duty", "excise",
               "rs", "propose", "revenue",  "sector", "proposed", "plan", "expenditure",
               "now", "income", "made", "india", "years", "additional", "central",
               "current", "rates", "customs", "present", "state", "act", "order", "national", 
               "duties", "financial", "public", "one", "two", "can", "like", "may", "total",
               "net", "level", "lakh", "however", "set", "areas", "certain", "number", 
               "reduce", "high", "amount", "next", "first", "time", "make", "need", "part", 
               "last", "shall", "large", "small", "must", "take", "country", "due", "basic", 
               "three", "section", "taken", "case", "existing", "well", "goods", "direct",
               "items", "members", "limit", "higher", "prices", "estimates", "estimated",
               "effect", "account", "therefore", "us", "ad", "outlay", "continue", "view",
               "indian", "units", "end", "measure", "increased", "million", "fiscal", 
               "important", "special", "give", "way", "cost", "use", "loss", "full",
               "available", "used", "levy", "reduced", "programme", "help", 'raise', "without",
               "including", "value", "given", "price", "various", "centre", "regard", "five",
               "basis", "period", "less", "specified", "domestic", "service", "revised", 
               "structure", "necessary", "provide", "industry", "provision", "fund", "capital",
               "sir", "rise", "introducing", "1speech", "shri", "annual")
budget_tok <- tokens_remove(budget_tok, super_stop, valuetype = "glob")

# Identify collocations
collocations <- textstat_collocations(budget_tok, size = 2, min_count = 10)
tok_bud <- tokens_compound(budget_tok, pattern = collocations[collocations$z > 17,])

# Remove whitespaces
tok_bud <- tokens_remove(quanteda::tokens(tok_bud), "") 

# Creating the document-feature matrix (dfm) and saving
dfm_budget <- dfm(tok_bud)
saveRDS(dfm_budget, "dfm_budget")

topfeatures(dfm_budget)

# Visualising the dfm using the textplots package from quanteda
dfm_budget %>%
  dfm_trim(min_termfreq = 3) %>%
  textplot_wordcloud(min_size = 1, max_size = 10, max_words = 100)

# Statistics with the dfm 
# Comparing the relative frequency of features
dfm_frq <- textstat_frequency(dfm_budget, n = 20)

dfm_frq %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "feature")

# Sentiment analysis over time
dfm_sentiment <- dfm_lookup(dfm_budget, data_dictionary_LSD2015[1:2])

# Once we have the frequency for positive and negative sentiment, we can 
# use a useful feature of R - vectorisation - to calculate net sentiment 
# across each day and plot it.
docvars(dfm_sentiment, "prop_negative") <- as.numeric(dfm_sentiment[,1] / ntoken(dfm_sentiment))
docvars(dfm_sentiment, "prop_positive") <- as.numeric(dfm_sentiment[,2] / ntoken(dfm_sentiment))
docvars(dfm_sentiment, "net_sentiment") <- docvars(dfm_sentiment, "prop_positive") - docvars(dfm_sentiment,"prop_negative")

net_sentiment_vector <- docvars(dfm_sentiment, "net_sentiment")
year_data <- as.data.frame(docvars(budget_tok))
year_data <- year_data[, c("year")]

sentiment_data <- data.frame(
  year = as.numeric(year_data),
  net_sentiment = as.numeric(net_sentiment_vector)
)

ggplot(sentiment_data, aes(x = year, y = net_sentiment)) +
  geom_point(color = "red") +
  geom_smooth(color = "blue") +
  labs(title = "Sentiment over Time", x = "Year", y = "Net Sentiment")

