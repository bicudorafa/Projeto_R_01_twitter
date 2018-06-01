## MUITO MELHOR para realizar analises de sentimento, possui muito menos bugs e muito mais facilidades


## Etapa 1: Autenticao, conexao e captura dos tweets

# Ativacao dos pacotes necessarios ao Text Mining e manipulacao dos dados
#install.packages(c('rtweet', 'dplyr'))
library(rtweet)
library(dplyr)

# Configurando autenticao e conexao com a API do Twitter
#consumer_key <- "grszK51RR77nWurVdFHadXKBw"
#consumer_secret <- "bQJP8r5ISNdSeJBJJxYNDsfHkWUpUGUqQQv6qHWJxQBii8id4J"
#access_token <- "983872756935725056-iAk7AbmOfzXmGDa1Cx0sWA98dPha2Cu"
#access_secret <- "VYKJnbkfL9x2ODPe4ECy40JhcEGJpHTjdrNVZ9WCt6zJ9"
#setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

twitter_tokens <- create_token(app = "Project_LanguageProcessing_rbr",
                               consumer_key = "grszK51RR77nWurVdFHadXKBw", 
                               consumer_secret = "bQJP8r5ISNdSeJBJJxYNDsfHkWUpUGUqQQv6qHWJxQBii8id4J")

# Realizando a coleta dos tweets (27/04/18), extraindo apenas dados referentes aos tweets e preparando para an?lise
tema <- "Dolly"
qtd_tweets <- 1000
lingua <- "pt"
#tweetsSearch <- search_tweets(tema, n = qtd_tweets, lang = lingua, include_rts = FALSE)
tweetsSearch <- as.tbl(read.csv('tweetsSearch_dolly.csv', stringsAsFactors = F))
tweets_text <- tweetsSearch %>% 
select(text) %>% 
mutate(tweet_id = row_number()) %>%
select(tweet_id, text)

## Etapa 2: Tratamento dos dados

# Ativacao dos pacotes necessarios ao Text Mining e manipulacao dos dados
#install.packages(c('tidytext', 'stringr', 'stopwords'))
library(tidytext)
library(stringr)
library(stopwords)
library(ggplot2)

# Remocao de problemas de encoding comuns ao fazer text mining pela internet 

tweets_text$text <- tweets_text$text %>% 
  iconv(to = "ASCII//TRANSLIT") %>% # Corrige encoding do texto do post (ao menos uma parte deles)
  iconv(sub="", 'UTF-8', 'ASCII') # Remove emojis

# Criacao dos tokens e limpeza final dos dados  

tweets_tidy <- tweets_text %>%   
  unnest_tokens(word, text) %>% # Transformando texto em tokens
  filter(!word %in% c("https", "t.co", "amp", 'http', 'htt','bmr9ju8t8y', 'fwplgwxu4o', 'f0', 
                      'es','009f','00a0', 'ac', 'sa3', '008e', '00bc'),   # Removendo alguns lixos conhecidos
         !word %in% tolower(tweetsSearch$screen_name), # Removendo nome de usuarios
         !grepl("^\\d+$", word)) # Removendo numeros

# Definicao das stopwords
stopwords_assunto <- c('nao', 'sao', 'pra', 'q', 'dolly','r', 'u', 'refrigerante', 'refrigerantes')
stopwords_tweets <- data_frame(word = c(stopwords('portuguese'), stopwords_assunto))

# Termos mais frequentes
tweets_tidy %>% 
  anti_join(stopwords_tweets) %>% 
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(title = 'Termos mais frequentes', y = 'contagem dos termos')

## Etapa 3: Wordcloud, analise das associacoes entre as palavras e clusterizacao hierarquica (dendogramas)

# Wordcloud
#install.packages(c('wordcloud', 'RColorBrewer'))
library(wordcloud)
library(RColorBrewer)

tweets_tidy %>%
  anti_join(stopwords_tweets) %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq = 5,
                 max.words = 200, random.order = FALSE, rot.per = 0.35, 
                 colors = brewer.pal(8, "Dark2")))

# Criacao do Corpus e TDM para analise lexica
#install.packages('tm')
library(tm)

tweets_tdm <- tweets_tidy %>%
  anti_join(stopwords_tweets) %>%
  group_by(tweet_id, word) %>%
  summarise(count = n()) %>%
  cast_tdm(document = tweet_id, term = word, value = count)

# Encontrando as palavras que aparecem com mais frequencia
findFreqTerms(tweets_tdm, lowfreq = 90)

# Buscando associacoes
findAssocs(tweets_tdm, c('dono'), c(0.30))

# Removendo termos esparsos (nao utilizados frequentemente)
tweets_maisUsados <-removeSparseTerms(tweets_tdm, sparse = 0.95)

# Distance Matrix
tweets_df = as.data.frame(as.matrix(tweets_maisUsados))
tweets_distancias <- dist(tweets_df)

# Realizando a clusterizacao hierarquica dos dados
tweets_hclust <- hclust(d = tweets_distancias)

# Criando o dendograma (verificando como as palvras se agrupam)
plot(tweets_hclust)

# Verificando os grupos
cutree(tweets_hclust, k = 5)

# Visualizando os grupos de palavras no dendograma
rect.hclust(tweets_hclust, k = 5, border = "red")

## Analise de Sentimento (Opinion Mining)

# Carregando pacotes necessarios
#install.packages(c('devtools', 'tidyr'))
#devtools::install_github("sillasgonzaga/lexiconPT")
library(lexiconPT)
library(tidyr)

# Carregando lexico escolhido
data("sentiLex_lem_PT02")
sentiLex <- as.tbl(sentiLex_lem_PT02)
sentimentos <- sentiLex %>%
  select(word = term, polarity)

# Calculando o sentimento de cada palavra 
tweets_sentimentos <- tweets_tidy %>%
  filter(!word %in% c('preso', 'fraude', 'acusado', 'imposto', 'acusado')) %>% 
# excluem-se essas palavras por possuirem score, mas facam parte do assunto diretamente
  inner_join(sentimentos)

# Plotando a distribuicao do sentimento agregado dos tweets
tweets_sentimentos %>%
  group_by(tweet_id) %>%
  summarise(sentimento_tweet = sum(polarity)) %>%
  ggplot(aes(sentimento_tweet)) +
  geom_bar(fill = 'red', colour = 'red', alpha = .1) +
  labs(title = 'Sentimentos Predominantes nos Tweets', x = 'Score dos Tweets')

# Analisando as palavras mais frequentes por sentimento
tweets_sentimentos %>%
  count(word, polarity, sort = TRUE) %>%
  filter(n > 5, !polarity == 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = as.factor(polarity))) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ polarity, scales = "free_y") +
  labs(title = 'Palavras mais frequentes por score', 
       y = "Frequencia por score",
       x = NULL) +
  coord_flip() +
  theme(legend.position = "none") 
  
