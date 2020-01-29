##########################################################################
##########################################################################
##########################################################################

#' @title Transform
#'
#' @description This function transforms JSTOR DFR files into single row tidy dataframes
#'
#' @param filePath
#'
#' @return A single row dataframe containing informations about: Journal, Publisher, Title, Year, Abstract and Language
#'
#' @examples Transform("C:/Users/.../Documents/metadata/file1.xml")
#'
#'
#' @export

###########################################
#Função que percorre os caminhos de um arquivo xml e retorna um df (1x1) com os dados
Transform <- function (filePath)
{
  a <- xmlParse (filePath)
  aTop <- xmlRoot (a)

  Journal <- xmlValue(aTop[["front"]][["journal-meta"]][["journal-title-group"]][["journal-title"]])
  Publisher <- xmlValue (aTop[["front"]][["journal-meta"]][["publisher"]][["publisher-name"]])
  Title <- xmlValue (aTop[["front"]][["article-meta"]][["title-group"]][["article-title"]])
  Year <- as.integer(xmlValue(aTop[["front"]][["article-meta"]][["pub-date"]][["year"]]))
  Abstract <- xmlValue(aTop[["front"]][["article-meta"]][["abstract"]])
  Language <- xmlValue(aTop[["front"]][["article-meta"]][["custom-meta-group"]][["custom-meta"]][["meta-value"]])

  df <- data.frame (Journal, Publisher, Title, Year, Abstract, Language, stringsAsFactors = FALSE)

  df
}

##########################################################################
##########################################################################
##########################################################################

#' @title JSTOR_df
#'
#' @description This function transforms all files in a folder with JSTOR's DFR metada into a tidy dataframes
#'
#' @param filePath
#'
#' @return A dataframe containing informations about: Journal, Publisher, Title, Year, Abstract and Language
#'
#' @examples JSTOR_df("C:/Users/.../Documents/metadata/")
#'
#'
#' @export
###########################################
#Função que cria um df com todos os arquivos
JSTOR_df <- function (filePath)
{

  files <- list.files(filePath, "*.xml", full.names=TRUE)
  df<-pblapply(files, Transform)
  df<-ldply(df, data.frame)
  
}

#Função que limpa os dados do data frame de abstracts ou titulos
CleanJSTOR_df <- function (df, Titles=FALSE)
{

  #Exclui Artigos com nomes repetidos: "front matter", "back matter"...
  dfJournals <- (df%>%
                   group_by (Title)%>%
                   summarise (n = n ())%>%
                   filter(n < 2) %>%
                   arrange(desc(n)))

  df <- df%>%
    inner_join (dfJournals)

  #Retira NAs
  if (Titles == FALSE){
    df <- df[complete.cases(select(df, Abstract)),]
  }else{
    df <- df[complete.cases(select(df, Title)),]
  }

  #Seleção de abstracts estritamente em inglês
  df <- (df%>%
           filter(Language == "eng" | Language == "en")%>%
           select(Title, Year, Abstract, Language, Journal, Publisher))

  df <- df%>%
    filter(!str_detect(Abstract, " et "))%>%
    filter(!str_detect(Abstract, " und "))%>%
    filter(!str_detect(Abstract, " la "))%>%
    filter(Year < 2011)

  df
}

##############################################
#Função que plota total de arquivos antes e após limpeza
plotCleaningResult <- function(dfClean, dfDirt)
{

  TotalLimpo <- (dfClean%>%
                   group_by (Year)%>%
                   summarise (n = n ()))

  TotalSujo <- (dfDirt%>%
                  group_by (Year)%>%
                  summarise (n = n ()))

  graph <-ggplot(TotalSujo, aes(Year, n)) +
    geom_line(stat = "identity", fill = "navyblue", color = "black") +
    geom_line(data=TotalLimpo, color = "green") +
    theme_minimal()

  graph
}

##############################################
#Função que plota palavras que aparecem pelo menos x anos no top y
JSTORrepeatedTopwords <- function (dfEco, y, x)
{

  #Principais palavras ao longo do tempo
  AbstractsTidyYear <- dfEco %>%
    unnest_tokens (word, Abstract)%>%
    anti_join(custom_stop_words)%>%
    count(Year, word, sort = TRUE)%>%
    group_by(Year)%>%
    mutate(n = n/n())%>%
    top_n(y)%>%
    arrange(desc(Year))%>%
    ungroup()

  #Filtrar palavras que aparecem pelo menos x vezes(anos) no top

  k <- !sum(str_detect(AbstractsTidyYear$word, AbstractsTidyYear$word[1]))<x
  i=2
  while(i<=nrow(AbstractsTidyYear))
  {
    a <- !sum(str_detect(AbstractsTidyYear$word, AbstractsTidyYear$word[i]))<x
    k <- rbind(k, a)
    i = i+1
  }

  k <- data.frame(k)
  AbstractsTidyYear <- cbind(AbstractsTidyYear, logi = k[,1])

  AbstractsTidyYear <- AbstractsTidyYear %>%
    filter(logi>0)

  #Vizualização
  graph<-ggplot(AbstractsTidyYear, aes(Year, reorder(word, Year), size = n)) +
    geom_point(alpha=0.5)

  graph
}

##############################################
#Função que plota palavras que aparecem pelo menos x anos no top y
JSTORrepeatedTopTrigrams <- function (dfEco, y, x)
{

  AbstractsTidyTriYear <- dfEco %>%
    unnest_tokens(trigram, Abstract, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% custom_stop_words$word,
           !word2 %in% custom_stop_words$word,
           !word3 %in% custom_stop_words$word) %>%
    count(Year, word1, word2, word3, sort = TRUE)%>%
    unite(trigram, word1, word2, word3, sep = " ")%>%
    group_by(Year)%>%
    mutate(n = n/n())%>%
    top_n(y)%>%
    arrange(desc(Year))%>%
    ungroup()

  #Filtrar trigrams que aparecem pelo menos x vezes(anos) no top

  k <- !sum(str_detect(AbstractsTidyTriYear$trigram, AbstractsTidyTriYear$trigram[1]))<x
  i=2
  while(i<=nrow(AbstractsTidyTriYear))
  {
    a <- !sum(str_detect(AbstractsTidyTriYear$trigram, AbstractsTidyTriYear$trigram[i]))<x
    k <- rbind(k, a)
    i = i+1
  }

  k <- data.frame(k)
  AbstractsTidyTriYear <- cbind(AbstractsTidyTriYear, logi = k[,1])

  AbstractsTidyTriYear <- AbstractsTidyTriYear %>%
    filter(logi>0)

  graph<-ggplot(AbstractsTidyTriYear, aes(Year, reorder(trigram, Year), size = n)) +
    geom_point(alpha=0.5)

  graph

}

#################################################################
#Função que plota palavras que aparecem pelo menos x anos no top y
JSTORrepeatedTopBigrams <- function (dfEco, y, x)
{

  #Bigram
  AbstractsTidyBiYear <- dfEco %>%
    unnest_tokens(bigram, Abstract, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% custom_stop_words$word,
           !word2 %in% custom_stop_words$word) %>%
    count(Year, word1, word2, sort = TRUE)%>%
    unite(bigram, word1, word2, sep = " ")%>%
    group_by(Year)%>%
    mutate(n = n/n())%>%
    top_n(y)%>%
    arrange(desc(Year))%>%
    ungroup()

  #Filtrar trigrams que aparecem pelo menos x(tal que x =3) vezes(anos) no top

  k <- !sum(str_detect(AbstractsTidyBiYear$bigram, AbstractsTidyBiYear$bigram[1]))<x
  i=2
  while(i<=nrow(AbstractsTidyBiYear))
  {
    a <- !sum(str_detect(AbstractsTidyBiYear$bigram, AbstractsTidyBiYear$bigram[i]))<x
    k <- rbind(k, a)
    i = i+1
  }

  k <- data.frame(k)
  AbstractsTidyBiYear <- cbind(AbstractsTidyBiYear, logi = k[,1])

  AbstractsTidyBiYear <- AbstractsTidyBiYear %>%
    filter(logi>0)

  graph<-ggplot(AbstractsTidyBiYear, aes(Year, reorder(bigram, Year), size = n)) +
    geom_point(alpha=0.5)

  graph
}

##############################################
#Função que plota contagem de palavras por ano daquele determinado dataframe (obs: pode ser uma seleção)
# exemplo ArrayVocab = c("theory", "theories", "theoretical")
JSTORplotVocabCount <- function (dfEco, ArrayVocab)
{

  #Tokenization = Transformar dados em tidy com tidytext = outro tipo de dado para análise de texto e sentimento
  AbstractsTidy <- dfEco %>%
    unnest_tokens (word, Abstract)

  AbstractsTidy <- (AbstractsTidy%>%
                      anti_join(custom_stop_words))

  Total <- (dfEco %>%
              group_by (Year)%>%
              summarise (totalJournalsEco = n ()))

  scoreVector <- rep(1, length(ArrayVocab))

  vocab <- data.frame(word = as.character(ArrayVocab),
                      score = scoreVector)

  AbstractsVocab <- AbstractsTidy %>%
    inner_join(vocab) %>%
    group_by(Year) %>%
    summarise (VocabScore = sum(score), Count = n())%>%
    inner_join(Total)%>%
    mutate(Normalizado = Count/totalJournalsEco)

  graph <-ggplot(AbstractsVocab, aes(Year, Normalizado)) +
    geom_line(stat = "identity", fill = "navyblue", color = "black") +
    theme_minimal()

  graph
}

##############################################
#Função que plota os x journals que mais publicaram para cada ano
JSTORplotJournals <- function (df, x)
{
  MençõesJournal <- df%>%
    count(Year, Journal, sort = TRUE)%>%
    group_by(Year)%>%
    top_n(x)%>%
    arrange(desc(Year))%>%
    ungroup()

  graph<-ggplot(MençõesJournal, aes(Year, Journal, size = n)) +
    geom_point()

  graph
}
