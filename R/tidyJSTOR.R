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
  a <- XML::xmlParse (filePath)
  aTop <- XML::xmlRoot (a)

  Journal <- XML::xmlValue(aTop[["front"]][["journal-meta"]][["journal-title-group"]][["journal-title"]])
  Publisher <- XML::xmlValue (aTop[["front"]][["journal-meta"]][["publisher"]][["publisher-name"]])
  Title <- XML::xmlValue (aTop[["front"]][["article-meta"]][["title-group"]][["article-title"]])
  Year <- as.integer(XML::xmlValue(aTop[["front"]][["article-meta"]][["pub-date"]][["year"]]))
  Abstract <- XML::xmlValue(aTop[["front"]][["article-meta"]][["abstract"]])
  Language <- XML::xmlValue(aTop[["front"]][["article-meta"]][["custom-meta-group"]][["custom-meta"]][["meta-value"]])

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
#Function to create a dataframe from a filepath
JSTOR_df <- function (filePath)
{

  files <- list.files(filePath, "*.xml", full.names=TRUE)
  df<-pbapply::pblapply(files, Transform)
  df <- do.call("rbind", df)
  
}

#Função que limpa os dados do data frame de abstracts ou titulos
CleanJSTOR_df <- function (df, Titles=FALSE)
{

  '%>%'<-purrr::'%>%'
  #Exclui Artigos com nomes repetidos: "front matter", "back matter"...
  dfJournals <- (df%>%
                   dplyr::group_by (Title)%>%
                   dplyr::summarise (n = n ())%>%
                   dplyr::filter(n < 2) %>%
                   dplyr::arrange(desc(n)))

  df <- df%>%
    dplyr::inner_join (dfJournals)

  #Retira NAs
  if (Titles == FALSE){
    df <- df[complete.cases(dplyr::select(df, Abstract)),]
  }else{
    df <- df[complete.cases(dplyr::select(df, Title)),]
  }

  #Seleção de abstracts estritamente em inglês
  df <- (df%>%
           dplyr::filter(Language == "eng" | Language == "en")%>%
           dplyr::select(Title, Year, Abstract, Language, Journal, Publisher))

  df <- df%>%
    dplyr::filter(!stringr::str_detect(Abstract, " et "))%>%
    dplyr::filter(!stringr::str_detect(Abstract, " und "))%>%
    dplyr::filter(!stringr::str_detect(Abstract, " la "))%>%
    dplyr::filter(Year < 2011)

  df
}

##############################################
#Função que plota total de arquivos antes e após limpeza
plotCleaningResult <- function(dfClean, dfDirt)
{

  '%>%'<-purrr::'%>%'
  TotalLimpo <- (dfClean%>%
                   dplyr::group_by (Year)%>%
                   dplyr::summarise (n = n ()))

  TotalSujo <- (dfDirt%>%
                  dplyr::group_by (Year)%>%
                  dplyr::summarise (n = n ()))

  comparison<-TotalSujo%>%
	dplyr::full_join(TotalLimpo, by = "Year")

	colnames(comparison)<-c("Year", "Dirt Data", "Clean Data")
	melted <- reshape2::melt(comparison ,  id.vars = 'Year', variable.name = 'series')

  ggplot2::ggplot (melted, 
	ggplot2::aes(Year, value, color=series))+
	ggplot2::geom_line()+
	ggplot2::theme_bw()+
	ggplot2::labs(x= "", y= "Citations")+
  	ggplot2::theme(panel.grid.minor = element_blank(), panel.border = element_rect(linetype = "solid", color = "grey", fill = NA), 
		legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),
		#strip.text.x = element_text(size = 12, colour = "black"),
		axis.text.y = ggplot2::element_text(size=14),
		axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
		text=ggplot2::element_text(family="serif")) 
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
#' @title JSTORplotVocabCount
#'
#' @description This function counts the number of appeares of a set of words in abstracts or titles of a tidyJSTOR dataframe
#'
#' @param dfEco, ArrayVocab, titles = FALSE, StopWords
#'
#' @return A ggplot2 graph 
#'
#' @examples JSTORplotVocabCount(df, c("theory", "theories", "theoretical"), titles=FALSE)
#'
#' @import dplyr
#'
#' @export
JSTORplotVocabCount <- function (dfEco, ArrayVocab, titles = FALSE, StopWords = TRUE, output = "dataframe", scores)
{
	

	if (StopWords == TRUE)
	{
	custom_stop_words<- rbind(data.frame(word = tm::stopwords("english"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("spanish"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("german"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("french"), lexicon = "custom"),
                               data.frame(word = c("ã", "dãf", "ãf", "d'ãf", "lãf", "paper", "i", "ii", 
						"iii", "iv", "conclusion", "introduction", "v", "vi", "vii",
						 "1", "91"), lexicon = "custom"))
	}else{
	custom_stop_words<- StopWords
	}
	'%>%'<-purrr::'%>%'
	#Tokenization = Transformar dados em tidy com tidytext = outro tipo de dado para análise de texto e sentimento
	if (titles == FALSE)
	{
	AbstractsTidy <- dfEco %>%
 		tidytext::unnest_tokens (word, Abstract)
	}else{
	AbstractsTidy <- dfEco %>%
 		tidytext::unnest_tokens (word, Title)
	}

	AbstractsTidy <- AbstractsTidy%>%
 		dplyr::anti_join(custom_stop_words)

	Total <- dfEco %>%
		dplyr::group_by (Year)%>%
		dplyr::summarise (YearlyPapers = n ())
	if (missing(scores))
	{
		scoreVector <- rep(1, length(ArrayVocab))
	}else{
	    	scoreVector <- scores
	}

	vocab <- data.frame(word = as.character(ArrayVocab),
		      		score = scoreVector)

	AbstractsVocab <- AbstractsTidy %>%
  		dplyr::inner_join(vocab) %>%
  		dplyr::group_by(Year) %>%
  		dplyr::summarise (VocabScore = sum(score), Count = n())%>%
  		dplyr::inner_join(Total)%>%
  		dplyr::mutate(Normalized = Count/YearlyPapers)

	graph <-ggplot2::ggplot(AbstractsVocab, 
		ggplot2::aes(Year, Normalized)) + 
		ggplot2::geom_line(stat = "identity", fill = "navyblue", color = "black") +
		ggplot2::theme_minimal()

if (output == "graphical")
{
	graph
}else{
	AbstractsVocab
}
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
