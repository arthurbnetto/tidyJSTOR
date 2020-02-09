##########################################################################
##########################################################################
##########################################################################

#' @title Transform
#'
#' @description This function transforms JSTOR DFR files into single row tidy datafrlabames
#'
#' @param filePath A file path
#'
#' @return A single row dataframe containing informations about: Journal, Publisher, Title, Year, Abstract and Language
#'
#' @examples Transform("C:/Users/.../Documents/metadata/file1.xml")
#'
#'
#' @export

###########################################
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
#' @param filePath A file path
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

##########################################################################
##########################################################################
##########################################################################

#' @title CleanJSTOR_df
#'
#' @description This cleans a JSTOR_df, excluding repeated name papers, non-english papers, NAs (in titles or abstracts)
#'
#' @param df A dataframe in the JSTOR_df format (see: JSTOR_df())
#'
#' @param Titles whether to clean titles (instead of abstracts). Default = FALSE.
#'
#' @return A JSTOR_df dataframe containing informations about: Journal, Publisher, Title, Year, Abstract and Language
#'
#' @examples CleanJSTOR_df(df)
#'
#'
#' @export

###########################################
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
##########################################################################
##########################################################################
##########################################################################

#' @title plotCleaningResult
#'
#' @description Plots number of documents per year in two distinct dataframes. Intended to use iwth a cleaned JSTOR_df and an original JSTOR_df
#'
#' @param dfClean A dataframe in the JSTOR_df format (see: JSTOR_df()) after using CleanJSTOR_df()
#'
#' @param dfDirt A dataframe in the JSTOR_df format (see: JSTOR_df())
#'
#' @return A plot
#'
#' @examples plotCleaningResult(df1, df2)
#'
#'
#' @export

##############################################
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
	ggplot2::labs(x= "", y= "Documents")+
  	ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
		legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
		#strip.text.x = element_text(size = 12, colour = "black"),
		axis.text.y = ggplot2::element_text(size=14),
		axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
		text=ggplot2::element_text(family="serif")) 
  
}

##########################################################################
##########################################################################
##########################################################################

#' @title JSTORrepeatedTopwords
#'
#' @description Creates dataframe or plot with information regarding top words that appear at least 
#' for a determined time period in the top.
#'
#' @param dfEco A dataframe in the JSTOR_df format (see: JSTOR_df())
#'
#' @param y Determines the size of the ranking being observed for each year
#'
#' @param x Determines the number of times a words has to be part of the rank to 
#' be included in the dataframe or plot
#'
#' @param StopWords Whether to use default StopWords or a custom dataframe of stop words. If opted
#' for custom, a dataframe of two columns (word, lexicon = "custom") must be provided. 
#'
#' @param output Whether to output a dataframe or a plot. Acceptable entries: "plot" and "dataframe".
#'
#' @param YearBounds Optional vector with inferior and superior Year bounds for the plot. Ex: c(1970,1990)
#'
#' @return A ggplot2 object or a dataframe
#'
#' @examples JSTORrepeatedTopwords(df, 3, 3)
#'
#'
#' @export

##############################################
JSTORrepeatedTopwords <- function (dfEco, y, x, StopWords = TRUE, output = "plot", YearBounds)
{
	if (StopWords == TRUE)
	{
	custom_stop_words<- rbind(data.frame(word = tm::stopwords("english"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("spanish"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("german"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("french"), lexicon = "custom"),
                               data.frame(word = c("ã", "dãf", "ãf", "d'ãf", "lãf", "paper", "i", "ii", 
						"iii", "iv", "conclusion", "introduction", "v", "vi", "vii",
						 "1", "91", NA), lexicon = "custom"))
	}else{
	custom_stop_words<- StopWords
	}
	
  '%>%'<-purrr::'%>%'
  #Principais palavras ao longo do tempo
  AbstractsTidyYear <- dfEco %>%
    tidytext::unnest_tokens (word, Abstract)%>%
    dplyr::anti_join(custom_stop_words)%>%
    dplyr::count(Year, word, sort = TRUE)%>%
    dplyr::group_by(Year)%>%
    dplyr::mutate(n = n/dplyr::n())%>%
    dplyr::top_n(y)%>%
    dplyr::arrange(dplyr::desc(Year))%>%
    dplyr::ungroup()

  #Filtrar palavras que aparecem pelo menos x vezes(anos) no top

  k <- !sum(as.numeric(stringr::str_detect(AbstractsTidyYear$word, AbstractsTidyYear$word[1])), na.rm=TRUE)<x
  i=2
  while(i<=nrow(AbstractsTidyYear))
  {
    a <- !sum(as.numeric(stringr::str_detect(AbstractsTidyYear$word, AbstractsTidyYear$word[i])), na.rm=TRUE)<x
    k <- rbind(k, a)
    i = i+1
  }

  k <- data.frame(k)
  AbstractsTidyYear <- cbind(AbstractsTidyYear, logi = k[,1])

  AbstractsTidyYear <- AbstractsTidyYear %>%
    dplyr::filter(logi>0)

  if (missing(YearBounds))
  {
       AbstractsTidyYear <- AbstractsTidyYear
  }else{
      AbstractsTidyYear <- AbstractsTidyYear%>%
         dplyr::filter(Year>YearBounds[1])%>%
         dplyr::filter(Year<YearBounds[2])
  }

  #Vizualização
  graph<- ggplot2::ggplot(AbstractsTidyYear, 
		ggplot2::aes(Year, reorder(word, Year), size = n)) +
    		ggplot2::geom_point()+
	ggplot2::theme_bw()+
	ggplot2::labs(x= "", y= "")+
  	ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
		legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
		#strip.text.x = element_text(size = 12, colour = "black"),
		axis.text.y = ggplot2::element_text(size=14),
		axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
		text=ggplot2::element_text(family="serif")) 

     if(output=="dataframe")
     {
         AbstractsTidyYear
     }else{if(output=="plot"){
         graph
     }}
}
##########################################################################
##########################################################################
##########################################################################

#' @title JSTORrepeatedTopTrigrams
#'
#' @description Creates dataframe or plot with information regarding top Trigrams that appear at least 
#' for a determined time period in the top.
#'
#' @param dfEco A dataframe in the JSTOR_df format (see: JSTOR_df()) 
#'
#' @param y Determines the size of the ranking being observed for each year
#'
#' @param x Determines the number of times a trigram has to be part of the rank to 
#' be included in the dataframe or plot
#'
#' @param StopWords Whether to use default StopWords or a custom dataframe of stop words. If opted
#' for custom, a dataframe of two columns (word, lexicon = "custom") must be provided. 
#'
#' @param output Whether to output a dataframe or a plot. Acceptable entries: "plot" and "dataframe".
#'
#' @param YearBounds Optional vector with inferior and superior Year bounds for the plot. Ex: c(1970,1990)
#'
#' @return A ggplot2 object or a dataframe
#'
#' @examples JSTORrepeatedTopTrigrams(df, 3, 3)
#'
#'
#' @export

##############################################
#Função que plota palavras que aparecem pelo menos x anos no top y
JSTORrepeatedTopTrigrams <- function (dfEco, y, x, StopWords=TRUE, output="plot", YearBounds)
{

	if (StopWords == TRUE)
	{
	custom_stop_words<- rbind(data.frame(word = tm::stopwords("english"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("spanish"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("german"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("french"), lexicon = "custom"),
                               data.frame(word = c("ã", "dãf", "ãf", "d'ãf", "lãf", "paper", "i", "ii", 
						"iii", "iv", "conclusion", "introduction", "v", "vi", "vii",
						 "1", "91", NA), lexicon = "custom"))
	}else{
	custom_stop_words<- StopWords
	}

	'%>%'<-purrr::'%>%'

	AbstractsTidyTriYear <- dfEco %>%
	tidytext::unnest_tokens(trigram, Abstract, token = "ngrams", n = 3) %>%
	tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
	dplyr::filter(!word1 %in% custom_stop_words$word,
           !word2 %in% custom_stop_words$word,
           !word3 %in% custom_stop_words$word) %>%
	dplyr::count(Year, word1, word2, word3, sort = TRUE)%>%
	tidyr::unite(trigram, word1, word2, word3, sep = " ")%>%
	dplyr::group_by(Year)%>%
	dplyr::mutate(n = n/dplyr::n())%>%
	dplyr::top_n(y)%>%
	dplyr::arrange(dplyr::desc(Year))%>%
	dplyr::ungroup()

	k <- !sum(as.numeric(stringr::str_detect(AbstractsTidyTriYear$trigram, AbstractsTidyTriYear$trigram[1])), na.rm=TRUE)<x
	i=2
	while(i<=nrow(AbstractsTidyTriYear))
	{
		a <- !sum(as.numeric(stringr::str_detect(AbstractsTidyTriYear$trigram, AbstractsTidyTriYear$trigram[i])), na.rm=TRUE)<x
		k <- rbind(k, a)
		i = i+1
	}
	k <- data.frame(k)
	
	AbstractsTidyTriYear <- cbind(AbstractsTidyTriYear, logi = k[,1])

	AbstractsTidyTriYear <- AbstractsTidyTriYear %>%
		dplyr::filter(logi>0)

  	if (missing(YearBounds))
  	{
       		AbstractsTidyTriYear <- AbstractsTidyTriYear
  	}else{
      		AbstractsTidyTriYear <- AbstractsTidyTriYear%>%
         		dplyr::filter(Year>YearBounds[1])%>%
         		dplyr::filter(Year<YearBounds[2])
  	}

	graph<- ggplot2::ggplot(AbstractsTidyTriYear, 
		ggplot2::aes(Year, reorder(trigram, Year), size = n)) +
    		ggplot2::geom_point()+
	ggplot2::theme_bw()+
	ggplot2::labs(x= "", y= "")+
  	ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
		legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
		#strip.text.x = element_text(size = 12, colour = "black"),
		axis.text.y = ggplot2::element_text(size=14),
		axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
		text=ggplot2::element_text(family="serif")) 

     if(output=="dataframe")
     {
         AbstractsTidyTriYear
     }else{if(output=="plot"){
         graph
     }}
}

##########################################################################
##########################################################################
##########################################################################

#' @title JSTORrepeatedTopBigrams
#'
#' @description Creates dataframe or plot with information regarding top bigrams that appear at least 
#' for a determined time period in the top.
#'
#' @param dfEco A dataframe in the JSTOR_df format (see: JSTOR_df()) 
#'
#' @param y Determines the size of the ranking being observed for each year
#'
#' @param x Determines the number of times a bigram has to be part of the rank to 
#' be included in the dataframe or plot
#'
#' @param StopWords Whether to use default StopWords or a custom dataframe of stop words. If opted
#' for custom, a dataframe of two columns (word, lexicon = "custom") must be provided. 
#'
#' @param output Whether to output a dataframe or a plot. Acceptable entries: "plot" and "dataframe".
#'
#' @param YearBounds Optional vector with inferior and superior Year bounds for the plot. Ex: c(1970,1990)
#'
#' @return A ggplot2 object or a dataframe
#'
#' @examples JSTORrepeatedTopBigrams(df, 3, 3)
#'
#'
#' @export

##############################################
JSTORrepeatedTopBigrams <- function (dfEco, y, x, StopWords = TRUE, output = "plot", YearBounds)
{

	if (StopWords == TRUE)
	{
	custom_stop_words<- rbind(data.frame(word = tm::stopwords("english"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("spanish"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("german"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("french"), lexicon = "custom"),
                               data.frame(word = c("ã", "dãf", "ãf", "d'ãf", "lãf", "paper", "i", "ii", 
						"iii", "iv", "conclusion", "introduction", "v", "vi", "vii",
						 "1", "91", NA), lexicon = "custom"))
	}else{
	custom_stop_words<- StopWords
	}

	'%>%'<-purrr::'%>%'


  #Bigram
  AbstractsTidyBiYear <- dfEco %>%
    tidytext::unnest_tokens(bigram, Abstract, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!word1 %in% custom_stop_words$word,
           !word2 %in% custom_stop_words$word) %>%
    dplyr::count(Year, word1, word2, sort = TRUE)%>%
    tidyr::unite(bigram, word1, word2, sep = " ")%>%
    dplyr::group_by(Year)%>%
    dplyr::mutate(n = n/dplyr::n())%>%
    dplyr::top_n(y)%>%
    dplyr::arrange(dplyr::desc(Year))%>%
    dplyr::ungroup()

  #Filtrar trigrams que aparecem pelo menos x(tal que x =3) vezes(anos) no top

  k <- !sum(as.numeric(stringr::str_detect(AbstractsTidyBiYear$bigram, AbstractsTidyBiYear$bigram[1])), na.rm=TRUE)<x
  i=2
  while(i<=nrow(AbstractsTidyBiYear))
  {
    a <- !sum(as.numeric(stringr::str_detect(AbstractsTidyBiYear$bigram, AbstractsTidyBiYear$bigram[i])), na.rm=TRUE)<x
    k <- rbind(k, a)
    i = i+1
  }

  k <- data.frame(k)
  AbstractsTidyBiYear <- cbind(AbstractsTidyBiYear, logi = k[,1])

  AbstractsTidyBiYear <- AbstractsTidyBiYear %>%
    dplyr::filter(logi>0)

  	if (missing(YearBounds))
  	{
       		AbstractsTidyBiYear <- AbstractsTidyBiYear
  	}else{
      		AbstractsTidyBiYear <- AbstractsTidyBiYear%>%
         		dplyr::filter(Year>YearBounds[1])%>%
         		dplyr::filter(Year<YearBounds[2])
  	}

  graph<- ggplot2::ggplot(AbstractsTidyBiYear, 
		ggplot2::aes(Year, reorder(bigram, Year), size = n)) +
    		ggplot2::geom_point()+
	ggplot2::theme_bw()+
	ggplot2::labs(x= "", y= "")+
  	ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
		legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
		#strip.text.x = element_text(size = 12, colour = "black"),
		axis.text.y = ggplot2::element_text(size=14),
		axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
		text=ggplot2::element_text(family="serif")) 

     if(output=="dataframe")
     {
         AbstractsTidyBiYear
     }else{if(output=="plot"){
         graph
     }}
}

##########################################################################
##########################################################################
##########################################################################

#' @title JSTORVocabCount
#'
#' @description This function counts the number of appearences of a set of words in abstracts or titles of a tidyJSTOR dataframe
#'
#' @param dfEco A dataframe in the JSTOR_df format (see: JSTOR_df())
#'
#' @param ArrayVocab a vector of charcters ot be counted as c("word1", "word2", ..., "wordn")
#'
#' @param titles Whether to analyze titles of abstracts. Default titles = FALSE 
#'
#' @param StopWords Whether to use default StopWords or a custom dataframe of stop words. If opted
#' for custom, a dataframe of two columns (word, lexicon = "custom") must be provided. 
#'
#' @param scores a vector of scores matching the vector of character in the parameter ArrayVocab
#'
#' @return A dataframe of counted words JSTORVocabCount 
#'
#' @examples JSTORplotVocabCount(df, c("theory", "theories", "theoretical"))
#'
#' JSTORVocabCount(df, c("theory", "theories", "theoretical"), scores = c(1,2,3))
#'
#' @export

##########################################################################

VocabCount <- function (dfEco, ArrayVocab, titles = FALSE, StopWords = TRUE, scores)
{
	

	if (StopWords == TRUE)
	{
	custom_stop_words<- rbind(data.frame(word = tm::stopwords("english"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("spanish"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("german"), lexicon = "custom"),
                               data.frame(word = tm::stopwords("french"), lexicon = "custom"),
                               data.frame(word = c("ã", "dãf", "ãf", "d'ãf", "lãf", "paper", "i", "ii", 
						"iii", "iv", "conclusion", "introduction", "v", "vi", "vii",
						 "1", "91", NA), lexicon = "custom"))
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
  		dplyr::inner_join(Total)
	if (missing(scores))
	{
	AbstractsVocab <- AbstractsVocab %>%
  		dplyr::mutate(Normalized = Count/YearlyPapers)
	}else{
	AbstractsVocab <- AbstractsVocab %>%
  		dplyr::mutate(Normalized = VocabScore/YearlyPapers)
	}


dplyr::select(AbstractsVocab, Year, Normalized)
}

##########################################################################
##########################################################################
##########################################################################

#' @title JSTORplotVocabCount
#'
#' @description This function plots the number of appearences of a set of words in abstracts or titles of a tidyJSTOR dataframe
#'
#' @param df.list A list or a single dataframe in the JSTOR_df format (see: JSTOR_df())
#'
#' @param legend A vector of charcters to be used as legends matching 
#' the number of df in df.list c("legend1", "legend2", ..., "legendN")
#'
#' @param smooth Whether to smooth curves. Default smooth = FALSE
#'
#' @param YearBounds Optional vector with inferior and superior Year bounds for the plot. Ex: c(1970,1990)
#'
#' @return A dataframe of counted words JSTORVocabCount 
#'
#' @examples JSTORplotVocabCount(df, c("theory", "theories", "theoretical"))
#'
#' JSTORplotVocabCount(c(df1, df2), c("df1Legend", "df2Legend"), smooth =TRUE, 2010)
#'
#' @export

##########################################################################
JSTORplotVocabCount <- function (df.list, legend, smooth=FALSE, YearBounds)
{

  '%>%'<-purrr::'%>%' 
  if (typeof(df.list[[1]]) == "integer")
  {
  	comparison<-df.list
  }else{
  	comparison <- df.list[[1]]
  	i = 2
 	 while (i <= length (df.list))
 	{
		comparison<-comparison%>%
		dplyr::full_join(df.list[[i]], by = "Year")
		i <- i+1
  	}
  }

  if (missing(legend) == TRUE) 
  {
  	comparison<-comparison
  }else{
	colnames(comparison)<-c("Year", legend)
  }
	
  melted <- reshape2::melt(comparison ,  id.vars = 'Year', variable.name = 'series')

  if (missing(YearBounds))
  {
       melted<-melted
  }else{
      melted<-melted%>%
         dplyr::filter(Year>YearBounds[1])%>%
         dplyr::filter(Year<YearBounds[2])
  }


	p<-ggplot2::ggplot(melted, 
	ggplot2::aes(Year, value, color=series))
  if (smooth == FALSE)
  {
	p <- p + ggplot2::geom_line()
  }else{
	p<- p+ ggplot2::geom_smooth(se=FALSE)
  }
	p + ggplot2::theme_bw()+
	ggplot2::labs(x= "", y= "Count(or Score)")+
  	ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
		legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
		#strip.text.x = element_text(size = 12, colour = "black"),
		axis.text.y = ggplot2::element_text(size=14),
		axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
		text=ggplot2::element_text(family="serif")) 

}

##########################################################################
##########################################################################
##########################################################################

#' @title JSTORplotJournals
#'
#' @description Creates dataframe or plot with information regarding top Journal that appear at least 
#' for a determined time period in the top.
#'
#' @param dfEco A dataframe in the JSTOR_df format (see: JSTOR_df()) 
#'
#' @param y Determines the size of the ranking being observed for each year
#'
#' @param x Determines the number of times a bigram has to be part of the rank to 
#' be included in the dataframe or plot
#'
#' @param output Whether to output a dataframe or a plot. Acceptable entries: "plot" and "dataframe".
#'
#' @param YearBounds Optional vector with inferior and superior Year bounds for the plot. Ex: c(1970,1990)
#'
#' @return A ggplot2 object or a dataframe
#'
#' @examples 
#'
#'
#' @export
##############################################

JSTORplotJournals <- function (df,y, x, output = "plot", YearBounds)
{
	'%>%'<-purrr::'%>%'
 
	MençõesJournal <- df%>%
		dplyr::count(Year, Journal, sort = TRUE)%>%
		dplyr::group_by(Year)%>%
		dplyr::top_n(y)%>%
		dplyr::arrange(dplyr::desc(Year))%>%
		dplyr::ungroup()

 	k <- !sum(as.numeric(stringr::str_detect(MençõesJournal$Journal, MençõesJournal$Journal[1])), na.rm=TRUE)<x
	i=2
  	while(i<=nrow(MençõesJournal))
  	{
    		a <- !sum(as.numeric(stringr::str_detect(MençõesJournal$Journal, MençõesJournal$Journal[i])), na.rm=TRUE)<x
    		k <- rbind(k, a)
    		i = i+1
  	}

  	k <- data.frame(k)
  	MençõesJournal <- cbind(MençõesJournal, logi = k[,1])

  	MençõesJournal <- MençõesJournal %>%
    		dplyr::filter(logi>0)

  	if (missing(YearBounds))
  	{
       		MençõesJournal <- MençõesJournal
  	}else{
      		MençõesJournal <- MençõesJournal%>%
         		dplyr::filter(Year>YearBounds[1])%>%
         		dplyr::filter(Year<YearBounds[2])
  	}

	graph<-ggplot2::ggplot(MençõesJournal, 
		ggplot2::aes(Year, reorder(Journal, Year), size = n)) +
		ggplot2::geom_point()+
		ggplot2::theme_bw()+
		ggplot2::labs(x= "", y= "")+
  		ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
			legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
			#strip.text.x = element_text(size = 12, colour = "black"),
			axis.text.y = ggplot2::element_text(size=14),
			axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
			text=ggplot2::element_text(family="serif")) 

     if(output=="dataframe")
     {
         MençõesJournal
     }else{if(output=="plot"){
         graph
     }}
}

##########################################################################
##########################################################################
##########################################################################

#' @title SearchCount
#'
#' @description Creates a dataframe with counted number of documents for each year in a JSTOR_df
#'
#' @param df A dataframe in the JSTOR_df format (see: JSTOR_df()) 
#'
#' @return A dataframe of counted documents per year for a JSTOR_df 
#'
#' @examples 
#'
#' @export
##############################################

SearchCount<-function(df)
{
'%>%'<-purrr::'%>%'
 	df%>%
	dplyr::group_by(Year)%>%
	dplyr::summarise(count = n())
}

##########################################################################
##########################################################################
##########################################################################

#' @title plot_search
#'
#' @description Plots number of documents per year in JSTOR_df after using SearchCount()
#'
#' @param df.list A list or a single dataframe in the JSTOR_df format (see: JSTOR_df())
#'
#' @param legend A vector of charcters to be used as legends matching 
#' the number of df in df.list c("legend1", "legend2", ..., "legendN")
#'
#' @param smooth Whether to smooth curves. Default smooth = FALSE
#'
#' @param YearBounds Optional vector with inferior and superior Year bounds for the plot. Ex: c(1970,1990)
#'
#' @return A plot of counted documents per year for a JSTOR_df 
#'
#' @examples
#'
#' @export
##############################################


plot_search<- function (df.list, legend, smooth=FALSE, YearBounds)
{
 '%>%'<-purrr::'%>%' 

  if (typeof(df.list[[1]]) == "character")
  {
  	comparison<-SearchCount(df.list)
  }else{
  	count<-lapply(df.list, SearchCount)
  	comparison <- count[[1]]
 	i =2
  	while (i <= length (df.list))
  	{
		comparison<-comparison%>%
		dplyr::full_join(count[[i]], by = "Year")
		i <- i+1
 	}
  }

  if (missing(legend) == TRUE) 
  {
  	comparison<-comparison
  }else{
	colnames(comparison)<-c("Year", legend)
  }
	
  melted <- reshape2::melt(comparison ,  id.vars = 'Year', variable.name = 'series')
  
  	if (missing(YearBounds))
  	{
       		melted <- melted
  	}else{
      		melted <- melted%>%
         		dplyr::filter(Year>YearBounds[1])%>%
         		dplyr::filter(Year<YearBounds[2])
  	}


  p<-ggplot2::ggplot(melted, 
	ggplot2::aes(Year, value, color=series))
  if (smooth == FALSE)
  {
	p <- p + ggplot2::geom_line()
  }else{
	p<- p+ ggplot2::geom_smooth(se=FALSE)
  }
	p + ggplot2::theme_bw()+
	ggplot2::labs(x= "", y= "Documents")+
  	ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
		legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
		#strip.text.x = element_text(size = 12, colour = "black"),
		axis.text.y = ggplot2::element_text(size=14),
		axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size=14),
		text=ggplot2::element_text(family="serif")) 
}

##########################################################################
##########################################################################
##########################################################################

#' @title compareJSTOR_dfs
#'
#' @description Creates dataframe or plot with information regarding top bigrams that appear at least 
#' for a determined time period in the top.
#'
#' @param df1 A dataframe in the JSTOR_df format (see: JSTOR_df()) 
#'
#' @param df2 A dataframe in the JSTOR_df format (see: JSTOR_df()) 
#'
#' @param y Determines the size of the ranking being observed for each year
#'
#' @param x Determines the number of times a bigram has to be part of the rank to 
#' be included in the dataframe or plot
#'
#' @param legend A vector of charcters to be used as legends matching 
#' the number of dfs (2): c("legend1", "legend2")
#'
#' @param comparisonType  Which type of caomparison to make. Acceptable entries: "Words", "Bigrams", "Trigrams" and "Journal".
#'
#' @param YearBounds Optional vector with inferior and superior Year bounds for the plot. Ex: c(1970,1990)
#'
#' @return A ggplot2 object 
#'
#' @examples compareJSTOR_dfs(dfx, dfk, 4, 4, c("legendx", "legendk", comparisonType = "Journal")
#'
#'
#' @export

##############################################
compareJSTOR_dfs <- function(df1, df2,y, x, legend, comparisonType, YearBounds)
{

	'%>%'<-purrr::'%>%'

	if(comparisonType == "Bigrams"){
		comparison_df1<-JSTORrepeatedTopBigrams(df1, y, x, StopWords = TRUE, output = "dataframe")
		comparison_df2<-JSTORrepeatedTopBigrams(df2, y, x, StopWords = TRUE, output = "dataframe")
	}
	if(comparisonType == "Words"){
		comparison_df1<-JSTORrepeatedTopwords(df1, y, x, StopWords = TRUE, output = "dataframe")
		comparison_df2<-JSTORrepeatedTopwords(df2, y, x, StopWords = TRUE, output = "dataframe")
	}
	if(comparisonType == "Trigrams"){
		comparison_df1<-JSTORrepeatedTopTrigrams(df1, y, x, StopWords = TRUE, output = "dataframe")
		comparison_df2<-JSTORrepeatedTopTrigrams(df2, y, x, StopWords = TRUE, output = "dataframe")
	}
	if(comparisonType == "Journal"){
		comparison_df1<-JSTORplotJournals(df1, y, x, output = "dataframe")
		comparison_df2<-JSTORplotJournals(df2, y, x, output = "dataframe")
	}



	if(missing(legend))
	{
  		comparison_df2 <- comparison_df2 %>%
			dplyr::mutate(type = "df2")
	}else{
		comparison_df2 <- comparison_df2 %>%
			dplyr::mutate(type = legend[2])
	}

	if(missing(legend))
	{
  		comparison_df1 <- comparison_df1 %>%
			dplyr::mutate(type = "df1")
	}else{
		comparison_df1 <- comparison_df1 %>%
			dplyr::mutate(type = legend[1])
	}

	Binded <- dplyr::bind_rows(comparison_df1, comparison_df2)

	toPlot <- Binded%>%
 		dplyr::group_by(Year, type)%>%
		dplyr::mutate(k = n/sum(n))

	toPlot <- toPlot%>%
 		dplyr::group_by(Year, type)%>%
 		dplyr::top_n(n = y, wt = k)%>%
 		dplyr::arrange(desc(Year))%>%
 		dplyr::ungroup()

	odd <- seq(1, ((nrow(toPlot))+nrow(toPlot)%%2), 2)
	even <- seq(2, (nrow(toPlot)), 2)

	if (missing(YearBounds))
  	{
       		toPlot <- toPlot
  	}else{
      		toPlot <- toPlot%>%
         		dplyr::filter(Year>YearBounds[1])%>%
         		dplyr::filter(Year<YearBounds[2])
  	}


	if(comparisonType == "Bigrams"){
		plot <- ggplot2::ggplot(toPlot, 
		 	ggplot2::aes(Year, reorder(stringr::str_wrap(bigram, 30), Year), shape = type, size = 200)) + 
		 	ggplot2::geom_point()
	}
	if(comparisonType == "Words"){
		plot <- ggplot2::ggplot(toPlot, 
		 	ggplot2::aes(Year, reorder(stringr::str_wrap(word, 30), Year), shape = type, size = 200)) + 
		 	ggplot2::geom_point()
	}
	if(comparisonType == "Trigrams"){
		plot <- ggplot2::ggplot(toPlot, 
		 	ggplot2::aes(Year, reorder(stringr::str_wrap(trigram, 30), Year), shape = type, size = 200)) + 
		 	ggplot2::geom_point()
	}
	if(comparisonType == "Journal"){
		plot <- ggplot2::ggplot(toPlot, 
		 	ggplot2::aes(Year, reorder(stringr::str_wrap(Journal, 30), Year), shape = type, size = 200)) + 
		 	ggplot2::geom_point()
	}

	plot<- plot + ggplot2::geom_hline(data=odd, yintercept=odd, Menções, linetype="longdash", color="grey") +
		 ggplot2::geom_hline(data=even, yintercept=even, Menções, linetype="dotdash",color="grey") +
		 ggplot2::geom_point(color = "black")+	
		 ggplot2::scale_shape_manual(values=c(4, 0))+ 
 		 ggplot2::labs(x="", y="") +
		 ggplot2::guides(size = FALSE)+
		 ggplot2::theme_bw() +  
		 ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
					panel.border = ggplot2::element_rect(linetype = "solid", color = "grey", fill = NA), 
					legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
					strip.text.x = ggplot2::element_text(size = 12, colour = "black"),
					axis.text.y = ggplot2::element_text(size=7),
					text=ggplot2::element_text(family="serif"))  
	plot
}
