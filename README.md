# tidyJSTOR
This package transforms JSTOR's DFR into tidy dataframes and produces ggplot2 visualizations

# 1. Downloading JSTOR's DFR
JSTOR offers for researchers the tool "data for research" (DFR) at: https://www.jstor.org/dfr/. With this tool it is possible to create datasets of XML documents containing metadata of articles indexed in JSTOR's database. Datasets can be created according to any search result. However, JSTOR only allows the download of 25000 documents per time. Therefore, in the case where the datasets are biger than 25000 documents it is necessary to divide the serach (usually by dates) in numerous parts. For instance, a serach for economic models gives 650000 results. Hence, it would be advisable to divide the search in 26 parts. This could be done by finding first result from 1950 to 1960, then 1961 to 1970 and so forth. Downloaded dataset come zipped. Hence, it doesn matter whethe you have multiple files or only one. Simply select all files and extract to wherever you want, this will put all XML files in a same folder called "metadata". For this tutorial will be using data of the following JSTOR search:(labor OR union OR wage OR industrial relation OR workers OR manpower OR evaluation) AND data, constrained to the discipline of economics and to the years 1940 to 1970.
# 2. Typical workflow
Once with the dataset extracted in hand, is time to initiate our workflow. The firs step is to transform the data into a dataframe using JSTOR_df. This is simple achieve using the locationof the files in your desktop, for instance JSTOR_df("C:/Users/.../Documents/metadata/"). A coutndown will initiate demonstrating the progress of the transformation. The code would be as follows:
    
    library(tidytext)
    library(tm)
    library(tidyr)
    library(ggplot2)
    library (XML)
    library(dplyr)
    library(stringr)
    library(cat)
    library(devtools)
    devtools::install_github(“arthurbnetto/tidyJSTOR”)
   
    df <- JSTOR_df("C:/Users/.../Documents/metadata/")
    
Now, with our df in hand is time to analyze it, but first let's clean it. JSTOR metadata counts with several papers with repeated names that don't have abstracts and pepers in several languages. In cleaning it we maintain only english documents with abstracts.

    dfClean <- CleanJSTOR_df(df)
    plotCleaningResult(dfClean, df)
    
A typical output would be as the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/clean.jpeg)

Now, with a clean dataset, it is possible to plot some graph. Still, as we are goning to analyze words, it is interesting to be abale to customize words we want out from our sample. for this we create an object using the tm package:

    custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"), lexicon = "custom"),
                               data_frame(word = tm::stopwords("german"), lexicon = "custom"),
                               data_frame(word = tm::stopwords("french"), lexicon = "custom"),
                               data_frame(word = c("ã", "dãf", "ãf", "d'ãf", "lãf", "paper", "i", "ii", 
						"iii", "iv", "conclusion", "introduction", "v", "vi", "vii",
						 "1", "91"), lexicon = "custom"))
             
The last array is that of all customizable words. Normally, we add strange occurences that appear because of the format of the documents, but also page nunmbers and words such as conclusion and introduction that are frequent bu unimportant. The first four tm addition are of pre conceived dictionaries with unimportant words such as "the" and "a". With this object in hand the first observable graph is that of the 10 top words that appeared at least 3 times in the top over the years:

    JSTORrepeatedTopwords(dfClean, 10, 3)

The output is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/topwords.jpeg)

The same can be done for bigrams and trigrams using the commands JSTORrepeatedTopBigrams and JSTORrepeatedTopTrigrams.

    JSTORrepeatedTopBigrams(dfClean, 10, 3)
    JSTORrepeatedTopTrigrams(dfClean, 10, 3)

Changing x and y modificates the graphs according to the number of top words the command is looking for and how many times the words appeared in the top. For instance, if instead of 10 and 3 we had 5 and 2, it would be the top 5 words which appeared at least two times in the top 5. 

Simlar graph can be built for journals. Instead of observing the main words over time, it is possible to observe which were the main journal publishing about our search. The next command line plots a graph of the top 10 journals over time:

     JSTORplotJournals(dfClean, 10)
     
the main journals over time of our dataset are the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/journals.jpeg)

The final graph allows to observe the trend of specific clusters of words in the abstracts. Hence, imagine we are interested in observe how words related to theory evolved inside the papers in labor about data we are looking at. we could create an array of words related to theory such as: c("theory", "theories", "theoretical"). With this array we could plot the evelution of this words inside our dataframe in the following manner:

    JSTORplotVocabCount(dfClean, c("theory", "theories", "theoretical"))
    
The output would be this:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/theory.jpeg)

Finally, it is interesting to notice that all plots are ggplot objects and can be saved and customized like the following:

    plot <- JSTORplotVocabCount(dfClean, c("theory", "theories", "theoretical"))
    plot + geom_smooth()

Giving more information or more beautiful graphs like the next one:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/smooth.jpeg)

