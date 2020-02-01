# tidyJSTOR
This package transforms JSTOR's DFR into tidy dataframes and produces ggplot2 visualizations

# 1. Downloading JSTOR's DFR
JSTOR offers for researchers the tool "data for research" (DFR) at: https://www.jstor.org/dfr/. With this tool it is possible to create datasets of XML documents containing metadata of articles indexed in JSTOR's database. Datasets can be created according to any search criteria that fits normal searches in JSTOR. Tips and tricks for making searches in JSTOR can be found here: https://support.jstor.org/hc/en-us/articles/115004701828-Searching-A-Practical-Overview. 

Be aware that JSTOR dfr only allows the download of 25000 documents per time. Therefore, in the case where the datasets are biger than 25000 documents it is necessary to divide the search (usually by dates) in numerous parts. For instance, a search for the word model, contrained to the discipline of eocnomics, gives around 650000 results. Hence, it would be advisable to divide the search in 26 parts. This could be done by finding first result from 1950 to 1960, then 1961 to 1970 and so forth. Downloaded datasets come zipped. Hence, it doesnt matter whether you have multiple files or only one. Simply select all files and extract to wherever you want, this will put all XML files in a same folder called "metadata". For the first part of the tutorial I will be using data of the following JSTOR search: "experiment"; constrained to the discipline of economics; constrained to only journals; constrained to the years 1940 to 2010. The search yielded 39,503 results (01/02/2020). The download was divided into two parts: one for the years 1940 to 1990 and another for 1991 to 2010.
# 2. Typical workflow
Once with the dataset extracted in hand, is time to initiate our workflow. The firs step is to transform the data into a dataframe using JSTOR_df. This is simple achieve using the locationof the files in your desktop, for instance JSTOR_df("C:/Users/.../Documents/metadata/"). A coutndown will initiate demonstrating the progress of the transformation. The code would be as follows:
    
    library(devtools)
    devtools::install_github(“arthurbnetto/tidyJSTOR”)
    library(tidyJSTOR)
   
    df <- JSTOR_df("C:/Users/.../Documents/metadata/")
    
Now, with our df in hand is time to analyze it, but first let's clean it. JSTOR metadata counts with several papers with repeated names that don't have abstracts and papers in several languages that are not supported by the package yet. In cleaning it, we maintain only english documents with abstracts. for allowing the comparability of the initial dataset and the cleaned one, it is necessary to create two different dataframe in the following manner:

    dfClean <- CleanJSTOR_df(df)

Once we have both dataframes it is possbile to plot the cleaning results.

    plotCleaningResult(dfClean, df)
    
A typical output would be as the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/clean.jpeg)

Now, with a clean dataset, it is possible to plot some graphs. 
           
    JSTORrepeatedTopwords(dfClean, 5, 3)

The output is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/topwords.jpeg)

The same can be done for bigrams and trigrams using the commands JSTORrepeatedTopBigrams and JSTORrepeatedTopTrigrams.

    JSTORrepeatedTopBigrams(dfClean, 10, 3)
    JSTORrepeatedTopTrigrams(dfClean, 10, 3)

Changing x and y modificates the graphs according to the number of top words the command is looking for and how many times the words appeared in the top. For instance, if instead of 10 and 3 we had 5 and 2, it would be the top 5 words which appeared at least two times in the top 5. 

A simlar graph can be built for journals. Instead of observing the main words over time, it is possible to observe which were the main journals publishing about our search. The next command line plots a graph of the journals that were in the top 5 in at least 3 distinct years:

     JSTORplotJournals(dfClean, 5, 3)
     
the main journals over time of our dataset are the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/journals.jpeg)

The final graph allows to observe the trend of specific clusters of words in the abstracts. Hence, imagine we are interested in observing how words related to theory evolved inside the papers in our dataset. We could create an array of words related to theory such as: c("theory", "theories", "theoretical"). With this array we could plot the evolution of this words inside our dataframe in the following manner:

    JSTORplotVocabCount(dfClean, c("theory", "theories", "theoretical"))
    
The output would be this:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/theory.jpeg)

Finally, it is interesting to notice that all plots are ggplot objects and can be saved and customized like the following:

    plot <- JSTORplotVocabCount(dfClean, c("theory", "theories", "theoretical"))
    plot + geom_smooth()

Giving more information or more beautiful graphs like the next one:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/smooth.jpeg)

