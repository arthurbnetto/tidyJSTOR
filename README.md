# tidyJSTOR
This package transforms JSTOR DFR's XML files into tidy dataframes and produces ggplot2 visualizations. This packages is a wrapper around tidyverse utilities and text-analisys tools intended for non-frequent R users to be able to realize bibliometric exploratory research of JSTOR datasets. Advanced R users may prefer to use the package solely for data transformation using JSTOR_df() and produce their own visualizations and analysis.

# 1. Downloading JSTOR's DFR
JSTOR offers for researchers the tool "data for research" (DFR) at: https://www.jstor.org/dfr/. With this tool it is possible to create datasets of XML documents containing metadata of articles indexed in JSTOR's database. Datasets can be created according to any search criteria that fits normal searches in JSTOR. Tips and tricks for making searches in JSTOR can be found here: https://support.jstor.org/hc/en-us/articles/115004701828-Searching-A-Practical-Overview. 

Be aware that JSTOR dfr only allows the download of 25000 documents per time. Therefore, in the case where the datasets are biger than 25000 documents it is necessary to divide the search (usually by dates) in numerous parts. For instance, a search for the word model, constrained to the discipline of economics, gives around 650000 results (this number of documents may take a long time to be processed). Hence, it would be advisable to divide the search in 26 parts. This could be done by finding first result from 1950 to 1960, then 1961 to 1970 and so forth. Downloaded datasets come zipped. Hence, it doesn't matter whether you have multiple files or only one. Simply select all files and extract to wherever you want, this will put all XML files in a same folder called "metadata". 

For the first part of the tutorial I will be using data of the following JSTOR search: "experiment"; constrained to the discipline of economics; constrained to only journals; constrained to the years 1940 to 2010 (https://www.jstor.org/dfr/results?searchType=facetSearch&cty_journal_facet=am91cm5hbA%3D%3D&sd=1940&ed=2010&disc_economics-discipline_facet=ZWNvbm9taWNzLWRpc2NpcGxpbmU%3D&Query=experiment&acc=dfr&acc=dfr). The search yielded 39,503 results (01/02/2020). The download was divided into two parts: one for the years 1940 to 1990 and another for 1991 to 2010.
# 2. Typical workflow
Once we download and extract the dataset, it is time to initiate our workflow. The firs step is to transform the acquired data into a dataframe using JSTOR_df. This is simply achieved using as input the files' location on the computer, for instance JSTOR_df("C:/Users/.../Documents/metadata/"). A coutndown will initiate demonstrating the progress of the transformation. The code should be as follows:
    
    library(devtools)
    devtools::install_github(“arthurbnetto/tidyJSTOR”)
    library(tidyJSTOR)
   
    df <- JSTOR_df("C:/Users/.../Documents/metadata/")
    
Now, with our df in hand wwe can start to analyze it, but first let's clean it. JSTOR metadata counts with numerous problems. Several papers cointain repeated names (such as "back matter" and "front matter"), others don't have abstracts, and others come in languages other than english that are not supported by the package yet. In cleaning it, we maintain only english documents with abstracts. In order to be able to observe the number of documents discarded during the cleaning process, it is necessary to create a different dataframe. If knowing the pattern of discard is not important, the previous dataframe can be overwritten. Nevertheless, it is advisable - when memory is not a problem - to keep both dataframes to trace possible important differences that may indicate some kind of selection bias during the process of cleaning.

    dfClean <- CleanJSTOR_df(df)

Once we have both dataframes it is possbile to plot the cleaning results.

    plotCleaningResult(dfClean, df)
    
A typical output would be as the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/clean.jpeg)

It seems that the cleaning process has not produced any problem other than discarding a high volume of documents. Let's see how the dataframe looks now:

    View(head(dfClean))


![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/dfHead.jpeg)

Now, with a clean dataset, it is possible to start some exploratory analysis. First, let's have a look into which were the words that were most used in our dataset. More specifically let's see which were the words that were in the top 5 most used words for at least 3 distinct years (not necessarily in sequence):
           
    JSTORrepeatedTopwords(dfClean, 5, 3)

The output is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/topwords.jpeg)

The same can be done for bigrams and trigrams using the commands JSTORrepeatedTopBigrams and JSTORrepeatedTopTrigrams.

    JSTORrepeatedTopBigrams(dfClean, 5, 3)
    JSTORrepeatedTopTrigrams(dfClean, 5, 3)

Changing x and y modificates the graphs according to the number of top words the command is looking for and how many times the words appeared in the top, respectively. For instance, if instead of 5 and 3 we had 4 and 2, it would be the words which appeared at least two times in the top 4. 

A simlar graph can be built for journals. Instead of observing the main words over time, it is possible to observe which were the main journals publishing about our search. The next command line plots a graph of the journals that were in the top 5 in at least 3 distinct years:

     JSTORplotJournals(dfClean, 5, 3)
     
the main journals over time of our dataset are the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/journals.jpeg)

The final graph allows to observe the trend of specific clusters of words in the abstracts. Hence, imagine we are interested in observing how words related to theory evolved inside the papers in our dataset. We could create an array of words related to theory such as: c("theory", "theories", "theoretical"). With this array we could plot the evolution of this words inside our dataframe in the following manner:

    theory<-VocabCount(dfClean, c("theory", "theories", "theoretical"))
    JSTORplotVocabCount(theory, "theory")
    
The output would be this:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/theory.jpeg)

The first function counts the number of appearences of those words and normalizes according to the number of papers in each year. The dataframe looks as follows and the second function simply plots it.

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/theoryHead.jpeg)

Our results are interesting, but we could go further and compare theoretical words with empirical words. We can now create an array of words related to empirical research such as: c("empirical", "data", "empirically"). Following the same procedure for creating the dataframe, the code would be as follows:

    empirical<-VocabCount(dfClean, c("data","empirical"))
    
The next step is creating a list with our searched sets of words for in the sequence plotting them together:

    VocabList<-list(theory, empirical)
    JSTORplotVocabCount(VocabList, c("theory","empirical"))

The resultant chart is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/theoryempirical.jpeg)
    
Considering that changes in mentions to specific words from year to year are hard to interpret, the function allows to smooth the data in order to oberve in a more clear way what were the trends. It is also possible to lower and upper bounds fo the years being analyzed (it is interesting to notice that YearBounds is a paramater of all plotting functions of the package). Putting together these two changes will result in the following line of code:

    JSTORplotVocabCount(VocabList, c("theory","empirical"), smooth = TRUE, YearBounds = c(1970,1990)
    
The output is arguably easier to interpret:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/theoryempiricalsmooth.jpeg)

The vocabCount function also allows a different kind of comparison between words. Remaining with empirical and theoretical words, let's say that we consider some words more important than others. We could say that the use of a word such as "empirical" is more important than the word "data" to define if a paper is empirical. The same could be said about the words "theoretical" and "theory". We could also imagine that the characterization of a paper between empirical and theoretical is a matter of how much higher is the score (count) for empirical words when compared to theoretical words. The VocabCount function allows to provide a score vector for words in a manner that we could say that one count of the word "empirical" is valued 5 and one of "data" only 1, while one count of the word "theoretical" is valued -5 and one of "theory" only -1. the code would be the following:

    EmpVs.Theo<-VocabCount(dfClean, c("data","empirical","theory","theoretical"), scores = c(1,5,-1,-5))
    
We surely want to see that on a graph. For that, the code is:

    JSTORplotVocabCount(EmpVs.Theo, "Empirical(<1) x Theory(>1)")
    
![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/empvstheo.jpeg)
    
Make sure to remember whether you used counts or scores for a sound interpretation of the graphs. We are almost done in our analysis, and we can see that empirical words have come to dominate our dataset over time. A question arises when noticing that. Experiments in economics can be both psychological experiments following a tradition of experimental/behavioral economics or field/quasi experiments that follow the tradition of labor eocnomics and evaluaiton of public policies. Has any of those been more important in our analysis. To discover that, I realized to different searches in JSTOR dfr: 1) "experiment AND 'Ashenfelter'", constrained to the economics discipline; constrained to only journals. 2) "experiment AND 'Kahneman'", constrained to the economics discipline; constrained to only journals. Ashenfelter is a famous labor economist and proposer of experimentation in the evaluation of public policies while Kahneman is a known proposer of experimental and behavioral economics. We want to know how many times they were cited together with the word experiment in economics' journals. We don't need to clean the dataset for that. We only need to plot the search count for each different search (this basically means plotting JSTOR results for each year in a similar manner as Google NGRAM does for google books). First lets transform our JSTOR files into dataframes and store them in a list.

    ashenfelter <- JSTOR_df("C:/Users/.../Ashenfelter/metadata")
    kahneman <- JSTOR_df("C:/Users/.../Kahneman/metadata")
    authors<-list(ashenfelter, kahneman)

With the list, we can now plot both search results:

    plot_search(authors, c("Ashenfelter", "Kahneman"))

The plot is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/ashenfelterkahneman1.jpeg)

Considering that for the last 5 or 10 years publishers held different agreements with JSTOR - which creates a severe drop in the number of articles and a considerably different selection of journals is available in tha dataset -, the drop in the count should not be taken into consideration. From this point of view, it is also advisable to constraint the analysis to the year before 2010. We can also smooth the data to avoid being lost in analysing small and artifactual changes. The following figure contraints the analysis to the years 1970 to 1990 and smoothes the data:

    plot_search(authors, c("Ashenfelter", "Kahneman"), smooth = TRUE, YearBounds=c(1970,1990))
    
The figure that we now have is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/ashenfelterkahneman2.jpeg)

Now that we have two different dataframes, some new questions may arise. Observing counts is interesting, but they can be very different when observed more closely. Were they mentioned in the same Journals? tidyJSTOR allow the comparison of two dataframes in respect to their Journals, Words, Bigrams and Trigrams. Basically, it plots a comparison of JSTORrepeatedTop...() or JSTORplotJournals in two different dataframes. Let's compare which were the Journals that mentioned Ashenfelter and Kahneman along with experiment. More specifically, let's compare which were the top 4 journals that appeared at least 4 distinct years for each author. The function is the following:

    compareJSTOR_dfs(ashenfelter, kahneman, 4,4, c("ashenfelter", "kahneman"), "Journal")
    
The chart looks like this:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/compareJournals.jpeg)

For curiosity, let's plot a comparison of their bigrams as well (with no cleaning this time):

    compareJSTOR_dfs(ashenfelter, kahneman, 4,4, c("ashenfelter", "kahneman"), "Bigrams")
   
This results in:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/imagesReadMe/compareBigrams.jpeg)
    
Words and trigrams can be compared with the following commands:

    compareJSTOR_dfs(ashenfelter, kahneman, 4,4, c("ashenfelter", "kahneman"), "Trigrams")
    compareJSTOR_dfs(ashenfelter, kahneman, 4,4, c("ashenfelter", "kahneman"), "Words")
