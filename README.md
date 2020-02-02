# tidyJSTOR
This package transforms JSTOR's DFR into tidy dataframes and produces ggplot2 visualizations

# 1. Downloading JSTOR's DFR
JSTOR offers for researchers the tool "data for research" (DFR) at: https://www.jstor.org/dfr/. With this tool it is possible to create datasets of XML documents containing metadata of articles indexed in JSTOR's database. Datasets can be created according to any search criteria that fits normal searches in JSTOR. Tips and tricks for making searches in JSTOR can be found here: https://support.jstor.org/hc/en-us/articles/115004701828-Searching-A-Practical-Overview. 

Be aware that JSTOR dfr only allows the download of 25000 documents per time. Therefore, in the case where the datasets are biger than 25000 documents it is necessary to divide the search (usually by dates) in numerous parts. For instance, a search for the word model, contrained to the discipline of eocnomics, gives around 650000 results. Hence, it would be advisable to divide the search in 26 parts. This could be done by finding first result from 1950 to 1960, then 1961 to 1970 and so forth. Downloaded datasets come zipped. Hence, it doesnt matter whether you have multiple files or only one. Simply select all files and extract to wherever you want, this will put all XML files in a same folder called "metadata". For the first part of the tutorial I will be using data of the following JSTOR search: "experiment"; constrained to the discipline of economics; constrained to only journals; constrained to the years 1940 to 2010. The search yielded 39,503 results (01/02/2020). The download was divided into two parts: one for the years 1940 to 1990 and another for 1991 to 2010.
# 2. Typical workflow
Once with the dataset extracted in hand, is time to initiate our workflow. The firs step is to transform the data into a dataframe using JSTOR_df. This is simply achieved using solely the files' location on the computer, for instance JSTOR_df("C:/Users/.../Documents/metadata/"). A coutndown will initiate demonstrating the progress of the transformation. The code should be as follows:
    
    library(devtools)
    devtools::install_github(“arthurbnetto/tidyJSTOR”)
    library(tidyJSTOR)
   
    df <- JSTOR_df("C:/Users/.../Documents/metadata/")
    
Now, with our df in hand is time to analyze it, but first let's clean it. JSTOR metadata counts with numerous problems. Several papers cointain repeated names (suck as "back matter" and "front matter"), others don't have abstracts, and normally it returns papers in languages other than englis that are not supported by the package yet. In cleaning it, we maintain only english documents with abstracts. In order to be able to observe the number of documnets discarded during the cleaning process, it is necessary to create a different dataframe. If knowing the patter of discard is not important, the previous dataframe can be overwritten. Nevertheless, it is advisable - when memory is not a problem - to keep both dataframes to trace possible important differences that may indicate some kind of selection bias during the process of cleaning.

    dfClean <- CleanJSTOR_df(df)

Once we have both dataframes it is possbile to plot the cleaning results.

    plotCleaningResult(dfClean, df)
    
A typical output would be as the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/clean.jpeg)

It seems that the cleaning process has not produced any problem other than discarding a hihg volume of documents. Let's see how the dataframe looks now:

    View(head(dfClean))


![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/dfHead.jpeg)

Now, with a clean dataset, it is possible to start some exploratory analysis. First, let's have a look into which were the words that were most used in our dataset. More specifically let's which were the words that were in the top 5 most used words for at least 3 years:
           
    JSTORrepeatedTopwords(dfClean, 5, 3)

The output is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/topwords.jpeg)

The same can be done for bigrams and trigrams using the commands JSTORrepeatedTopBigrams and JSTORrepeatedTopTrigrams.

    JSTORrepeatedTopBigrams(dfClean, 5, 3)
    JSTORrepeatedTopTrigrams(dfClean, 5, 3)

Changing x and y modificates the graphs according to the number of top words the command is looking for and how many times the words appeared in the top. For instance, if instead of 5 and 3 we had 4 and 2, it would be the words which appeared at least two times in the top 4. 

A simlar graph can be built for journals. Instead of observing the main words over time, it is possible to observe which were the main journals publishing about our search. The next command line plots a graph of the journals that were in the top 5 in at least 3 distinct years:

     JSTORplotJournals(dfClean, 5, 3)
     
the main journals over time of our dataset are the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/journals.jpeg)

The final graph allows to observe the trend of specific clusters of words in the abstracts. Hence, imagine we are interested in observing how words related to theory evolved inside the papers in our dataset. We could create an array of words related to theory such as: c("theory", "theories", "theoretical"). With this array we could plot the evolution of this words inside our dataframe in the following manner:

    theory<-VocabCount(dfClean, c("theory", "theories", "theoretical"))
    JSTORplotVocabCount(theory, "theory")
    
The output would be this:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/theory.jpeg)

The first function counts the number of appearences of those words and normalizes according to the number of papers in each year. The dataframe looks as follows and the graph simply plots it.

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/theoryHead.jpeg)

Our results are interesting, but we could go further and compare theoretical words with empirical words. We can now create an array of words related to empirical research such as: c("empirical", "data", "empirically"). Following the same procedure for creating the dataframe, the code wwould be as follows:

    empirical<-VocabCount(dfClean, c("data","empirical"))
    
The next step is creating a list with our searched sets of words for in the sequence plot them together:

    VocabList<-list(theory, empirical)
    JSTORplotVocabCount(VocabList, c("theory","empirical"))

The resultant chart is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/theoryempirical.jpeg)
    
Considering that changes in mentions to specific words from year to year are hard to interpret, the function allows to smooth the data in order to oberve in a more clear way what were the trends. It is also possible to constraint maximum year of analysis. Putting together these two changes will result in the following line of code:

    JSTORplotVocabCount(VocabList, c("theory","empirical"), smooth = TRUE, 2000)
    
The output is arguably easier to interpret:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/theoryempiricalsmooth.jpeg)

The vocabCount function also allows a different kind of comparison between words. Remaining with empirical and theoretical words, let's say that we consider some words more important than others. We could say that the use of a word such as "empirical" is more important than the word "data" to define if a paper is empirical. The same could be said about the words "theoretical" and "theory". We could also imagine that the characterization of a paper between empirical and theoretical is a matter of how higher is the score (count) for empirical words when compared to theoretical words. The VocabCount function allows to provide a score vector for words in a manner that we could say that one count of the word "empirical" is valued 5 and one of "data" only 1, while one count of the word "theoretical" is valued -5 and one of "theory" only -1. the code would be the following:

    EmpVs.Theo<-VocabCount(dfClean, c("data","empirical","theory","theoretical"), scores = c(1,5,-1,-5))
    
We surely want to see that on a graph. For that, the code is:

    JSTORplotVocabCount(EmpVs.Theo, "Empirical(<1) x Theory(>1)")
    
![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/empvstheo.jpeg)
    
Make sure to remember whether you used counts or scores for a sound interpretation of the graphs. We are almost done in our analysis, and we can see that empirical words have come to dominate our dataset over time. A question arises when noticing that. Experiments in economics can be both psychological experiments following a tradition of experimental/behavioral economics aor field/quasi experiments that follow the tradition of labor eocnomics and evaluaiton of public policies. Has any of those been more important in our analysis. To discover that, I realized to different searches in JSTOR dfr: 1) "experiment AND 'Ashenfelter'", constrained to the economics discipline; constrained to only journals. 2) "experiment AND 'Kahneman'", constrained to the economics discipline; constrained to only journals. Ashenfelter is a famous labor economist and proposer of experimentation in the evaluation of public policies while Kahneman is a known proposer of experimental and behavioral economics. We want to know how many times they were cited together with the word experiment in economics' journals. We don't need to clean the dataset for that. We only need to plot the search count for each different search. First lets transform our JSTOR files into dataframes and store them in a list.

    ashenfelter <- JSTOR_df("C:/Users/.../Ashenfelter/metadata")
    kahneman <- JSTOR_df("C:/Users/.../Kahneman/metadata")
    authors<-list(ashenfelter, kahneman)

With the list, we can now plot both search results:

    plot_search(authors, c("Ashenfelter", "Kahneman"))

The plot is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/ashenfelterkahneman1.jpeg)

Considering that for the last 5 or 10 years publishers held different agreements with JSTOR - which creates a severe drop in the number of articles and a considerably different selection of journals is available in tha dataset -, the drop in the count should not be taken into consideration. From this point of view, it is also advisable to constraint the analysis to the year before 2010. We can also smooth the data to avoid being lost in analysing small and artifactual changes:

    plot_search(authors, c("Ashenfelter", "Kahneman"), smooth = TRUE, 2010)
    
The figure that we have now is the following:

![alt text](https://raw.githubusercontent.com/arthurbnetto/tidyJSTOR/master/ashenfelterkahneman2.jpeg)
