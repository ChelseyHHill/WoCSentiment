# WoCSentiment
The WoCSentiment R package allows users to perform unsupervised, lexicon-based sentiment analysis using either a custom crowd or the default crowd of 13 popular, publicly available sentiment lexicons.

## Introduction
The package is introduced in the article [The Wisdom of the Lexicon Crowds: Leveraging on Decades of Lexicon-Based Sentiment Analysis for Improved Results](https://rdcu.be/enoHG). The WoCSentiment package can be used for applying wisdom of the crowd sentiment analysis. 

## Citation
If you use the package to perform WoC sentiment analysis for research purposes, please cite the article:

*Hill, C. H., Fresneda, J. E., & Anandarajan, M. (2025). The wisdom of the lexicon crowds: leveraging on decades of lexicon-based sentiment analysis for improved results. Journal of Big Data, 12(1), 1-30.*

## Installation
To install the package in R, first download the GitHub Repository. Set your working directory to the location of the unzipped file. Run the code below to install the package and load the library for use in your R session.
```r
install.packages("WoCSentiment_0.1.0.tar.gz", repos=NULL, source=TRUE)
library(WoCSentiment)
```

## Getting Started
The core analysis function in the WoCSentiment package, `score_woc`, uses parallel processing, which requires you to create a cluster and setup a cluster for use.
```r
library(parallel)
library(doParallel)
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)
```

Text data for WoC sentiment analysis using WoCSentiment should be UTF-8. For non-social media text data, data preprocessing is handled during analysis using the `score_woc` function. If text data is twitter or other social media data, using the `twit_pre` function is recommended. The function removes social-media specific text that is not needed for WoC sentiment analysis, including: mentions, retweets, hashtags, and links.

The text data input in the `score_woc` function can either be a vector or in dataframe format with a column named "text", which contains the text data for analysis. By default, the function returns a vector of sentiment polarity scores (-1, 0, 1) for each text observation used as input. Changing the default arguments for one or more of `intensity` (the numeric WoC sentiment intensity score), `all_lex` (the numeric sentiment intensity scores for all lexicons in the WoC crowd) and `majority` (the sentiment polarity score based on majority voting, with ties broken at random) will return a dataframe.

> [!NOTE]
> When using the `score_woc` function, if `majority = TRUE`, a random seed should be set prior to analysis for reproducibility.

The example below uses the `course_evaluations` dataset available in the [sentimentr](https://github.com/trinker/sentimentr) package.
```r
library(sentimentr)
score_woc(course_evaluations)
```


## Crowd Lexicons
The thirteen publicly-available lexicons included in the WoCSentiment package as the crowd are: 

* `"afinn"`: AFINN (Nielsen 2011)
* `"bing"`: Opinion (Hu & Liu 2004)
* `"inquirer"`: General Inquirer (Stone et al. 1966)
* `"loughran"`: Loughran (Loughran & McDonald 2016)
* `"nrc`: NRC (Mohammad & Turney 2010)
* `"senticnet"`: SenticNet (Cambria et al. 2016)
* `"sentiwn"`: SentiWordNet (Baccianella et al. 2010)
* `"slang"`: SlangSD (Wu et al. 2016)
* `"socal"`: SO-CAL (Taboada et al. 2011)
* `"syuzhet"`: Jockers (Jockers 2017)
* `"textblob"`: TextBlob (Loria 2017)
* `"vader"`: Vader (Hutto & Gilbert 2014)
* `"wkwsci"`: WKWSCI (Khoo & Johnkhan 2018)


Any of the above crowd lexicons can be accessed as a dataframe with columns "word" and "value" by using the `get_WoC_lexicon` function. 

## Contact
Chelsey Hill
Montclair State University
[Email](mailto:hillc@montclair.edu)


## References

Baccianella S., Esuli, A. and Sebastiani, F. (2010). SentiWordNet 3.0: An Enhanced Lexical Resource for Sentiment Analysis and Opinion Mining. International Conference on Language Resources and Evaluation.

Cambria, E., Poria, S., Bajpai, R. and Schuller, B. (2016). SenticNet 4: A semantic resource for sentiment analysis based on conceptual primitives. In: COLING, pp. 2666-2677, Osaka (2016)

Hill, C. H., Fresneda, J. E., & Anandarajan, M. (2025). The wisdom of the lexicon crowds: leveraging on decades of lexicon-based sentiment analysis for improved results. Journal of Big Data, 12(1), 1-30.

Hu, M. and Liu, B. "Mining and Summarizing Customer Reviews." Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, Washington, USA.

Hutto, C.J. and Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based Model for Sentiment Analysis of Social Media Text. Eighth International Conference on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.

Jockers, M. L. (2017). Syuzhet: Extract sentiment and plot arcs from Text.

Khoo, C. S., & Johnkhan, S. B. (2017). Lexicon-based sentiment analysis: Comparative evaluation of six sentiment lexicons. Journal of Information Science, pp. 1–21.

Liu, B., Hu, M., and Cheng, J. "Opinion Observer: Analyzing and Comparing Opinions on the Web." Proceedings of the 14th International World Wide Web conference (WWW-2005), May 10-14, 2005, Chiba, Japan.

Loria, S. TextBlob: Simplified Text Processing (2017).

Loughran, T. and McDonald, B. (2016). Textual analysis in accounting and finance: A survey. Journal of Accounting Research 54(4), 1187-1230.

Mohammad, S. and  Turney, P. "Emotions Evoked by Common Words and Phrases: Using Mechanical Turk to Create an Emotion Lexicon." In Proceedings of the NAACL-HLT 2010 Workshop on Computational Approaches to Analysis and Generation of Emotion in Text, June 2010, LA, California.

Nielsen, F.Å. A new ANEW: Evaluation of a word list for sentiment analysis in microblogs, arXiv preprint arXiv:1103.2903, (2011).

Stone, P.J.,  Dunphy, D.C., and Smith, M.S. The general inquirer: A computer approach to content analysis, (M.I.T. Press, Cambridge, MA, 1966).

Taboada, M., Brooke, J.,  Tofiloski, M.,  Voll, K., and Stede, M. Lexicon-Based Methods for Sentiment Analysis, Computational Linguistics, 37(2) (2011) 267-307. 

Wu, L., Morstatter, F., and Liu, H. (2016). SlangSD: Building and using a sentiment dictionary of slang words for short-text sentiment classification. CoRR. abs/1168.1058. 1-15.

