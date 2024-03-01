## data

#' AFINN Lexicon
#' 
#' The preprocessed and deduplicated AFINN lexicon (Nielsen 2011). The original dataset was retrieved from \pkg{syuzhet} and preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @name afinn
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 3,380 rows and 2 columns
#' @references Nielsen, F.Å. A new ANEW: Evaluation of a word list for sentiment analysis in microblogs, arXiv preprint arXiv:1103.2903, (2011).
#' @source \link{http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010}
NULL


#' Vader Lexicon
#' 
#' A dataframe containing the preprocessed and deduplicated Vader lexicon (Hutto & Gilbert 2014). The original dataset was preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' }
#' 
#' @section License: The MIT License (MIT). 
#' 
#' @section Copyright: (c) 2016 C.J. Hutto. 
#' 
#' Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#'
#' The above copyright notice and this permission notice shall be included in all
#' copies or substantial portions of the Software.
#'
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#' SOFTWARE.
#' 
#' @name vader
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 7,495 rows and 2 columns
#' @references Hutto, C.J. and Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based Model
#' for Sentiment Analysis of Social Media Text. Eighth International Conference
#' on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.
#' @source \link{https://github.com/cjhutto/vaderSentiment}
NULL


#' Opinion Lexicon
#' 
#' The preprocessed and deduplicated Opinion lexicon (Hu & Liu 2004). The original dataset was retrieved from \pkg{syuzhet} and preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment polarity score (-1, 1)
#' 
#' }
#' 
#' @name bing
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 6,782 rows and 2 columns
#' @references
#' Liu, B., Hu, M., and Cheng, J. "Opinion Observer: Analyzing and Comparing Opinions on the Web." Proceedings of the 14th International World Wide Web conference (WWW-2005), May 10-14, 2005, Chiba, Japan.
#'  
#' Hu, M. and Liu, B. "Mining and Summarizing Customer Reviews." Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, Washington, USA.
#' 
#' @source http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
NULL

 
#' General Inquirer Lexicon
#' 
#' The preprocessed and deduplicated General Inquirer lexicon (Stone et al. 1966). The original dataset was preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment polarity score
#' 
#' }
#' 
#' @name inquirer
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 3,610 rows and 2 columns
#' @references Stone, P.J., Dunphy, D.C., Smith, M.S. The general inquirer: A computer approach to content analysis, (M.I.T. Press, Cambridge, MA, 1966).
#' @source \link{https://inquirer.sites.fas.harvard.edu/homecat.htm}
NULL


#' Jockers Lexicon
#' 
#' The preprocessed and deduplicated Jockers lexicon (Jockers 2017). The original \pkg{syuzhet} dataset was retrieved and preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @name syuzhet
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 10,747 rows and 2 columns
#' @references Jockers, M. L. (2017). Syuzhet: Extract sentiment and plot arcs from Text.
#' @source \link{https://github.com/mjockers/syuzhet}
NULL



#' Loughran and McDonald Lexicon
#' 
#' The preprocessed and deduplicated Loughran & McDonald lexicon (Loughran & McDonald 2016). The original dataset was retrieved from \pkg{lexicon} and preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment polarity score
#' 
#' }
#' 
#' @section License: The original authors note the data is available for 
#' non-commercial, research use: "The data compilations provided on 
#' this website are for use by individual researchers.".  For more details see:
#' https://sraf.nd.edu/textual-analysis/resources/#Master%20Dictionary.
#' @section Copyright: Copyright holder University of Notre Dame
#' @name loughran
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 2,702 rows and 2 columns
#' @references Loughran, T. and McDonald, B. (2016). Textual analysis in accounting and finance: A survey. Journal of Accounting Research 54(4), 1187-1230.
#' @source \link{https://sraf.nd.edu/textual-analysis/resources}
NULL


#' NRC Lexicon
#' 
#' The preprocessed and deduplicated NRC lexicon (Mohammad & Turney 2010). The original dataset was preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment polarity score
#' 
#' }
#' 
#' @section License: The original authors note the data is available for 
#' non-commercial use: "If interested in commercial use of any of these lexicons, 
#' send email to Saif M. Mohammad (Senior Research Officer at NRC and creator of 
#' these lexicons): saif.mohammad@@nrc-cnrc.gc.ca and Pierre Charron (Client 
#' Relationship Leader at NRC): Pierre.Charron@@nrc-cnrc.gc.ca. A nominal 
#' one-time licensing fee may apply."
#' @section Copyright: Copyright holder University of Notre Dame.
#' @name nrc
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 5,474 rows and 2 columns
#' @references Mohammad, S. and Turney, P. "Emotions Evoked by Common Words and Phrases: Using Mechanical Turk to Create an Emotion Lexicon." In Proceedings of the NAACL-HLT 2010 Workshop on Computational Approaches to Analysis and Generation of Emotion in Text, June 2010, LA, California.
#' @source \link{http://saifmohammad.com/WebPages/lexicons.html}
NULL


#' SenticNet Lexicon
#' 
#' The preprocessed and deduplicated SenticNet lexicon (Cambria et al. 2016). The original dataset was retrieved from \pkg{lexicon} and preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @name senticnet
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 23,626 rows and 2 columns
#' @references Cambria, E., Poria, S., Bajpai, R. and Schuller, B. SenticNet 4: A semantic resource for sentiment analysis based on conceptual primitives. In: COLING, pp. 2666-2677, Osaka (2016)
#' @source \link{http://sentic.net/downloads}
NULL


#' SentiWordNet Lexicon
#' 
#' The preprocessed and deduplicated SentiWordNet lexicon (Baccianella et al. 2010). The original dataset was preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @section License: \link{https://creativecommons.org/licenses/by-sa/3.0/legalcode}
#' @name sentiwn
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 19,920 rows and 2 columns
#' @references Baccianella S., Esuli, A. and Sebastiani, F. (2010). SentiWordNet 3.0: An Enhanced Lexical Resource for Sentiment Analysis and Opinion Mining. International Conference on Language Resources and Evaluation.
#' @source \link{https://sentiwordnet.isti.cnr.it}
NULL


#' SlangSD Lexicon
#' 
#' The preprocessed and deduplicated SlangSD lexicon (Wu et al. 2016). The original dataset was retrieved from \pkg{lexicon} and preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @section License: The dictionary is free to use. If you use it for an academic publication, we 
#' ask that you cite it using the citation below. If it is used in anything other 
#' than an academic publication, we ask that you provide a credit and link to SlangSD.com.
#' @name slang
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 48,214 rows and 2 columns
#' @references Wu, L., Morstatter, F., and Liu, H. (2016). SlangSD: Building and using a sentiment dictionary of slang words for short-text sentiment classification. CoRR. abs/1168.1058. 1-15.
#' @source \link{http://slangsd.com}
NULL


#' SO-CAL Lexicon
#' 
#' The preprocessed and deduplicated SO-CAL lexicon (Taboada et al. 2011). The original dataset was preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @section License: This work is licensed under a Creative Commons 
#' Attribution-NonCommercial-ShareAlike 4.0 International License.
#' https://creativecommons.org/licenses/by-nc-sa/4.0/
#' @name socal
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 5,931 rows and 2 columns
#' @references Taboada, M., Brooke, J., Tofiloski, M., Voll, K., and Stede, M. Lexicon-Based Methods for Sentiment Analysis, Computational Linguistics, 37(2) (2011) 267-307.
#' @source \link{https://github.com/sfu-discourse-lab/SO-CAL}
NULL


#' WKWSCI Lexicon
#' 
#' The preprocessed and deduplicated SO-CAL lexicon (Khoo & Johnkhan 2017). The original dataset was preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}. 
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @section License: This work is licensed under a Creative Commons 
#' Attribution-NonCommercial-ShareAlike 4.0 International License.
#' https://creativecommons.org/licenses/by-nc-sa/4.0/
#' @name wkwsci
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 10,181 rows and 2 columns
#' @references Khoo, C. S., & Johnkhan, S. B. (2017). Lexicon-based sentiment analysis: Comparative evaluation of six sentiment lexicons. Journal of Information Science, pp. 1–21.
#' @source \link{https://researchdata.ntu.edu.sg/dataset.xhtml?persistentId=doi:10.21979/N9/DWWEBV}
NULL


#' TextBlob Lexicon
#' 
#' The preprocessed and deduplicated TextBlob lexicon (Loria 2017). The original dataset was preprocessed using \pkg{WoCSentiment} function \code{cleanse_lex}.
#' 
#' @details
#' \itemize{
#' \item word. An n-gram representing a term or phrase
#' \item value. The sentiment intensity score
#' 
#' }
#' 
#' @section Copyright: Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#' 
#' The above copyright notice and this permission notice shall be included in
#' all copies or substantial portions of the Software.
#' 
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#' SOFTWARE.
#' 
#' @name textblob
#' @docType data
#' @format A \code{\link[base]{data.frame}} with 1,254 rows and 2 columns
#' @references S. Loria, TextBlob: Simplified Text Processing (2017).
#' @source \link{https://github.com/sloria/TextBlob/blob/dev/src/textblob/en/en-sentiment.xml}
NULL


#' WoC Lexicons
#' 
#' A vector including 13 character strings representing the names of the 13 lexicons included in the default WoC crowd in the \code{score_woc} function. 
#' @name WoC_Crowd
#' @docType data
#' @format A \code{\link[base]{vector}} with 13 character-valued lexicon object names
NULL