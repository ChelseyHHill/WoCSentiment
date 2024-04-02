## WOC FUNCTIONS FINAL


## WOC Functions

#' Preprocess Sentiment Lexicons
#' @description
#' Cleanse lexicons of duplicate terms, neutral sentiment terms, and punctuation, where appropriate. 
#'
#' @param lexicon A character string identifying the lexicon to cleanse. There must be a \code{\link[base]{data.frame}} in your global environment of the same name containing two columns, "word" and "value". 
#' @param emot A \code{\link[base]{data.frame}} containing two columns, the first column containing emoticons and the second column containing the meanings of the emoticons in words. Defaults to \pkg{lexicon} \code{\link[lexicon]{hash_emoticons}}. 
#' @returns A \code{\link[base]{data.frame}} with "word" and "value" columns that combines duplicates and removes neutrals.
#' @export
#' @examples
#' \dontrun{
#' library(syuzhet)
#' syuzhet <- syuzhet::get_sentiment_dictionary()
#' syuzhet <- cleanse_lex(syuzhet)
#' }
#'
cleanse_lex <- function(lexicon, emot = lexicon::hash_emoticons){
  lex <- get(lexicon)
  if(lexicon == "vader"){
    word_punc <- lex$word[grep('[[:punct:]]', lex$word)]
    word_punc_ind <- which(lex$word %in% lex$word[grep('[[:punct:]]', lex$word)])
    word_punc <- word_punc[!word_punc %in% lexicon::hash_emoticons$x]
    word_punc_ind <- which(lex$word %in% word_punc)
    word_punc <- c(word_punc[grep("(\\w)-(\\w)", word_punc)], word_punc[grep("(\\w)'(\\w)", word_punc)])
    word_punc_ind <- which(lex$word %in% word_punc)
    word_punc <- word_punc[!word_punc %in% word_punc[grep("(^[[:alnum:]]{1})-([[:alnum:]]{1})", word_punc)]]
    word_punc_ind <- which(lex$word %in% word_punc)
    word_punc <- gsub("-", " ", word_punc)
    word_punc <- gsub("'", "", word_punc)
    lex$word[word_punc_ind] <- word_punc
  } else {
  if(any(grepl("(\\w)'(\\w)", lex$word))){
    lex$word <- gsub("(\\w)'(\\w)", "\\1\1\\2", lex$word,  perl = TRUE)
    lex$word <- gsub("\1", "", lex$word, fixed = TRUE)
  }
    if(lexicon != "bing"){
      lex$word <- gsub("-", " ", lex$word)
      } else {
  if(any(grepl("(\\w)-(\\w)", lex$word))){
    lex$word <- gsub("(\\w)-(\\w)", "\\1\2\\2", lex$word,  perl = TRUE)
    lex$word <- gsub("\2", " ", lex$word, fixed = TRUE)
  }}}
  if(lexicon == "sentiwn" || lexicon == "slang" || lexicon == "socal"){
    lex$word <- gsub("'", "", lex$word)
  }
  if(any(duplicated(lex$word))){
    dupe_words <- lex$word[duplicated(lex$word)] # identify words
    dupe_avg_scores <- aggregate(value ~ word, lex[lex$word %in% dupe_words,], mean) # compute avg scores
    lex <- lex[!duplicated(lex$word), ] # deduplicate
    lex[lex$word %in% dupe_words, "value"] <- dupe_avg_scores$value # replace score with average score
  }
  if(any(lex$value == 0)){
    lex <- lex[lex$value != 0, ] # remove neutrals
  }
  return(lex)
}

#' WoC Sentiment Lexicons
#' 
#' @description
#' Obtain an individual lexicon from the WoC lexicon crowd.
#' 
#' @param lexicon a character string identifying the lexicon from the default WoC crowd to return. Options include: 
#' 
#' \itemize{
#' \item \code{"afinn"}: AFINN sentiment intensity lexicon (Nielsen 2011)
#' \item \code{"bing"}: Opinion sentiment polarity lexicon (Hu & Liu 2004)
#' \item \code{"inquirer"}: General Inquirer sentiment polarity lexicon (Stone et al. 1966)
#' \item \code{"loughran"}: Loughran and McDonald sentiment polarity lexicon (Loughran & McDonald 2016)
#' \item \code{"nrc"}: NRC sentiment polarity lexicon (Mohammad & Turney 2010)
#' \item \code{"senticnet"}: SenticNet sentiment intensity lexicon (Cambria et al. 2016)
#' \item \code{"sentiwn"}: SentiWordNet sentiment intensity lexicon (Baccianella et al. 2010)
#' \item \code{"slang"}: SlangSD sentiment intensity lexicon (Wu et al. 2016)
#' \item \code{"socal"}: SO-CAL sentiment intensity lexicon (Taboada et al. 2011)
#' \item \code{"syuzhet"}: Jockers sentiment intensity lexicon (Jockers 2017)
#' \item \code{"textblob"}: TextBlob sentiment intensity lexicon (Loria 2017)
#' \item \code{"vader"}: Vader sentiment intensity lexicon (Hutto & Gilbert 2014)
#' \item \code{"wkwsci"}: WKWSCI sentiment intensity lexicon (Khoo & Johnkhan 2018)
#' }
#' 
#' @returns A \code{\link[base]{data.frame}} with:
#' \itemize{
#' \item word. n-grams representing a term or phrase
#' \item value. Sentiment intensity or polarity scores
#' 
#' }
#' @export
#' @references 
#' 
#' Baccianella S., Esuli, A. and Sebastiani, F. (2010). SentiWordNet 3.0: An Enhanced Lexical Resource for Sentiment Analysis and Opinion Mining. International Conference on Language Resources and Evaluation.
#' 
#' Cambria, E., Poria, S., Bajpai, R. and Schuller, B. SenticNet 4: A semantic resource for sentiment analysis based on conceptual primitives. In: COLING, pp. 2666-2677, Osaka (2016)
#' 
#' Hu, M. and Liu, B. "Mining and Summarizing Customer Reviews." Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, Washington, USA.
#' 
#' Hutto, C.J. and Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based Model
#' for Sentiment Analysis of Social Media Text. Eighth International Conference
#' on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.
#' 
#' Jockers, M. L. (2017). Syuzhet: Extract sentiment and plot arcs from Text.
#' 
#' Khoo, C. S., & Johnkhan, S. B. (2017). Lexicon-based sentiment analysis: Comparative evaluation of six sentiment lexicons. Journal of Information Science, pp. 1–21.
#' 
#' Liu, B., Hu, M., and Cheng, J. "Opinion Observer: Analyzing and Comparing Opinions on the Web." Proceedings of the 14th International World Wide Web conference (WWW-2005), May 10-14, 2005, Chiba, Japan.
#' 
#' Loria, S. TextBlob: Simplified Text Processing (2017).
#' 
#' Loughran, T. and McDonald, B. (2016). Textual analysis in accounting and finance: A survey. Journal of Accounting Research 54(4), 1187-1230.
#' 
#' Mohammad, S. and  Turney, P. "Emotions Evoked by Common Words and Phrases: Using Mechanical Turk to Create an Emotion Lexicon." In Proceedings of the NAACL-HLT 2010 Workshop on Computational Approaches to Analysis and Generation of Emotion in Text, June 2010, LA, California.
#' 
#' Nielsen, F.Å. A new ANEW: Evaluation of a word list for sentiment analysis in microblogs, arXiv preprint arXiv:1103.2903, (2011).
#' 
#' Stone, P.J.,  Dunphy, D.C., and Smith, M.S. The general inquirer: A computer approach to content analysis, (M.I.T. Press, Cambridge, MA, 1966).
#' 
#' Taboada, M., Brooke, J.,  Tofiloski, M.,  Voll, K., and Stede, M. Lexicon-Based Methods for Sentiment Analysis, Computational Linguistics, 37(2) (2011) 267-307. 
#' 
#' Wu, L., Morstatter, F., and Liu, H. (2016). SlangSD: Building and using a sentiment dictionary of slang words for short-text sentiment classification. CoRR. abs/1168.1058. 1-15.
#' 
#' @examples
#' get_WoC_lexicon("afinn")
#' 
get_WoC_lexicon <- function(lexicon){
  lex <- switch(lexicon,
                afinn = afinn,
                bing = bing,
                inquirer = inquirer,
                loughran = loughran,
                nrc = nrc,
                senticnet = senticnet,
                sentiwn = sentiwn,
                slang = slang,
                syuzhet = syuzhet,
                textblob = textblob,
                vader = vader, 
                wkwsci = wkwsci,
                socal = socal, 
                stop("lexicon must be one of the WoC lexicons, in the WoC_Crowd object. Valid input to the lexicon argument include: 'afinn', 'bing', 'inquirer', 'loughran', 'nrc', 'senticnet', 'sentiwn', 'slang', 'syuzhet', 'textblob', 'vader', 'wkwsci', or 'socal'")
  )
  return(lex)
}
  


#' Social Media Text Preprocessing
#'
#' @description
#' Remove social-media specific text from a dataset prior to performing WoC Sentiment Analysis, including: mentions, retweets, hashtags, and links. 
#' 
#' @param x A text string.
#' @returns A text string with mentions, retweets, hashtags, and urls removed.
#' @export
#' @examples
#' text_str <- "RT @HenryHill is the best dog ever #InstaHen"
#' twit_pre(text_str)
#'
twit_pre <- function(x){
  x <- gsub('\\b+RT', " ", x) #RT
  x <- gsub('@\\S+', " ", x) #mentions
  x <- gsub('#\\S+', " ", x) #hashtags
  x <- gsub('(f|ht)tp\\S+\\s*',"", x) #urls
  return(x)
}


#' Prepare Text Data for WoC Sentiment Analysis
#'
#' @description
#' Preprocess and create n-grams for WoC Sentiment Analysis according to lexicon-specific requirements, including: maximum n-grams, emoticons, and retained punctuation. 
#' 
#' @param dat A text string
#' @param n The maximum length of n-grams to create
#' @param lexicon A text string of the lexicon name, ie "syuzhet"
#' @param lex A lexicon \code{\link[base]{data.frame}} containing two columns, "word" and "value"
#' @param emot A \code{\link[base]{data.frame}} containing two columns, the first column containing emoticons and the second column containing the meanings of the emoticons in words. Defaults to \pkg{lexicon} \code{\link[lexicon]{hash_emoticons}}. 
#' @returns A \code{\link[base]{vector}} of n-grams
#' @importFrom NLP words ngrams
#' @export
get_grams_ems <- function(dat, n, lexicon, lex, emot = lexicon::hash_emoticons){
  word <- words(dat)

  if(lexicon != "vader" & any(word %in% emot$x)){
    word[which(word %in% emot$x)] <- emot$y[match(word[word %in% emot$x],emot$x)]
  }

  if(lexicon %in% c("bing", "inquirer", "sentiwn", "socal", "vader", "textblob")){
    punct_list <- word[word %in% lex$word[grep('[[:punct:]]', lex$word)] | tolower(word) %in% lex$word[grep('[[:punct:]]', lex$word)]]
    if(lexicon != "vader"){
      punct_list <- tolower(punct_list)
      word[which(word %in% punct_list)] <- punct_list
    }
    terms_li <- word[!word %in% punct_list]
    term_ind <- which(!word %in% punct_list)
  } else {
    terms_li <- word
    term_ind <- 1:length(word)
  }
  if (lexicon == "syuzhet"){
    terms_li <- gsub("-", "", terms_li)
  } else {
    terms_li <- gsub("-", " ", terms_li)
  }

  terms_li <- gsub("'", "", terms_li)
  terms_li <- gsub('[[:punct:]]', " ", terms_li)
  terms_li <- tolower(terms_li)

  word[term_ind] <- terms_li
  sent <- paste(word, collapse = " ")
  toks <- sapply(ngrams(words(tolower(sent)), 1:n), paste, collapse = " ")
  return(toks)
}





#' Handle lexicon-specific switch negation
#'
#' @description
#' Identifies negated n-grams and performs switch negation.
#' 
#' @param grams A vector of n-grams of text string created using \pkg{WoCSentiment} \code{get_grams_ems}.
#' @param word_li A vector of sentiment-bearing n-grams based on lexicon matching
#' @param tot_words The number of unigrams in the text document, indicating document length.
#' @param negators A negators A vector of negation terms. Defaults to \pkg{lexicon} \code{\link[lexicon]{hash_valence_shifters}}, isolating those terms identified as negators (\code{y = 1}). 
#' @returns A vector of signs (-1, 0, 1), of \code{length(word_li)}
#' @export
negates <- function(grams, word_li, tot_words, negators = lexicon::hash_valence_shifters){
  neg <- negators[negators$y == 1, ]$x
  sent <- grams[1:tot_words]
  gram_ind <- which(grams %in% word_li)
  unigrams <- grams[gram_ind[gram_ind <= tot_words]]
  scores <- rep(1, length(gram_ind))
  # If negation word in lexicon, assign negator score of 0
  scores[grams[gram_ind] %in% neg] <- 0 # assign 0 if negator and in lexicon
  if (length(grams[gram_ind[gram_ind > tot_words]]) > 0) {
    ngram_li <- grams[gram_ind[gram_ind > tot_words]]
    ngram_toks <- strsplit(ngram_li, " ")
    ngram_inds <- which(grams %in% ngram_li)
    ok <- lapply(ngram_toks, FUN = id_ngram_loc, sent_toks = sent)
    names(ok) <- ngram_li
    ok <- ok[!duplicated(names(ok))]
    ngram_inds_start <- rep(1, length(ngram_li))
    ngram_inds_end <- rep(1, length(ngram_li))
    for(i in 1:length(ngram_li)){
      ngram_inds_start[i] <- ok[[ngram_li[i]]][1]
      ngram_inds_end[i] <- ngram_inds_start[i] + (sapply(ngram_toks[i], length)) - 1
      ok[[ngram_li[i]]] <- ok[[ngram_li[i]]][-1]
      scores[gram_ind %in% (ngram_inds_start[i]:ngram_inds_end[i])] <- 0
    }
    if(length(ngram_li) > 1){
      for(i in 1:length(ngram_li)){
        ngram_locats <- mapply(':', ngram_inds_start[-i], ngram_inds_end[-i])
        self_ind <- ngram_inds_start[i]:ngram_inds_end[i]
        if(any(unlist(lapply(ngram_locats, function(x) setequal(intersect(self_ind, x), self_ind)))) == TRUE){
          scores[gram_ind %in% ngram_inds[i]] <- 0
        }
      }
    }
    all_neg_ind <- c((which(grams %in% unigrams) - 1), (ngram_inds_start - 1))
  } else {
    all_neg_ind <- which(grams %in% unigrams) - 1
  }
  all_neg_ind[all_neg_ind == 0] <- NA
  scores <- ifelse(scores == 0, 0,
                   ifelse(gram_ind == 1 | is.na(all_neg_ind) ==TRUE , 1,
                          ifelse(grams[all_neg_ind] %in% neg == TRUE, -1, 1)))
  
  return(scores)
}
#negates <- function(grams, word_li, tot_words, negators = lexicon::hash_valence_shifters){
#  neg <- negators[negators$y == 1, ]$x
#  sent <- grams[1:tot_words]
#  gram_ind <- which(grams %in% word_li)
#  scores <- rep(1, length(gram_ind))
#  ngram_li <- grams[gram_ind[gram_ind > tot_words]]
#  ngram_toks <- strsplit(ngram_li, " ")
#  ngram_inds <- which(grams %in% ngram_li)
#  # dedupe negated unigrams and ngrams containing negation
#  # omit unigrams in phrases, use ngram score instead
#  scores[grams[gram_ind] %in% neg] <- 0 # assign 0 if negator and in lexicon
#  unigrams <- grams[gram_ind[gram_ind <= tot_words]] # grams[!ngram_inds]
#  uni_ind <- which(grams %in% unigrams)
#  #if(length(ngram_li) > 0){
#  #  ok <- lapply(ngram_toks, FUN = id_ngram_loc, sent_toks = sent)
#  #  names(ok) <- ngram_li
#  #  ok <- ok[!duplicated(names(ok))]
#  #  ngram_inds_start <- rep(1, length(ngram_li))
#  #  ngram_inds_end <- rep(1, length(ngram_li))
#  #for(i in 1:length(ngram_li)){
#  #  ngram_inds_start[i] <- ok[[ngram_li[i]]][1]
#  #  ngram_inds_end[i] <- ngram_inds_start[i] + (sapply(ngram_toks[i], length)) - 1
#  #  ok[[ngram_li[i]]] <- ok[[ngram_li[i]]][-1]
#  #  scores[gram_ind %in% (ngram_inds_start[i]:ngram_inds_end[i])] <- 0
#  #}
#  #all_neg_ind <- c((which(grams %in% unigrams)-1), (ngram_inds_start - 1))
##} else {
##  all_neg_ind <- which(grams %in% unigrams) - 1
##}
##ngram_inds_all <- ngram_inds_sent:(ngram_inds_sent+(length(ngram_toks[[i]])-1))
##all_neg_ind[all_neg_ind == 0] <- NA
#  if(length(ngram_li) > 0){
#    ok <- lapply(ngram_toks, FUN = id_ngram_loc, sent_toks = sent)
#    names(ok) <- ngram_li
#    ok <- ok[!duplicated(names(ok))]
#    ngram_inds_start <- rep(1, length(ngram_li))
#    ngram_inds_end <- rep(1, length(ngram_li))
#    for(i in 1:length(ngram_li)){
#      ngram_inds_start[i] <- ok[[ngram_li[i]]][1]
#      ngram_inds_end[i] <- ngram_inds_start[i] + (sapply(ngram_toks[i], length)) - 1
#      ok[[ngram_li[i]]] <- ok[[ngram_li[i]]][-1]
#      scores[gram_ind %in% (ngram_inds_start[i]:ngram_inds_end[i])] <- 0
#    }
#    if(length(ngram_li) > 1){
#      for(i in 1:length(ngram_li)){
#        ngram_locats <- mapply(':', ngram_inds_start[-i], ngram_inds_end[-i])
#        self_ind <- ngram_inds_start[i]:ngram_inds_end[i]
#        if(any(unlist(lapply(ngram_locats, function(x) setequal(intersect(self_ind, x), self_ind)))) == TRUE){
#          scores[gram_ind %in% ngram_inds[i]] <- 0
#        }
#      }
#    }
#    all_neg_ind <- c((which(grams %in% unigrams) - 1), (ngram_inds_start - 1))
#  } else {
#    all_neg_ind <- which(grams %in% unigrams) - 1
# }
#  all_neg_ind[all_neg_ind == 0] <- NA
#  #if(length(ngram_li) > 0){
#  #ngram_inds_sent <- unlist(lapply(ngram_toks, FUN = id_ngram_loc, sent_toks = sent))
#  #ngram_neg_inds_sent <- ngram_inds_sent - 1
#  #all_neg_ind <- c((which(grams %in% unigrams)-1), (ngram_inds_sent - 1))} else {all_neg_ind <- which(grams %in% unigrams)-1}#

#  scores <- ifelse(scores == 0, 0,
#                   ifelse(gram_ind == 1 | is.na(all_neg_ind) ==TRUE , 1,
#                          ifelse(grams[all_neg_ind] %in% neg == TRUE, -1, 1)))
#  
#  return(scores)
#  }



#' Identify n-Gram Location in Text
#'
#' @description
#' A helper function to find the starting location(s) of n-grams in text document for the purpose of switch negation for n-grams with n > 1.
#' 
#' @param toks A list of n-gram vectors, with length equal to the number of n-grams with n > 1
#' @param sent_toks A vector of the unigrams in the text document
#' @returns The starting location(s) of n-grams in the text document for the purpose of identifying the preceding token for negation identification.
#' @importFrom zoo rollapply
#' @export
id_ngram_loc <- function(toks, sent_toks){
  which(rollapply(data = sent_toks, width = length(toks), FUN = identical, y = toks) == TRUE)

}




#' Individual Lexicon Sentiment Analysis: Document Score
#'
#' @description
#' Obtain the text document-level sentiment score according to a chosen scoring method and individual lexicon. 
#' 
#' @param x A text document
#' @param lexicon A text string of the lexicon name, ie "syuzhet"
#' @param lex A lexicon \code{\link[base]{data.frame}} with columns "word" and "value.
#' @param n A value indicating the maximum length of n-grams for lexicon
#' @param scoring A character string indicating the type of scoring function to use (absolute = "abs", relative = "rel", subtraction = "sub"). Defaults to \code{scoring = "abs"}
#' @returns A value indicating the aggregated sentiment score for a text document based on the identified lexicon and scoring method.
#' @export
lex_grams <- function(x, lexicon, lex, n, scoring = "abs"){
  grams <- get_grams_ems(x, n, lexicon = lexicon, lex = lex)
  if(length(grams) == 0){
    tot_score <- 0
    tot_words <- NULL
    wordy <- NULL
  } else {
  tot_words <- length(grams[sapply(strsplit(grams, " "), length) == 1])
  #tot_words <- length(get_grams_ems(x, 1, lexicon = lexicon, lex = lex))
  wordy <- grams[which(grams %in% lex$word)]}
  ## Special Cases
  # 1. If unigram in negators, set unigram to score to 0 and treat as negator
  # 2. If unigram parts of n-gram are sentiment-annotated, only count n-gram and set unigrams to 0 (avoid double counting)
  # Only consider is n-grams are present
  # 3. If subset of n-gram part of larger n-gram, set smaller n-gram to 0 and only count larger n-gram.
  # Only consider if length(ngrams) > 1
  # 4. If negator is a unigram in larger n-gram, treat as part of n-gram, not negator
  # Only consider if length(ngrams) > 1
  
  if(length(wordy) == 0){
    tot_score <- 0
  } else {
    signs <- negates(grams = grams, word_li = wordy, tot_words = tot_words)
    if(scoring == "abs"){
      tot_score <- sum(lex$value[match(wordy[wordy %in% lex$word], lex$word)]*signs)}
    else if (scoring == "rel") {tot_score <- sum(lex$value[match(wordy[wordy %in% lex$word], lex$word)]*signs)/tot_words
    } else if (scoring == "sub"){
      int_score <- sign(lex$value[match(wordy[wordy %in% lex$word], lex$word)]*signs)
      tot_score <- (length(which(int_score == 1)) - length(which(int_score == -1)))/tot_words
    } #else if (scoring == "rat"){
    #int_score <- sign(lex$value[match(wordy[wordy %in% lex$word], lex$word)]*signs)
    #tot_score <- length(which(int_score == 1))/(1+length(which(int_score == -1)))
    #}
  }
  return(tot_score)}


## get_all_sents(dat, lexicon, scoring = "abs")

#' Individual Lexicon Sentiment Analysis: Dataset Scores
#' 
#' @description
#' Compute Sentiment Analysis scores for all text documents in a dataset based on a chosen individual lexicon and scoring method.
#' 
#'
#' @param dat A \code{\link[base]{data.frame}} with column named "text" to perform sentiment analysis on
#' @param lexicon A text string of the lexicon name, ie "syuzhet"
#' @param lex A lexicon \code{\link[base]{data.frame}} containing two columns, "word" and "value"
#' @param scoring A character string indicating the type of scoring function to use (absolute = "abs", relative = "rel", subtraction = "sub"). Defaults to \code{scoring = "abs"}
#' @returns A \code{\link[base]{vector}} containing all sentiment scores for text documents in a dataset for the provided lexicon.
#' @export
get_all_sents <- function(dat, lexicon, lex, scoring = "abs"){
  ok <- sapply(dat, lex_grams, lexicon = lexicon, lex = lex, n = max(sapply(strsplit(lex$word, " "), length)), scoring = scoring, USE.NAMES = FALSE)
  return(ok)
}


#' Majority Voting Sentiment Analysis Scores
#'
#' @description
#' A helper function to compute majority voting-based sentiment scores. The assigned sentiment is based on the most frequently assigned sentiment (-1, 0, 1) from an ensemble of lexicons (the WoC crowd).
#' 
#' @param x a vector of individual lexicon sentiment scores
#' @returns The majority vote-based sentiment score for a text document based on a vector of individual lexicon sentiment scores
#' @export

majvote <- function(x){
  if(any(tabulate(match(x, unique(x))) > 1)){
    out <- unique(x)[which(tabulate(match(x, unique(x))) == max(tabulate(match(x, unique(x)))))]
    outp = sample(out,1)
  } else {
    outp <- 0}
  return(outp)
}



#' WoC Lexicon Sentiment Analysis: Dataset Scores
#' 
#' @description
#' Perform WoC Sentiment Analysis using the default WoC lexicon crowd or a custom lexicon crowd and obtain sentiment polarity scores. Individual lexicon sentiment intensity scores are also returned. Majority Voting ensemble sentiment polarity scores and WoC sentiment intensity scores can also be computed and returned. Scores are computed based on the chosen scoring method. 
#' 
#' To use parallel processing, a cluster must be initialized. If no cluster is intialized, analysis will be performed sequentially instead of in parallel.
#' 
#' @param dat A \code{\link[base]{data.frame}} with a column named "text" containing text documents to perform sentiment analysis on or a \code{\link[base]{vector}} containing text documents. If a vector is provided it is coerced to a dataframe.
#' @param crowdnames A named vector object containing character strings of names of lexicons in WoC crowd. Defaults to \code{WoC_Crowd}, where \code{WoC_Crowd =  c("afinn", "bing", "inquirer", "loughran", "nrc", "senticnet", "sentiwn", "slang", "syuzhet", "textblob", "vader", "wkwsci", "socal")}
#' @param scoring A character string indicating the type of scoring function to use for individual lexicon and WoC Sentiment Analysis. Defaults to \code{scoring = "abs"}. Scoring can be:
#' 
#' \itemize{
#' \item \code{"abs"}: The document-level sentiment score is the summation of n-gram sentiment scores for the observation.
#' \item \code{"rel"}: The document-level sentiment score is the summation of n-gram sentiment scores divided by the number of unigrams in the observation. 
#' \item \code{"sub"}: The document-level sentiment score is computed as the difference between the number of negative and positive sentiment n-grams divided by the number of unigrams in the observation.
#' 
#' }
#' @param majority A logical value. If \code{TRUE}, sentiment scores based on a Majority Vote ensemble method sentiment will be returned in a column named 'maj_vote'. Defaults to \code{majority = FALSE}. 
#' @param intensity A logical value. If \code{TRUE}, WoC sentiment intensity scores are returned in a column named "WoC_intensity". Otherwise, only polarity scores are returned (in a column named "woc"). Defaults to \code{intensity = FALSE}. 
#' @param all_lex A logical value. If \code{TRUE}, sentiment intensity scores are returned for all individual lexicons in the crowd. Defaults to \code{all_lex = FALSE}. 
#' @returns A \code{\link[base]{vector}} of \code{nrow(dat)} length containing WoC sentiment analysis scores. If \code{all_lex = TRUE}, a \code{\link[base]{data.frame}} is returned, which contains one column for each lexicon in the crowd and one "woc" column. If \code{intensity = TRUE}, a column named 'woc_intensity' is also included in the dataframe output. If \code{majority = TRUE}, a column named 'maj_vote' is also included in the dataframe output.
#' @importFrom foreach foreach %dopar%
#' @importFrom zoo rollapply
#' @importFrom NLP words ngrams
#' @export
#' @examples
#' \dontrun{
#' library(parallel)
#' library(doParallel)
#' library(sentimentr)
#' cores <- detectCores() - 1
#' cl <- makeCluster(cores)
#' registerDoParallel(cl)
#' ce <- course_evaluations[course_evaluations$sentiment != 0,]
#' cesc <- score_woc(dat = ce)
#' }
#' 
score_woc <- function(dat, crowdnames = WoC_Crowd, scoring = "abs", majority = FALSE, intensity = FALSE, all_lex = FALSE){
  if(any(!crowdnames %in% WoC_Crowd) & any(sapply(crowdnames[!crowdnames %in% WoC_Crowd], exists) == FALSE)){
    stop("All custom lexicons included in the crowd must exist in your global environment.")
  }
  crowd <- list()
  for(i in 1:length(crowdnames)){
   crowd[[i]] <- get(crowdnames[i])
  }
  names(crowd) <- crowdnames
  if(is.data.frame(dat)){
  if("text" %in% names(dat) == FALSE || is.character(dat[,"text"]) == FALSE) {
    stop("Dataframe data provided in dat argument must have a column named 'text' containing character data.")
  }
  }
  if(is.vector(dat)){
    dat <- cbind(doc_id = 1:length(dat), text = dat)
  }
  scores <- foreach(i = 1:length(crowdnames), .combine = cbind, 
                    .packages = c("NLP", "zoo", "WoCSentiment", "tm")) %dopar% {
                      WoCSentiment::get_all_sents(dat[,"text"], lexicon = crowdnames[i], lex = crowd[[i]], scoring = scoring)
                    }
  scores <- as.data.frame(scores)
  colnames(scores) <- crowdnames
  if(majority == TRUE){
  scores$maj_vote <- apply(sign(scores[1:length(crowd)]), 1, FUN = majvote)
  }
  scores$woc <- sign(rowMeans(scores[1:length(crowd)]))
  if(intensity == TRUE){
  scores$woc_intensity <- rowMeans(scores[1:length(crowd)])}
  if(all_lex == FALSE){
    scores <- scores[,!names(scores) %in% crowdnames]
  }
  return(scores)}



#' Obtain WoC Sentiment Analysis Performance
#' 
#' @description
#' Obtain Accuracy, Sensitivity, and Specificity for individual lexicons in the WoC Crowd, Majority Voting Ensemble, and WoC Sentiment Analysis. 
#'
#' @param scores A \code{\link[base]{vector}} or \code{\link[base]{data.frame}} output of \code{scores_woc_pl} function
#' @param truesent \code{\link[base]{vector}} of known (binary) sentiment labels, where -1 = negative, 1 = positive.
#' @returns A \code{\link[base]{data.frame}} containing 3 rows ("Accuracy", "Sensitivity", and "Specificity") and \code{nrow(scores)} rows.
#' @importFrom caret confusionMatrix
#' @importFrom sentimentr validate_sentiment
#' @export
perf <- function(scores, truesent){
  if(is.vector(scores)== FALSE){
  scores <- scores[,!names(scores) %in% "woc_intensity"]}
  signmat <- sign(scores)
  if(is.vector(signmat) == TRUE){
    ok <- validate_sentiment(signmat, truesent)
    conf <- t(attributes(ok)$confusion_matrix)
    cm <- confusionMatrix(conf, positive = "1")
    #acc <- cm$overall[1]
    #spec <- cm$byClass[1,1]
    #sens <- cm$byClass[3,1]
    woc <- c(accuracy = cm$overall[1], sensitivity = cm$byClass[3,1], specificity = cm$byClass[1,1])
    perform <- as.data.frame(woc, row.names = c("Accuracy", "Sensitivity", "Specificity"))
  } else{
    conttabs <- list()
  for (i in 1:length(signmat)){
    ok <- validate_sentiment(signmat[,i], truesent)
    conf <- t(attributes(ok)$confusion_matrix)
    cm <- confusionMatrix(conf, positive = "1")
    #acc <- cm$overall[1]
    #spec <- cm$byClass[1,1]
    #sens <- cm$byClass[3,1]
    conttabs[[i]] <- c(accuracy = cm$overall[1], sensitivity = cm$byClass[3,1], specificity = cm$byClass[1,1])
    names(conttabs)[[i]] <- colnames(signmat)[i]
  }
    perform <- as.data.frame(conttabs, row.names = c("Accuracy", "Sensitivity", "Specificity"))}
  return(perform)
}
