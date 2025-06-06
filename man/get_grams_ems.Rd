% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WOC_Functions_FINAL.R
\name{get_grams_ems}
\alias{get_grams_ems}
\title{Prepare Text Data for WoC Sentiment Analysis}
\usage{
get_grams_ems(dat, n, lexicon, lex, emot = lexicon::hash_emoticons)
}
\arguments{
\item{dat}{A text string}

\item{n}{The maximum length of n-grams to create}

\item{lexicon}{A text string of the lexicon name, ie "syuzhet"}

\item{lex}{A lexicon \code{\link[base]{data.frame}} containing two columns, "word" and "value"}

\item{emot}{A \code{\link[base]{data.frame}} containing two columns, the first column containing emoticons and the second column containing the meanings of the emoticons in words. Defaults to \pkg{lexicon} \code{\link[lexicon]{hash_emoticons}}.}
}
\value{
A \code{\link[base]{vector}} of n-grams
}
\description{
Preprocess and create n-grams for WoC Sentiment Analysis according to lexicon-specific requirements, including: maximum n-grams, emoticons, and retained punctuation.
}
