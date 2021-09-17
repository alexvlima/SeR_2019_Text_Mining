# Text Mining

This repository has the analysis that was did for 3 politicians from Rio de Janeiro using their tweets. 

The paper is published and can be downloaded at this url: <https://periodicos.uff.br/anaisdoser/article/view/29333>.

That was the following steps for the text analysis: 

## Twitter

Using the social network API (package twitteR), get tweets from users. 

## Manipulate data 
  
  1) To lower
  2) Tokenization
  3) Remove punctuation
  4) Remove stopwords
  5) Stem (to join with the sentimental lexicon)
  
## Sentimental Analysis
  
  Using sentiLex_lem_PT02 dictionary
  
## TF-IDF
  
  Identify importants terms to each poltician. 
  This technique consider the term frequency and the inverse document frequency. 
  
## Topic Modelling
 
  We used the LDA (Latent Dirichlet Allocation) algorithm to build a model to predict each tweet and classify them into a group. 
