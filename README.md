Google n-gram data & R: some methods
------------------------------------

An R-based guide to accessing/sampling Google n-gram data & building historical term-feature matrices for investigating lexical semantic change historically.

-   [1 Download-Sample-Aggregate](#1-Download-Sample-Aggregate)
-   [2 Restructuring corpus](#2-Restructuring-corpus)
-   [3 Building historical term-feature matrices](#3-Building-historical-term-feature-matrices)
-   [4 Condensing and filtering historical term-feature matrices](#4-Condensing-and-filtering-historical-term-feature-matrices)
-   [4a Building lemma lexicon](#4a-Building-lemma-lexicon)
-   [4b Lemmatizing terms and features](#4b-Lemmatizing-terms-and-features)
-   [4c Filtering features based on frequency](4c-Filtering-features-based-on-frequency)

-   [4 Building a lemma-ish lexicon](#4-Building-a-lemma-ish-lexicon)
-   [5 Lemmatizing terms and features](#5-Lemmatizing-words-and-features)
-   [6 Filtering features based on frequency](#6-Filtering-features-based-on-frequency)
-   [7 PPMI and SVD](#7-PPMI-and-SVD)
-   [8 Exploring synonymny historically](#8-Exploring-synonymny-historically)
-   [9 Summary](#9-Summary)

This guide focuses on working with Google n-gram data locally. So, lots of sampling & intermediary file structures. A smarter aproach to working with n-gram data in its entriety would be to build a SQL database. Here, we just want to steal some n-gram data to demonstrate a few methods & take a peak into some changes in word distributions historically.

Google n-gram data are a bit weird as a text structure. As such, many existing text-analytic R packages/functions (that often assume raw text as a starting point) are not especially helpful here. So, we have to hack-about some to get from Google n-gram data to historical term-feature matrices.

**ENDGAME:** Finding historical synonyms (-ish). The tables below summarize nearest neighbors for the word *GRASP* over the last 200 years (by quarter century), including cosine-based similarities (value) & term frequencies in parts per million (ppm).

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

------------------------------------------------------------------------

### 1 Download-Sample-Aggregate

Google has a host of corpora -- here we work with the corpus dubbed the **English One Million** corpus. The corpus is comprised of texts published from the 16th century to the start of the 21st, and includes over 100 billion words. **The 5-gram corpus** is comprised of ~800 files (or sub-corpora). File composition for this corpus version is not structured alpabetically or chronologically. Instead, it seems fairly arbitrary.

``` r
library(tidyverse)
library(data.table)
```

To start the sampling process, we build two simple functions. The **first function** downloads & unzips a single file of the corpus to a temporary folder.

``` r
get_zip_csv <- function (url) {

  temp <- tempdir()
  zip_name <- paste0(temp, '\\', basename(url))
  download.file(url, zip_name, 
                quiet = TRUE)
  unzip(zip_name, exdir = temp)
  out <- data.table::fread(gsub('\\.zip', '', zip_name), 
                           blank.lines.skip = TRUE, 
                           quote="", 
                           encoding = 'UTF-8')
  unlink(temp) 
  out}
```

A **random portion** of the first file of the 5-gram corpus is presented below:

-   V1 = 5-gram
-   V2 = Date of publication
-   V3 = token frequency of 5-gram in sub-corpus
-   V4 = page count of 5-gram in sub-corpus
-   V5 = volume (or text frequency) count of 5-gram in sub-corpus

``` r
url <- 'http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-1M-5gram-20090715-1.csv.zip'
unzipped_eg <- get_zip_csv(url)  #~11 million rows.
unzipped_eg %>% sample_n(5) %>% knitr::kable()
```

| V1                               |    V2|   V3|   V4|   V5|
|:---------------------------------|-----:|----:|----:|----:|
| "subjects . Indeed               |  1875|    1|    1|    1|
| in the States of his             |  1949|    1|    1|    1|
| he came back home .              |  1944|    2|    2|    2|
| demonstrated the accuracy of the |  1886|    4|    4|    4|
| time and space . We              |  1924|    7|    7|    6|

The **second function** performs a variety of tasks with the aim of sampling & aggregating the raw 5-gram files. Function parameters & details:

-   filter sub-corpus by dates of publication
-   sample sub-corpus
-   remove 5-ngrams with punctuation
-   create new time bins
-   aggregate 5-gram frequencies per new time bins
-   sample again

Sampling procedure could certainly be more systematic. Here, we are only interested in token frequencies.

``` r
sample_ngram <- function (x, 
                          start_date, end_date, 
                          generation, 
                          samp1, samp2) {
  
  x <- x[V2 >= start_date & V2 <= end_date ]
  set.seed(99)
  x <- x[sample(1:nrow(x), samp1, 
                replace=FALSE),] 
  x <- x[grepl("^[a-z ]+$", V1, ignore.case = TRUE)] 
  #Remove grams with punctuation
  x$V9 <- cut(x$V2, seq(start_date,end_date,generation), 
              right=FALSE,
              include.lowest = TRUE,
              dig.lab = 4) #Create new time bins
  x[, V1 := toupper(V1)]
  x <- x[, list(V3 = sum(V3)), by = list(V1, V9)] 
  #Aggregate freqs to new time bins
  setnames(x, 
           old = c('V1', 'V9', 'V3'), 
           new = c('five_gram', 'quarter', 'freq'))
  set.seed(99)
  x[sample(1:nrow(x), samp2,
                replace=FALSE),]
}
```

The table below presents a random portion of the sampled/aggregated output:

``` r
unzipped_eg %>%
  sample_ngram(start_date = 1808,
               end_date = 2008,
               generation = 25,
               samp1 = 5000000,
               samp2 = 200000) %>%
  sample_n(5) %>%
  knitr::kable()
```

| five\_gram                     | quarter      |  freq|
|:-------------------------------|:-------------|-----:|
| IN THE AGGREGATE TO FIVE       | \[1858,1883) |     7|
| BREAK ALL THE TEN COMMANDMENTS | \[1958,1983) |     4|
| THAT TIME HE WAS WELL          | \[1858,1883) |     3|
| OF HIS EXPERIMENTS AND THEIR   | \[1908,1933) |     3|
| AS SOON AS ORDER WAS           | \[1933,1958) |    16|

<br>

We then **apply functions** to all ~800 files/sub-corpora, and store the output locally. Depending on connection speed, this could take a while. A good processing rate would be 3/4 files per minute. Downoading/unzipping is the limiting part of the process. Total size of processed files is ~6.7 Gb.

``` r
file_names <- c(1:799)
setwd(local_raw)

for (i in 1:length(file_names)) {
  url <- paste0('http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-1M-5gram-20090715-', file_names[i], '.csv.zip')
  
  get_zip_csv(url) %>%
    sample_ngram(start_date = 1808,
                 end_date = 2008,
                 generation = 25,
                 samp1 = 5000000,
                 samp2 = 200000)%>%
    write.csv(., 
            gsub('(^.*googlebooks-)(.*)(\\.zip)', '\\2', url), 
            row.names = FALSE) 
  }
```

------------------------------------------------------------------------

### 2 Restructuring corpus

At this point, we have successfully stolen a very small portion of the 5-gram corpus derived from the 100+ billion word Google corpus. At ~6.7 Gb, it is still a bit big for use locally in R. With the goal of building n-gram-based co-occurence matrices, the next step is to restructure the 5-gram data some.

Per each file/sub-corpus generated above, here we:

-   sample 5-grams again
-   uniquely id 5-grams
-   flip 5-grams as character string to long format
-   remove stop words

Per the table above, the 5-gram "MURDERED THE TURNKEY ON FRIDAY" occurred 8 times during the quarter-century spanning 1858-1882 in the *first file* of the ngram corpus. The pipe below seperates each form in the ngram into five rows, assigns each row/form the frequency of the ngram (8), uniquely identifies the ngram in the sub-corpus, and removes rows in the ngram containing stopwords (here, "THE" and "ON"). The ID serves to preserve the ngram as a context of usage (or mini-text).

``` r
setwd(local_raw)
gfiles <- list.files(path=local_raw, 
                     pattern = ".csv", 
                     recursive=TRUE) 

grams <- lapply(1:length(gfiles), function (y)
  data.table::fread(gfiles[y])%>% 
    #filter(!quarter %in% c("[1808,1833)", "[1833,1858)")) %>%
    sample_n(75000) %>%
    rename(ngram = five_gram) %>%
    mutate(id = as.integer(row_number())) %>%
    separate_rows (ngram, sep = ' ') %>% #Make ngram long
    filter(!ngram %in% toupper(corpuslingr::clr_ref_stops))%>% #Remove stop words
    as.data.table()
)

names(grams) <- file_names  #Store locally.
```

The **resulting data structure** is a list of data frames, with each data frame representing a sub-corpus as a bag-of-words (with frequencies aggregated by ngram constituents and quarter-century). A sample portion of this structure is presented below.

| ngram       | quarter       |  freq|   id|
|:------------|:--------------|-----:|----:|
| DESIRE      | \[1983,2008\] |    12|    1|
| KIND        | \[1983,2008\] |    12|    1|
| PROPOSAL    | \[1883,1908)  |     2|    2|
| STRENUOUSLY | \[1883,1908)  |     2|    2|
| OPPOSED     | \[1883,1908)  |     2|    2|
| NEW         | \[1983,2008\] |     1|    3|

The next step is to convert our list of randomly assembled sub-corpora into a list of generation-based sub-corpora. So, we first collapse our list of sub-corpora into a single corpus, and uniquely identify each 5-gram.

``` r
grams <- grams %>% data.table::rbindlist(idcol = 'corp') 
setkey(grams, corp, id)
grams[ , id := .GRP, by = key(grams)]
grams[, corp := NULL]  #n = 120,920,432
```

Then we re-split the corpus into eight sub-corpora, one for each quarter-century.

``` r
setorder(grams, quarter, id)
grams <- split(grams, f = grams$quarter) 
grams <- lapply(grams, select, -quarter) 
```

------------------------------------------------------------------------

### 3 Building historical term-feature matrices

At this point, we are finished with the time- & memory-consumptive portion of the workflow. Next, we want to transform each of our sub-corpora into a term-feature matrix (TFM).

Treating each uniquely identified 5-gram as a "document," we first transform each sub-corpus into a Document-Term Matrix (DTM) using the `cast_sparse` function from the `tidytext` package. For our purposes here, this is an intermediary data structure. We then convert the DTM to a term-feature matrix using the `Dtm2Tcm` function from the `testmineR` package. This particular workflow is ideal when working with aggregated text structures as a starting point.

``` r
tfms <- lapply(1:8, function (y)
  
  grams[[y]] %>%
    tidytext::cast_sparse(id, 
                          ngram, 
                          freq) %>%
    textmineR::Dtm2Tcm() %>%
    .[, order(colnames(.))] %>%
    .[order(rownames(.)), ]
  
) #543.1Mb

names(tfms) <- names(grams)
```

A small portion of the TFM for the 1908-1932 sub-corpus is presented below. Full data structure is a list of TFMs by quarter.

``` r
library(Matrix)
tfms[[5]][1:10,1:15] 
```

    ## 10 x 15 sparse Matrix of class "dgCMatrix"
    ##                                             
    ## AA        153 . .  . .   . . . . . . . . . .
    ## AALAND      . 4 .  . .   . . . . . . . . . .
    ## AAN         . . 5  . .   . . . . . . . . . .
    ## AAR         . . . 31 .   . . . . . . . . . .
    ## AARNE       . . .  . 1   . . . . . . . . . .
    ## AARON       . . .  . . 822 . . . . . . . . .
    ## AARONIC     . . .  . .   . 1 . . . . . . . .
    ## AARONSOHN   . . .  . .   . . 1 . . . . . . .
    ## AASOR       . . .  . .   . . . 1 . . . . . .
    ## AAZAZ       . . .  . .   . . . . 1 . . . . .

------------------------------------------------------------------------

### 4 Building a lemma-ish lexicon

Some different approaches to condensing our matrices.

Data compiled by folks at the English Lexicon Project. As a bit of "supervision" -- some details about words included in the corpus. Filter out funky/poor OCR-ed words included in the corpus as well. Super-imperfect.

``` r
library(lexvarsdatr)
```

    ## Data included in this package were obtained from supplemental materials made available from these sources:
    ##  
    ## Kuperman, V., Stadthagen-Gonzalez, H., & Brysbaert, M. (2012). Age-of-acquisition ratings for 30,000 English words. Behavior Research Methods, 44(4), 978-990.
    ## 
    ## Balota, D. A., Yap, M. J., Hutchison, K. A., Cortese, M. J., Kessler, B., Loftis, B., ... & Treiman, R. (2007). The English lexicon project. Behavior research methods, 39(3), 445-459.
    ## 
    ## Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. Behavior research methods, 46(3), 904-911.
    ## 
    ## Nelson, D. L., McEvoy, C. L., & Schreiber, T. A. (2004). The University of South Florida free association, rhyme, and word fragment norms. Behavior Research Methods, Instruments, & Computers, 36(3), 402-407.
    ## 
    ## Baayen, R. H., Piepenbrock, R., & Gulikers, L. (1995). The CELEX lexical database [webcelex]. Philadelphia, PA: University of Pennsylvania, Linguistic Data Consortium.

An imperfect resource. Derived from the British National Corpus (BNC).

``` r
lemma_lexicon <- read.csv( url('https://raw.githubusercontent.com/skywind3000/lemma.en/master/lemma.en.txt'), 
                      header = FALSE, 
                      skip = 10, sep = '\t')%>%
  separate(V1, into = c('lemma', 'form'), sep = ' -> ') %>%
  mutate(lemma = toupper(gsub('/.*$', '', lemma)),
         form = toupper(form))%>%
  separate_rows (form, sep = ',') %>%
  filter(grepl("^[A-Z]+$", lemma)) %>%
  group_by(form) %>% slice(1) 
#some single forms are mapped to multiple lemmas -- n=136 
```

``` r
elp_lexicon_lem <- lexvarsdatr::lvdr_behav_data %>%  
  filter(!is.na(POS)) %>%
  rename(form = Word, pos = POS) %>%
  mutate(form = toupper(form)) %>%
  select(form, pos) %>%
  filter(!grepl("-|'", form)) %>%
  left_join(lemma_lexicon)%>%
  mutate(lemma = ifelse(is.na(lemma), form, lemma),
         pos = gsub('\\|','-', pos))
```

Our lexicon, then, contains ~39k forms. A sample of the lexicon is presented below:

| form        | pos   | lemma       |
|:------------|:------|:------------|
| ABANDON     | VB-NN | ABANDON     |
| ABANDONED   | VB-JJ | ABANDON     |
| ABANDONING  | VB    | ABANDON     |
| ABANDONMENT | NN    | ABANDONMENT |
| ABASE       | VB    | ABASE       |
| ABASEMENT   | NN    | ABASEMENT   |
| ABASH       | VB    | ABASH       |
| ABATE       | VB    | ABATE       |
| ABATED      | VB    | ABATE       |

------------------------------------------------------------------------

### 5 Lemmatizing terms and features

Feature/lexicon/FCM compression.

For good measure, we next demonstrate some methods for "lemmatizing" our historical FCMs. A lemma is properly defined as a word form/part-of-speech pair, and all of its inflectional variants.

Filter features of TFM to feature list created above. And re-order.

Lemmatize list of matrices. Perhaps lemma & filter as two steps.

``` r
library(Matrix.utils)

lemmatize_matrix <- function (x) {
  
  x <- x[,colnames(x) %in% elp_lexicon_lem$form]
  x <- x[rownames(x) %in% elp_lexicon_lem$form,]
  
  cs <- data.frame(form = colnames(x)) %>% 
    inner_join(elp_lexicon_lem %>% select(-pos))
  rs <- data.frame(form = rownames(x)) %>% 
    inner_join(elp_lexicon_lem %>% select(-pos)) 
  
  colnames(x) <- cs$lemma  
  rownames(x) <- rs$lemma
  
  y <- t(aggregate.Matrix(x, colnames(x), fun = 'sum')) 
  aggregate.Matrix(y, colnames(x), fun = 'sum')
}
```

Apply function.

``` r
tfms_lemmed <- lapply(tfms, lemmatize_matrix)
```

``` r
tfms_lemmed[[5]][1:10,1:20]
```

    ## 10 x 20 sparse Matrix of class "dgCMatrix"

    ##    [[ suppressing 20 column names 'AARON', 'ABACK', 'ABACUS' ... ]]

    ##                                                                       
    ## AARON       822   .  .     .    .  .  . .    .   . . . . . . . . . . .
    ## ABACK         . 829  .     .    .  .  . .    .   . . . . . . . . . . .
    ## ABACUS        .   . 21     .    .  .  . .    .   . . . . . . . . . . .
    ## ABANDON       .   .  . 15957    .  .  . .    .   . . . . . . . . . . .
    ## ABANDONMENT   .   .  .     . 3969  .  . .    .   . . . . . . . . . . .
    ## ABASE         .   .  .     .    . 50  . .    .   . . . . . . . . . . .
    ## ABASEMENT     .   .  .     .    .  . 39 .    .   . . . . . . . . . . .
    ## ABASH         .   .  .     .    .  .  . 1    .   . . . . . . . . . . .
    ## ABATE         .   .  .     .    .  .  . . 1062   . . . . . . . . . . .
    ## ABBESS        .   .  .     .    .  .  . .    . 110 . . . . . . . . . .

------------------------------------------------------------------------

### 6 Filtering features based on frequency

Reduce. But also a homogenization process.

While our matrices can have different lengths (ie, include different terms), we want them to be comprised of the same features. So that we can compare them historically.

Redo frequency -- diagonals.

``` r
freqs_by_gen <- lapply(1:8, function (x)
  data.frame(lemma = rownames(tfms_lemmed[[x]]), 
             freq = diag(tfms_lemmed[[x]]),
             quarter = rep(names(tfms_lemmed[x]), nrow(tfms_lemmed[[x]])),
             stringsAsFactors = FALSE) 
  ) %>%
  bind_rows() %>%
  group_by(quarter) %>%
  mutate(corpus = sum(freq)) %>%
  ungroup() %>%
  mutate(ppm = round(freq/corpus *1000000, 2))%>%
  select(-corpus)
```

Historical frequencies for a random sample of forms in the lexicon:

``` r
freqs_by_gen %>%
  select(-freq) %>%
  spread(quarter, ppm) %>%
  sample_n(5) %>%
  knitr::kable()
```

| lemma       |  \[1808,1833)|  \[1833,1858)|  \[1858,1883)|  \[1883,1908)|  \[1908,1933)|  \[1933,1958)|  \[1958,1983)|  \[1983,2008\]|
|:------------|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|--------------:|
| PSYCHIATRIC |          0.02|            NA|          0.01|          0.02|          0.48|          4.04|         13.73|          23.12|
| SLANG       |          0.28|          0.39|          0.69|          1.18|          0.59|          0.35|          0.51|           1.71|
| ENERVATION  |          0.08|          0.08|            NA|          0.05|          0.01|            NA|            NA|             NA|
| EXAMINATION |        138.76|        170.53|        196.98|        202.25|        214.38|        198.50|        219.39|         177.35|
| CALIBER     |          0.12|          0.35|          0.66|          1.00|          1.91|          1.93|          1.45|           1.51|

Filter features to forms that

The 50th to 5,049th most frequent lemmas.

``` r
terms <- freqs_by_gen %>%
  group_by(lemma) %>%
  mutate(quarter_count = length(quarter),
         ppm = median(ppm)) %>%
  filter (quarter == "[1983,2008]", quarter_count == 8)%>%
  arrange (desc(ppm)) %>%
  ungroup() %>%
  slice(50:5049) 
```

``` r
tfms_filtered <- lapply(1:8, function (x)
  tfms_lemmed[[x]][,colnames(tfms_lemmed[[x]]) %in% terms$lemma] )
```

------------------------------------------------------------------------

### 7 PPMI and SVD

Whether or not some or all of the compression steps presented above, ...

*Positive Pointwise Mutual Information* (PPMI)

The function below calculates PPMI values on sparse matrices, which has been slightly modified from an SO post available [here](https://stackoverflow.com/questions/43354479/how-to-efficiently-calculate-ppmi-on-a-sparse-matrix-in-r).

``` r
build_sparse_ppmi <- function (pmat) {
  
  tcmrs <- Matrix::rowSums(pmat)
  tcmcs <- Matrix::colSums(pmat)

  N <- sum(tcmrs)
  colp <- tcmcs/N
  rowp <- tcmrs/N

  pp <- pmat@p+1
  ip <- pmat@i+1

  tmpx <- rep(0,length(pmat@x))

  for(i in 1:(length(pmat@p)-1) ){ 
    ind <- pp[i]:(pp[i+1]-1)
    not0 <- ip[ind]
    icol <- pmat@x[ind]
    tmp <- log( (icol/N) / (rowp[not0] * colp[i] )) #PMI
    tmpx[ind] <- tmp    
  }

  pmat@x <- tmpx
  pmat@x[pmat@x < 0] <- 0 ##PPMI
  drop0(pmat, tol=0) }
```

Apply function to the list of quarter-century sparse matrices. Here we use the lemmatized/feature-filtered list, although rawer renditions can be used as well.

``` r
tfms_ppmi <- lapply(tfms_filtered, build_sparse_ppmi)
```

*Singular value decomposition*

``` r
tfms_svd <- lapply(tfms_ppmi, irlba::irlba, nv = 200) 
```

------------------------------------------------------------------------

### 8 Exploring synonymny historically

Simple matrix.

``` r
tfms_mats <- list()

for (i in 1:8) {
  x <- as.matrix(data.matrix(tfms_svd[[i]]$u))
  dimnames(x) <- list(rownames(tfms_ppmi[[i]]), c(1:length(tfms_svd[[i]]$d)))
  tfms_mats[[i]] <- x
}
```

`neighbors` function from the `LSAfun` package.

``` r
lapply(tfms_mats, LSAfun::neighbors, x = toupper('awful'), n = 10)
```

    ## $`[1808,1833)`
    ##     AWFUL    SQUINT STILLNESS BURLESQUE  JEOPARDY    SOLEMN   HANCOCK 
    ## 1.0000000 0.7020868 0.5028618 0.4839309 0.4702018 0.4563006 0.4284432 
    ##  GRANDEUR     HAVOC  SOLITUDE 
    ## 0.4171006 0.3955748 0.3864050 
    ## 
    ## $`[1833,1858)`
    ##          AWFUL  UNIMPEACHABLE    UNFAILINGLY ACCOUNTABILITY           HUSH 
    ##      1.0000000      0.9226495      0.6078013      0.5852461      0.4575165 
    ##       TERRIBLE        SUBLIME        SCOURGE     MYSTERIOUS      STILLNESS 
    ##      0.4396760      0.4345747      0.4301781      0.4220981      0.3972905 
    ## 
    ## $`[1858,1883)`
    ##        AWFUL         GONG      FEARFUL    SOLEMNITY     DREADFUL 
    ##    1.0000000    0.5778842    0.5266688    0.4900255    0.4737300 
    ##     TERRIBLE   PLEASINGLY        DREAD    AVALANCHE PRESENTIMENT 
    ##    0.4676711    0.4619683    0.4583404    0.4435379    0.4400827 
    ## 
    ## $`[1883,1908)`
    ##       AWFUL    TERRIBLE    GRANDEUR        WARN   EMERGENCY      CRISIS 
    ##   1.0000000   0.4907441   0.4695427   0.4525932   0.4330937   0.4208578 
    ##    FASTENER  IMPRESSIVE      ENIGMA CATASTROPHE 
    ##   0.4145791   0.4131937   0.4087609   0.4072729 
    ## 
    ## $`[1908,1933)`
    ##       AWFUL      SOLEMN      UNVEIL   SOLEMNITY      ENIGMA         SAD 
    ##   1.0000000   0.5052558   0.4615295   0.4551648   0.4539749   0.4229689 
    ## CATASTROPHE     WARNING    TERRIBLE        FUSS 
    ##   0.4101368   0.4056449   0.4014751   0.3954055 
    ## 
    ## $`[1933,1958)`
    ##       AWFUL  IMPRESSIVE CATASTROPHE       APPAL    TERRIBLE      SOLEMN 
    ##   1.0000000   0.4828261   0.4747451   0.4509247   0.4337449   0.4310660 
    ##        HUSH     SUBLIME   MORTICIAN        FUSS 
    ##   0.4237701   0.4219196   0.4056325   0.4045679 
    ## 
    ## $`[1958,1983)`
    ##       AWFUL    TERRIBLE  MYSTERIOUS        DIRE     SUBLIME    FASTENER 
    ##   1.0000000   0.4792760   0.4356997   0.4300309   0.4273717   0.4116683 
    ## CATASTROPHE     SECRECY        WARN       AWAIT 
    ##   0.3892807   0.3792135   0.3753816   0.3736390 
    ## 
    ## $`[1983,2008]`
    ##       AWFUL   CATACLYSM     CARNAGE    DREADFUL    TERRIBLE   BURLESQUE 
    ##   1.0000000   0.9214979   0.6547534   0.6330563   0.5387366   0.5235076 
    ##   SOLEMNITY    STIRRING    GRANDEUR CATASTROPHE 
    ##   0.5221464   0.5009578   0.4978548   0.4856450

Clean output.

``` r
strip_syns <- function (x) {
  lapply(1:length(x), function(y)  
    x[[y]] %>%
    as.tibble %>% 
    rownames_to_column() %>%
    mutate(syn = paste0(tolower(rowname), ' (', round(value,2),')'),
         quarter = names(x[y]))%>%
    select(quarter, syn) %>%
    slice(-1) %>%
    group_by(quarter) %>%
    summarize(syn = paste0(syn, collapse = ', '))) %>%
    bind_rows() }
```

``` r
lapply(tfms_mats, LSAfun::neighbors, x = toupper('communicate'), n = 10) %>%
  strip_syns() %>%
  knitr::kable()
```

| quarter       | syn                                                                                                                                                   |
|:--------------|:------------------------------------------------------------------------------------------------------------------------------------------------------|
| \[1808,1833)  | inform (0.52), transmit (0.45), comply (0.39), obligingly (0.37), jameson (0.36), officially (0.36), pungently (0.36), diocesan (0.33), avail (0.33)  |
| \[1833,1858)  | inform (0.54), transmit (0.48), convey (0.43), assure (0.43), notify (0.39), impart (0.39), lordship (0.37), request (0.37), officially (0.36)        |
| \[1858,1883)  | inform (0.48), impart (0.45), respond (0.41), convey (0.4), notify (0.39), introduce (0.37), transmit (0.35), avail (0.35), transmission (0.34)       |
| \[1883,1908)  | landing (0.6), inform (0.6), reassurance (0.54), impart (0.44), transmit (0.44), announce (0.39), comply (0.38), notify (0.37), assure (0.36)         |
| \[1908,1933)  | inform (0.61), transmit (0.44), request (0.44), convey (0.42), avail (0.4), accede (0.39), send (0.38), comply (0.38), notify (0.37)                  |
| \[1933,1958)  | inform (0.52), convey (0.5), cesspool (0.48), respond (0.47), recognize (0.4), avail (0.39), comply (0.39), outmatch (0.38), verbalize (0.38)         |
| \[1958,1983)  | inform (0.57), verbalize (0.52), decode (0.51), convey (0.49), someday (0.47), familiarize (0.46), manipulate (0.46), identify (0.46), respond (0.45) |
| \[1983,2008\] | reuse (0.59), enshroud (0.59), inform (0.55), outflank (0.55), convey (0.5), decode (0.5), discern (0.5), respond (0.49), capitalize (0.49)           |

------------------------------------------------------------------------

### 9 Summary

While academic linguists are often critical of Google n-gram data, it is still an incredible cultural resource.

Many of the methodological decisions made here can certainly be tweaked to improve results.
