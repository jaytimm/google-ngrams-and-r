Google n-gram data & R: some methods
------------------------------------

An R-based guide to accessing/sampling Google n-gram data & building historical term-feature matrices for investigating lexical semantic change historically.

-   [1 Download-Sample-Aggregate](#1-Download-Sample-Aggregate)
-   [2 Restructuring corpus](#2-Restructuring-corpus)
-   [3 Building historical term-feature matrices](#3-Building-historical-term-feature-matrices)
-   [4 Building a lemma-ish lexicon](#4-Building-a-lemma-ish-lexicon)
-   [5 Lemmatizing terms and features](#5-Lemmatizing-words-and-features)
-   [6 Filtering features based on frequency](#6-Filtering-features-based-on-frequency)
-   [7 PPMI and SVD](#7-PPMI-and-SVD)
-   [8 Exploring synonymny historically](#8-Exploring-synonymny-historically)
-   [9 Summary](#9-Summary)

This guide focuses on working with Google n-gram data locally. So, lots of sampling & intermediary file structures. A smarter aproach to working with n-gram data in its entriety would be to build a SQL database. Here, we just want to steal some n-gram data to demonstrate a few methods & take a peak into some changes in word distributions historically.

**Endgame:** Finding historical synonyms (-ish). The table below summarizes nearest neighbors for the word *GRASP* over the last 200 years (by quarter century).

| quarter       | syn                                                                                                                                                                           |
|:--------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| \[1808,1833)  | legionary (0.66), snatch (0.44), wield (0.44), hilt (0.44), sword (0.43), rigidly (0.43), herculean (0.43), hand (0.43), unsheathe (0.42), spoiler (0.41)                     |
| \[1833,1858)  | omnivorous (0.73), nerveless (0.55), statesmanlike (0.54), wrench (0.45), hand (0.44), weapon (0.44), snatch (0.43), legionary (0.4), sceptre (0.39), wrest (0.39)            |
| \[1858,1883)  | clutch (0.52), hand (0.51), snatch (0.5), unclasp (0.5), tighten (0.49), holster (0.47), nerveless (0.47), soothingly (0.44), shifty (0.44), hilt (0.44)                      |
| \[1883,1908)  | penknife (0.58), throttle (0.55), tentacle (0.51), legionary (0.5), nerveless (0.49), wrest (0.48), snatch (0.47), unclasp (0.47), hand (0.46), grip (0.45)                   |
| \[1908,1933)  | understand (0.59), comprehend (0.56), realize (0.55), perceive (0.5), appreciate (0.49), visualize (0.47), repaint (0.45), masterful (0.45), coworker (0.44), rower (0.44)    |
| \[1933,1958)  | comprehend (0.6), understand (0.6), realize (0.55), westerner (0.54), appreciate (0.52), perceive (0.52), recognize (0.5), extricate (0.48), discern (0.48), verbalize (0.48) |
| \[1958,1983)  | comprehend (0.67), understand (0.64), decode (0.51), appreciate (0.51), cope (0.5), verbalize (0.48), lightweight (0.46), discern (0.46), foresee (0.46), westerner (0.45)    |
| \[1983,2008\] | understand (0.6), comprehend (0.58), grip (0.55), enshroud (0.49), trainee (0.49), parse (0.49), reuse (0.49), disassociate (0.49), perceive (0.48), cope (0.48)              |

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

The **resulting data structure** is a list of data frames, with each data frame representing a sub-corpus as a bag-of-words (with frequencies aggregated by ngram constituents and generation). A sample portion of this structure is presented below.

| ngram       | quarter       |  freq|   id|
|:------------|:--------------|-----:|----:|
| DESIRE      | \[1983,2008\] |    12|    1|
| KIND        | \[1983,2008\] |    12|    1|
| PROPOSAL    | \[1883,1908)  |     2|    2|
| STRENUOUSLY | \[1883,1908)  |     2|    2|
| OPPOSED     | \[1883,1908)  |     2|    2|
| NEW         | \[1983,2008\] |     1|    3|

The next step is to convert our list of randomly assembled sub-corpora into a list of generation-based sub-corpora. So, we first collapse our list of sub-corpora into a single corpus, and uniquely identify each 5-gram. ~3.2 Gb.

``` r
grams <- grams %>% data.table::rbindlist(idcol = 'corp') 
setkey(grams, corp, id)
grams[ , id := .GRP, by = key(grams)]
grams[, corp := NULL]  #n = 120,920,432, 3.2Gb
```

Then we re-split the corpus into eight sub-corpora, one for each quarter.

``` r
#Split corpus by generation
setorder(grams, quarter, id)
grams <- split(grams, f = grams$quarter) 
grams <- lapply(grams, select, -quarter) #1.8 Gb
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
  #Some freely available lexical resources. As post.
  filter(!is.na(POS)) %>%
  rename(form = Word, pos = POS) %>%
  mutate(form = toupper(form)) %>%
  select(form, pos) %>%
  filter(!grepl("-|'", form)) %>%
  left_join(lemma_lexicon)%>%
  mutate(lemma = ifelse(is.na(lemma), form, lemma))
```

Our lexicon, then, contains ~39k forms. A sample of the lexicon is presented below:

| form        | pos   | lemma       |
|:------------|:------|:------------|
| ABANDON     | VB|NN | ABANDON     |
| ABANDONED   | VB|JJ | ABANDON     |
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

names(tfms_mats) <- names(tfms)
```

`neighbors` function from the `LSAfun` package.

``` r
lapply(tfms_mats, LSAfun::neighbors, x = toupper('awful'), n = 10)
```

    ## $`[1808,1833)`
    ##     AWFUL    SQUINT STILLNESS    SOLEMN   SILENCE   HANCOCK  SOLITUDE 
    ## 1.0000000 0.6138022 0.4913693 0.4875673 0.4516866 0.4511762 0.4432864 
    ## SOLEMNITY  GRANDEUR REVERENCE 
    ## 0.4326415 0.4029595 0.3956272 
    ## 
    ## $`[1833,1858)`
    ##          AWFUL  UNIMPEACHABLE        SUBLIME    UNFAILINGLY ACCOUNTABILITY 
    ##      1.0000000      0.8234118      0.5248169      0.4980198      0.4648382 
    ##     MYSTERIOUS        SILENCE        MYSTERY          APPAL       TERRIBLE 
    ##      0.4538010      0.4512602      0.4296436      0.4204683      0.4047735 
    ## 
    ## $`[1858,1883)`
    ##      AWFUL  SOLEMNITY   DREADFUL    FEARFUL   TERRIBLE MYSTERIOUS 
    ##  1.0000000  0.5721841  0.5622825  0.5369559  0.4886308  0.4587954 
    ##       GONG    PAINFUL    OVERSEE   GRANDEUR 
    ##  0.4442043  0.4302043  0.4194969  0.4172082 
    ## 
    ## $`[1883,1908)`
    ##      AWFUL   GRANDEUR   TERRIBLE       WARN IMPRESSIVE      BRINK 
    ##  1.0000000  0.5275426  0.4692250  0.4446320  0.4205578  0.4133154 
    ## MYSTERIOUS      STERN    FEARFUL  SOLEMNITY 
    ##  0.4092081  0.4090976  0.4033459  0.3977852 
    ## 
    ## $`[1908,1933)`
    ##     AWFUL  GRANDEUR SOLEMNITY    SOLEMN   MYSTERY       SAD   FEARFUL 
    ## 1.0000000 0.4414021 0.4332082 0.4303716 0.4007711 0.3987826 0.3852825 
    ##  DREADFUL  TERRIBLE   WEIGHTY 
    ## 0.3751914 0.3577784 0.3552299 
    ## 
    ## $`[1933,1958)`
    ##       AWFUL CATASTROPHE    GRANDEUR  IMPRESSIVE      SOLEMN       APPAL 
    ##   1.0000000   0.4397561   0.4361084   0.4305086   0.4118108   0.4015999 
    ##  MYSTERIOUS     SUBLIME    CALAMITY   SPECTACLE 
    ##   0.3950894   0.3888246   0.3864190   0.3844930 
    ## 
    ## $`[1958,1983)`
    ##      AWFUL   TERRIBLE MYSTERIOUS   FASTENER       DIRE    SUBLIME 
    ##  1.0000000  0.4276057  0.4121773  0.3761976  0.3679445  0.3644093 
    ##   CALAMITY    SILENCE      CRUEL    SECRECY 
    ##  0.3635237  0.3501145  0.3428076  0.3410534 
    ## 
    ## $`[1983,2008]`
    ##     AWFUL CATACLYSM   CARNAGE  DREADFUL SOLEMNITY      POMP  GRANDEUR 
    ## 1.0000000 0.8497796 0.5876992 0.5775361 0.5227230 0.5114711 0.5091410 
    ##  TERRIBLE BURLESQUE  CALAMITY 
    ## 0.4900803 0.4864659 0.4747446

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

| quarter       | syn                                                                                                                                                 |
|:--------------|:----------------------------------------------------------------------------------------------------------------------------------------------------|
| \[1808,1833)  | inform (0.51), transmit (0.46), comply (0.41), convey (0.37), receive (0.35), diocesan (0.34), subserve (0.34), advertise (0.33), report (0.33)     |
| \[1833,1858)  | inform (0.52), impart (0.44), convey (0.43), transmit (0.42), receive (0.38), intelligence (0.34), rid (0.34), transmission (0.33), announce (0.33) |
| \[1858,1883)  | inform (0.47), respond (0.45), transmit (0.44), impart (0.42), convey (0.41), avail (0.4), discover (0.36), receive (0.36), introduce (0.36)        |
| \[1883,1908)  | inform (0.54), landing (0.52), impart (0.5), reassurance (0.45), transmit (0.41), consult (0.35), receive (0.35), rejoin (0.35), excellency (0.34)  |
| \[1908,1933)  | inform (0.53), jameson (0.46), request (0.44), transmit (0.39), address (0.39), message (0.37), send (0.37), accede (0.37), notify (0.36)           |
| \[1933,1958)  | convey (0.5), inform (0.44), respond (0.44), impart (0.39), verbalize (0.38), divert (0.36), transmit (0.35), perceive (0.35), send (0.34)          |
| \[1958,1983)  | verbalize (0.55), decode (0.53), inform (0.51), someday (0.49), discover (0.49), convey (0.47), identify (0.47), ferret (0.47), dislodge (0.46)     |
| \[1983,2008\] | enshroud (0.63), trainee (0.63), parse (0.63), reuse (0.63), decode (0.61), outflank (0.56), outwit (0.55), visualize (0.54), replicate (0.53)      |

------------------------------------------------------------------------

### 9 Summary
