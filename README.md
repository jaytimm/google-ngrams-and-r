Google n-gram data & R: some methods
------------------------------------

An R-based guide to accessing & sampling Google n-gram data, with a focus on/aim of building some text structures for investigating lexical semantic change historically.

-   [1 Overview of n-gram corpus](#1-Overview-of-n-gram-corpus)
-   [2 Download-sample-aggregate](#2-Download-sample-aggregate)
-   [3 Sample-aggregate-restructure](#Sample-aggregate-restructure)
-   [4 Building historical corpora](#4-Building-historical-corpora)
-   [5 Building historical feature matrices](#5-Building-historical-feature-matrices)
-   [5 Form frequency from matrices](#4-Building-historical-corpora))

Smart approach, versus less smart approach. Sample ngram data / reduce mass ... to a size that is manageable locally.

A developing resource. ... if we wanted to engage with ngram data in earnest, we would build SQL database, etc. Here, we just want to:

-   steal some historical text data to demonstrate a set of methods,
-   take a peak into some changes in word distributions historically, and

``` r
library(tidyverse)
library(data.table)
```

------------------------------------------------------------------------

### 1 Download, sample & aggregate

Google has a host of corpora -- here we work with the corpus dubbed the **English One Million** corpus. The corpus is comprised of texts published from the 16th century to the start of the 21st, and includes over 100 billion words. **The 5-gram corpus** is comprised of ~800 files (or sub-corpora). File composition for this corpus version is not structured alpabetically or chronologically. Instead, it seems fairly arbitrary.

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
| hands of the Portuguese and      |  1816|    3|    3|    3|
| that lead to certain tones       |  1896|    1|    1|    1|
| . Multiplying by the number      |  1947|    1|    1|    1|
| opposite sides of the mountains  |  1920|    1|    1|    1|
| to prescribe rules of government |  1875|    1|    1|    1|

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

| five\_gram                     | quarter       |  freq|
|:-------------------------------|:--------------|-----:|
| FOR ME BY MY ASSISTANT         | \[1983,2008\] |     1|
| THE YOUNG ARE HATCHED ABOUT    | \[1858,1883)  |     7|
| THE STRONG OPPOSITION OF MANY  | \[1958,1983)  |    13|
| MURDERED THE TURNKEY ON FRIDAY | \[1858,1883)  |     8|
| YEAR OR TWO AND SEE            | \[1933,1958)  |     5|

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

### Restructuring corpus

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

### Building historical term-feature matrices

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

### Odds & ends: some optional matrix reduction steps

Some different approaches to condensing our matrices.

#### Building a lemma-based lexicon

Data compiled by folks at the English Lexicon Project. As a bit of "supervision" -- some details about words included in the corpus. Filter out funcky/poor OCR words included in the corpus as well. Super-imperfect.

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
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

``` r
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

    ## Joining, by = "form"

#### Lemmatizing terms & features

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

#### Filtering features based on frequency

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

| lemma        |  \[1808,1833)|  \[1833,1858)|  \[1858,1883)|  \[1883,1908)|  \[1908,1933)|  \[1933,1958)|  \[1958,1983)|  \[1983,2008\]|
|:-------------|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|--------------:|
| CLEVERNESS   |          0.06|          0.60|          1.03|          1.01|          1.02|          0.54|          0.50|           0.61|
| CIRCUMSCRIBE |          9.57|          6.97|          6.76|          7.08|          4.60|          3.81|          3.20|           1.74|
| WARTIME      |            NA|            NA|            NA|          0.02|          0.05|          5.37|          1.50|           2.24|
| GAB          |          0.02|          0.10|          0.75|          0.04|          0.88|          0.40|          0.03|           0.86|
| SEASONALLY   |            NA|            NA|            NA|            NA|            NA|          0.01|            NA|             NA|

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

### PPMI

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

### Singular value decomposition

``` r
tfms_svd <- lapply(tfms_ppmi, irlba::irlba, nv = 200) 
```

### Find synonymns (& antonymns)

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
lapply(tfms_mats, LSAfun::neighbors, x = toupper('communicate'), n = 10)
```

    ## $`[1808,1833)`
    ## COMMUNICATE      INFORM    TRANSMIT      COMPLY      CONVEY     RECEIVE 
    ##   1.0000000   0.5116124   0.4591025   0.4128344   0.3737732   0.3539243 
    ##    DIOCESAN    SUBSERVE   ADVERTISE      REPORT 
    ##   0.3442259   0.3436778   0.3281411   0.3260907 
    ## 
    ## $`[1833,1858)`
    ##  COMMUNICATE       INFORM       IMPART       CONVEY     TRANSMIT 
    ##    1.0000000    0.5226340    0.4365555    0.4283545    0.4180967 
    ##      RECEIVE INTELLIGENCE          RID TRANSMISSION     ANNOUNCE 
    ##    0.3836830    0.3368491    0.3352775    0.3329867    0.3312306 
    ## 
    ## $`[1858,1883)`
    ## COMMUNICATE      INFORM     RESPOND    TRANSMIT      IMPART      CONVEY 
    ##   1.0000000   0.4653688   0.4467924   0.4359360   0.4248891   0.4115102 
    ##       AVAIL    DISCOVER     RECEIVE   INTRODUCE 
    ##   0.4026853   0.3608841   0.3606188   0.3585911 
    ## 
    ## $`[1883,1908)`
    ## COMMUNICATE      INFORM     LANDING      IMPART REASSURANCE    TRANSMIT 
    ##   1.0000000   0.5407639   0.5189833   0.5013072   0.4451960   0.4139865 
    ##     CONSULT     RECEIVE      REJOIN  EXCELLENCY 
    ##   0.3543093   0.3492977   0.3491751   0.3403443 
    ## 
    ## $`[1908,1933)`
    ## COMMUNICATE      INFORM     JAMESON     REQUEST    TRANSMIT     ADDRESS 
    ##   1.0000000   0.5323924   0.4622641   0.4394124   0.3924223   0.3879937 
    ##     MESSAGE        SEND      ACCEDE      NOTIFY 
    ##   0.3702342   0.3697170   0.3654982   0.3569189 
    ## 
    ## $`[1933,1958)`
    ## COMMUNICATE      CONVEY      INFORM     RESPOND      IMPART   VERBALIZE 
    ##   1.0000000   0.4966556   0.4408290   0.4351647   0.3866844   0.3791684 
    ##      DIVERT    TRANSMIT    PERCEIVE        SEND 
    ##   0.3597616   0.3515893   0.3514909   0.3437842 
    ## 
    ## $`[1958,1983)`
    ## COMMUNICATE   VERBALIZE      DECODE      INFORM     SOMEDAY    DISCOVER 
    ##   1.0000000   0.5544150   0.5252992   0.5052551   0.4926164   0.4898755 
    ##      CONVEY    IDENTIFY      FERRET    DISLODGE 
    ##   0.4722842   0.4702213   0.4698178   0.4577539 
    ## 
    ## $`[1983,2008]`
    ## COMMUNICATE    ENSHROUD     TRAINEE       PARSE       REUSE      DECODE 
    ##   1.0000000   0.6343909   0.6343909   0.6343909   0.6343909   0.6143108 
    ##    OUTFLANK      OUTWIT   VISUALIZE   REPLICATE 
    ##   0.5601198   0.5454435   0.5409273   0.5308103

``` r
#setwd("C:\\Users\\jtimm\\Google Drive\\GitHub\\git_projects\\google_ngrams_and_R\\matrices")
#saveRDS(tfms_mats, 'tfms_mats.rds')
```

### Summary
