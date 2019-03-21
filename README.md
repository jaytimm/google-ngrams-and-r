Google n-gram data & R: some methods
------------------------------------

An R-based guide to accessing/sampling Google n-gram data & building historical term-feature matrices for investigating lexical semantic change historically.

-   [1 Download-Sample-Aggregate](#1-Download-Sample-Aggregate)
-   [2 Restructuring corpus](#2-Restructuring-corpus)
-   [3 Building historical term-feature matrices](#3-Building-historical-term-feature-matrices)
-   [4 Condensing and filtering historical term-feature matrices](#4-Condensing-and-filtering-historical-term-feature-matrices)
    -   [4a Building lemma lexicon](#4a-Building-lemma-lexicon)
    -   [4b Lemmatizing terms and features](#4b-Lemmatizing-terms-and-features)
    -   [4c Filtering features based on frequency](#4c-Filtering-features-based-on-frequency)
-   [5 PPMI and SVD](#5-PPMI-and-SVD)
-   [6 Exploring synonymny historically](#6-Exploring-synonymny-historically)
-   [7 Summary](#7-Summary)

This guide focuses on working with Google n-gram data locally. So, lots of sampling & intermediary file structures. A smarter aproach to working with n-gram data in its entriety would be to build a SQL database. Here, we just want to steal some n-gram data to demonstrate a few methods & take a peak into some changes in word distributions historically.

Google n-gram data are a bit weird as a text structure. As such, many existing text-analytic R packages/functions (that often assume raw text as a starting point) are not especially helpful here. So, we have to hack-about some to get from Google n-gram data to historical term-feature matrices.

**ENDGAME:** Finding historical synonyms (-ish). The tables below summarize nearest neighbors for the word *GRASP* over the last 200 years (by quarter century), including cosine-based similarities (value) & term frequencies in parts per million (ppm).

<br>

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

Per the table above, the 5-gram "BREAK ALL THE TEN COMMANDMENTS" occurred 4 times during the quarter-century spanning 1958-1983 in the *first file* of the ngram corpus. The pipe below seperates each form in the ngram into five rows, assigns each row/form the frequency of the ngram (4), uniquely identifies the ngram in the sub-corpus, and removes rows in the ngram containing stopwords (here, "ALL" and "THE"). The ID serves to preserve the ngram as a context of usage (or mini-text).

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

### 4 Condensing and filtering historical term-feature matrices

At present, each historical TFM is quite large (~75k x 75k), and comprised of a uniqe set of terms & features. Here we present some optional steps for condensing & homogenizing (& cleaning) TFM composition.

#### 4a Building a lexeme-lemma lexicon

One approach to compressing elements of a TFM is to aggregate forms to lemmas. A lemma is the dictionary representation of a word-part-of-speech pair, and an abstraction over the inflectional variants/forms of a given word-POS pair. Example lemma-form paradigms are presented below:

-   \[stretch, verb\] - stretch, stretches, streched - stretched/stretching;
-   \[stretch, noun\] - stretch, stretches;
-   \[stretched, past.part as adjective\] - stretched;
-   \[stretching, present.part as noun\] - stretching.

Forms included in Google ngram data, however, are not POS-annotated. So, the distinctions presented above (eg, stretches - 3PerSingPres and stretches - NPlur) are lost. In the absence of POS distinctions, then, the four lemmas presented above become a single "orthographic" lemma \[stretch\] with four forms/variants \[stretch, stretches, stretched, stretching\], each ambiguous with respect to grammatical category.

To map forms to orthographic lemmas, we use a British National Corpus (BNC)-derived resource made Git Hub available here. DESCRIBE. It is an imperfect resource, but mostly ideal for our purposes here. We do some restructuring below:

``` r
lemma_lexicon <- read.csv( url('https://raw.githubusercontent.com/skywind3000/lemma.en/master/lemma.en.txt'), 
                      header = FALSE, 
                      skip = 10, sep = '\t')%>%
  separate(V1, into = c('lemma', 'form'), sep = ' -> ') %>%
  mutate(lemma = toupper(gsub('/.*$', '', lemma)),
         form = toupper(form),
         form = paste0(lemma,',',form))%>%
  separate_rows (form, sep = ',') %>%
  filter(grepl("^[A-Z]+$", form)) %>%
  group_by(form) %>% slice(1) %>%
  arrange(lemma, form)
#some single forms are mapped to multiple lemmas -- n=136 
```

The **English Lexicon Project** (ELP) has aggregated a host of behavioral data & lexical features for a large portion of the English lexicon, which I have included in my R package `lexvarsdatr`. Here, we filter the lemma lexicon from above to only lemmas included in the ELP. Again, this step is not necessary. We do it here simply to focus on the portion of the lexicon most familiar to speakers, and to eliminate any potential funk from the original lexeme-lemma crosswalk.

``` r
elp_lexicon <- lexvarsdatr::lvdr_behav_data %>%  
  filter(!is.na(POS))

elp_lemma_lexicon <- subset(lemma_lexicon, 
                        lemma %in% toupper(elp_lexicon$Word))
```

Our new (common) lexicon, then, contains ~54k forms and ~22k lemmas. A sample of the lexicon is presented below:

| lemma     | form        |
|:----------|:------------|
| ABDUCT    | ABDUCT      |
| ABDUCT    | ABDUCTED    |
| ABDUCT    | ABDUCTING   |
| ABDUCT    | ABDUCTS     |
| BIG       | BIG         |
| BIG       | BIGGER      |
| BIG       | BIGGEST     |
| INDIGNITY | INDIGNITIES |
| INDIGNITY | INDIGNITY   |
| WIN       | WIN         |
| WIN       | WINNING     |
| WIN       | WINS        |
| WIN       | WON         |

------------------------------------------------------------------------

#### 4b Lemmatizing terms and features

Next, we filter terms & features comprising the historical TFMs to forms included in the common lexicon, and then aggregate term-feature co-occurrence frequencies by lemma. The function below performs both tasks.

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

Apply function to the list of historical TFMs.

``` r
tfms_lemmed <- lapply(tfms, lemmatize_matrix)
```

A small portion of the **lemmatized TFM** for the 1908-1932 sub-corpus is presented below. Again, full data structure is a list of TFMs by quarter.

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

#### 4c Filtering features based on frequency

At this point, the composition of the matrices is limited to lemmas included in the common lexicon. However, two issues remain:

-   First, the actual term & feature composition of each matrix is still different. While differing number of terms is not necessarily problematic, we want term embeddings to be comprised of the same features historically.

-   Second, our matrices are still comprised of a substantial number of features, even after lemmatization (max ~22k, per composition of commom lexicon), making for super-sparse term vectors.

To address the first issue, we limit features to only those that occur in every quarter-century of the full corpus. To address the second issue, we limit features to only those that occur within a given frequency range.

Below, we extract lemma frequencies from each quarter-century TFM via matrix diagonals.

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

Historical frequencies for a small set of lemmas in the sampled ngram corpus are presented below. Note that these frequencies are very rough, and will likely differ some from numbers obtained from Google's ngram viewer.

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

Based on the frequency table above, we create a list of lemmas that occur in every quarter-century; then we filter these lemmas to the 50th to 5,049th most frequent based on median frequencies historically.

``` r
filtered_features <- freqs_by_gen %>%
  group_by(lemma) %>%
  mutate(quarter_count = length(quarter),
         ppm = median(ppm)) %>%
  filter (quarter == "[1983,2008]", quarter_count == 8)%>%
  arrange (desc(ppm)) %>%
  ungroup() %>%
  slice(50:5049) 
```

Then we subset features in the set of lemmatized TFMs. The result is 8 new TFMs, *n* x 5,000 in dimension.

``` r
tfms_filtered <- lapply(1:8, function (x)
  tfms_lemmed[[x]][,colnames(tfms_lemmed[[x]]) %in% filtered_features$lemma] )
```

------------------------------------------------------------------------

### 5 PPMI and SVD

Whether or not some or all of the compression steps presented above, ...

*Positive Pointwise Mutual Information* (PPMI)

The function below calculates PPMI values on sparse matrices, which has been slightly modified from an SO post available [here](https://stackoverflow.com/questions/43354479/how-to-efficiently-calculate-ppmi-on-a-sparse-matrix-in-r).

Apply function to the list of quarter-century sparse matrices. Here we use the lemmatized/feature-filtered list, although rawer renditions can be used as well.

``` r
tfms_ppmi <- lapply(tfms_filtered, lexvarsdatr::lvdr_build_sparse_ppmi)
```

*Singular value decomposition*

``` r
tfms_svd <- lapply(tfms_ppmi, irlba::irlba, nv = 200) 
```

------------------------------------------------------------------------

### 6 Exploring synonymny historically

Simple matrix.

``` r
tfms_mats <- list()

for (i in 1:8) {
  x <- as.matrix(data.matrix(tfms_svd[[i]]$u))
  dimnames(x) <- list(rownames(tfms_ppmi[[i]]), c(1:length(tfms_svd[[i]]$d)))
  tfms_mats[[i]] <- x
}
```

Using the `neighbors` function from the `LSAfun` package.

``` r
x <- lapply(tfms_mats, LSAfun::neighbors, x = toupper('communicate'), n = 100)
```

Clean output.

``` r
strip_syns <- function (x) {
  lapply(1:length(x), function(y)  
    x[[y]] %>%
    as.tibble %>% 
    rownames_to_column(var = 'lemma') %>%
    mutate (quarter = names(x[y]),
            value = round(value,2))) %>%
    bind_rows() }
```

Add frequencies, and filter neighbors ... Frequency filter is super helpful.

``` r
syns <- x %>% strip_syns() %>%
  inner_join(freqs_by_gen) %>%
  mutate(ppm = round(ppm, 1)) %>%
  select(-freq) %>%
  filter (ppm > 1) %>% #This is super effective -- 
  group_by(quarter) %>%
  arrange( desc(value))%>%
  slice(1:10)%>%
  ungroup()
```

Plot below...

``` r
g <- list(length(tfms_mats))
tt <- gridExtra::ttheme_default(base_size = 7)

for (i in 1:length(tfms_mats)) {
  g[[i]] <- syns %>% 
    filter (quarter == names(tfms_mats[i])) %>%
    rename(!!names(tfms_mats[i]) := lemma) %>% 
    select(-quarter)%>%
    gridExtra::tableGrob(rows=NULL, theme = tt) }

gridExtra::grid.arrange(grobs = g, nrow = 2)
```

![](README_files/figure-markdown_github/unnamed-chunk-47-1.png)

------------------------------------------------------------------------

### 7 Summary

While academic linguists (of the functional/cognitive/usage-based varieties) are often critical of Google n-gram data, it is still an incredible cultural resource.

Many of the methodological decisions made here can certainly be tweaked to improve results.
