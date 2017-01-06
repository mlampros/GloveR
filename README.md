[![Travis-CI Build Status](https://travis-ci.org/mlampros/GloveR.svg?branch=master)](https://travis-ci.org/mlampros/GloveR)
[![codecov.io](https://codecov.io/github/mlampros/GloveR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/GloveR?branch=master)

## GloveR
<br>

The GloveR package is an R wrapper for the [*Global Vectors for Word Representation*](http://nlp.stanford.edu/projects/glove/) (GloVe). *GloVe* is an unsupervised learning algorithm for obtaining vector representations for words. Training is performed on aggregated global word-word co-occurrence statistics from a corpus, and the resulting representations showcase interesting linear substructures of the word vector space. For more information consult : *Jeffrey Pennington, Richard Socher, and Christopher D. Manning. 2014. GloVe: Global Vectors for Word Representation*. COPYRIGHTS file and LICENSE can be found in the *inst* folder of the R package.

<br>

This R package has some limitations:

* it works only on a unix OS
* the data file should be big enough for the package-function *Glove* to work properly


To install the package from Github use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github('mlampros/GloveR')


```
<br>

Use the following link to report bugs/issues (for the R wrapper),
<br><br>

[https://github.com/mlampros/GloveR/issues](https://github.com/mlampros/GloveR/issues)


<br>


#### **Example usage**


<br>

```R

# example input data ---> 'dat.txt'



library(GloveR)


#-----------------------------
# vocabulary count computation
#-----------------------------


res = vocabulary_counts(train_data = '/data_GloveR/dat.txt', MAX_vocab = 0,

                        MIN_count = 5, output_vocabulary = '/data_GloveR/VOCAB.txt', 
                        
                        trace = TRUE)
                        

               
               
#-------------------------
# cooccurrence statistics
#-------------------------


co_mat = cooccurrence_statistics(train_data = '/data_GloveR/dat.txt', vocab_input = '/data_GloveR/VOCAB.txt',
                                  
                                 output_cooccurences = '/data_GloveR/COOCUR.bin', symmetric_both = TRUE, 
                                 
                                 context_words = 15, memory_gb = 4.0, MAX_product = 0, overflowLength = 0, 
                                 
                                 trace = TRUE)




#---------------------------
# shuffling of cooccurrences
#---------------------------


shfl = shuffle_cooccurrences(input_cooccurences = '/data_GloveR/COOCUR.bin',

                             output_cooccurences = '/data_GloveR/COOCUR_output.bin',

                             memory_gb = 4.0, arraySize = 0, trace = TRUE)




#---------------------------------------
# Global Vectors for Word Representation
#---------------------------------------


gl = Glove(input_cooccurences = '/data_GloveR/COOCUR_output.bin',

           output_vectors = '/data_GloveR/vectors',

           vocab_input = '/data_GloveR/VOCAB.txt',

           model_output = 2, iter_num = 5, learn_rate = 0.05, 
           
           save_squared_grads_file = NULL, alpha_weight = 0.75, 
           
           cutoff = 10, binary_output = 0, vectorSize = 50, threads = 6, 
           
           trace = TRUE)


```

<br>

More information about the parameters of each function can be found in the package documentation.


<br>

