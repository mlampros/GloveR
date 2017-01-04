

if (.Platform$OS.type == "windows") {
  
  paste_delim = "\\"
}

if (.Platform$OS.type == "unix") {
  
  paste_delim = "/"
}



context('glove functions')


#-----------------------------
# 'vocabulary_counts' function
#-----------------------------


testthat::test_that("it returns an error if the train-path is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  testthat::expect_error( vocabulary_counts(train_data = path_in, MAX_vocab = 0, MIN_count = 1, output_vocabulary = path_out, trace = FALSE) )
})


testthat::test_that("it returns an error if the output_vocabulary-path is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data1", "glove_out.txt"), collapse = paste_delim)))
  
  testthat::expect_error( vocabulary_counts(train_data = path_in, MAX_vocab = 0, MIN_count = 1, output_vocabulary = path_out, trace = FALSE) )
})


testthat::test_that("it returns an error if the output_vocabulary-path is NULL", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  testthat::expect_error( vocabulary_counts(train_data = path_in, MAX_vocab = 0, MIN_count = 1, output_vocabulary = NULL, trace = FALSE) )
})


testthat::test_that("it returns an error if MAX_vocab is less than 0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  testthat::expect_error( vocabulary_counts(train_data = path_in, MAX_vocab = -1, MIN_count = 1, output_vocabulary = path_out, trace = FALSE) )
})


testthat::test_that("it returns an error if MIN_count is less than 1", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  testthat::expect_error( vocabulary_counts(train_data = path_in, MAX_vocab = 0, MIN_count = 0, output_vocabulary = path_out, trace = FALSE) )
})


testthat::test_that("it returns an error if the trace parameter is not logical", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  testthat::expect_error( vocabulary_counts(train_data = path_in, MAX_vocab = 0, MIN_count = 1, output_vocabulary = path_out, trace = 'FALSE') )
})


testthat::test_that("it saves the output to a file if all parameters are valid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  res = vocabulary_counts(train_data = path_in, MAX_vocab = 0, MIN_count = 1, output_vocabulary = path_out, trace = FALSE)
  
  testthat::expect_silent(res)
})



#----------------------------------
# 'cooccurence statistics' function
#----------------------------------


testthat::test_that("it returns an error if the train-path is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))

  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 15, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the vocabulary-path is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 15, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the co-occurence-path is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data1", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 15, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the co-occurence-file is not a binary file", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 15, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the symmetric_both parameter is not logical", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = 'TRUE', context_words = 15, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the context_words parameter is less than 1", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 0, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE) )
})



testthat::test_that("it returns an error if the memory_gb parameter is less than 0.5", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 10, 
                                                  
                                                  memory_gb = 0.4, MAX_product = 0, overflowLength = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the MAX_product parameter is less than 0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 10, 
                                                  
                                                  memory_gb = 4.0, MAX_product = -1, overflowLength = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the overflowLength parameter is less than 0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 10, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = -1, trace = FALSE) )
})


testthat::test_that("it returns an error if the trace parameter is not logical", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  testthat::expect_error( cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 10, 
                                                  
                                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = 'FALSE') )
})



testthat::test_that("it saves the output to a file if all parameters are valid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  vocab_in = paste0(getwd(), path.expand(paste(c("", "test_data", "glove_out.txt"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  res = cooccurrence_statistics(train_data = path_in, vocab_input = vocab_in, output_cooccurences = path_out, symmetric_both = TRUE, context_words = 10, 
                                
                                memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE)
  
  testthat::expect_silent(res)
})



#----------------------------------
# 'shuffle cooccurrences' function
#----------------------------------


testthat::test_that("it returns an error if the input_cooccurences-path is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out_shuffl.bin"), collapse = paste_delim)))
  
  testthat::expect_error( shuffle_cooccurrences(input_cooccurences = path_in, output_cooccurences = path_out, memory_gb = 4.0, arraySize = 0, trace = FALSE) )
})



testthat::test_that("it returns an error if the output_cooccurences-path is invalid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data1", "coocur_out_shuffl.bin"), collapse = paste_delim)))
  
  testthat::expect_error( shuffle_cooccurrences(input_cooccurences = path_in, output_cooccurences = path_out, memory_gb = 4.0, arraySize = 0, trace = FALSE) )
})


testthat::test_that("it returns an error if the output_cooccurences-file is not a binary file", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out_shuffl"), collapse = paste_delim)))
  
  testthat::expect_error( shuffle_cooccurrences(input_cooccurences = path_in, output_cooccurences = path_out, memory_gb = 4.0, arraySize = 0, trace = FALSE) )
})



testthat::test_that("it returns an error if the memory_gb parameter is less than 0.4", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out_shuffl.bin"), collapse = paste_delim)))
  
  testthat::expect_error( shuffle_cooccurrences(input_cooccurences = path_in, output_cooccurences = path_out, memory_gb = 0.4, arraySize = 0, trace = FALSE) )
})



testthat::test_that("it returns an error if the arraySize parameter is less than 0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out_shuffl.bin"), collapse = paste_delim)))
  
  testthat::expect_error( shuffle_cooccurrences(input_cooccurences = path_in, output_cooccurences = path_out, memory_gb = 4.0, arraySize = -1, trace = FALSE) )
})



testthat::test_that("it returns an error if the trace parameter is not logical", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out_shuffl.bin"), collapse = paste_delim)))
  
  testthat::expect_error( shuffle_cooccurrences(input_cooccurences = path_in, output_cooccurences = path_out, memory_gb = 4.0, arraySize = 0, trace = 'FALSE') )
})



testthat::test_that("it saves the output to a file if all parameters are valid", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out.bin"), collapse = paste_delim)))
  
  path_out = paste0(getwd(), path.expand(paste(c("", "test_data", "coocur_out_shuffl.bin"), collapse = paste_delim)))
  
  res = shuffle_cooccurrences(input_cooccurences = path_in, output_cooccurences = path_out, memory_gb = 4.0, arraySize = 0, trace = FALSE)
  
  testthat::expect_silent(res)
})



#------------------
# 'Glove' function
#------------------


testthat::test_that("it returns an error if the input_cooccurences parameter is not a valid path to a file", {
  
  path_invalid = paste0(getwd(), path.expand(paste(c("", "invalid", "invalid.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_invalid) )
})



testthat::test_that("it returns an error if the input_cooccurences parameter is not a character string", {
  
  path_invalid = paste0(getwd(), path.expand(paste(c("", "invalid", "invalid.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = NULL) )
})


testthat::test_that("it returns an error if the output_vectors parameter is not a valid path or the output_vectors include any file extensions", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_invalid_vecs = paste0(getwd(), path.expand(paste(c("", "invalid", "invalid.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_invalid_vecs) )
})


testthat::test_that("it returns an error if the output_vectors parameter is not a character string", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = NULL) )
})


testthat::test_that("it returns an error if the vocab_input parameter is not a valid path to a file", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_invalid_vocab = paste0(getwd(), path.expand(paste(c("", "invalid", "invalid.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_invalid_vocab) )
})


testthat::test_that("it returns an error if the vocab_input parameter is not a character string", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = NULL) )
})


testthat::test_that("it returns an error if the model_output parameter is not of type numeric", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, model_output = NULL) )
})


testthat::test_that("it returns an error if the model_output parameter is numeric but not 0, 1 or 2", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, model_output = 3) )
})


testthat::test_that("it returns an error if the iter_num parameter is less than 1", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, iter_num = 0) )
})


testthat::test_that("it returns an error if the learn_rate parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, learn_rate = 0.0) )
})


testthat::test_that("it returns an error if the save_squared_grads_file parameter is a character string", {

  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))

  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))

  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))

  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, save_squared_grads_file = list()) )
})



testthat::test_that("it returns an error if the save_squared_grads_file parameter is not a character string of length 1", {

  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))

  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))

  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))

  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, save_squared_grads_file = c('a', 'd')) )
})


testthat::test_that("it returns an error if the alpha_weight parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, alpha_weight = 0.0) )
})


testthat::test_that("it returns an error if the cutoff parameter is less than or equal to 0.0", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, cutoff = 0) )
})



testthat::test_that("it returns an error if the binary_output parameter is not 0,1, or 2", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, binary_output = 3) )
})


testthat::test_that("it returns an error if the vectorSize parameter is less than 1", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, vectorSize = 0) )
})


testthat::test_that("it returns an error if the threads parameter is less than 1", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, threads = 0) )
})


testthat::test_that("it returns an error if the trace parameter is not boolean", {
  
  path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
  
  path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
  
  path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
  
  testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab, trace = 'FALSE') )
})


#=============================================================================================================
# expect true for the 'Glove' function: If the size of the data file is small probably it will give a segfault
#=============================================================================================================


# testthat::test_that("it returns an error if the trace parameter is not boolean", {
#   
#   path_in = paste0(getwd(), path.expand(paste(c("", "test_data", "doc.txt"), collapse = paste_delim)))
#   
#   path_vecs = paste0(getwd(), path.expand(paste(c("", "test_data", "vectors"), collapse = paste_delim)))
#   
#   path_vocab = paste0(getwd(), path.expand(paste(c("", "test_data", "VOCAB.txt"), collapse = paste_delim)))
#   
#   testthat::expect_error( Glove(input_cooccurences = path_in, output_vectors = path_vecs, vocab_input = path_vocab) )
# })

