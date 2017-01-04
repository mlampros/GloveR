
#' vocabulary counts
#'
#' @param train_data a character string specifying the path to the train text file
#' @param MAX_vocab a value specifying the number of terms in the vocabulary. For instance a \emph{MAX_vocab} value of 0 includes all the vocab-terms.
#' @param MIN_count a value greater or equal to 1. It specifies the minimum occurrences (counts of words) for inclusion in the vocabulary
#' @param output_vocabulary a character string specifying the path to the output text file
#' @param trace either TRUE or FALSE. If TRUE information will be printed out
#' @return a character string specifying the location of the saved data
#' @export
#' @references
#' https://github.com/stanfordnlp/GloVe
#' 
#' http://nlp.stanford.edu/projects/glove/
#' 
#' http://nlp.stanford.edu/pubs/glove.pdf
#' @examples
#'
#' # library(GloveR)
#' 
#' # res = vocabulary_counts(train_data = '/data_GloveR/dat.txt', MAX_vocab = 0,
#' 
#' #                         MIN_count = 5, output_vocabulary = '/data_GloveR/VOCAB.txt', trace = T)


vocabulary_counts = function(train_data = NULL, MAX_vocab = 0, MIN_count = 1, output_vocabulary = NULL, trace = FALSE) {
  
  try_err_files = inherits(tryCatch(normalizePath(train_data, mustWork = T), error = function(e) e), "error")
  if (!is.character(train_data) || try_err_files) stop("the train_data parameter should be a valid character string path")
  if (is.null(output_vocabulary)) stop("the output_vocabulary parameter should be a non-NULL valid character string path")
  if (.Platform$OS.type == 'unix') {
    first = strsplit(output_vocabulary, "/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "/")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  if (.Platform$OS.type == 'windows') {
    first = strsplit(output_vocabulary, "\\\\|/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "\\")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  if (!is.character(output_vocabulary) || try_err_files_out) stop("the output_vocabulary parameter should be a valid character string path")
  if (MAX_vocab < 0) stop("the maximum number of vocabulary must be greater or equal to 0")
  if (MIN_count < 1) stop("the mininum number of occurences (word counts) must be greater or equal to 1")
  if (!is.logical(trace)) stop("the trace parameter should be either TRUE or FALSE")
  
  flag = F
  
  if (trace) { flag = T }
  
  if (flag) { start = Sys.time() }
  
  trace = as.integer(trace)
  
  res = .C("vocabulary_counts", 
           trace = as.character(trace), 
           MAX_vocab = as.character(MAX_vocab),
           MIN_count = as.character(MIN_count), 
           train_data = as.character(train_data), 
           output_vocabulary = as.character(output_vocabulary))
  
  if (flag) {
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }
  
  return(paste0("the output is saved in: ", output_vocabulary))
}



#' cooccurence statistics
#' 
#' @param train_data a character string specifying the path to the train text file
#' @param vocab_input a character string specifying the path to the vocabulary text file (the output file of the \emph{vocabulary_counts} function)
#' @param output_cooccurences a character string specifying the path to the output cooccurences text file
#' @param symmetric_both either TRUE or FALSE. If TRUE then both left and right context will be used, otherwise only the left context (see .pdf file in references for more information)
#' @param context_words the number of context words to the left (and to the right, if symmetric_both = TRUE). The default is 15 
#' @param memory_gb a float number specifying the limit for memory consumption, in GB -- based on simple heuristic, so not extremely accurate; the default is 4.0
#' @param MAX_product a number specifying the size-limit of dense cooccurrence array by specifying the max product (integer) of the frequency counts of the two cooccurring words. Typically only needs adjustment for use with very large corpora
#' @param overflowLength a number specifying the length-limit of the sparse overflow array, which buffers cooccurrence data that does not fit in the dense array, before writing to disk. Typically only needs adjustment for use with very large corpora
#' @param trace either TRUE or FALSE. If TRUE information will be printed out
#' @return a character string specifying the location of the saved data
#' @export
#' @references 
#' https://github.com/stanfordnlp/GloVe
#' 
#' http://nlp.stanford.edu/projects/glove/
#' 
#' http://nlp.stanford.edu/pubs/glove.pdf
#' @examples 
#' 
#' # library(GloveR)
#' 
#' # co_mat = cooccurrence_statistics(train_data = '/data_GloveR/dat.txt', vocab_input = '/data_GloveR/VOCAB.txt', 
#'                                  
#' #                                  output_cooccurences = '/data_GloveR/COOCUR.bin', symmetric_both = TRUE, context_words = 15, 
#' 
#' #                                  memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = TRUE)


cooccurrence_statistics = function(train_data = NULL, vocab_input = NULL, output_cooccurences = NULL, symmetric_both = TRUE, context_words = 15, 
                                   
                                   memory_gb = 4.0, MAX_product = 0, overflowLength = 0, trace = FALSE) {
  
  try_err_file_train = inherits(tryCatch(normalizePath(train_data, mustWork = T), error = function(e) e), "error")
  if (!is.character(train_data) || try_err_file_train) stop("the train_data parameter should be a valid character string path")
  try_err_file_vocab = inherits(tryCatch(normalizePath(vocab_input, mustWork = T), error = function(e) e), "error")
  if (!is.character(vocab_input) || try_err_file_vocab) stop("the vocab_input parameter should be a valid character string path")
  if (is.null(output_cooccurences)) stop("the output_cooccurences parameter should be a non-NULL valid character string path")
  if (.Platform$OS.type == 'unix') {
    first = strsplit(output_cooccurences, "/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "/")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  if (.Platform$OS.type == 'windows') {
    first = strsplit(output_cooccurences, "\\\\|/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "\\")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  type_bin = strsplit(first[length(first)], '[.]')[[1]]
  type_bin = type_bin[length(type_bin)]
  if (!is.character(output_cooccurences) || try_err_files_out) stop("the output_cooccurences parameter should be a valid character string path")
  if (type_bin != "bin") stop("the 'output_cooccurences' parameter should be a binary file, such as /file.bin ")
  if (!is.logical(symmetric_both)) stop("the symmetric_both parameter should be either TRUE or FALSE")
  if (context_words < 1) stop("the context_words parameter should be greater or equal to 1")
  if (memory_gb < 0.5) stop("the memory_gb parameter should be greater or equal to 0.5 GB")
  if (MAX_product < 0) stop("the MAX_product parameter should be greater or equal to 0")
  if (overflowLength < 0) stop("the overflowLength parameter should be greater or equal to 0")
  if (!is.logical(trace)) stop("the trace parameter should be either TRUE or FALSE")
  
  flag = F
  
  if (trace) { flag = T }
  
  if (flag) { start = Sys.time() }
  
  symmetric_both = as.integer(symmetric_both)
  
  trace = as.integer(trace)
  
  res = .C("cooccurrence_statistics", 
           train_data = as.character(train_data), 
           vocab_input = as.character(vocab_input), 
           output_cooccurences = as.character(output_cooccurences), 
           symmetric_both = as.character(symmetric_both), 
           context_words = as.character(context_words), 
           memory_gb = as.character(memory_gb), 
           MAX_product = as.character(MAX_product), 
           overflowLength = as.character(overflowLength), 
           trace = as.character(trace))
  
  if (flag) {
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }

  return(paste0("the output is saved in: ", output_cooccurences))
}




#' Function to shuffle the entries of the word-word cooccurrence file
#' 
#' @param input_cooccurences a character string specifying the path to the cooccurences text file
#' @param output_cooccurences a character string specifying the path to the shuffled-output-cooccurences-text-file
#' @param memory_gb a float number specifying the limit for memory consumption, in GB -- based on simple heuristic, so not extremely accurate; the default is 4.0
#' @param arraySize a number specifying the length-limit to the buffer, which stores chunks of data to shuffle before writing to disk. This value overrides what is automatically produced by \emph{memory_gb}
#' @param trace either TRUE or FALSE. If TRUE information will be printed out
#' @return a character string specifying the location of the saved data
#' @export
#' @references 
#' https://github.com/stanfordnlp/GloVe
#' 
#' http://nlp.stanford.edu/projects/glove/
#' 
#' http://nlp.stanford.edu/pubs/glove.pdf
#' @examples 
#' 
#' # library(GloveR)
#' 
#' # shfl = shuffle_cooccurrences(input_cooccurences = '/data_GloveR/COOCUR.bin', 
#' 
#' #                              output_cooccurences = '/data_GloveR/COOCUR_output.bin', 
#' 
#' #                              memory_gb = 4.0, arraySize = 0, trace = TRUE)


shuffle_cooccurrences = function(input_cooccurences = NULL, output_cooccurences = NULL, memory_gb = 4.0, arraySize = 0, trace = FALSE) {
  
  try_err_file_occ = inherits(tryCatch(normalizePath(input_cooccurences, mustWork = T), error = function(e) e), "error")
  if (!is.character(input_cooccurences) || try_err_file_occ) stop("the input_cooccurences parameter should be a valid character string path")
  if (is.null(output_cooccurences)) stop("the output_cooccurences parameter should be a non-NULL valid character string path")
  if (.Platform$OS.type == 'unix') {
    first = strsplit(output_cooccurences, "/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "/")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  if (.Platform$OS.type == 'windows') {
    first = strsplit(output_cooccurences, "\\\\|/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "\\")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  type_bin = strsplit(first[length(first)], '[.]')[[1]]
  type_bin = type_bin[length(type_bin)]
  if (!is.character(output_cooccurences) || try_err_files_out) stop("the output_cooccurences parameter should be a valid character string path")
  if (type_bin != "bin") stop("the 'output_cooccurences' parameter should be a binary file, such as /file.bin ")
  if (memory_gb < 0.5) stop("the memory_gb parameter should be greater or equal to 0.5 GB")
  if (arraySize < 0) stop("the arraySize parameter should be greater or equal to 0")
  if (!is.logical(trace)) stop("the trace parameter should be either TRUE or FALSE")
  
  flag = F
  
  if (trace) { flag = T }
  
  if (flag) { start = Sys.time() }

  trace = as.integer(trace)
  
  res = .C("shuffle_cooccurrences", 
           input_cooccurences = as.character(input_cooccurences),
           output_cooccurences = as.character(output_cooccurences),
           memory_gb = as.character(memory_gb), 
           arraySize = as.character(arraySize), 
           trace = as.character(trace))
  
  if (flag) {
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }
  
  return(paste0("the output is saved in: ", output_cooccurences))
}



#' GloVe (Global Vectors for Word Representation)
#' 
#' @param input_cooccurences a character string specifying the path to the cooccurences text file
#' @param output_vectors a character string specifying the path to the output-vectors-file(s) (the output depending on the \emph{binary_output} parameter can be a .bin and/or a .txt file)
#' @param vocab_input a character string specifying the path to the vocabulary text file (the output file of the \emph{vocabulary_counts} function)
#' @param model_output an integer specifying the model-for-word-vector-output (for text output only). [ \strong{0}: output all data, for both word and context word vectors, including bias terms; \strong{1}: output word vectors, excluding bias terms; \strong{2}: output word vectors + context word vectors, excluding bias terms ]
#' @param iter_num an integer specifying the number of training iterations
#' @param learn_rate a float number specifying the learning rate
#' @param save_squared_grads_file either NULL or a character string specifying the location where the \emph{save_squared_grads_file} data should be saved (\emph{accumulated squared gradients})
#' @param alpha_weight a float number specifying the parameter in exponent of the weighting function
#' @param cutoff a number specifying the cutoff parameter of the weighting function
#' @param binary_output an integer specifying the format output of the saved data (\strong{0}: text, \strong{1}: binary, \strong{2}: both)
#' @param vectorSize a number specifying the dimension of word vector representations (excluding the bias term)
#' @param threads an integer specifying the number of threads to run in parallel
#' @param trace either TRUE or FALSE. If TRUE information will be printed out
#' @return a character string specifying the location of the saved data and the number of the word vectors
#' @export
#' @references 
#' https://github.com/stanfordnlp/GloVe
#' 
#' http://nlp.stanford.edu/projects/glove/
#' 
#' http://nlp.stanford.edu/pubs/glove.pdf
#' @examples 
#' 
#' # library(GloveR)
#' 
#' # gl = Glove(input_cooccurences = '/data_GloveR/COOCUR_output.bin', 
#' 
#' #            output_vectors = '/data_GloveR/vectors', 
#' 
#' #            vocab_input = '/data_GloveR/VOCAB.txt', 
#' 
#' #            model_output = 2, iter_num = 5, learn_rate = 0.05, save_squared_grads_file = NULL, 
#' 
#' #            alpha_weight = 0.75, cutoff = 10, binary_output = 0, vectorSize = 50, threads = 6, trace = TRUE)


Glove = function(input_cooccurences = NULL, output_vectors = NULL, vocab_input = NULL, model_output = 0, iter_num = 5, learn_rate = 0.1, save_squared_grads_file = NULL, 
                 
                 alpha_weight = 0.75, cutoff = 100, binary_output = 0, vectorSize = 10, threads = 1, trace = FALSE) {
  
  
  try_err_file_occ = inherits(tryCatch(normalizePath(input_cooccurences, mustWork = T), error = function(e) e), "error")
  if (!is.character(input_cooccurences) || try_err_file_occ) stop("the input_cooccurences parameter should be a valid character string path")
  if (is.null(output_vectors)) stop("the output_vectors parameter should be a non-NULL valid character string path")
  if (.Platform$OS.type == 'unix') {
    first = strsplit(output_vectors, "/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "/")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  if (.Platform$OS.type == 'windows') {
    first = strsplit(output_vectors, "\\\\|/")[[1]]
    second = first[-length(first)]
    res = paste(second, collapse = "\\")
    try_err_files_out = inherits(tryCatch(normalizePath(res, mustWork = T), error = function(e) e), "error")
  }
  type_bin = strsplit(first[length(first)], '[.]')[[1]]
  if (length(type_bin) > 1) stop("the output_vectors parameter should be a valid path with no file extensions. Example path:  ../vectors")
  if (!is.character(output_vectors) || try_err_files_out) stop("the output_vectors parameter should be a valid character string path")
  try_err_file_vocab = inherits(tryCatch(normalizePath(vocab_input, mustWork = T), error = function(e) e), "error")
  if (!is.character(vocab_input) || try_err_file_vocab) stop("the vocab_input parameter should be a valid character string path")
  if (!inherits(model_output, c('integer', 'numeric'))) stop("the model_output parameter should be of type numeric")
  if (!model_output %in% c(0:2)) stop("the model_output parameter should be either 0, 1 or 2")
  if (iter_num < 1) stop("the iter_num parameter should be greater than 0")
  if (learn_rate <= 0.0) stop("the learn_rate parameter should be a float number greater than 0.0")
  
  if (!is.null(save_squared_grads_file)) {
    if (!inherits(save_squared_grads_file, "character")) stop("the save_squared_grads_file parameter should be a valid character string path")
    if (length(save_squared_grads_file) == 0 || length(save_squared_grads_file) > 1) stop("the save_squared_grads_file should be a character string of length 1")}
  if (is.null(save_squared_grads_file)) save_squared_grads_file = 0
  
  if (alpha_weight <= 0.0) stop("the alpha_weight parameter should be a float number greater than 0.0")
  if (cutoff < 1) stop("the cutoff parameter should be greater than 0")
  if (!binary_output %in% c(0:2)) stop("the binary_output parameter should be either 0, 1 or 2")
  if (vectorSize < 1) stop("the vectorSize parameter should be greater than 0")
  if (threads < 1) stop("the threads parameter should be greater than 0")
  if (!is.logical(trace)) stop("the trace parameter should be either TRUE or FALSE")
  
  flag = F
  
  if (trace) { flag = T }
  
  if (flag) { start = Sys.time() }
  
  trace = as.integer(trace)
  
  res = .C("Glove", 
           trace = as.character(trace), 
           vectorSize = as.character(vectorSize), 
           iter_num = as.character(iter_num), 
           threads = as.character(threads), 
           alpha_weight = as.character(alpha_weight), 
           cutoff = as.character(cutoff), 
           learn_rate = as.character(learn_rate), 
           binary_output = as.character(binary_output), 
           model_output = as.character(model_output),
           vocab_input = as.character(vocab_input), 
           output_vectors = as.character(output_vectors), 
           save_squared_grads_file = as.character(save_squared_grads_file),
           input_cooccurences = as.character(input_cooccurences))
  
  if (flag) {
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }
  
  return(structure(list(file_location = paste0("the output is saved in: ", output_vectors), num_vectors = vectorSize), 
                   
                   class = 'GloveR'))
}

