#include "R.h"
#include "Rmath.h"
#include "word2vec.h"

void tmcn_word2vec(char *train_file0, 
        char *output_file0, 
        char *binary0, 
        char *cbow0, 
        char *num_threads0,
        char *num_features0,
        char *window0,
        char *min_count0,
        char *sample0,
        char *classes0
        )
{
	int i;
    
    cbow = atoi(cbow0); 
	binary = atoi(binary0);
    num_threads = atoi(num_threads0);
    layer1_size = atoi(num_features0);
    window = atoi(window0);
    min_count = atoi(min_count0);
    sample = atof(sample0);
    classes = atoi(classes0);
    
	strcpy(train_file, train_file0);
	strcpy(output_file, output_file0);
	vocab = (struct vocab_word *)calloc(vocab_max_size, sizeof(struct vocab_word));
	vocab_hash = (int *)calloc(vocab_hash_size, sizeof(int));
	expTable = (real *)malloc((EXP_TABLE_SIZE + 1) * sizeof(real));
	for (i = 0; i < EXP_TABLE_SIZE; i++) {
    	expTable[i] = exp((i / (real)EXP_TABLE_SIZE * 2 - 1) * MAX_EXP); // Precompute the exp() table
		expTable[i] = expTable[i] / (expTable[i] + 1);                   // Precompute f(x) = x / (x + 1)
	}
	TrainModel();
}


void CWrapper_word2vec(char **train_file, 
        char **output_file, 
        char **binary, 
        char **cbow, 
        char **num_threads,
        char **num_features,
        char **window,
        char **min_count,
        char **sample,
        char **classes)
{
    tmcn_word2vec(*train_file, *output_file, *binary, *cbow, *num_threads, *num_features, 
            *window, *min_count, *sample, *classes);
}

