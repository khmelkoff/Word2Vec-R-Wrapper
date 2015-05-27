#include "R.h"
#include "Rmath.h"
#include "word2phrase.h"

void word2phrase(char *train_file0, 
        char *output_file0,
        char *min_count0,
        char *threshold0)
{

    
    min_count = atoi(min_count0); 
	threshold = atof(threshold0);
    
    
	strcpy(train_file, train_file0);
	strcpy(output_file, output_file0);

	TrainPhrase();
}


void CWrapper_word2phrase(char **train_file, 
        char **output_file,
        char **min_count, 
        char **threshold)
{
    word2phrase(*train_file, *output_file, *min_count, *threshold);
}

