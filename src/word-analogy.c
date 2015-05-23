
#include "word-analogy.h"

void CWrapper_analogy(char **file_name, char **words, char **returnw, double *returnd, char **size)
{
    analogy(*file_name, *words, *returnw, returnd, *size);
}
