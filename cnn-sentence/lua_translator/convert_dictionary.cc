#include <cstdio>
#include "dictionary.h"

using namespace cnn_sentence::lua_translator;

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Usage: convert_dictionary <FILE>\n");
    printf("  FILE: binary word2vec projections.\n");
    return 0;
  }

  Dictionary dictionary;
  dictionary.Load(argv[1]);
  
  printf("Vocabulary: %d words.\n", dictionary.size());
  printf("Dimension:  %d.\n", dictionary.dimension());
  for (int i = 0; i < 10; ++i) {
    printf("%s\n", dictionary.GetWord(i));
    const float *vec = dictionary.GetVector(i);
    for (int j = 0; j < dictionary.dimension(); ++j) {
      printf("%.3f ", vec[j]);
    }
    printf("\n");
  }
  return 0;
}
