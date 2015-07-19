#ifndef _DICTIONARY_H_
#define _DICTIONARY_H_

#include <array>
#include <cstdio>
#include <string>
#include <vector>

namespace cnn_sentence {
namespace lua_translator {

class Dictionary {
 public:
  Dictionary()
      : vocabulary_(), vectors_() {}

  void Load(const std::string &path) {
    FILE *input = fopen(path.c_str(), "rb");
    if (nullptr == input) {
      printf("[FATAL] Failed to open %s.\n", path.c_str());
    }

    // Number of words
    long long  words = 0;
    fscanf(input, "%lld", &words);

    // Word vector dimension
    long long dimension = 0;
    fscanf(input, "%lld", &dimension);

    // Load Vocabulary and word vectors.
    vocabulary_.resize(words);
    vectors_.resize(words * dimension);
    for (int i = 0; i < words; ++i) {
      // Load word.
      char *current = &vocabulary_[i][0];
      *current = 0;
      while (' ' != (*current)) {
        do {
          *current = fgetc(input);
        } while ('\n' == (*current));
        if (' ' != (*current)) current++;
      }
      *current = 0;

      // Load word vector.
      for (int j = 0; j < dimension; ++j) {
        fread(&vectors_[i * dimension + j], sizeof(float), 1, input);
      }
    }
    fclose(input);
  }

  const char *GetWord(int index) {
    if (vocabulary_.size() <= index) {
      printf("[FATAL] Dictionary: index out of bound.\n");
      exit(-1);
    }
    return &vocabulary_[index][0];
  }

  const float *GetVector(int index) {
    if (vocabulary_.size() <= index) {
      printf("[FATAL] Dictionary: index out of bound.\n");
      exit(-1);
    }
    return &vectors_[index * dimension()];
  }

  int size() const {
    return static_cast<int>(vocabulary_.size());
  }

  bool empty() const {
    return vocabulary_.empty();
  }

  int dimension() const {
    return static_cast<int>(vectors_.size() / vocabulary_.size());
  }

 private:
  std::vector<std::array<char, 50> > vocabulary_;
  std::vector<float> vectors_;
};

}  // namespace lua_translator
}  // cnn_sentence

#endif  // _DICTIONARY_H_
