#! /bin/bash

# th training.lua \
#     -positive /home/breakds/tmp/bilibili/input/training.txt \
#     -gen_negative 1 \
#     -batch_size 300 \
#     -max_epoch 2 \
#     -save_training_set /home/breakds/tmp/bilibili/training/201507190504 \
#     -save_model /home/breakds/tmp/bilibili/training/201507190504


th training.lua \
    -learning_rate 0.0005 \
    -batch_size 300 \
    -max_epoch 2 \
    -load_training_set /home/breakds/tmp/bilibili/training/201507190504 \
    -load_model /home/breakds/tmp/bilibili/training/201507190504 \
    -save_model /home/breakds/tmp/bilibili/training/201507210730
