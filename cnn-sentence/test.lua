require 'nn'

local word_vector_dim = 100
local sentence_len = 16
local conv_dim = 100
local target_dim = 100


-- Construct Neural Network

encoder = nn.Sequential()

-- Convolution Layer
encoder:add(nn.TemporalConvolution(word_vector_dim, conv_dim, 5))
encoder:add(nn.ReLU())
encoder:add(nn.TemporalMaxPooling(conv_dim))
encoder:add(nn.Linear(conv_dim, target_dim))
encoder:add(nn.ReLU())

-- Classifier Layer
classifier = nn.Sequential()
classifier:add(nn.Dropout(0.5))
classifier:add(nn.Linear(target_dim, 2))
o
-- Integrated network
cascade = nn.Sequential()
cascade:add(encoder)
cascade:add(calssifier)







