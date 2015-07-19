require 'torch'
require 'nn'
require 'os'

---------- Command Line Options ----------
local cmd = torch.CmdLine()
cmd:option('-dictionary', '/home/breakds/tmp/bilibili/input/word_vecs.bin', 
           'path to the word2vec binary dictionary.')
cmd:option('-model', '', 'Specifies the folder where trained model resides.')
cmd:option('-input', '', 'Path to the input file')
cmd:option('-sentence_size', 12, 'Max allowed length of sentences.')

local arguments = cmd:parse(arg)

function Testing()
   local Dictionary = require 'DataHandler.Dictionary'
   local DataSet = require 'DataHandler.DataSet'
   local SimpleCNN = require 'Model.SimpleCNN'

   assert(arguments.dictionary)
   assert(arguments.model)
   assert(arguments.input)

   -- Load Dictionary
   local dictionary = Dictionary(arguments.dictionary)

   -- Load Model
   local model = SimpleCNN {load_path = arguments.model}

   -- Options
   local options = {}
   options.sentence_size = arguments.sentence_size
   options.dictionary = dictionary
   
   -- Load Input
   local input = DataSet.Load(arguments.input, options)
   
   -- Testing
   local input_file = assert(torch.DiskFile(arguments.input, 'r'))
   for i = 1, input:size(1) do
      local input_vector = torch.Tensor(arguments.sentence_size, 
                                        dictionary.dimension)
      for j = 1, arguments.sentence_size do
         if 0 < input[i][j] then
            input_vector[j] = dictionary.vectors[input[i][j]]
         else
            input_vector[j]:fill(0)
         end
      end

      local result = torch.exp(model.network:forward(input_vector))
      result = result / (result[1] + result[2])
      text = input_file:readString('*l')
      print(string.format('%.4f - %s', result[1], text))
   end
   
   input_file:close()
end

Testing()

