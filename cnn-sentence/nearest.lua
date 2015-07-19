require 'torch'
require 'nn'
require 'os'

---------- Command Line Options ----------
local cmd = torch.CmdLine()
cmd:option('-dictionary', '/home/breakds/tmp/bilibili/input/word_vecs.bin', 
           'path to the word2vec binary dictionary.')
cmd:option('-model', '', 'Specifies the folder where trained model resides.')
cmd:option('-template', '', 
           'Specifies the path to the file that contains predefined templates.')
cmd:option('-input', '', 'Path to the input file')
cmd:option('-sentence_size', 20, 'Max allowed length of sentences.')

local arguments = cmd:parse(arg)


function LoadData(DataSet, dictionary, model, path)
   local options = {}
   options.sentence_size = arguments.sentence_size
   options.dictionary = dictionary
   
   local input = DataSet.Load(path, options)
   
   local data = {}
   data.feature = {}
   data.text = {}
   
   local input_file = assert(torch.DiskFile(path, 'r'))

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

      local feature = model.encoder:forward(input_vector)

      -- Normalization
      feature = feature / torch.sqrt(torch.dot(feature, feature))
      
      local text = input_file:readString('*l')

      table.insert(data.feature, feature)
      table.insert(data.text, text)
   end
   
   input_file:close()

   function data:size()
      return #self.feature
   end

   return data
end

function Testing()
   local Dictionary = require 'DataHandler.Dictionary'
   local DataSet = require 'DataHandler.DataSet'
   local SimpleCNN = require 'Model.SimpleCNN'

   assert(arguments.dictionary)
   assert(arguments.model)
   assert(arguments.input)
   assert(arguments.template)
   

   -- Load Dictionary
   local dictionary = Dictionary(arguments.dictionary)

   -- Load Model
   local model = SimpleCNN {load_path = arguments.model}

   -- Load Templates and Inputs
   local template = LoadData(DataSet, dictionary, model, arguments.template)
   local input = LoadData(DataSet, dictionary, model, arguments.input)

   for i = 1, input:size() do
      local distances = torch.Tensor(template:size())
      for j = 1, template:size() do
         distances[j] = torch.dot(input.feature[i], template.feature[j])
      end
      
      new_distances, sorted_indices = torch.sort(distances, 1, true)

      -- Output
      print('--------------------------------------------------')
      print(input.text[i])
      
      for j = 1, template:size() do
         print(string.format('%.5f  %s', 
                             distances[sorted_indices[j]],
                             template.text[sorted_indices[j]]))
      end

      print('')
   end
end

Testing()

