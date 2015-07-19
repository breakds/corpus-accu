require 'torch'
require 'nn'
require 'os'

---------- Command Line Options ----------
local cmd = torch.CmdLine()
cmd:option('-sentence_size', 20, 'Max allowed length of sentences.')


-- Dictionary
cmd:option('-dictionary', '/home/breakds/tmp/bilibili/input/word_vecs.bin', 
           'path to the word2vec binary dictionary.')
cmd:option('-save_dictionary', '', 
           'Save the dictionary object to the specified path.')

-- Training 
cmd:option('-positive', '', 'path to valid sentences dataset.')
cmd:option('-negative', '', 'path to invalid sentences dataset.')
cmd:option('-gen_negative', 0, 
           'when > 0, randomly generate specified number of invalid sentences.')
cmd:option('-save_training_set', '',
           'Save the training set to the specified path.')
cmd:option('-load_training_set', '',
           'Load the training set from the specified path.')

-- Model
cmd:option('-save_model', '', 'Specify path to store the trained model.')
cmd:option('-learning_rate', 0.001, 'The learning rate.')
cmd:option('-max_iteration', 50, 'The max per batch iteration.')
cmd:option('-max_epoch', 200, 'The max number of epoches.')
cmd:option('-batch_size', 5000, 'The number of sentences in a batch.')

local arguments = cmd:parse(arg)

-- Initialize the seed using /dev/urandom
torch.seed()

function LoadTraningSet(DataSet, dictionary)
   -- Assert that we have non empty positive data and negative data
   assert(('' ~= arguments.load_training_set) or
             ('' ~= arguments.positive and
                 ((0 < arguments.gen_negative) or 
                     ('' ~= arguments.negative))))

   local options = {}
   options.sentence_size = arguments.sentence_size
   options.dictionary = dictionary

   local buffer_table = {}
   buffer_table.input = {}
   buffer_table.target = {}

   -- Load base set
   if '' ~= arguments.load_training_set then
      print('[info] Loading base dataset ...')
      local input = torch.load(
         arguments.load_training_set .. '/input.torch')
      local target = torch.load(
         arguments.load_training_set .. '/target.torch')
      assert(input:size(1) == target:size(1))
      for i = 1, input:size(1) do
         table.insert(buffer_table.input, input[i])
         table.insert(buffer_table.target, target[i])
      end
      print('[ ok ] Base dataset loaded.')
   end

   -- Extra positive dataset
   local positive = nil
   if '' ~= arguments.positive then
      print('[info] Loading positive dataset ...')
      positive = DataSet.Load(arguments.positive, options)
      for i = 1, positive:size(1) do
         table.insert(buffer_table.input, positive[i])
         table.insert(buffer_table.target, 1)  -- label 1 = valid sentence
      end
      print('[ ok ] Positive dataset loaded.')
   end

   -- Extra negative dataset
   local negative = nil
   if arguments.gen_negative > 0 then
      print('[info] Generating negative dataset ...')
      negative = DataSet.GenNegative(arguments.gen_negative, options)
   elseif '' ~= arguments.negative then
      print('[info] Loading negative dataset ...')
      negative = DataSet.Load(arguments.negative, options)
   end

   if negative then
      for i = 1, negative:size(1) do
         table.insert(buffer_table.input, negative[i])
         table.insert(buffer_table.target, 2)  -- label 2 = invalid sentence
      end
      print('[info] Negative dataset loaded.')
   end

   
   -- Form training data
   local training_set = {}
   training_set.size = #buffer_table.input
   training_set.dictionary = dictionary
   training_set.input = torch.Tensor(#buffer_table.input, 
                                     arguments.sentence_size)
   training_set.target = torch.Tensor(#buffer_table.input)

   shuffle = torch.randperm(training_set.size)
   
   for i = 1, training_set.size do
      training_set.input[i] = buffer_table.input[shuffle[i]]
      training_set.target[i] = buffer_table.target[shuffle[i]]
   end

   -- Save training set
   if '' ~= arguments.save_training_set then
      os.execute('mkdir -p ' .. arguments.save_training_set)
      torch.save(arguments.save_training_set .. '/input.torch',
                 training_set.input)
      torch.save(arguments.save_training_set .. '/target.torch',
                 training_set.target)
      print(string.format('[ ok ] Training set saved to %s.',
                          arguments.save_training_set))
   end

   return training_set
end
   

function Training()
   ---------- Part 1: Initialization ----------
   assert(arguments.save_model)

   -- Load Package
   local Dictionary = require 'DataHandler.Dictionary'
   local DataSet = require 'DataHandler.DataSet'

   -- Load Dictionary
   print('[info] Loading dictionary ...')
   local dictionary = {}
   if not pcall(function () 
                   dictionary = torch.load(arguments.dictionary) 
                   print('[ ok ] Dictionary loaded.')
                end) then
      dictionary = Dictionary(arguments.dictionary)
      if '' ~= arguments.save_dictionary then
         torch.save(arguments.save_dictionary, dictionary)
         print(string.format('[ ok ] Dictionary saved to %s', 
                             arguments.save_dictionary))
      end
   end

   ---------- Part II: Load Traning Set ----------
   local training_set = LoadTraningSet(DataSet, dictionary)
   
   ---------- Part III: Construct Neural Network ----------
   local SimpleCNN = require 'Model.SimpleCNN'
   local model = SimpleCNN {
      input_frame_size = dictionary.dimension,
      sentence_size = training_set.input:size()[2],
      kernel_size = 5}
   print('[ ok ] Neural Network Constructed.')
   print('[info] Neural Network Summary:')
   print(model.network)


   ---------- Part IV: Traning ----------
   model:train(DataSet, training_set, 
               {
                  learningRate = arguments.learning_rate,
                  maxIteration = arguments.max_iteration,
                  maxEpoch = arguments.max_epoch,
                  batchSize = arguments.batch_size
               })

   ---------- Part V: Save Model ----------
   model:save(arguments.save_model)
end

Training()


