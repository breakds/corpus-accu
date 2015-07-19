--- @module Model.SimpleCNN

require 'torch'
require 'nn'

-- The package/class variable
local SimpleCNN = {}

function SimpleCNN:train(DataSet, training_set, options)
   local optimizer = nn.StochasticGradient(self.network, self.criterion)
   optimizer.learningRate = options.learningRate or 0.001
   optimizer.maxIterator = options.maxIteration or 50
   
   local maxEpoch = options.maxEpoch or 50
   
   for epoch = 1, maxEpoch do
      local iterator = DataSet.BatchIterator1 {
         training_set = training_set,
         batch_size = options.batchSize or 1000
      }

      for i = 1, iterator.num_of_batches do
         local batch = iterator:fetch()
         print(batch:size())
         optimizer:train(batch)
         iterator:next()
      end
      print(string.format('[ ok ] Epoch %d/%d done.', 
                          epoch, maxEpoch))
   end  -- for epoch
end

function SimpleCNN:save(path)
   torch.save(path .. '/encoder.model', self.encoder)
   torch.save(path .. '/classifier.model', self.classifier)
   torch.save(path .. '/criterion.model', self.criterion)
   print(string.format('[ ok ] Model saved to %s', path))
end

-- Simple CNN Constructor
local metatable = {
   __call = function(first, options)
      local self = {}
      setmetatable(self, {__index = SimpleCNN})

      -- if load_path is specified, just load it
      if options.load_path then
         local path = options.load_path
         self.encoder = torch.load(path .. '/encoder.model')
         self.classifier = torch.load(path .. '/classifier.model')
         self.network = nn.Sequential()
         self.network:add(self.encoder)
         self.network:add(self.classifier)
         self.criterion = torch.load(path .. '/criterion.model')
         return self
      end
      
      -- word vector dimension (required)
      assert(options.input_frame_size)  

      -- the size of sentences
      assert(options.sentence_size)
      
      -- number of convolutions (optional)
      options.num_convolusions = options.num_convolusions or 160

      -- the kernel size
      options.kernel_size = options.kernel_size or 5

      -- the feature layer output dimension
      options.feature_size = options.feature_size or 100

      -- the dropout rate
      options.dropout = options.dropout or 0.5

                     
      -- Encoder
      self.encoder = nn.Sequential()
      self.encoder:add(nn.TemporalConvolution(
                          options.input_frame_size,
                          options.num_convolusions,
                          options.kernel_size))
      self.encoder:add(nn.ReLU())
      self.encoder:add(nn.TemporalMaxPooling(
                          options.sentence_size - options.kernel_size + 1))
      self.encoder:add(nn.Linear(options.num_convolusions, options.feature_size))
      self.encoder:add(nn.ReLU())
      
      -- Classifier
      self.classifier = nn.Sequential()
      self.classifier:add(nn.Dropout(options.dropout))
      self.classifier:add(nn.Linear(options.feature_size, 2))
      self.classifier:add(nn.View(2))
      self.classifier:add(nn.LogSoftMax())
      
      -- Integrated
      self.network = nn.Sequential()
      self.network:add(self.encoder)
      self.network:add(self.classifier)

      -- Criterion
      self.criterion = nn.ClassNLLCriterion()
      
      return self
   end
}
setmetatable(SimpleCNN, metatable)

return SimpleCNN
