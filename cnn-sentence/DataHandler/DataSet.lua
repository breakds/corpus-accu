--- @module DataHandler.DataSet

-- Provides definition for the DataSet class.

require 'torch'

local DataSet = {}

local function GetFormat(path)
   local reverse_format = string.match(string.reverse(path), '%a+%.')
   if 'txt.' == reverse_format then
      return 'plain'
   else
      return 'torch'
   end
end

function DataSet.Load(path, options)
   -- default sentence length 12
   local sentence_size = options['sentence_size'] or 12
   
   -- format can be either 'torch' or 'plain'
   local format = options['format'] or GetFormat(path)

   local buffer = {}
   
   if 'plain' == format then
      -- In case it is reading plain sentences, we need the dictionary
      -- to encode/translate the words.
      assert(options['dictionary'])

      local input_file = assert(torch.DiskFile(path, 'r'))

      local sentence_count = 0
      local text = nil

      if not pcall(function() text = input_file:readString('*l') end) then
         -- Set text to nil on eof.
         text = nil
      end
      
      while text do
         sentence_count = sentence_count + 1
         
         local words = string.split(text, ' ')
         if #words > sentence_size then
            -- Print warning when the number of words in this
            -- sentence is greater than sentence length, since we
            -- are going to truncate it.
            print(string.format('[WARNING] Sentence length %d > %d, truncated.',
                                #words, sentence_size))
         end

         -- Build up word indices. Apply zero-padding when the
         -- sentence is not long enough, and truncate it if it is
         -- too long.
         local word_indices = {}
         for j = 1, sentence_size do
            if j <= #words then
               word_indices[j] = options['dictionary']:IndexOfWord(words[j])
            else
               word_indices[j] = 0
            end
         end
         buffer[sentence_count] = word_indices

         if not pcall(function() text = input_file:readString('*l') end) then
            -- Set text to nil on eof.
            text = nil
         end      
      end  -- end while text do

      input_file:close()

      local result = torch.Tensor(sentence_count, sentence_size)
   
      for i = 1, sentence_count do
         for j = 1, sentence_size do
            result[i][j] = buffer[i][j]
         end
      end

      return result

   else
      local result = torch.load(path)
      return result
   end  -- end if 'plain' == format
end

function DataSet.GenNegative(count, options)
   local sentence_size = options.sentence_size or 12
   local upper_bound = options.dictionary:size()

   local result = torch.Tensor(count, sentence_size)

   for i = 1, count do
      -- The non-zero part of word indices in the sentence to be
      -- generated.
      local actual_length = torch.ceil(torch.uniform(0, sentence_size))
      for j = 1, sentence_size do
         if j <= actual_length then
            result[i][j] = torch.ceil(torch.uniform(0, upper_bound))
         else
            result[i][j] = 0
         end
      end  -- end for j
   end  -- end for i
   
   return result
end

function DataSet.BatchIterator(options)
   assert(options.training_set)
   options.batch_size = options.batch_size or 1000

   local dictionary = options.training_set.dictionary
   local input = options.training_set.input
   
   local iterator = {}

   iterator.num_of_batches = torch.ceil(input:size()[1] / options.batch_size)
   iterator.current_batch = 1

   function iterator:fetch()
      -- If we pass the end of batches, return nil
      if self.current_batch > self.num_of_batches then
         return nil
      end
      
      local start = (self.current_batch - 1) * options.batch_size + 1

      -- It is possible that the last batch is smaller than usual.
      local actual_batch_size = options.batch_size
      if start + actual_batch_size - 1 > input:size()[1] then
         actual_batch_size = input:size()[1] - start + 1
      end

      local result = torch.Tensor(actual_batch_size, 
                                  input:size()[2],
                                  dictionary.dimension)

      for i = start, start + actual_batch_size - 1 do
         for j = 1, input:size()[2] do
            if 0 < input[i][j] then
               result[i - start + 1][j] = dictionary.vectors[input[i][j]]
            else 
               -- zero padding if the sentence is not long enough
               result[i - start + 1][j]:fill(0)
            end
         end  -- end for j
      end  -- end for i
                                  
      return result
   end  -- end function iterator:fetch

   function iterator:next()
      self.current_batch = self.current_batch + 1
   end

   return iterator
end

-- For nn.StochasticGradient
function DataSet.BatchIterator1(options)
   assert(options.training_set)
   options.batch_size = options.batch_size or 1000

   local dictionary = options.training_set.dictionary
   local input = options.training_set.input
   local target = options.training_set.target
   
   local iterator = {}

   iterator.num_of_batches = torch.ceil(input:size()[1] / options.batch_size)
   iterator.current_batch = 1

   function iterator:fetch()
      -- If we pass the end of batches, return nil
      if self.current_batch > self.num_of_batches then
         return nil
      end
      
      local start = (self.current_batch - 1) * options.batch_size + 1

      -- It is possible that the last batch is smaller than usual.
      local actual_batch_size = options.batch_size
      if start + actual_batch_size - 1 > input:size()[1] then
         actual_batch_size = input:size()[1] - start + 1
      end

      local result = {}

      for i = start, start + actual_batch_size - 1 do
         result[i - start + 1] = {}
         result[i - start + 1][1] = 
            torch.Tensor(input:size()[2], dictionary.dimension)
         result[i - start + 1][2] = target[i]
         for j = 1, input:size()[2] do
            if 0 < input[i][j] then
               result[i - start + 1][1][j] = dictionary.vectors[input[i][j]]
            else 
               -- zero padding if the sentence is not long enough
               result[i - start + 1][1][j]:fill(0)
            end
         end  -- end for j
      end  -- end for i

      function result:size()
         return actual_batch_size
      end
                                  
      return result
   end  -- end function iterator:fetch

   function iterator:next()
      self.current_batch = self.current_batch + 1
   end

   return iterator
end

return DataSet
