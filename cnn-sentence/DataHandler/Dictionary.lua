--- @module DataLoader.Dictionary

-- Provides definitions for the Dictionary class, which stores:
-- 1) The map from word strings to their indices.
-- 2) The map from word indices to their vector representation.

require 'torch'

-- The package/class variable
local Dictionary = {}

--------------------  Member Methods --------------------
function Dictionary:size()
   return self['num_words']
end

function Dictionary:dimension()
   return self['dimension']
end

function Dictionary:IndexOfWord(word)
   return self['vocabulary'][word]
end


-- Tranlate a word indices vector into a sentence. This is for debug
-- only and is REALLY slow.
function Dictionary:Translate(word_indices)
   local result = {}
   for i = 1, word_indices:size()[1] do
      for word, index in pairs(self.vocabulary) do
         if index == word_indices[i] then
            result[i] = word
            break
         end
      end  -- end for word, index
   end  -- end for i
   return table.concat(result, ' ')
end

-- Make Dictionary itself a callable, so that we have a constructor.
-- The constructor loads the word2vec binary file from path.
local metatable = {
   __call = function(first, path)
      local self = {}
      setmetatable(self, {__index = Dictionary})

      -- Read the header (number of words, word vector dimension)
      local input_file = assert(torch.DiskFile(path, 'r'))
      input_file:binary() -- Force binary format.

      local header = string.split(input_file:readString('*l'), ' ')
      self['num_words'] = tonumber(header[1])
      self['dimension'] = tonumber(header[2])
      self['vocabulary'] = {}
      self['vectors'] = torch.Tensor(self['num_words'], self['dimension'])
      vocabulary = self['vocabulary']

      for i = 1, self['num_words'] do
         local word = ''
         current = input_file:readByte()

         -- Read Word
         -- Stop at space (32)
         while 32 ~= current do
            -- Skip newline (10)
            if 10 ~= current then
               word = word .. string.char(current)
            end
            current = input_file:readByte()
         end
         self['vocabulary'][word] = i
         
         for j = 1, self['dimension'] do
            self['vectors'][i][j] = input_file:readFloat()
         end
         
         -- Progress Message
         if 0 == i % 10000 then
            xlua.progress(i, self.num_words)
         end
      end
      
      -- Complete Message
      print()
      print('[Load Complete]')
      
      assert(input_file:close())
      return self
   end
}
setmetatable(Dictionary, metatable)

return Dictionary
   
   
