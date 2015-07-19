import jieba
import os

# Note that min_length is the minimum number of chinese characters. A
# sentence with n chinese characters's string length is 3n.
def ProcessSingle(input_file, output_file, min_length = 6):
    with open(input_file, "r") as f:
        with open(output_file, "a") as o:
            content = f.read().split()
            num = 0
            for sentence in content:
                num = num + 1
                if 0 == num % 100000:
                    print "%8d/%d" % (num, len(content))
                if len(sentence) > min_length * 3:
                    segmentation = jieba.cut(sentence, cut_all=False)
                    for seg in segmentation:
                        o.write(seg.encode('utf8'))
                        o.write(' ')
                    o.write('\n')

def TestLength(input_file):
    with open(input_file, "r") as f:
        content = f.read().split()
        i = 0
        for sentence in content:
            print sentence, len(sentence)
            i = i + 1
            if i > 100:
                break
        
    

def ProcessFiles(directory, output_file):
    all_files = next(os.walk(directory))[2]
    i = 0
    for file_name in all_files:
        i = i + 1
        ProcessSingle(os.path.join(directory, file_name),
                      output_file)
        print '[Done] %d/%d' % (i, len(all_files))
    
    

        
        
    
