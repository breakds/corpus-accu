import jieba
import os

def ProcessSingle(input_file, output_file):
    with open(input_file, "r") as f:
        with open(output_file, "a") as o:
            content = f.read().split()
            for sentence in content:
                segmentation = jieba.cut(sentence, cut_all=False)
                for seg in segmentation:
                    o.write(seg.encode('utf8'))
                    o.write(' ')

def ProcessFiles(directory, output_file):
    all_files = next(os.walk(directory))[2]
    i = 0
    for file_name in all_files:
        i = i + 1
        ProcessSingle(os.path.join(directory, file_name),
                      output_file)
        print '[Done] %d/%d' % (i, len(all_files))
    
    

        
        
    
