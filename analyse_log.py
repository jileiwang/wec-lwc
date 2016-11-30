#! /usr/bin/python

import os
import sys

def getData(floder):
    """ write file, fun erl
    """
    '''
    tracelist = ["finance", "websearch1", "websearch2", "build1", "build2",
                "cctv1", "cctv2", "as1", "as2", "mix", "test", "fbrd",
                "mds1", "lm", "datanode3", "builds", "fbrfa", "fbnw",
                "datanode3a", "cam2", "fbfs", "fbfsr", "fbn", "metanodea",
                "metanodej","metanodes","d1hs"]
    '''

    # TODO: fbrfa, mix, in lru_lwc have some bug, debug it

    # TODO: only this 9 traces can run successfully, find out why
    tracelist = ['websearch1', 'cctv1', 'as2', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs']
    # TODO: 'larc' failed on all the traces, find out why
    algorithmlist = ['lru', 'lfu', 'arc', 'mq', 'lru_lwc']
    

    dataDic = {}
    for trace in tracelist:
        dataDic[trace] = {}
        for algo in algorithmlist:

            print "reading ./%s/%s_%s.out" % (floder, trace, algo)

            dataDic[trace][algo] = {}

            inputfile = open("./%s/%s_%s.out" % (floder, trace, algo), 'r')
            lines = inputfile.readlines()
            #print lines[0]
            inputfile.close()

            if "init terminating in do_boot" in lines[-1]:
                print "ERROR in ./%s/%s_%s.out" % (floder, trace, algo)
                continue

            for i in range(len(lines)):
                if '*******' in lines[i]:
                    break
            
            # read_req, ram_hit, ssd_hit, eb_hit, eb_write
            pos = i + 1
            #print lines[pos]
            items = lines[pos].split(',')
            for item in items:
                tokens = item.split('=')
                dataDic[trace][algo][tokens[0].strip()] = int(tokens[1])

            # Total hit rate
            for i in range(4):
                pos = pos + 1
                #print lines[pos]
                tokens = lines[pos].split('=')
                dataDic[trace][algo][tokens[0].strip()] = float(tokens[1][:-2])

            # SsdAddIn
            pos = pos + 2
            #print lines[pos]
            items = lines[pos].split(',')
            for item in items:
                tokens = item.split('=')
                dataDic[trace][algo][tokens[0].strip()] = int(tokens[1])

            # Write Amply Rate
            pos = pos + 1
            #print lines[pos]
            tokens = lines[pos].split('=')
            dataDic[trace][algo][tokens[0].strip()] = float(tokens[1][:-2])

            # Ssd_evict_noaccessblok_num
            pos = pos + 1
            #print lines[pos]
            items = lines[pos].split(',')
            tokens = items[0].split('=')
            dataDic[trace][algo][tokens[0].strip()] = int(tokens[1])
            tokens = items[1].split('=')
            dataDic[trace][algo][tokens[0].strip()] = float(tokens[1][:-2])

            # 
            pos = pos + 1
            #print lines[pos]
            tokens = lines[pos].split('=')
            dataDic[trace][algo][tokens[0].strip()] = int(tokens[1])
    #print dataDic

    # show data
    return dataDic




def not_failed_traces():
    faild_traces = {
        1:['as2', 'fbn', 'websearch1', 'd1hs', 'cctv1', 'metanodej', 'fbnw'],
        2:['mix', 'fbrfa'],
        3:[],
        4:[],
        5:[],
        6:['as1', 'datanode3', 'build2', 'build1', 'cam2', 'lm', 'websearch2', 'fbfs', 'test', 'datanode3a', 'finance', 'metanodes', 'fbfsr', 'metanodea', 'builds', 'fbrd', 'cctv2', 'mds1']
    }
    tracelist = ["finance", "websearch1", "websearch2", "build1", "build2",
                "cctv1", "cctv2", "as1", "as2", "mix", "test", "fbrd",
                "mds1", "lm", "datanode3", "builds", "fbrfa", "fbnw",
                "datanode3a", "cam2", "fbfs", "fbfsr", "fbn", "metanodea",
                "metanodej","metanodes","d1hs"]
    not_failed = []
    for t in tracelist:
        if not t in faild_traces[6]:
            not_failed.append(t)
    print not_failed


def showData(data):
    tracelist = ['websearch1', 'cctv1', 'as2', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs']
    algorithmlist = ['lru', 'lfu', 'arc', 'mq', 'lru_lwc']

    for trace in tracelist:
        print ""
        print "====", trace, "===="
        for algo in algorithmlist:
            #print data[trace][algo].keys()
            if len(data[trace][algo].keys()) == 0:
                continue
            print algo, "\t",
            if algo != 'lru_lwc':
                print '\t',
            print data[trace][algo]["Ssd Hit Rate"], "\t",
            print data[trace][algo]["SsdAddIn"]


def find_legal_trace():
    """
    find out which trace we can use
    """

    filenames = os.popen('grep -n "init terminating in do_boot" ./log_01/* | awk -F : \'{print $1}\' | awk -F / \'{print $3}\'').readlines()
    dic = {}
    for filename in filenames:
        filename = filename[:-4]
        tokens = filename.split('_')
        if tokens[0] in dic.keys():
            dic[tokens[0]].append(tokens[1])
        else:
            dic[tokens[0]] = [tokens[1]]
    
    result = {}
    for i in range(1,7):
        result[i] = []
    for key in dic.keys():
        #print len(dic[key])
        #print dic[key]
        result[len(dic[key])].append(key)

    for key in range(1,7):
        print result[key]
        #for 
        #line = result[key].join('", "')
        #line = '"' + line + '"'
        #print line

if __name__ == '__main__':
    #main()
    #find_legal_trace()
    #not_failed_traces()
    if len(sys.argv) < 2:
        print "Usage: ./analyse_log.py logfilepath"
    else:
        data = getData(sys.argv[1])
        showData(data)




