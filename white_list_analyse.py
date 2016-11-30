#! /usr/bin/python

import os

def multi_run(tracelist, algorithmlist, index = 0, argvListList = []):
    data = {}
    for argvList in argvListList:
        logpath = "./log_%i" % index
        for x in argvList:
            logpath = "%s_%i" % (logpath, x)

        data[argvList[1]] = {}
        for trace in tracelist:
            line = os.popen("grep -n WhiteList %s/%s_*.out " % (logpath, trace)).readlines()[0]
            data[argvList[1]][trace] = int(line.split(':')[-1])

    # print data
    print "n", "\t",
    for trace in tracelist:
        print trace, '\t',
    print ""
    for argvList in argvListList:
        print argvList[1], '\t',
        for trace in tracelist:
            print data[argvList[1]][trace], '\t',
        print ""
        



if __name__ == '__main__':
    # 'WhitelistLen', 'WindowLen', 'WindowNum', 'Thre1', 'ThreMulti'

    # TODO: only this 9 traces can run successfully, find out why
    #tracelist = ['as2', 'websearch1', 'cctv1', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs']
    tracelist = ['as2', 'websearch1', 'cctv1', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs']
    #tracelist = ['as2']
    #algorithmlist = ['lru', 'lru_lwc']
    algorithmlist = ['lru_lwc']
    """
    argvListList = []
    for i in range(1,21):
        argvListList.append([i*10,i*10,3,2,2])
    multi_run(tracelist, algorithmlist, 7, argvListList)

    # TODO different paramaters have no influnce on other algorithms, run once is enough

    """
    
    argvListList = []
    for i in range(1,21):
        argvListList.append([500,i*10,3,2,2])
    multi_run(tracelist, algorithmlist, 9, argvListList)
    