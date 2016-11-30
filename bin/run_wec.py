#! /usr/bin/python

import os

def run(tracelist, algorithmlist, argvList):
    """ write file, fun erl
    """
    '''
    tracelist = ["finance", "websearch1", "websearch2", "build1", "build2",
				"cctv1", "cctv2", "as1", "as2", "mix", "test", "fbrd",
				"mds1", "lm", "datanode3", "builds", "fbrfa", "fbnw",
				"datanode3a", "cam2", "fbfs", "fbfsr", "fbn", "metanodea",
				"metanodej","metanodes","d1hs"]
    algorithmlist = ['lru', 'lfu', 'arc', 'larc', 'mq', 'lru_lwc']
    '''

    # TODO: fbrfa, mix, in lru_lwc have some bug, debug it

    # TODO: only this 9 traces can run successfully, find out why
    #tracelist = ['as2', 'websearch1', 'cctv1', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs']
    
    #tracelist = ['as2', 'cctv1', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs'] #'websearch1', 
    
    #tracelist = ['as2']


    # TODO: 'larc' failed on all the traces, find out why
    
    #algorithmlist = ['lru', 'lfu', 'arc', 'mq', 'lru_lwc']
    
    #algorithmlist = ['lru']

    #argvList = ['WhitelistLen', 'WindowLen', 'WindowNum', 'Thre1', 'ThreMulti']
    for i in range(5):
        os.popen("echo %i > %i.config" % (argvList[i], i))


    os.popen("date > ../log/_start_time")
    for trace in tracelist:
        os.popen("echo %s > trace.config" % trace)
        for algo in algorithmlist:
            os.popen("echo %s > algorithm.config" % algo)
            os.popen("date > ../log/_%s_%s_start_time" % (trace, algo))
            print ''
            print '============'
            print 'running ', trace, algo
            os.popen("erl -noshell -s wec_fr start_config -s init stop > ../log/%s_%s.out" % (trace, algo))
            os.popen("date > ../log/_%s_%s_end_time" % (trace, algo))

    os.popen("date > ../log/_end_time")


def multi_run(tracelist, algorithmlist, index = 0, argvListList = []):
    for argvList in argvListList:
        os.popen("rm -rf ../log")
        os.popen("mkdir ../log")
        print ''
        print "****************************"
        print "****", argvList, "****"
        print "****************************"
        
        run(tracelist, algorithmlist, argvList)

        logpath = "../log_%i" % index
        for x in argvList:
            logpath = "%s_%i" % (logpath, x)

        print os.popen("mv ../log %s" % logpath).readlines()
        #print "mv log %s" % logpath
        #exit()



if __name__ == '__main__':
    # 'WhitelistLen', 'WindowLen', 'WindowNum', 'Thre1', 'ThreMulti'

    # TODO: only this 9 traces can run successfully, find out why
    #tracelist = ['as2', 'websearch1', 'cctv1', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs']
    tracelist = ['as2', 'websearch1', 'cctv1', 'mix', 'fbrfa', 'fbnw', 'fbn', 'metanodej', 'd1hs']
    #tracelist = ['as2']
    #algorithmlist = ['lru', 'lru_lwc']
    #algorithmlist = ['lru_lwc']
    algorithmlist = ['lru']
    """
    argvListList = []
    for i in range(1,21):
        argvListList.append([i*10,i*10,3,2,2])
    multi_run(tracelist, algorithmlist, 7, argvListList)

    # TODO different paramaters have no influnce on other algorithms, run once is enough

    """

    argvListList = []
    for i in range(1,2):
        argvListList.append([500,i*10,3,2,2])
    multi_run(tracelist, algorithmlist, 11, argvListList)
    