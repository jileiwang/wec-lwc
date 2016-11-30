#! /usr/bin/python

import os
import sys
import time
import datetime

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
            start_file = "%s/_%s_%s_start_time" % (floder, trace, algo)
            if not os.path.exists(start_file):
                continue
            end_file = "%s/_%s_%s_end_time" % (floder, trace, algo)
            if not os.path.exists(end_file):
                continue

            # TODO

if __name__ == '__main__':
    #main()
    #find_legal_trace()
    #not_failed_traces()
    if len(sys.argv) < 2:
        print "Usage: ./analyse_log.py logfilepath"
    else:
        data = getData(sys.argv[1])
        showData(data)




