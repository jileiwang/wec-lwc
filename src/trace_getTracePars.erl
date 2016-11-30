-module (trace_getTracePars).
-export ([getTraceLen/1,getUCLN/1,getTraceFile/1,getAverage/1]).

getTraceLen(Trace) ->
  case Trace of
    finance -> 1555474;
    websearch1 -> 5047596;
    websearch2 -> 4742197;
    build1 -> 927691;
    build2 -> 957596;
    cctv1 -> 550310;
    cctv2 -> 508280;
    as1 -> 409918;
    as2 -> 215678;
    mix -> 3832519;
    test -> 10;
    fbrd -> 1447580;
    mds1 -> 1554727;
    lm -> 1563865;
    datanode3 -> 392459;
    builds -> 1058504;      
    fbrfa -> 2000000;
    fbnw -> 415195;
    datanode3a -> 1291420;
    cam2 -> 3705175;
    fbfs ->  2000000;
    fbfsr -> 2000000;
    fbn -> 312452;
    metanodea ->  144025;
    metanodej -> 554561;
    metanodes ->  91640;
    d1hs-> 419723

  end.


  getFilePath1() -> "../trace/".


getTraceFile0() -> getFilePath1() ++ "test.req". 
getTraceFile1() -> getFilePath1() ++ "spc-financial-150w-4K.req".
getTraceFile2() -> getFilePath1() ++ "spc-websearch1-500w-4K.req".
getTraceFile3() -> getFilePath1() ++ "spc-websearch2-470w-4K.req".
getTraceFile4() -> getFilePath1() ++ "production-build00-1-4K.req".
getTraceFile5() -> getFilePath1() ++ "production-build00-2-4K.req".
getTraceFile6() -> getFilePath1() ++ "as1-4K.req".
getTraceFile7() -> getFilePath1() ++ "as2-4K.req".
getTraceFile8() -> getFilePath1() ++ "cctv1-5h-40K.req". 
getTraceFile9() -> getFilePath1() ++ "cctv2-4h-40K.req".
getTraceFile10() -> getFilePath1() ++ "mix-trace.req". 

getTraceFile(Trace) ->
  case Trace of
    finance -> getTraceFile1();
    websearch1 -> getTraceFile2();
    websearch2 -> getTraceFile3();
    build1 -> getTraceFile4();
    build2 -> getTraceFile5();
    as1 -> getTraceFile6();
    as2 -> getTraceFile7();
    cctv1 -> getTraceFile8();
    cctv2 -> getTraceFile9();
    mix -> getTraceFile10();
    test -> getTraceFile0();
    fbrd -> getFilePath1() ++ "filebench-randomread.req";
    fbrfa -> getFilePath1() ++ "filebench-randomfileaccess.req";
    generate -> getFilePath1() ++ "cctv-generated.req";
    fbnw -> getFilePath1() ++ "filebench-networkfs.req";
    mds1 -> getFilePath1() ++ "cambridge-mds1-155w-4K.req";
    msnfs -> getFilePath1() ++ "production-MSN-FS-4k.req";
      lm -> getFilePath1() ++ "production-LiveMap-Backend-4K.req";
    datanode3 -> getFilePath1() ++ "datanode3-hive-select.req";    
    cam1 -> getFilePath1() ++ "CAM-01-SRV-lvm0.req";
    builds -> getFilePath1() ++ "24.hour.BuildServer.11-28-2007.07-39-PM.trace.req";
    datanode3a -> getFilePath1() ++ "datanode3-hive-aggregation.req";
    cam2 -> getFilePath1() ++ "CAM-02-SRV-lvm0.req";
    fbfs -> getFilePath1() ++ "filebench-fileserver.req";
    fbfsr -> getFilePath1() ++ "filebench-fivestreamread.req";
    fbn -> getFilePath1() ++ "filebench-netsfs.req";
    metanodea -> getFilePath1() ++ "metanode-hive-aggregation.req";
    metanodej -> getFilePath1() ++ "metanode-hive-join.req";
    metanodes -> getFilePath1() ++ "metanode-hive-select.req";
    usr2 -> getFilePath1() ++ "cambridge-usr2-22w-10k.req";
    cweb2 -> getFilePath1() ++ "cambridge-web2-36w-10K.req";
    d1hs -> getFilePath1() ++ "datanode1-hive-select.req"

  end.

  getUCLN(Trace) ->
  case Trace of
    finance -> 298;
    websearch1 -> 5431;
    websearch2 -> 5421;
    build1 -> 904;
    build2 -> 1129;
    cctv1 -> 78530;
    % cctv1 -> 110000;
    %cctv2 -> 69677;
    cctv2 -> 100000;
    as1 -> 1695;
    as2 -> 1347;
    mix -> 195423;
    test -> 10000;
    generate -> 118268;
    fbrd -> 639858;
    mds1 -> 952005;
    datanode3a -> 1713;
    builds -> 12374;
    cam2 -> 430228;    
    fbfs -> 414708;
    fbfsr -> 1057383;
    fbn -> 47949;
    fbnw -> 42045;
    fbrfa -> 103003;
    metanodes -> 631;
    metanodej -> 785;
    metanodea -> 641;
    d1hs -> 924
    
  end.


 getAverage(Trace) ->
 case Trace of
 	websearch1 ->
 		4.704;
    as2 -> 33.21;
    datanode3a -> 32.01;
    finance -> 37.9587;
    metanodea-> 46.637;
    metanodes -> 30.872;
    test -> 100
    
 end.