-module(smp).
-compile(export_all).



start() ->
% start(Trace, Alg, RamCacheSize, SsdCacheSize, Logname) 
  spawn(wec_fr, start, [metanodej, fifo, 5, 40, metanodej ]),
  spawn(wec_fr, start, [fbrfa, fifo, 104, 5151, fbrfa ]),
  spawn(wec_fr, start, [fbn, fifo, 48, 2398, fbn ]),
  spawn(wec_fr, start, [cctv1, fifo, 79, 3927, cctv1 ]),

  spawn(wec_fr, start, [mix, fifo, 196, 1955, mix_1 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 3909, mix_2 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 5863, mix_3 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 7817, mix_4 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 9772, mix_5 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 11726, mix_6 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 13680, mix_7 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 15634, mix_8 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 17589, mix_9 ]),
  spawn(wec_fr, start, [mix, fifo, 196, 19543, mix_10 ]).
  
  


  

