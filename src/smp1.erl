-module(smp1).
-compile(export_all).

start() ->
%  start(Alg,Trace, RAM_Perc, SSD_Perc,Throttling, Log, Coefficient, Warmup, Sleep, AlgOfSSD, EnableMC, VQ, VQAlg) ->

        
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1000,  mix_lazy1, {0.1, 3, 3, 0.4}, 3, {50, 15}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1000,  mix_lazy2, {0.1, 3, 3, 0.4}, 3, {50, 20}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1000,  mix_lazy3, {0.1, 3, 3, 0.4}, 3, {50, 25}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1000,  mix_lazy4, {0.1, 3, 3, 0.4}, 3, {50, 30}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1000,  mix_lazy5, {0.1, 3, 3, 0.4}, 3, {50, 35}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1000,  mix_lazy6, {0.1, 3, 3, 0.4}, 3, {50, 40}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 700,  mix_update1, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 800,  mix_update2, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 900,  mix_update3, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1100,  mix_update4, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1200,  mix_update5, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1300,  mix_update6, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]).

        % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.03, 800,  mix_lfu_fifo_3, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.04, 800,  mix_lfu_fifo_4, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.05, 1000,  mix_lfu_fifo_5, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.06, 1100,  mix_lfu_fifo_6, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.07, 1200,  mix_lfu_fifo_7, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.08, 1500,  mix_lfu_fifo_8, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.09, 1500,  mix_lfu_fifo_9, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, mix, 0.001, 0.1, 1500,  mix_lfu_fifo_10, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, metanodej, 0.001, 0.05, 5,  metanodej, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, fbn, 0.001, 0.05, 1500, fbn, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, fbrfa, 0.001, 0.05, 500,  fbrfa, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]),
        % spawn(wec_frlgb, start, [lgb, cctv1, 0.001, 0.05, 500,  cctv1, {0.1, 3, 3, 0.4}, 3, {50, 10}, {lfu, fifo}, true, true, lfu]).

        