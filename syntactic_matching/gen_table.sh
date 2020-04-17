#!/bin/bash
echo " "
echo "Optimization logs will be saved in paper_result directory"
echo " "
echo "  Name      Old Depth     Lobster (Syntactic Matching)        "
echo "                           New Depth      Opt Time(s)"
 ./main.native paper_bench/cardio.eqn paper_cases/leave-cardio > paper_result/cardio
 ./main.native paper_bench/dsort.eqn paper_cases/leave-dsort > paper_result/dsort
 ./main.native paper_bench/hd01.eqn paper_cases/leave-hd01 > paper_result/hd01
 ./main.native paper_bench/hd02.eqn paper_cases/leave-hd02 > paper_result/hd02
 ./main.native paper_bench/hd03.eqn paper_cases/leave-hd03 > paper_result/hd03
 ./main.native paper_bench/hd04.eqn paper_cases/leave-hd04 > paper_result/hd04
 ./main.native paper_bench/hd05.eqn paper_cases/leave-hd05 > paper_result/hd05
 ./main.native paper_bench/hd06.eqn paper_cases/leave-hd06 > paper_result/hd06
 ./main.native paper_bench/hd07.eqn paper_cases/leave-hd07 > paper_result/hd07
 ./main.native paper_bench/hd08.eqn paper_cases/leave-hd08 > paper_result/hd08
 ./main.native paper_bench/hd09.eqn paper_cases/leave-hd09 > paper_result/hd09
 ./main.native paper_bench/hd10.eqn paper_cases/leave-hd10 > paper_result/hd10
 ./main.native paper_bench/hd11.eqn paper_cases/leave-hd11 > paper_result/hd11
 ./main.native paper_bench/hd12.eqn paper_cases/leave-hd12 > paper_result/hd12
 ./main.native paper_bench/bar.eqn paper_cases/leave-bar > paper_result/bar
 ./main.native paper_bench/cavlc.eqn paper_cases/leave-cavlc > paper_result/cavlc
 ./main.native paper_bench/ctrl.eqn paper_cases/leave-ctrl > paper_result/ctrl
 ./main.native paper_bench/i2c.eqn paper_cases/leave-i2c > paper_result/i2c
 ./main.native paper_bench/int2float.eqn paper_cases/leave-int2float > paper_result/int2float
 ./main.native paper_bench/dec.eqn paper_cases/leave-dec > paper_result/dec
 ./main.native paper_bench/router.eqn paper_cases/leave-router > paper_result/router
 ./main.native paper_bench/msort.eqn paper_cases/leave-msort > paper_result/msort
 ./main.native paper_bench/isort.eqn paper_cases/leave-isort > paper_result/isort
 ./main.native paper_bench/bsort.eqn paper_cases/leave-bsort > paper_result/bsort
 ./main.native paper_bench/osort.eqn paper_cases/leave-osort > paper_result/osort

