#!/bin/bash
echo " "
echo "Optimization logs will be saved in paper_result directory"
echo " "
echo "  Name      Old Depth            Carpov.el.al                       Lobster        "
echo "                           New Depth      Opt Time(s)      New Depth      Opt Time(s)"

../baseline/main.native ../baseline/paper_bench/cardio.eqn ../baseline/baseline_cases > ../baseline/paper_result/cardio
 ./main.native paper_bench/cardio.eqn paper_cases/leave-cardio > paper_result/cardio
../baseline/main.native ../baseline/paper_bench/dsort.eqn ../baseline/baseline_cases > ../baseline/paper_result/dsort
 ./main.native paper_bench/dsort.eqn paper_cases/leave-dsort > paper_result/dsort 
../baseline/main.native ../baseline/paper_bench/hd01.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd01
 ./main.native paper_bench/hd01.eqn paper_cases/leave-hd01 > paper_result/hd01
../baseline/main.native ../baseline/paper_bench/hd02.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd02
 ./main.native paper_bench/hd02.eqn paper_cases/leave-hd02 > paper_result/hd02
../baseline/main.native ../baseline/paper_bench/hd03.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd03
 ./main.native paper_bench/hd03.eqn paper_cases/leave-hd03 > paper_result/hd03
../baseline/main.native ../baseline/paper_bench/hd04.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd04
 ./main.native paper_bench/hd04.eqn paper_cases/leave-hd04 > paper_result/hd04
../baseline/main.native ../baseline/paper_bench/hd05.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd05
 ./main.native paper_bench/hd05.eqn paper_cases/leave-hd05 > paper_result/hd05
../baseline/main.native ../baseline/paper_bench/hd06.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd06
 ./main.native paper_bench/hd06.eqn paper_cases/leave-hd06 > paper_result/hd06

