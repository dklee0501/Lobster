#!/bin/bash
echo " "
echo "Optimization logs will be saved in paper_result directory"
echo " "
echo "  Name      Old Depth            Carpov.el.al                       Lobster        "
echo "                           New Depth      Opt Time(s)      New Depth      Opt Time(s)"
../baseline/main.native ../baseline/paper_bench/hd07.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd07
 ./main.native paper_bench/hd07.eqn paper_cases/leave-hd07 > paper_result/hd07
../baseline/main.native ../baseline/paper_bench/hd08.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd08
 ./main.native paper_bench/hd08.eqn paper_cases/leave-hd08 > paper_result/hd08
../baseline/main.native ../baseline/paper_bench/hd09.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd09
 ./main.native paper_bench/hd09.eqn paper_cases/leave-hd09 > paper_result/hd09
../baseline/main.native ../baseline/paper_bench/hd10.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd10
 ./main.native paper_bench/hd10.eqn paper_cases/leave-hd10 > paper_result/hd10
../baseline/main.native ../baseline/paper_bench/hd11.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd11
 ./main.native paper_bench/hd11.eqn paper_cases/leave-hd11 > paper_result/hd11
../baseline/main.native ../baseline/paper_bench/hd12.eqn ../baseline/baseline_cases > ../baseline/paper_result/hd12
 ./main.native paper_bench/hd12.eqn paper_cases/leave-hd12 > paper_result/hd12

