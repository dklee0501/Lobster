#!/bin/bash
echo " "
echo "Optimization logs will be saved in paper_result directory"
echo " "
echo "  Name      Old Depth            Carpov.el.al                       Lobster        "
echo "                           New Depth      Opt Time(s)      New Depth      Opt Time(s)"
../baseline/main.native ../baseline/paper_bench/bar.eqn ../baseline/baseline_cases > ../baseline/paper_result/bar
./main.native paper_bench/bar.eqn paper_cases/leave-bar > paper_result/bar
../baseline/main.native ../baseline/paper_bench/cavlc.eqn ../baseline/baseline_cases > ../baseline/paper_result/cavlc
./main.native paper_bench/cavlc.eqn paper_cases/leave-cavlc > paper_result/cavlc
../baseline/main.native ../baseline/paper_bench/ctrl.eqn ../baseline/baseline_cases > ../baseline/paper_result/ctrl
./main.native paper_bench/ctrl.eqn paper_cases/leave-ctrl > paper_result/ctrl
../baseline/main.native ../baseline/paper_bench/dec.eqn ../baseline/baseline_cases > ../baseline/paper_result/dec
./main.native paper_bench/dec.eqn paper_cases/leave-dec > paper_result/dec

