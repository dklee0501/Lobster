#!/bin/bash
echo " "
echo "Optimization logs will be saved in paper_result directory"
echo " "
echo "  Name      Old Depth            Carpov.el.al                       Lobster        "
echo "                           New Depth      Opt Time(s)      New Depth      Opt Time(s)"

../baseline/main.native ../baseline/paper_bench/msort.eqn ../baseline/baseline_cases > ../baseline/paper_result/msort
./main.native paper_bench/msort.eqn paper_cases/leave-msort > paper_result/msort
../baseline/main.native ../baseline/paper_bench/isort.eqn ../baseline/baseline_cases > ../baseline/paper_result/isort
./main.native paper_bench/isort.eqn paper_cases/leave-isort > paper_result/isort





