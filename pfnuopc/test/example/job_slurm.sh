#!/usr/bin/csh
#SBATCH --job-name=Lnd-ParFlow-Test
#SBATCH --ntasks=1
#SBATCH --time=00:10:00
#SBATCH --partition=<partition_names>
#SBATCH --account=<account>
#SBATCH --constraint=<list>
#SBATCH --qos=<qos>
#SBATCH --output=slurm.%j.log

# environment settings
#   TODO

# set umask
umask 022
# set limits
limit cputime      unlimited
limit filesize     unlimited
limit datasize     unlimited
limit stacksize    unlimited
limit coredumpsize unlimited
limit memoryuse    unlimited

echo "Job ID: ${SLURM_JOB_ID}"

set s_tm=`date +%s`
set s_hr=`date +%H`; set s_mn=`date +%M`; set s_sc=`date +%S`
echo "Model Start    ${s_hr}:${s_mn}:${s_sc}"

mpirun -np 1 ./parflow-nuopc-test
set exec_s=$status

set e_tm=`date +%s`
set e_hr=`date +%H`; set e_mn=`date +%M`; set e_sc=`date +%S`
echo "Model End      ${e_hr}:${e_mn}:${e_sc}"

@ r_tm=(${e_tm} - ${s_tm})
@ r_hr=($r_tm / 3600)
@ r_mn=((${r_tm} % 3600) / 60)
@ r_sc=(${r_tm} % 60)
echo "Model Runtime  ${r_hr}:${r_mn}:${r_sc}"

if ($exec_s != 0) then
  echo "RESULT: ERROR ${exec_s}"
else
  echo "RESULT: SUCCESS"
endif
