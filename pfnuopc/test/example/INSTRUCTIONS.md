# LND-ParFlow NUOPC Test Application Instructions

## Configuration
1. edit nuopc_test.cfg

| hydAttributes      | Description                                    |
| ------------------ | ---------------------------------------------- |
| Verbosity          | integer interpreted as a bit field             |
| Diagnostic         | integer interpreted as a bit field             |
| coord_type         | GRD_COORD_SPHDEG_LW or GRD_COORD_CARTESIAN_LW  |
| water_flux_3d      | true, false                                    |
| water_flux_2d      | true, false                                    |

| hydAttributes      | Description                                 |
| ------------------ | ------------------------------------------- |
| Verbosity          | integer interpreted as a bit field          |
| Diagnostic         | integer interpreted as a bit field          |
| realize_all_import | true, false                                 |
| realize_all_export | true, false                                 |
| filename           | ParFlow configuration file                  |
| initialize_import  | true, false                                 |
| initialize_export  | true, false                                 |
| check_import       | check import time stamp                     |
| coord_type         | GRD_COORD_CARTESIAN or GRD_COORD_CLMVEGTF   |
| coord_filename     | file used when coordinates loaded from file |
| output_directory   | ParFlow cap output directory                |

## Execution
```
mpirun -np 4 ./parflow-nuopc-test
```

## Slurm Workload Manager
1. edit slurm_template.sh
    - partition=\<partition\_names\>
    - account=\<account\>
    - constraint=\<list\>
    - qos=\<qos\>
    - setup environment as needed
```
sbatch slurm_template.sh
```

## PBS Workload Manager
1. edit pbs_template.sh
    - \-A \<account\>
    - \-q \<queue\>
    - setup environment as needed
```
qsub pbs_template.sh
```

## Validation
Successful execution produces evaptrans, press, and satur files through 00003.

[Slurm Documentation](https://slurm.schedmd.com/documentation.html)

[PBS Pro Documentation](https://www.altair.com/pbs-works-documentation)
