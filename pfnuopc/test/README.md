# LND-ParFlow NUOPC Test Application

## Description
A coupled LND-ParFlow application used to test the ParFlow NUOPC cap.

## Build
The parflow\_nuopc\_test executable will build with
-DPARFLOW\_ENABLE\_NUOPC=ON and will install into the
-DCMAKE\_INSTALL\_PREFIX/bin directory.

## Execution
```
    mpirun -np <# of processes> ./parflow_nuopc_test
```

## Output Files
| File                | Description                      |
| ------------------- | -------------------------------- |
| PET\*.ESMF\_LogFile | ESMF errors will be written here |

## Source Files
| File                     | Description                 |
| ------------------------ | --------------------------- |
| CMakeLists.txt           | CMake configuration file    |
| pf\_nuopc\_test\_lnd.F90 | The LND stub component      |
| pf\_nuopc\_test\_drv.F90 | The driver component        |
| pf\_nuopc\_test\_app.F90 | The test application        |

