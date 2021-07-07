# LND-ParFlow NUOPC Test Application

## Description
A coupled LND-ParFlow application used to test the ParFlow NUOPC cap.

## Build
The parflow-nuopc-test executable will build with
-DPARFLOW\_ENABLE\_NUOPC=ON and -DCMAKE\_BUILD\_TYPE=DEBUG
The parflow-nuopc-test will install into the
-DCMAKE\_INSTALL\_PREFIX/nuopc\_test directory.

## Test Case
The Little Washita watershed test case can be found in the example directory.
* Distributed over 1 MPI task
* ParFlow excludes CLM
* Two-way exchanges water flux, porosity, pressure, and saturation
* Stub land surface model initializes fields with 0.1

## Execution
```
    mpirun -np 1 ./parflow-nuopc-test
```

## Output Files
| File                         | Description                      |
| ---------------------------- | -------------------------------- |
| PET\*.ESMF\_LogFile          | ESMF errors will be written here |
| LW.out.log                   | ParFlow output log               |
| LW.out.evaptrans.#.pfb       | Evaporative transpiration output |
| LW.out.press.#.pfb           | Pressure output                  |
| LW.out.satur.#.pfb           | Saturation output                |
| LW.out.kinsol.log            |                                  |
| LW.out.mask.pfb              | Mask information                 |
| LW.out.perm\_x.pfb           |                                  |
| LW.out.perm\_y.pfb           |                                  |
| LW.out.perm\_z.pfb           |                                  |
| LW.out.pfmetadata            |                                  |
| LW.out.porosity.pfb          |                                  |
| LW.out.specific\_storage.pfb |                                  |

## Source Files
| File                     | Description                 |
| ------------------------ | --------------------------- |
| CMakeLists.txt           | CMake configuration file    |
| pf\_nuopc\_test\_lnd.F90 | The LND stub component      |
| pf\_nuopc\_test\_drv.F90 | The driver component        |
| pf\_nuopc\_test\_app.F90 | The test application        |

