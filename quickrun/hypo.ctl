! headopts.prm and headopts.vol contain the setup parameters
! for running HYPOELLIPSE.
jump headopts.prm
jump headopts.vol
!
! crustal.prm specifies the velocity model.  The first
! model, which is the one that will be used, shows how to set
! up a linear increase over a halfspace.  The second model 
! illustrates a multilayer velocity model.
jump crustal.prm
!
! caldata.prm contains the calibration parameters for the
! Akutan stations.
! for pc version use caldata.prm
! uofacal option    caldata.prm
! for unix version caldata.bin
uofacal option    caldata.bin
!
! Constants noprint = 1 will cause documentation of the
! parameter values, crustal model, station locations, etc
! to be added to the output (.out) file.  Note that if the
! same parameter value it specified more than once, the last
! setting will be the one used (in this case the value will
! be set to 1).
constants noprint    0
constants noprint    1
!
! Printer option 1 adds a blow by blow description of
! every iteration step and is useful for debugging purposes.
! Reverse the order of the following records to turn this
! option on.
printer option       1
printer option       0
!
begin station list +1 19960101
jump akutan.sta
arrival times next
jump akutan.pha
