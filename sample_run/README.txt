STANDARD NETWORK PROCESSING SETUP

The following lines show the run of HYPOELLIPSE on one
event.  The program is started with the command "Hypoe"
and then a series of questions is answered.  This run is
set up the same way that the network data was routinely
processed for inclusion on this CD.

The input phase file was 871101114127.p and the output files were:

871101114127.arc    Identical to 871101114127.p, but reflecting the solution
                      obtained during this run of the program.
871101114127.out    The "printer" file for this event.
871101114127.log    The summary statistics for this run.  This would be
                      more interesting for a batch run of many events.

spurr:lahr 122 tcsh>> Hypoel
 begin hypoellipse
 
 hypoellipse input filename (stdin)? runone.ctl
 runone.ctl
 allow overwriting of existing output files (no)? y
 y
 root for output filenames (hypoel)? 871101114127
 871101114127
 log filename or "stdout" (871101114127.log)? 
 871101114127.log
 filename for printer output or "screen" (871101114127.out)? 
 871101114127.out
 will this job generate a summary file (no)? n
 n
 will this job generate an archive file (no)? y
 y
 archive output filename (871101114127.arc)? 
 871101114127.arc
 use travel time tables (no)? 
 n
 does this job relocate events with revised station delays (no)? 
 n

 
 
spurr:lahr 123 tcsh>>
