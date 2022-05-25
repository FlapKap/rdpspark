# rdpspark

## files
- rdp.gpr: project file
- spark.adc: spark configuration
- main.adb: main entry point of the program, and where the test is set up
- rdp.ads: spec file for rdp package
- rdp.adb: implementation file for rdp package
- BoundedQueues.ads: spec file for bounded queue package
- BoundedQueues.adb: implementation file for boundedqueue package

### leftovers
An attempt to implement a linked list based queue was made. These two files are leftovers from that
- queuesPtrBased.ads: spec file
- queuesPtrBased.adb: impl. file

## Verifying
assuming gnat is installed just run  `gnatprove -Prdp -j0  --output=oneline --level=2` in this folder.