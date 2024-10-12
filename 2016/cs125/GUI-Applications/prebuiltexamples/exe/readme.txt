These executable files were built using jsmooth.
JSmooth can build 'exe' wrappers that start a jar file
or, as in these examples, 
it can include the jar file as part of the exe file.

When run, it first unpacks the jar into Window's temp directory.

The advantage of this approach for small programs is that
the only file that is required is the exe file itself.

For larger programs the startup overhead may be unacceptable.
In which case don't embed the jar files: Instead provide
the exe alongside the jar file.