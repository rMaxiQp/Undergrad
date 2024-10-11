To view the build script just open build.xml

To build the applications right click on build.xml
and select Run As>Ant Script

Eclipse does not detect changes to the filesystem
that happen behind its back:

If you highlight the build directory and press 'F5' (the function key)
or right click and select 'Refresh' the new files in
dogyears and starfield directories (e.g. DogYears.exe) will appear.

ps. The jsmooth configuration file requires a reference to an image icon.
If this file is missing, the jsmooth exe builder fails without a useful error message

pps. To make this build work on a Mac (and linux) you need to change the paths - 

<executableName>dogyears\DogYearsBundled.exe</executableName>
<iconLocation>..\src\dogyears\dog.jpg</iconLocation>
<jarLocation>dogyears\DogYears.jar</jarLocation>

to use forward slashes - 

<executableName>dogyears/DogYearsBundled.exe</executableName>
<iconLocation>../src/dogyears/dog.jpg</iconLocation>
<jarLocation>dogyears/DogYears.jar</jarLocation>
