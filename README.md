cl-riff
=======

The Resource Interchange File Format (RIFF) is a generic file
container format for storing data in tagged chunks. It is primarily
used to store multimedia such as sound and video, though it may also
be used to store any arbitrary data.

Example
-------

	> (cl-riff:read-riff-file "c:/windows/media/ding.wav")


References
----------

http://en.wikipedia.org/wiki/Resource_Interchange_File_Format

http://msdn.microsoft.com/en-us/library/windows/desktop/dd798636(v=vs.85).aspx

http://msdn.microsoft.com/en-us/library/windows/desktop/ee415713(v=vs.85).aspx


Rob Blackwell    
February 2014
