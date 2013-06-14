## C++ code skeleton generator from Slice files

[![Build Status](https://travis-ci.org/paulkoerbitz/slice-cpp-gen.png)](https://travis-ci.org/paulkoerbitz/slice-cpp-gen)

This module generates simple C++ skeletons based on parsing a slice file.
It relies on [language-slice](https://github.com/paulkoerbitz/language-slice).


#### Usage:

    $ slice-cpp-gen --targetdir=MyModuleDirectory mySliceFile.ice

use '--overwrite=True' to overwrite existing files.

## Todo
- Improve help message and options when run from command line

## Done
- Option '--fwdfctmthds': If both 'interface XyzFactory' and 'interface Xyz'
  exist then give XyzI static methods with the same name as those in XyzFactory
  and call these from XyzFactoryI.