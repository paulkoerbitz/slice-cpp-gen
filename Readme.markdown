## C++ code skeleton generator from Slice files

This module generates simple C++ skeletons based on parsing a slice file.
It relies on [language-slice](https://github.com/paulkoerbitz/language-slice).

## Todo
- Option 'fwd-factory-methods': If both 'interface XyzFactory' and 'interface Xyz'
  exist then give XyzI static methods with the same name as those in XyzFactory
  and call these from XyzFactoryI.