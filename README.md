# join

Toy project that tries to build a compile time checked AST for joining data streams (represented as case classes), which could then be interpreted by a stream processing framework.

Initially I imagined doing something with `LabelledGeneric` but then found [this](https://stackoverflow.com/questions/36006772/how-to-get-the-name-of-a-case-class-field-as-a-string-symbol-at-compile-time-usi/36018254#comment80454963_36018254) from Travis Brown.

