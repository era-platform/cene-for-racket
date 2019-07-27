An innate main tag name consists of:

  * a name

An innate main tag entry consists of:

  * an innate main tag name
  * authorization conditions of the function implementation author
  * authorization conditions of the clients who can create expressions which construct and deconstruct values with this tag

An innate projection name consists of:

  * a name

A sink-struct is a sink that consists of:

  * an innate main tag entry
  * a table from innate projection names to sinks

A surface-syntax main tag string consists of:

  * a string

A surface-syntax projection string consists of:

  * a string

A struct tag metadata entry contains:

  * a maybe of a witness of authorization as the function implementation author
  * a maybe of a witness of authorization as a client who can create expressions which construct and deconstruct values with this tag
  * an innate main tag entry
  * an ordered association list from mutually unique surface-syntax projection strings to mutually unique innate projection names

Usually, a surface-syntax main tag string is turned into a struct tag metadata entry (and the innate main tag entry thereof) by obtaining its name, qualifying it, and sending it through `getfx-get`. When *defining* struct tag metadata, it's instead turned into an innate main tag name by obtaining its name.

Usually, a surface-syntax projection string is turned into an innate projection name by looking it up in some given struct tag metadata entry's association list. When *defining* struct tag metadata, it's instead turned into an innate projection name by obtaining its name.
