An innate main tag name consists of:

  * a name

An innate main tag entry consists of:

  * an innate main tag name
  * (TODO) authorization conditions of the function implementation author
  * (TODO) authorization conditions of the clients who can create expressions which construct and deconstruct values with this tag

An innate projection name consists of:

  * a name

A sink-struct is a sink that consists of:

  * an innate main tag entry
  * a table from innate projection names to sinks

(TODO) An author-authorized main tag name (used to define a function implementation) needs to contain:

  * an innate main tag entry
  * a set of innate projection names
  * proof of authorization as the function implementation author

(TODO) A client-authorized main tag name (used to create expressions which construct and deconstruct values with the tag) needs to contain:

  * an innate main tag entry
  * a set of innate projection names
  * proof of authorization as a client who can create expressions which construct and deconstruct values with this tag

A surface-syntax main tag string consists of:

  * a string

A surface-syntax projection string consists of:

  * a string

A struct tag metadata entry needs to contain:

  * an innate main tag entry
  * an ordered association list from mutually unique surface-syntax projection strings to mutually unique innate projection names

Usually, a surface-syntax main tag string needs to be (and is) turned into a struct tag metadata entry (and the innate main tag entry thereof) by obtaining its name, qualifying it, and sending it through `getfx-get`. When *defining* struct tag metadata, it instead needs to be turned into an innate main tag name by obtaining its name ((TODO) rather than the current practice of obtaining its name, qualifying it, and obtaining the name corresponding to the qualified authorized name).

Usually, a surface-syntax projection string needs to be (and is) turned into an innate projection name by looking it up in some given struct tag metadata entry's association list. When *defining* struct tag metadata, it instead needs to be turned into an innate projection name by obtaining its name ((TODO) rather than the current practice of obtaining its name, qualifying it along with a given innate main tag name, and obtaining the name corresponding to the qualified authorized name).
