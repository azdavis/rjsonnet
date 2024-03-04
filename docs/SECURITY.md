# Security

The language server performs I/O to read arbitrary user input.

It should never access the network.

A user could probably "DOS" themselves by asking the language server to analyze a massive or complicated set of files, but we don't really consider this to be a security threat.

That being said, if there is a security issue with the project, let us know at:

ariel DOT z DOT davis AT icloud DOT com
