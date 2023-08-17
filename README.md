# schemeKV - A simple key-value database in scheme

## Introduction
This project contains a custom scheme interpreter, able to run schemeKV, a s-expression-based key-value database.

## Interpreter
The basic scheme interpreter is from this well-known [tutorial](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/) (can be found in the 'base' branch).

I extended the interpreter to include functionality for hashtables and network sockets, implemented as bindings to the
corresponding haskell packages [Data.Hashtable](https://hackage.haskell.org/package/hashtables) and [Netwock.Socket](https://hackage.haskell.org/package/network)
and other common scheme features not included in the tutorial.

## Key-value database
The scheme files for the database are located in [src/](src/).
They include a server.scm and client.scm file.

What's special about schemeKV is that all `set`, `get`, etc. requests are sent as scheme [s-expressions](https://en.wikipedia.org/wiki/S-expression)
and evaluated by the server, making use of scheme's [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity).

This means that the database can not only store strings or numbers, but entire scheme expressions.

Running client.scm after connecting with the server:
```scm
Scheme-KV> (set 'string "test")

"test"

Scheme-KV> (get 'string)

"test"

Scheme-KV> (set 'increment '(lambda (x) (+ x 1)))

(lambda (x) (+ x 1))

Scheme-KV> (ev ((ex 'increment) 5))

6

Scheme-KV> (pers 'increment)

"Data saved."

Scheme-KV> 
```

The ability to store lambda expressions is especially useful for storing macros.
One could store a macro like `(lambda (k) (if (< (get k) 18) "Not saved." (pers k)))`,
which could take in the key of a newly created key-value pair containing the age of a person.
The macro would then only make the key-value pair persistent on disk, if the age is above 18.

This feature makes the database program extensible by the user.

### schemeKV commands
- `set`: create key-value pair
- `get`: get the value stored at key
- `del`: delete the value stored at key
- `ex`: evaluate the expression stored at key (mainly useful for stored lambda-expressions)
- `ev`: evaluate any scheme expression
- `pers`: save the key-value pair at key to a file to make it persistent
