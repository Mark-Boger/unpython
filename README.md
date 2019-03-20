# unpython

Read pickled python object structures into Common Lisp.

*Only tested on SBCL*

## Using

```lisp
CL-USER> (unpython:load-pickle "<path to project>/t/data/character_list.pkl")
("a" "b")
CL-USER> (unpython:load-pickle "<path to project>/t/data/large_dict.pkl")
#<HASH-TABLE :TEST EQUAL :COUNT 21241 {1001A1B873}>
CL-USER> 
```

## Extending

*This structure may (will probably) change*

To modify how `unpython` reads in a specific op-code change the `perform-op` method for that code.

```lisp
(defmethod perform-op ((op-code (eql +empty-list+)) stream)
    (print "Hello World."))
```

This will print `Hello World` every time the `+empty-list+` op-code is seen.

There's a wrapper macro to get rid of some of the boiler plate.

```lisp
(do-for +empty-list+ ()
    (print "Hello World."))
```

When you give `()` after the op-code you ignore the stream.
If you give `(<symbol>)` then you can pass that as a handle for the passed stream.

```lisp
(do-for +empty-list+ (stream)
    (read-byte stream))
```

## Implemented

* Strings 
  - Unicode doesn't break it's only partly implemented
  - Currently this a naive implementation so it is unsafe
* Dictionaries
* Lists

## TODO

* Implement other types. (Added as needed)
* Actually do error handling.

## License

Do whatever I don't really care.
