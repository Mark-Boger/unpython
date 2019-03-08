# unpython

Read pickled python object structures into Common Lisp.

*Only tested on SBCL*

## Implemented

* Strings (Unicode doesn't break it but it isn't handled correctly)
* Dictionaries
* Lists

## TODO

* Implement other types. (Added as needed)
* Actually do error handling.

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

## License

Do whatever I don't really care.
