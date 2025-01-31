[![Actions Status](https://github.com/lizmat/Hash2Class/actions/workflows/linux.yml/badge.svg)](https://github.com/lizmat/Hash2Class/actions) [![Actions Status](https://github.com/lizmat/Hash2Class/actions/workflows/macos.yml/badge.svg)](https://github.com/lizmat/Hash2Class/actions) [![Actions Status](https://github.com/lizmat/Hash2Class/actions/workflows/windows.yml/badge.svg)](https://github.com/lizmat/Hash2Class/actions)

NAME
====

Hash2Class - A role to create class instances out of a Hash

SYNOPSIS
========

```raku
use Hash2Class;

class UpdateInfo does Hash2Class[
  added   => Date(Str),
  changed => Date(Str),
] { }

class FBB does Hash2Class[
  "foo",
  bar        => Int,
  baz        => UpdateInfo,
  '@bazlist' => UpdateInfo,
  '%bazmap'  => UpdateInfo,
  zap => {
    type => Str,
    name => "zippo",
    why  => "Because we can",
  },
] { }

my %hash =
  foo => "foo",
  bar => 42,
  baz => {
    added   => "2020-07-18",
    changed => "2020-07-19",
  },
  bazlist => [
    { added => "2020-07-14", changed => "2020-07-15" },
    { added => "2020-07-16", changed => "2020-07-17" },
  ],
  bazmap => {
    first  => { added => "2020-07-01", changed => "2020-07-02" },
    second => { added => "2020-07-03", changed => "2020-07-04" },
    third  => { added => "2020-07-05", changed => "2020-07-06" },
  },
  zap => "Groucho",
;

my $fbb = FBB.new(%hash);
dd $fbb.foo;                    # "foo"
dd $fbb.bar;                    # 42
dd $fbb.zippo;                  # "Groucho"
dd $fbb.bazlist[1].added;       # Date.new("2020-07-01")
dd $fbb.bazmap<third>.changed;  # Date.new("2020-07-06")
```

DESCRIPTION
===========

The `Hash2Class` role allows one to create a class from a parameterization of the role. The parameterization consists of a list of `Pair`s in which the key indicates the name of key in the hash, and the value indicates the type the value in the hash is supposed to have, or be coerced to. The key becomes the name of a method accessing that key in the hash, unless it is overriden in more extensive parameterization.

A key can be prefixed with `@` to indicate an Array of values in the hash, or be prefixed with `%` to indicate a hash, or `$` to indicate a scalar value (which is the default).

The types specified can **also** be classes created by the `Hash2Class` role, so recursive structures are possible, as long as they are defined in the correct order.

Classes made with the `Hash2Class` role are instantiated by calling `.new` with a hash as its only parameter. Such a hash is typically the result of rakufication of a `JSON` blob (e.g. with `from-json` of the [JSON::Fast](JSON::Fast) module). But the hash can be created in any manner.

Values are checked lazily, so no work is done on parts of the hash that are not accessed.

WHY
===

Hashes can be filled in many ways: JSON just being one of them. And data is not always as clean as you would hope they would be. This role allows you to add lazy typechecking to such a hash of data. It also prevents problems caused by spelling errors in keys in your code: instead of silently returning `Nil`, you will get a "Method not found" error on misspelled method names.

Since the type checking occurs lazily, no CPU is spent on typechecking values you do not actually need. Should you do want to have complete typechecking on all keys / values in the hash, then you can call the `.invalid` method on the object: this will visit **all** values in the hash recursively and produce a corresponding hash of any errors found, or `Nil` if all is ok.

PARAMETERIZATION
================

There are three modes of parameterization:

  * identifier

```raku
"foo",
```

Just specifying an identifier (a string of a single word), will create a method with the same name, and assume the value is a `Str`.

  * identifier => type

```raku
bar => Int,
```

A pair consisting of an identifier and a type, will create a method with the same name as the identifier, and assume the value is constraint by the given type.

The type can also be specified as a string if necessary:

```raku
bar => "Int",
```

Coercing types are also supported:

```raku
bar => Int(Str),
```

  * identifier => { ... }

```raku
zap => {
  type    => Str,
  name    => "zippo",
  default => "(none)",
  why     => "Because we can",
},
```

A pair consisting of an identifier and a `Hash` with further parameterization values.

Four keys are recognized in such as Hash: `type` (the type to constrain to), `name` (the name to create the method with, useful in case the key conflicts with other methods, such as `new`), `default` to indicate a default value (defaults to `Nil`) and `why` (to set the contents of the `.WHY` function on the method object.

CREATING A CLASS DEFINITION FROM A JSON BLOB
============================================

If you have a file with a JSON blob for which you need to create a class definition, you can call the `h2c-skeleton` script. You call this script with the JSON blob on standard input, and it will print a class definition on standard output.

Class names will be selected randomly, but will be consistent within the definition of the classes. The order in which classes are defined, is also correct for compilation: generally one only needs to globally modify the class names to something that makes more sense for the given data. And possibly tweak some standard types into subsets with a more limited range of values, e.g. `Int` to `UInt`, or `Str` to `DateTime(Str)`.

METHODS
=======

new
---

```raku
my $foo = Foo.new(%hash);
```

An object of a class that does the `Hash2Class` role, is created by calling the `new` method with a hash of keys and values. Each of these values can be another hash or array: these will be handled automatically if they were so parameterized.

invalid
-------

```raku
with $foo.invalid {
    note "Errors found:";
    dd $_;
}
```

The `invalid` method either returns `Nil` if all values in the hash where valid. Otherwise it returns a hash of error messages of which the keys are the names of the `methods`, and the values are the error messages. Please note that this check will access *all* values in the hash, so it may take some time for big hashes.

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/Hash2Class . Comments and Pull Requests are welcome.

If you like this module, or what I'm doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2020, 2021, 2024 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

