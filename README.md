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
    foo        => Str,
    bar        => Int,
    baz        => UpdateInfo,
    '@bazlist' => UpdateInfo,
    '%bazmap'  => UpdateInfo,
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
  ;

  my $fbb = FBB.new(%hash);
  dd $fbb.foo;                    # "foo"
  dd $fbb.bar;                    # 42
  dd $fbb.bazlist[1].added;       # Date.new("2020-07-01")
  dd $fbb.bazmap<third>.changed;  # Date.new("2020-07-06")
```

DESCRIPTION
===========

The `Hash2Class` role allows one to create a class from a parameterization of the role. The parameterization consists of a list of `Pair`s in which the key indicates the name of key in the hash, and the value indicates the type the value in the hash is supposed to have, or be coerced to. The key becomes the name of a method accessing that key in the hash.

A key can be prefixed with `@` to indicate an Array of values in the hash, or be prefixed with `%` to indicate a hash, or `$` to indicate a scalar value (which is the default).

The types specified can **also** be classes created by the `Hash2Class` role, so recursive structures are possible, as long as they are defined in the correct order.

Classes made with the `Hash2Class` role are instantiated by calling `.new` with a hash as its only parameter. Such a hash is typically the result of rakufication of a `JSON` blob (e.g. with `from-json` of the [JSON::Fast](JSON::Fast) module). But the hash can be created in any manner.

Values are checked lazily, so no work is done on parts of the hash that are not accessed.

CREATING A CLASS DEFINITION FROM A JSON BLOB
============================================

If you have a file with a JSON blob for which you need to create a class definition, you can call the `h2c-skeleton` script. You call this script with the JSON file as the parameter, and it will print a class definition on standard output.

Class names will be selected randomly, but will be consistent within the definition of the classes. The order in which classes are defined, is also correct for compilation: generally one only needs to globally modify the class names to something that makes more sense for the given data. And possibly tweak some standard types into subsets with a more limited range of values, e.g. `Int` to `UInt`, or `Str` to `DateTime(Str)`.

AUTHOR
======

Elizabeth Mattijsen <liz@wenzperl.nl>

Source can be located at: https://github.com/lizmat/Hash2Class . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2020 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

