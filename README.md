NAME
====

Pod::To::Markdown - Render Pod as Markdown

SYNOPSIS
========

From command line:

    $ perl6 --doc=Markdown lib/To/Class.pm

From Perl6:

```perl6
use Pod::To::Markdown;

=NAME
foobar.pl

=SYNOPSIS
    foobar.pl <options> files ...

print pod2markdown($=pod);
```

EXPORTS
=======

    class Pod::To::Markdown
    sub pod2markdown

DESCRIPTION
===========



### method render

```perl6
method render(
    $pod,
    Bool :$no-fenced-codeblocks
) returns Str
```

Render Pod as Markdown

To render without fenced codeblocks (```` ``` ````), as some markdown engines don't support this, use the :no-fenced-codeblocks option. If you want to have code show up as ```` ```perl6```` to enable syntax highlighting on certain markdown renderers, use:

    =begin code :lang<perl6>

### sub pod2markdown

```perl6
sub pod2markdown(
    $pod,
    Bool :$no-fenced-codeblocks
) returns Str
```

Render Pod as Markdown, see .render()

LICENSE
=======

This is free software; you can redistribute it and/or modify it under the terms of The [Artistic License 2.0](http://www.perlfoundation.org/artistic_license_2_0).
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

AUTHOR
======

Elizabeth Mattijsen <liz@wenzperl.nl>

Source can be located at: https://github.com/lizmat/Hash2Class . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2020 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

