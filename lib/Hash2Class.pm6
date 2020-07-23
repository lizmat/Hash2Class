# Since performance is one of the goals of this module, it will be using
# nqp::ops wherever that is necessary or important for performance.

use nqp;

#---------- support for handling arrays in the original hash -------------------

# Class to replace original Array with a List with objectification
my class Hash2Class::List is List {
    has &!objectifier;

    method !SET-SELF(\array, \objectifier) {
        nqp::bindattr(self,List,'$!reified',
          nqp::getattr(nqp::decont(array),List,'$!reified'));
        &!objectifier := objectifier;
        self
    }
    method new(\array, &objectifier) {
        nqp::create(self)!SET-SELF(array, &objectifier)
    }

    class Atposerator does Iterator {
        has List $!list;
        has int  $!pos;

        method new(\list) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!list',list)
        }
        method pull-one() is raw {
            nqp::islt_i($!pos,nqp::elems(nqp::getattr($!list,List,'$!reified')))
              ?? $!list.AT-POS($!pos++)
              !! IterationEnd
        }
    }
    method iterator() { Atposerator.new(self) }

    method AT-POS(int $pos) {
        nqp::islt_i($pos,0)
          ?? Failure.new(X::OutOfRange.new(
               :what<Index>,
               :got($pos),
               :range("0..^Inf")
             ))
          !! nqp::iscont(my $value :=
               nqp::atpos(nqp::getattr(self,List,'$!reified'),$pos))
            ?? nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,
                 &!objectifier($value))
            !! nqp::ifnull($value,Nil)
    }
    method ASSIGN-POS(int $pos, $value) {
        X::Assignment::RO.new(:$value).throw
    }
}

# Returns method for checking elements of an array
my sub array-type(str $name, \type) is raw {
    method () is raw {
        nqp::iscont(
          my $list := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               Hash2Class::List.new($list, -> \value {
                   nqp::istype(value,type)
                     ?? nqp::decont(value)
                     !! X::TypeCheck.new(
                          got      => value,
                          expected => type,
                        ).throw
               })
             )
          !! $list
    }
}

# Returns method for handling array with a coercer
my sub array-coercer(str $name, \type) {
    my \target      := type.^target_type;
    my \constraint  := type.^constraint_type;

    # need to coerce to refinee of subset and test type
    if target.HOW.^name.ends-with('::SubsetHOW') {
        my str $typename = target.^refinee.^name;

        method () is raw {
            nqp::iscont(
              my $list :=
                nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
            ) ?? nqp::bindkey(
                   nqp::getattr(self,self.WHAT,'$!data'),$name,
                   Hash2Class::List.new($list, -> $value is copy {
                       nqp::istype($value,constraint)
                         ?? nqp::istype(($value := $value."$typename"()),target)
                           ?? $value
                           !! X::TypeCheck.new(
                                got      => $value,
                                expected => target,
                              ).throw
                         !! X::TypeCheck.new(
                              got      => $value,
                              expected => constraint,
                            ).throw
                   })
                 )
              !! $list
        }
    }

    # just typecheck and coerce
    else {
        my str $typename = target.^name;

        method () is raw {
            nqp::iscont(
              my $list :=
                nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
            ) ?? nqp::bindkey(
                   nqp::getattr(self,self.WHAT,'$!data'),$name,
                   Hash2Class::List.new($list, -> \value {
                       nqp::istype(value,constraint)
                         ?? value."$typename"()
                         !! X::TypeCheck.new(
                              got      => value,
                              expected => constraint,
                            ).throw
                   })
                 )
              !! $list
        }
    }
}

# Return sub for converting hash to object
my sub array-hash2class(str $name, \type) is raw {
    method () is raw {
        nqp::iscont(
          my $list := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               Hash2Class::List.new($list, -> \value {
                   type.new(value)
               })
             )
          !! $list
    }
}

#---------- support for handling hashes in the original hash -------------------

# Class to replace original Hash with a Map with objectification
my class Hash2Class::Map is Map {
    has &!objectifier;

    method !SET-SELF(\hash, \objectifier) {
        nqp::bindattr(self,Map,'$!storage',
          nqp::getattr(nqp::decont(hash),Map,'$!storage'));
        &!objectifier := objectifier;
        self
    }
    method new(\hash, &objectifier) {
        nqp::create(self)!SET-SELF(hash, &objectifier)
    }

    class Atkeyerator does Iterator {
        has Map $!map;
        has     $!iter;

        method !SET-SELF(\map) {
            $!map  := map;
            $!iter := nqp::iterator(nqp::getattr(map,Map,'$!storage'));
            self
        }
        method new(\map) { nqp::create(self)!SET-SELF(map) }

        method pull-one() is raw {
            $!iter
              ?? $!map.AT-KEY(nqp::iterkey_s(nqp::shift($!iter)))
              !! IterationEnd
        }
    }
    method iterator() { Atkeyerator.new(self) }

    method AT-KEY(str $key) {
        nqp::iscont(
          my $value := nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key)
        ) ?? nqp::bindkey(nqp::getattr(self,Map,'$!storage'),$key,
               &!objectifier($value))
          !! nqp::ifnull($value,Nil)
    }
    method ASSIGN-KEY(str $key, $value) {
        X::Assignment::RO.new(:$value).throw
    }
}

# Returns method for checking keys of a hash
my sub hash-type(str $name, \type) is raw {
    method () is raw {
        nqp::iscont(
          my $map := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               Hash2Class::Map.new($map, -> \value {
                   nqp::istype(value,type)
                     ?? nqp::decont(value)
                     !! X::TypeCheck.new(
                          got      => value,
                          expected => type,
                        ).throw
               })
             )
          !! $map
    }
}

# Returns method for handling hash with a coercer
my sub hash-coercer(str $name, \type) {
    my \target      := type.^target_type;
    my \constraint  := type.^constraint_type;

    # need to coerce to refinee of subset and test type
    if target.HOW.^name.ends-with('::SubsetHOW') {
        my str $typename = target.^refinee.^name;

        method () is raw {
            nqp::iscont(
              my $map :=
                nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
            ) ?? nqp::bindkey(
                   nqp::getattr(self,self.WHAT,'$!data'),$name,
                   Hash2Class::Map.new($map, -> $value is copy {
                       nqp::istype($value,constraint)
                         ?? nqp::istype(($value := $value."$typename"()),target)
                           ?? $value
                           !! X::TypeCheck.new(
                                got      => $value,
                                expected => target,
                              ).throw
                         !! X::TypeCheck.new(
                              got      => $value,
                              expected => constraint,
                            ).throw
                   })
                 )
              !! $map
        }
    }

    # just typecheck and coerce
    else {
        my str $typename = target.^name;

        method () is raw {
            nqp::iscont(
              my $map :=
                nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
            ) ?? nqp::bindkey(
                   nqp::getattr(self,self.WHAT,'$!data'),$name,
                   Hash2Class::Map.new($map, -> \value {
                       nqp::istype(value,constraint)
                         ?? value."$typename"()
                         !! X::TypeCheck.new(
                              got      => value,
                              expected => constraint,
                            ).throw
                   })
                 )
              !! $map
        }
    }
}

# Return sub for converting hash to object
my sub hash-hash2class(str $name, \type) is raw {
    method () is raw {
        nqp::iscont(
          my $map := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               Hash2Class::Map.new($map, -> \value {
                   type.new(value)
               })
             )
          !! $map
    }
}

#---------- support for handling scalars in the original hash ------------------

# Returns method for handling a simple typecheck
my sub scalar-type(str $name, $type is raw) {
    method () is raw {
        nqp::iscont(
          my $value := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::istype($value,$type)
            ?? nqp::bindkey(
                 nqp::getattr(self,self.WHAT,'$!data'),
                 $name,
                 nqp::decont($value)
               )
            !! X::TypeCheck.new(
                 got      => $value,
                 expected => $type,
               ).throw
          !! $value
    }
}

# Returns method for handling a simple coercer
my sub scalar-coercer(str $name, \type) {
    my \target      := type.^target_type;
    my \constraint  := type.^constraint_type;
    my $method;

    # need to coerce to refinee of subset and test type
    if target.HOW.^name.ends-with('::SubsetHOW') {
        my str $typename = target.^refinee.^name;

        method () is raw {  # base type, assume simple coercion
            nqp::iscont(
              my $value :=
                nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
            ) ?? nqp::istype($value,constraint)
                ?? nqp::istype(($value := $value."$typename"()),target)
                  ?? nqp::bindkey(
                       nqp::getattr(self,self.WHAT,'$!data'),
                       $name,
                       $value
                     )
                  !! X::TypeCheck.new(
                       got      => $value,
                       expected => target,
                     ).throw
                !! X::TypeCheck.new(
                     got      => $value,
                     expected => constraint,
                   ).throw
              !! $value
        }
    }
    
    # base type, assume simple coercion
    else {
        my str $typename = target.^name;

        method () is raw {
            nqp::iscont(
              my $value :=
                nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
            ) ?? nqp::istype($value,constraint)
                ?? nqp::bindkey(
                     nqp::getattr(self,self.WHAT,'$!data'),
                     $name,
                     $value."$typename"()
                   )
                !! X::TypeCheck.new(
                     got      => $value,
                     expected => constraint,
                   ).throw
              !! $value
        }
    }
}

# Returns method for handling a Hash2Class class
my sub scalar-hash2class(str $name, $type is raw) {
    method () is raw {
        nqp::iscont(
          my $value := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               $type.new($value)
             )
          !! $value
    }
}

# Mapper for valid sigils
my $sigils := nqp::hash('$', 1, '@', 1, '%', 1);

# The actual role that does the work
role Hash2Class:ver<0.0.4>:auth<cpan:ELIZABETH>[*@list, *%hash] {
    has $!data;  # the raw data in a Hash

    method !data() is raw { $!data }

    # fetch whatever parameters we got
    for (@list, %hash).flat {
        my $sigil;
        my $name;
        my $type;

        if $_ ~~ Pair {
            $sigil := .key.substr(0,1);
            $name  := .key.substr(1);
            $type  := .value;
        }
        else {
            $sigil := .substr(0,1);
            $name  := .substr(1);
            $type  := Str;
        }

        # fix sigilless names
        unless nqp::existskey($sigils,$sigil) {
            $name  := $sigil ~ $name;
            $sigil := '$';
        }

        die "Key must have a name." unless $name;
        die "Already has a method called '$name'"
          if $?CLASS.^methods.first(*.name eq $name);

        my $method := nqp::istype($type,Hash2Class)
          ?? $sigil eq '$'
            ?? scalar-hash2class($name, $type)  # $
            !! $sigil eq '@'
              ?? array-hash2class($name, $type) # @
              !! hash-hash2class($name, $type)  # %
          !! $type.HOW.^name.ends-with('::CoercionHOW')
            ?? $sigil eq '$'
              ?? scalar-coercer($name, $type)  # $
              !! $sigil eq '@'
                ?? array-coercer($name, $type) # @
                !! hash-coercer($name, $type)  # %
            !! $sigil eq '$'
              ?? scalar-type($name, $type)  # $
              !! $sigil eq '@'
                ?? array-type($name, $type) # @
                !! hash-type($name, $type); # %

        $method.set_name($name);
        $?CLASS.^add_method($name, $method);
    }

    method new(%data) {
        nqp::p6bindattrinvres(
          nqp::create(self),self,'$!data',nqp::getattr(%data,Map,'$!storage')
        )
    }

    method raku() {
        $?CLASS.^name ~ '.new(' ~ nqp::hllize($!data).raku ~ ')'
    }
}

=begin pod

=head1 NAME

Hash2Class - A role to create class instances out of a Hash

=head1 SYNOPSIS

=begin code :lang<raku>

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

=end code

=head1 DESCRIPTION

The C<Hash2Class> role allows one to create a class from a parameterization
of the role.  The parameterization consists of a list of C<Pair>s in which
the key indicates the name of key in the hash, and the value indicates the
type the value in the hash is supposed to have, or be coerced to.  The key
becomes the name of a method accessing that key in the hash.

A key can be prefixed with C<@> to indicate an Array of values in the hash,
or be prefixed with C<%> to indicate a hash, or C<$> to indicate a scalar
value (which is the default).

The types specified can B<also> be classes created by the C<Hash2Class>
role, so recursive structures are possible, as long as they are defined
in the correct order.

Classes made with the C<Hash2Class> role are instantiated by calling
C<.new> with a hash as its only parameter.  Such a hash is typically the
result of rakufication of a C<JSON> blob (e.g. with C<from-json> of the
L<JSON::Fast> module).  But the hash can be created in any manner.

Values are checked lazily, so no work is done on parts of the hash that
are not accessed.

=head1 CREATING A CLASS DEFINITION FROM A JSON BLOB

If you have a file with a JSON blob for which you need to create a class
definition, you can call the C<bin/skeleton> script.  You call this script
with the JSON file as the parameter, and it will print a class definition
on standard output.

Class names will be selected randomly, but will be consistent within the
definition of the classes.  The order in which classes are defined, is also
correct for compilation: generally one only needs to globally modify the
class names to something that makes more sense for the given data.  And
possibly tweak some standard types into subsets with a more limited range
of values, e.g. C<Int> to C<UInt>, or C<Str> to C<DateTime(Str)>.

=head1 AUTHOR

Elizabeth Mattijsen <liz@wenzperl.nl>

Source can be located at: https://github.com/lizmat/Hash2Class . Comments and
Pull Requests are welcome.

=head1 COPYRIGHT AND LICENSE

Copyright 2020 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under
the Artistic License 2.0.

=end pod

# vim: expandtab shiftwidth=4
