# Since performance is one of the goals of this module, it will be using
# nqp::ops wherever that is necessary or important for performance.

use nqp;

sub wrongtype(Mu $got, Mu $expected) {
    X::TypeCheck.new(operation => "fetching", :$got, :$expected).throw
}

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
    method iterator(::?CLASS:D:) { Atposerator.new(self) }

    method AT-POS(::?CLASS:D: int $pos) {
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
    method ASSIGN-POS(::?CLASS:D: int $pos, $value) {
        X::Assignment::RO.new(:$value).throw
    }
}

# Returns method for checking elements of an array
my sub array-type(str $name, \type, \default) is raw {
    method () is raw {
        nqp::iscont(
          my $list := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               Hash2Class::List.new($list, -> \value {
                   nqp::istype(value,type)
                     ?? nqp::decont(value)
                     !! wrongtype(value, type)
               })
             )
          !! nqp::ifnull($list,default)
    }
}

# Returns method for handling array with a coercer
my sub array-coercer(str $name, \type, \default) {
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
                           !! wrongtype($value, target)
                         !! wrongtype($value, constraint)
                   })
                 )
              !! nqp::ifnull($list,default)
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
                         !! wrongtype(value, constraint)
                   })
                 )
              !! nqp::ifnull($list,default)
        }
    }
}

# Return sub for converting hash to object
my sub array-hash2class(str $name, \type, \default) is raw {
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
          !! nqp::ifnull($list,default)
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
    method iterator(::?CLASS:D:) { Atkeyerator.new(self) }

    method AT-KEY(::?CLASS:D: str $key) {
        nqp::iscont(
          my $value := nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key)
        ) ?? nqp::bindkey(nqp::getattr(self,Map,'$!storage'),$key,
               &!objectifier($value))
          !! nqp::ifnull($value,Nil)
    }
    method ASSIGN-KEY(::?CLASS:D: str $key, $value) {
        X::Assignment::RO.new(:$value).throw
    }
}

# Returns method for checking keys of a hash
my sub hash-type(str $name, \type, \default) is raw {
    method () is raw {
        nqp::iscont(
          my $map := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               Hash2Class::Map.new($map, -> \value {
                   nqp::istype(value,type)
                     ?? nqp::decont(value)
                     !! wrongtype(value, type)
               })
             )
          !! nqp::ifnull($map,default)
    }
}

# Returns method for handling hash with a coercer
my sub hash-coercer(str $name, \type, \default) {
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
                           !! wrongtype($value, target)
                         !! wrongtype($value, constraint)
                   })
                 )
              !! nqp::ifnull($map,default)
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
                         !! wrongtype(value, constraint)
                   })
                 )
              !! nqp::ifnull($map,default)
        }
    }
}

# Return sub for converting hash to object
my sub hash-hash2class(str $name, \type, \default) is raw {
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
          !! nqp::ifnull($map,default)
    }
}

#---------- support for handling scalars in the original hash ------------------

# Returns method for handling a simple typecheck
my sub scalar-type(str $name, $type is raw, \default) {
    method () is raw {
        nqp::iscont(
          my $value := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::istype($value,$type)
            ?? nqp::bindkey(
                 nqp::getattr(self,self.WHAT,'$!data'),
                 $name,
                 nqp::decont($value)
               )
            !! wrongtype($value, $type)
          !! nqp::ifnull($value,default)
    }
}

# Returns method for handling a simple coercer
my sub scalar-coercer(str $name, \type,\default) {
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
                  !! wrongtype($value, target)
                !! wrongtype($value, constraint)
              !! nqp::ifnull($value,default)
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
                !! wrongtype($value, constraint)
              !! nqp::ifnull($value,default)
        }
    }
}

# Returns method for handling a Hash2Class class
my sub scalar-hash2class(str $name, $type is raw, \default) {
    method () is raw {
        nqp::iscont(
          my $value := nqp::atkey(nqp::getattr(self,self.WHAT,'$!data'),$name)
        ) ?? nqp::bindkey(
               nqp::getattr(self,self.WHAT,'$!data'),
               $name,
               $type.new($value)
             )
          !! nqp::ifnull($value,default)
    }
}

#---------- the actual role to be mixed in -------------------------------------

# Mapper for valid sigils
my $sigils := nqp::hash('$', 1, '@', 1, '%', 1);

role Hash2Class:ver<0.1.6>:auth<zef:lizmat>[*@list, *%hash] {
    has $!data;  # the raw data in a Hash

    # fetch whatever parameters we got
    for (@list, %hash).flat {
        my $sigil;
        my $key;
        my $method;
        my $type;
        my $why;
        my $default := Nil;

        if $_ ~~ Pair {
            $sigil := .key.substr(0,1);
            $key   := .key.substr(1);
            with .value {
                if $_ ~~ Map {
                    with .<type> {
                        $type := $_ ~~ Str
                          ?? ::($_)
                          !! die "Only type objects can be used to indicate type";
                    }
                    else {
                        $type := $_;
                    }

                    $method  := $_ with .<name>;
                    $why     := $_ with .<why>;
                    $default := .<default> if .<default>:exists;
                }
                elsif $_ ~~ Str {
                    $type := ::($_);
                }
                else {
                    die "Unsupported value for key: $_.raku()";
                }
            }
            else {
                $type := $_;
            }
        }
        else {
            $sigil := .substr(0,1);
            $key   := .substr(1);
            $type  := Str;
        }

        # fix sigilless names
        unless $sigil && nqp::existskey($sigils,$sigil) {
            $key   := $sigil ~ $key;
            $sigil := '$';
        }

        # sane default?
        unless $default =:= Nil {
            die "Default '$default.raku()' does not match '$type.raku()' for '$key'"
              unless $default ~~ $type;
        }

        $method := $key unless $method;
        die "Key must have a name." unless $key;
        die "Already has a method called '$method'"
          if $?CLASS.^methods.first(*.name eq $method);

        my &method := nqp::istype($type,Hash2Class)
          ?? $sigil eq '$'
            ?? scalar-hash2class($key, $type, $default)  # $
            !! $sigil eq '@'
              ?? array-hash2class($key, $type, $default) # @
              !! hash-hash2class($key, $type, $default)  # %
          !! $type.HOW.^name.ends-with('::CoercionHOW')
            ?? $sigil eq '$'
              ?? scalar-coercer($key, $type, $default)  # $
              !! $sigil eq '@'
                ?? array-coercer($key, $type, $default) # @
                !! hash-coercer($key, $type, $default)  # %
            !! $sigil eq '$'
              ?? scalar-type($key, $type, $default)  # $
              !! $sigil eq '@'
                ?? array-type($key, $type, $default) # @
                !! hash-type($key, $type, $default); # %

        &method.set_why($why) if $why;
        &method.set_name($method);
        $?CLASS.^add_method($method, &method);
    }

    method new(%data) {
        nqp::p6bindattrinvres(
          nqp::create(self),self,'$!data',nqp::getattr(%data,Map,'$!storage')
        )
    }

    my constant $skip := nqp::hash('new',1,'invalid',1,'raku',1,'BUILDALL',1);

    method invalid(::?CLASS:D:) {
        my $sorries := nqp::hash;

        for self.^methods -> &method {
            my $name := &method.name;
            next if nqp::existskey($skip,$name);

            my $result := try method(self);
            if nqp::eqaddr($result,Nil) {
                nqp::bindkey($sorries,$name,$!.message);
            }

            elsif nqp::istype($result,Hash2Class) {
                my $invalid := $result.invalid;
                nqp::bindkey($sorries,$name,$invalid) if $invalid;
            }

            elsif nqp::istype($result,Hash2Class::List) {
                my $list-sorries := nqp::create(IterationBuffer);
                my int $elems = $result.elems;
                my int $i     = -1;

                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::istype((my $val := $result.AT-POS($i)),Hash2Class),
                    nqp::unless(
                      nqp::eqaddr((my $invalid := $val.invalid),Nil),
                      nqp::bindpos($list-sorries,$i,$invalid)
                    )
                  )
                );
                nqp::bindkey($sorries,$name,$list-sorries.List)
                  if nqp::elems($list-sorries);
            }

            elsif nqp::istype($result,Hash2Class::Map) {
                my $map-sorries := nqp::hash;
                my $iter :=
                  nqp::iterator(nqp::getattr($result,Map,'$!storage'));

                nqp::while(
                  $iter,
                  nqp::stmts(
                    (my str $key = nqp::iterkey_s(nqp::shift($iter))),
                    nqp::if(
                      nqp::istype(
                        (my $val := $result.AT-KEY($key)),
                        Hash2Class
                      ),
                      nqp::unless(
                        nqp::eqaddr((my $invalid := $val.invalid),Nil),
                        nqp::bindkey($map-sorries,$key,$invalid)
                      )
                    )
                  )
                );
                nqp::bindkey($sorries,$name,nqp::hllize($map-sorries))
                  if nqp::elems($map-sorries);
            }
        }

        nqp::elems($sorries) ?? $sorries !! Nil
    }

    multi method raku(::?CLASS:D:) {
        $?CLASS.^name ~ '.new(' ~ nqp::hllize($!data).raku ~ ')'
    }
}

# vim: expandtab shiftwidth=4
