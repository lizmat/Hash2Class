use Test;
use Hash2Class;

class B does Hash2Class[
  added   => Date(Str),
  changed => Date(Str),
] { }

class A does Hash2Class[
  foo        => Str,
  bar        => Int(Str),
  baz        => B,
  '@bazlist' => B,
  '%bazmap'  => B,
] { }

my %hash =
  foo => "foo",
  bar => "42",
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

my $a = A.new(%hash);

isa-ok $a, A, 'did we get an A';
is-deeply $a.foo, "foo", 'was foo foo';
is-deeply $a.bar,    42, 'was bar 42';

my $b = $a.baz;
isa-ok $b, B, 'did we get a B';
is-deeply $b.added,   "2020-07-18".Date, 'did we get 2020-07-18 as Date';
is-deeply $b.changed, "2020-07-19".Date, 'did we get 2020-07-19 as Date';

my $bl = $a.bazlist;
is $bl.elems, 2, 'have 2 Bs in bazlist';
isa-ok $bl[0], B, 'did we get a B';
is-deeply $bl[0].added,   "2020-07-14".Date, 'did we get 2020-07-14 as Date';
is-deeply $bl[0].changed, "2020-07-15".Date, 'did we get 2020-07-15 as Date';
isa-ok $bl[1], B, 'did we get a B';
is-deeply $bl[1].added,   "2020-07-16".Date, 'did we get 2020-07-16 as Date';
is-deeply $bl[1].changed, "2020-07-17".Date, 'did we get 2020-07-17 as Date';

my $bm = $a.bazmap;
is $bm.elems, 3, 'have 3 Bs in bazmap';
isa-ok $bm<first>, B, 'did we get a B';
is-deeply $bm<first>.added,   "2020-07-01".Date, 'did we get 2020-07-01 as Date';
is-deeply $bm<first>.changed, "2020-07-02".Date, 'did we get 2020-07-02 as Date';
isa-ok $bm<second>, B, 'did we get a B';
is-deeply $bm<second>.added,   "2020-07-03".Date, 'did we get 2020-07-03 as Date';
is-deeply $bm<second>.changed, "2020-07-04".Date, 'did we get 2020-07-04 as Date';
isa-ok $bm<third>, B, 'did we get a B';
is-deeply $bm<third>.added,   "2020-07-05".Date, 'did we get 2020-07-05 as Date';
is-deeply $bm<third>.changed, "2020-07-06".Date, 'did we get 2020-07-06 as Date';

done-testing;
