use Test;
use Hash2Class;

class B does Hash2Class[
  added   => Date(Str),
  changed => Date(Str),
] { }

class A does Hash2Class[
  "foo",
  bar        => Int(Str),
  baz        => B,
  '@bazlist' => B,
  '%bazmap'  => B,
  zap        => {
    type    => Str,
    name    => "zippo",
    default => "(none)",
  }
] { }

my %valid =
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

my $a = A.new(%valid);

isa-ok $a, A, 'did we get an A';
is-deeply $a.foo,       "foo", 'was foo foo';
is-deeply $a.bar,          42, 'was bar 42';
is-deeply $a.zippo, '(none)', 'was zap / zippo (none)';
is-deeply $a.invalid, Nil, 'is A valid';

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

my %invalid =
  foo => 42,
  bar => "foo",
  baz => {
    added   => "v2020-07-18",
    changed => "v2020-07-19",
  },
  bazlist => [
    { added => "v2020-07-14", changed => "v2020-07-15" },
    { added => "v2020-07-16", changed => "v2020-07-17" },
  ],
  bazmap => {
    first  => { added => "v2020-07-01", changed => "v2020-07-02" },
    second => { added => "v2020-07-03", changed => "v2020-07-04" },
    third  => { added => "v2020-07-05", changed => "v2020-07-06" },
  },
  zap => 666,
;

my $c = A.new(%invalid);
isa-ok $c, A, 'did we get an A';

my $invalid = $c.invalid;
isa-ok $invalid, Hash, 'did we get a hash with errors';
diag $invalid<foo> unless
  ok $invalid<foo>.contains("expected Str but got Int"),
    'is error message for foo correct';
diag $invalid<bar> unless
  ok $invalid<bar>.contains("Cannot convert string to number"),
    'is error message for bar correct';
diag $invalid<zippo> unless
  ok $invalid<zippo>.contains("expected Str but got Int"),
    'is error message for zippo correct';

my @bl := $invalid<bazlist>;
isa-ok $bl, List, 'did we get a list with errors';
my %bm := $invalid<bazmap>;
isa-ok $bm,  Map, 'did we get a map with errors';

for |@bl,|%bm.values {
    diag .<added> unless
      ok .<added>.contains("Invalid Date string"),
        'did we get invalid Date error for added';
    diag .<changed> unless
      ok .<changed>.contains("Invalid Date string"),
        'did we get invalid Date error for changed';
}


done-testing;
