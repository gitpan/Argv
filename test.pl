# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

my $final = 0;

# Automatically generates an ok/nok msg, incrementing the test number.
BEGIN {
   my($next, @msgs);
   sub printok {
      push @msgs, ($_[0] ? '' : 'not ') . "ok @{[++$next]}\n";
      return !$_[0];
   }
   END {
      print "\n1..", scalar @msgs, "\n", @msgs;
   }
}

use Argv;
$final += printok(1);

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

print "+ Testing basic construction and execution ...\n";
my $pl = Argv->new($^X, '-v');
$pl->system;
$final += printok($? == 0);

print "+ Testing construction using references ...\n";
$final += printok(Argv->new($^X, [qw(-v)])->system('') == 0);

print "+ Testing 'noexec' instance-method form ...\n";
$pl->noexec(1);
$pl->system;
$final += printok($? == 0);

print "+ Testing 'glob' method ...\n";
my $echo = Argv->new(qw(echo *));
$echo->glob;
print "'echo *' globs to: ";
$echo->system;
$final += printok($? == 0);

print "+ Testing 'dbglevel' instance-method form ...\n";
Argv->dbglevel(1);
my $ld = Argv->new($^X, qw(-bogus flag -V:ld));
$ld->dbglevel(0);
$ld->parse(qw(bogus=s));
$ld->system('-');
$final += printok($? == 0);
$ld->dbglevel(0);

$ld->dfltsets(['-']);
print $ld->qx;
$final += printok($? == 0);

my $e2 = Argv->new(qw(foo -y -x foo -r Next-to-last-test ...));
$e2->optset(qw(ONE TWO THREE));
$e2->parseONE(qw(x=s));
$e2->parseTWO(qw(y z));
$e2->parseTHREE('r');
$e2->prog('echo');
$e2->system;
$final += printok(!$? && ($e2->optsONE + $e2->optsTWO + $e2->optsTHREE) == 4);

exit $final if $final;

my $id2 = Argv->new(qw(id -y -x foo -a -r));
$id2->optset(qw(REMOVED));
my @removed = $id2->parseREMOVED(qw(r y x=s));
local $, = ' ';
print "Removed '@removed', left '@{[$id2->prog, $id2->opts, $id2->args]}'\n";
print "NOTE: this last test sets the 'noexec' flag so no exec happens\n";
Argv->noexec(1);
$id2->exec(qw(-));
