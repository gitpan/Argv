package Argv;

use strict;
use vars qw($VERSION @ISA @EXPORT_OK);
use Carp;
require Exporter;
@ISA = qw(Exporter);
$VERSION = '0.39';

@EXPORT_OK = qw(system exec qv); # to support the "FUNCTIONAL INTERFACE"

use constant MSWIN	=> $^O =~ /win32/i;

# Adapted from perltootc (see): an "eponymous meta-object" implementing
# "translucent attributes".
# For each key in the hash below, a method is automatically generated.
# Each of these sets the object attr if called as an instance method or
# the class attr if called as a class method. They return the instance
# attr if it's defined, the class attr otherwise. The method name is
# lower-case; e.g. 'qxargs'. The default values of each attribute comes
# from the hash value but may be overridden in the environment as shown.
use vars qw(%Argv);
%Argv = (
   AUTOCHOMP		=> $ENV{ARGV_AUTOCHOMP} || 0,
   AUTOGLOB		=> $ENV{ARGV_AUTOGLOB} || 0,
   AUTOQUOTE		=> defined($ENV{ARGV_AUTOQUOTE}) ?
			 $ENV{ARGV_AUTOQUOTE} : 1,
   DBGLEVEL		=> $ENV{ARGV_DBGLEVEL} || 0,
   DFLTSETS		=> [''],
   EXECWAIT		=> defined($ENV{ARGV_EXECWAIT}) ?
			 $ENV{ARGV_EXECWAIT} : 1,
   NATIVEPATH		=> $ENV{ARGV_NATIVEPATH} || scalar(MSWIN),
   NOEXEC		=> $ENV{ARGV_NOEXEC} || 0,
   QXARGS		=> $ENV{ARGV_QXARGS} || (MSWIN ? 16 : 128),
   STDERR		=> defined($ENV{ARGV_STDERR}) ?
			 $ENV{ARGV_STDERR} : 1,
   STDOUT		=> defined($ENV{ARGV_STDOUT}) ?
			 $ENV{ARGV_STDOUT} : 1,
   SYSTEMXARGS		=> 0,
);

sub stdmethod {
   my $meta = shift;
   no strict 'refs'; # need to evaluate $meta as a symbolic ref
   my @data = @_ ? @_ : keys %{$meta};
   for my $datum (@data) {
      my $method = lc $datum;
      *$method = sub {
	 use strict "refs";
	 my $self = shift;
	 if (ref $self) {
	   if (@_) {
	       $self->{$datum} = shift;
	       return $self;
	   } else {
	       return defined($self->{$datum}) ?
				 $self->{$datum} : __PACKAGE__->{$datum};
	   }
	 } else {
	    if (@_) {
	       __PACKAGE__->{$datum} = shift;
	       return $self;
	    } else {
	       return __PACKAGE__->{$datum};
	    }
	 }
      }
   }
}

__PACKAGE__->stdmethod;

# This class method is much like the above but needs some special
# logic so can't be auto-generated: If called with a param which is
# true, it starts up a coprocess. If called with false (aka 0) it
# shuts down the coprocess and destroys the IPC::ChildSafe object. And
# if called with no params at all it returns the IPC::ChildSafe object.
sub childsafe {
   my $self = shift;
   my $ipc_state = $_[0];
   my $ipc_obj;
   if ($ipc_state) {
      eval { require IPC::ChildSafe };
      if ($@ =~ /^Can't locate/) {
	 warn "Warning: IPC::ChildSafe not found - continuing in normal mode\n";
	 return undef;
      } elsif ($@) {
	 warn $@;
	 return undef;
      } else {
	 IPC::ChildSafe->VERSION(3.01);
      }
      $ipc_obj = IPC::ChildSafe->new(@_);
   }
   if (ref $self) {
      if (defined $ipc_state) {
	 $self->{IPC} = $ipc_obj;
	 return $self;
      } else {
	 return defined($self->{IPC}) ? $self->{IPC} : __PACKAGE__->{IPC};
      }
   } else {
      if (defined $ipc_state) {
	  __PACKAGE__->{IPC} = $ipc_obj;
	  return $self;
      } else {
	  return __PACKAGE__->{IPC};
      }
   }
}

sub stdopts {
    my $self = shift;
    my $class = ref $self || $self;
    require Getopt::Long;
    my @configs = @_ ? @_ : qw(no_auto_abbrev pass_through);
    Getopt::Long::config(@configs);
    no strict 'refs';
    my @flags = map lc, keys %{$class};
    my %opt;
    if (ref $self) {
	local @ARGV = @ARGV;
	Getopt::Long::GetOptions(\%opt, map {"$_=i"} @flags);
	$self->args(@ARGV);	# now a possibly depleted array
    } else {
	Getopt::Long::GetOptions(\%opt, map {"$_=i"} @flags);
    }
    for my $method (keys %opt) {
	$self->$method($opt{$method});
    }
    return $self;
}

# A class method which prints a summary of operations performed.
sub summary {
    my $class = shift;
    my($cmds, $operands);
    return unless %Argv::Summary;
    my $fmt = "%30s:  %4s\t%s\n";
    printf STDERR $fmt, "$class SUMMARY", 'Cmds', 'Operands';
    for (keys %Argv::Summary) {
	my @stats = @{$Argv::Summary{$_}};
	$cmds += $stats[0];
	$operands += $stats[1];
	printf STDERR $fmt, $_, $stats[0], $stats[1];
    }
    printf STDERR $fmt, 'TOTAL', $cmds, $operands if defined $cmds;
    %Argv::Summary = ();
}

# Constructor.
sub new {
   my $proto = shift;
   my($class, $self);
   if (ref $proto) {
      $class = ref $proto;
      $self = $proto;
      bless $self, $class;
      $self->optset('');
      if (@_) {
	 $self->{PROG} = [];
	 $self->{ARGS} = [];
      }
   } else {
      $class = $proto;
      $self = {};
      bless $self, $class;
      $self->optset('');
      $self->{PROG} = [];
      $self->{ARGS} = [];
   }
   $self->prog(shift) if @_;
   $self->opts(@{shift @_}) if ref $_[0];
   $self->args(@_) if @_;
   return $self;
}

# Instance methods; most class methods are auto-generated above.

sub prog {
   my $self = shift;
   if (@_ || !defined(wantarray)) {
      my @prog = ref $_[0] ? @{$_[0]} : @_;
      $self->dbg("setting prog to '@prog'");
      @{$self->{PROG}} = @prog;
   }
   if (@_) {
      return $self;
   } else {
      return wantarray ? @{$self->{PROG}} : ${$self->{PROG}}[0];
   }
}

sub args {
   my $self = shift;
   if (@_ || !defined(wantarray)) {
      $self->dbg("setting args to '@_'");
      @{$self->{ARGS}} = ref $_[0] ? @{$_[0]} : @_;
   }
   if (@_) {
      return $self;
   } else {
      return @{$self->{ARGS}};
   }
}

sub optset {
   my $self = shift;
   for (@_) {
      my $set = uc $_;
      $self->{OPTS}{$set} = [];
      $self->{LINKAGE}{$set} = {};
      my($p_meth, $o_meth, $f_meth) = map { $_ . $set } qw(parse opts flag);
      $self->dbg("installing optset methods $p_meth() $o_meth() $f_meth()");
      $self->{DESC}{$set} = [];
      no strict 'refs'; # needed to muck with symbol table
      *$p_meth = sub {
	 my $self = shift;
	 $self->{DESC}{$set} ||= [];
	 if (@_) {
	    if (ref($_[0]) eq 'ARRAY') {
		$self->{CFG}{$set} = shift;
	    } elsif (ref($_[0]) eq 'HASH') {
		$self->warning("do not provide a linkage specifier");
		shift;
	    }
	    $self->dbg("setting optset '$set' descs to '@_'");
	    @{$self->{DESC}{$set}} = @_;
	    $self->factor($set, $self->{DESC}{$set}, $self->{OPTS}{$set},
					    $self->{ARGS}, $self->{CFG}{$set});
	    if (defined $self->{OPTS}{$set}) {
	       my @parsedout = @{$self->{OPTS}{$set}};
	       $self->dbg("parsed out '@parsedout' into optset '$set'");
	    }
	 }
	 return @{$self->{OPTS}{$set}};
      } unless $Argv::{$p_meth};
      *$o_meth = sub {
	 my $self = shift;
	 $self->{OPTS}{$set} ||= [];
	 if (@_ || !defined(wantarray)) {
	    $self->dbg("setting opts to '@_' for optset '$set'");
	    @{$self->{OPTS}{$set}} = @_;
	 }
	 $self->dbg("returning opts '@{$self->{OPTS}{$set}}' for optset $set");
	 return @{$self->{OPTS}{$set}};
      } unless $Argv::{$o_meth};
      *$f_meth = sub {
	 my $self = shift;
	 if (@_ > 1) {
	    while(my($key, $val) = splice(@_, 0, 2)) {
	       $self->{LINKAGE}{$set}{$key} = $val;
	    }
	 } else {
	    my $key = shift;
	    return $self->{LINKAGE}{$set}{$key};
	 }
      } unless $Argv::{$f_meth};
   }
   return keys %{$self->{DESC}}; # this is the set of known optsets.
}

sub factor {
   my $self = shift;
   my($pset, $r_desc, $r_opts, $r_args, $r_cfg) = @_;
   my %vgra;
   {
      require Getopt::Long;
      my @configs = $r_cfg ? @$r_cfg : qw(auto_abbrev pass_through);
      push(@configs, 'debug') if $self->dbglevel >= 4;
      Getopt::Long::config(@configs);
      local @ARGV = @$r_args;
      Getopt::Long::GetOptions($self->{LINKAGE}{$pset}, @$r_desc) if @$r_desc;
      for (0..$#ARGV) { $vgra{$ARGV[$_]} = $_ }
   }
   my(@opts, @args);
   for (@$r_args) {
      if (defined $vgra{$_}) {
	 push(@args, $_);
      } else {
	 push(@opts, $_);
      }
   }
   @$r_opts = @opts if $r_opts;
   @$r_args = @args;
   return @opts;
}

sub extract {
   my $self = shift;
   my $set = shift;
   $self->optset($set) unless defined $self->{LINKAGE}{$set};
   my $p_meth = 'parse' . $set;
   my $o_meth = 'opts' . $set;
   $self->$p_meth(@_);
   my @extracts = $self->$o_meth();
   $self->dbg("removed '@extracts' for set '$set'");
   return @extracts;
}

sub quote {
   my $self = shift;
   for (@_) {
      # If requested, change / for \ in Windows file paths.
      s%/%\\%g if $self->nativepath;
      # Skip arg if already quoted ...
      next if substr($_, 0, 1) eq '"' && substr($_, -1, 1) eq '"';
      # ... or contains no special chars.
      next unless m%[^-=:_.\w/\\]% || tr%\n%%;
      # Special case - turn any expanded newlines back into literal \n.
      s%\n%\\n%g if MSWIN;
      # Special case - leave things that look like redirections alone.
      next if /^\d?(?:<{1,2})|(?:>{1,2})/;
      # Now quote embedded quotes ...
      $_ =~ s%(\\*)"%$1$1\\"%g;
      # quote trailing \ so it won't quote the " ...
      s%\\{1}$%\\\\%;
      # and last the entire string.
      $_ = qq("$_");
   }
   return @_;
}

sub glob {
   my $self = shift;
   my(@orig, @globbed) = $self->args;
   if (! @orig) {
      $self->warning("no arguments to glob");
      return 0;
   }
   for (@orig) {
      if (/^'(.*)'$/) {		# allow '' to escape globbing
	 push(@globbed, $1);
      } elsif (/[*?]/) {
	 push(@globbed, glob)
      } else {
	 push(@globbed, $_)
      }
   }
   $self->args(@globbed);
   $self->dbg("globbed to '@globbed'") if @globbed != @orig;
   return @globbed > @orig;
}

sub _sets2opts {
   my $self = shift;
   my(@sets, @opts);
   if (! @_) {
       @sets = @{$self->dfltsets};
   } elsif ($_[0] eq '-') {
      @sets = ();
   } elsif ($_[0] eq '+') {
      @sets = $self->optset;
   } else {
      my %known = map {$_ => 1} $self->optset;
      for (@_) { $self->warning("Unknown optset '$_'\n") if !$known{$_} }
      @sets = @_;
   }
   for my $set (@sets) {
      next unless $self->{OPTS}{$set} && @{$self->{OPTS}{$set}};
      push(@opts, @{$self->{OPTS}{$set}});
   }
   $self->dbg("adding opts from sets '@sets': [@opts]");
   return @opts;
}

sub _addstats {
    my $self = shift;
    my($prog, $argcnt) = @_;
    my $stats = $Argv::Summary{$prog} || [0, 0];
    $$stats[0]++;
    $$stats[1] += $argcnt;
    $Argv::Summary{$prog} = $stats;
}

sub exec {
   return Argv->new(@_)->exec if !ref($_[0]);
   my $self = shift;
   if ($self->childsafe) {
      exit($self->system(@_) | $self->childsafe->finish);
   } elsif (MSWIN && $self->execwait) {
      exit $self->system(@_);
   } else {
      my @cmd = (@{$self->{PROG}}, $self->_sets2opts(@_), @{$self->{ARGS}});
      if ($self->noexec) {
	 print STDERR "+ @cmd\n";
      } else {
	 $self->dbg("+ @cmd");
	 if (!$self->stdout) { open(_O, ">&STDOUT"); close(STDOUT) }
	 if (!$self->stderr) { open(_E, ">&STDERR"); close(STDERR) }
	 my $rc = CORE::exec @cmd;
	 if (defined(fileno(_O))) { open(STDOUT, ">&_O"); close(_O) }
	 if (defined(fileno(_E))) { open(STDERR, ">&_E"); close(_E) }
	 return $rc;
      }
   }
}

sub ipccmd {
    my $self = shift;
    my $cmd = shift;
    if (@_) {
	$cmd = "@_";
    } else {
	$cmd =~ /^\w+\s*(.*)/;
    }
    my %results = $self->childsafe->cmd($cmd);
    return %results;
}

sub system {
   return Argv->new(@_)->system if !ref($_[0]);
   my $self = shift;
   $self->glob if MSWIN && $self->autoglob;
   my @prog = @{$self->{PROG}};
   my @opts = $self->_sets2opts(@_);
   my @args = @{$self->{ARGS}};
   my @cmd = (@prog, @opts, @args);
   # Must pass (@prog, @opts, @args) in order for quoting to stick.
   @cmd = $self->quote(@prog, @opts, @args)
	 if (((MSWIN && @cmd > 1) || $self->childsafe) && $self->autoquote);
   my $rc = 0;
   if ($self->noexec) {
      print STDERR "+ @cmd\n";
   } else {
      if ($self->childsafe) {
	 my %results = $self->ipccmd(@cmd);
	 $? = $results{status} << 8;
	 print STDOUT @{$results{stdout}} if $self->stdout;
	 print STDERR @{$results{stderr}} if $self->stderr;
      } else {
	 $self->dbg("+ @cmd");
	 if (!$self->stdout) { open(_O, ">&STDOUT"); close(STDOUT) }
	 if (!$self->stderr) { open(_E, ">&STDERR"); close(STDERR) }
	 my $limit = $self->systemxargs;
	 if ($limit && @args) {
	    while (my @chunk = splice(@args, 0, $limit)) {
	       @cmd = (@prog, @opts, @chunk);
	       $rc |= CORE::system @cmd;
	    }
	 } else {
	    $rc = CORE::system @cmd;
	 }
	 if (defined(fileno(_O))) { open(STDOUT, ">&_O"); close(_O) }
	 if (defined(fileno(_E))) { open(STDERR, ">&_E"); close(_E) }
      }
      $self->_addstats("@prog", scalar @args);
   }
   return $rc;
}

sub qx {
   my $self = shift;
   my @prog = @{$self->{PROG}};
   my @opts = $self->_sets2opts(@_);
   my @args = @{$self->{ARGS}};
   my @cmd =(@prog, @opts, @args);
   # Must pass (@prog, @opts, @args) in order for quoting to stick.
   @cmd = $self->quote(@prog, @opts, @args)
		  if ((@cmd > 1 || $self->childsafe) && $self->autoquote);
   my @data;
   if ($self->childsafe) {
      my %results = $self->ipccmd(@cmd);
      $? = $results{status} << 8;
      print STDERR @{$results{stderr}} if $self->stderr;
      @data = @{$results{stdout}};
   } else {
      my $limit = $self->qxargs;
      if ($limit && @args) {
	 while (my @chunk = splice(@args, 0, $limit)) {
	    @cmd = (@prog, @opts, @chunk);
	    if ($self->noexec) {
	       print STDERR "+ @cmd\n";
	    } else {
	       $self->dbg("+ @cmd");
	       if (!$self->stderr) { open(_E, ">&STDERR"); close(STDERR) }
	       push(@data, CORE::qx(@cmd));
	       if (defined(fileno(_E))) { open(STDERR, ">&_E"); close(_E) }
	    }
	 }
      } else {
	 if ($self->noexec) {
	    print STDERR "+ @cmd\n";
	 } else {
	    $self->dbg("+ @cmd");
	    if (!$self->stderr) { open(_E, ">&STDERR"); close(STDERR) }
	    @data = CORE::qx(@cmd);
	    if (defined(fileno(_E))) { open(STDERR, ">&_E"); close(_E) }
	 }
      }
   }
   $self->_addstats("@prog", scalar @args);
   if (wantarray) {
      chomp(@data) if $self->autochomp;
      return @data;
   } else {
      my $data = join('', @data);
      chomp($data) if $self->autochomp;
      return $data;
   }
}

# Can't override qx() so we export an alias instead.
sub qv { return Argv->new(@_)->qx }

sub warning {
   my $self = shift;
   carp("Warning: ${$self->{PROG}}[-1]: ", @_);
}

# Not documented; primarily for internal use.
sub dbg {
   my $self = shift;
   my $level = $self->dbglevel;
   return unless $level;
   my $msg = "@_";
   my @caller = caller(1);
   $msg = join(': ', join(':', @caller[3,2]), $msg) if @caller;
   $msg = '+ ' . $msg;
   chomp $msg;
   if ($level == 1) {
      warn "@_\n" if $caller[3] =~ /system|exec|qx/;
   } elsif ($level == 2) {
      warn($msg . "\n");
   } elsif ($level == 3) {
      carp($msg);
   } elsif ($level == 4) {
      Carp::cluck($msg);
   } else {
      die "no such debug level: $level";
   }
}

1;

__END__

=head1 NAME

Argv - Provide an O-O interface to an ARGV

=head1 SYNOPSIS

    use Argv;

    # A roundabout way of getting perl's version.
    my $pl = Argv->new(qw(perl -v));
    $pl->exec;

    # Run /bin/cat, showing how to provide "predigested" options.
    Argv->new('/bin/cat', [qw(-u -n)], @ARGV)->system;

    # A roundabout way of globbing.
    my $echo = Argv->new(qw(echo M*));
    $echo->glob;
    my $globbed = $echo->qx;
    print "'echo M*' globs to: $globbed";

    # A demonstration of the builtin xargs-like behavior.
    my @files = split(/\s+/, $globbed);
    my $ls = Argv->new(qw(ls -d -l), @files);
    $ls->parse(qw(d l));
    $ls->dbglevel(1);
    $ls->qxargs(1);
    my @long = $ls->qx;
    $ls->dbglevel(0);
    print @long;

    # A demonstration of how to use option sets in a wrapper program.
    my $wrapper = Argv->new(qw(Who -y -x foo -r));
    $wrapper->dbglevel(1);
    $wrapper->optset(qw(ONE TWO THREE));
    $wrapper->parseONE(qw(x=s));
    $wrapper->parseTWO(qw(y z));
    $wrapper->parseTHREE(qw(r));
    $wrapper->prog(lc($wrapper->prog));
    $wrapper->exec(qw(THREE));

    ## More advanced examples can be lifted from the test script.

=head1 RAISON D'ETRE

This module presents an O-O approach to command lines, allowing you to
instantiate an 'argv object', manipulate it, and eventually run it,
e.g.:

    my $ls = Argv->new('ls', ['-l']));
    my $rc = $ls->system;	# or $ls->exec or $ls->qx

Which raises the immediate question - what value does this mumbo-jumbo
add over Perl's native support such as:

    my $rc = system(qw(ls -l));

The answer comes in a few parts:

=item * STRUCTURE

First, by recognizing the underlying properties of an arg vector. Every
argv begins with a program name which is followed by (potentially)
options and operands. The object factors its raw argv into these three
groups, and provides accessor methods which allow operations on each
group independently.

=item * OPTION SETS

Second, the module encapsulates and extends C<Getopt::Long> to allow
parsing of the argv's options into different I<option sets>. This is
useful in the case of wrapper programs which may, for instance, need to
parse out one set of flags which direct the behavior of the wrapper
itself, parse a different set and pass them to program X, then
another for program Y, then exec program Z with the remainder.  Doing
this kind of thing on a basic @ARGV using indexing and C<splice()> is
do-able but leads to spaghetti-ish code and lots of off-by-one errors.

=item * EXTRA FEATURES

The I<execution methods> C<system, exec, and qx> extend their Perl
builtin analogues in a few ways:

=over 4

=item 1. An xargs-like capability.

You can set a maximum number of arguments to be processed at a time,
which allows you to blithely invoke e.g. C<$obj->qx> on a list of any size
without fear of exceeding your shell's limits.

=item 2. Unix-like exec behavior on Windows.

The Perl builtin exec() on Windows behaves differently from that of
Unix; it returns to the shell after invoking the new process. In
contrast, the Argv->exec method blocks until the new process is
finished, which makes it "feel" like Unix exec().

=item 3. Automatic quoting of system()

Win32 system() always uses the shell, whereas other systems offer
ways to avoid a shell. The C<$obj->system> method automatically quotes
its arguments on Win32 to protect against the shell in those
cases where it wouldn't be used on more reasonable platforms.

=item 4. Automatic globbing.

On Win32 systems, the shell doesn't expand wildcards but rather leaves
this to the application. Perl provides the glob() function for this
purpose; this module can optionally apply glob() to each operand in the
argv before execution, automatically.

=item 5. Automatic chomping.

The module can be set to automatically chomp the results of C<$obj->qx>,
though by default lines are un-chomped.

=item 6. Pathname conversion.

By default, the I<execution methods> will convert pathnames to their
native format before executing. I.e. pathnames containing / will be
converted to \ on a Windows platform.

=back

All of these behaviors can be toggled - see CLASS METHODS below.

=head1 DESCRIPTION

An Argv object treats the command line as 3 separate entities: the
I<program>, the I<options>, and the I<args>. The I<options> may be
futher subdivided into named I<option sets> by use of the C<optset>
method. When one of the I<execution methods> is called, the parts are
reassmbled into a single list and passed to the underlying Perl
primitive.

Contrast this with the way Perl handles its own programs, putting the
0th element of the argv in C<$0> and the rest in C<@ARGV>.

By default there's one option set, known as the I<anonymous option
set>, whose name is the null string. All parsed options go there. The
advanced user can define more option sets, parse options into them
according to Getopt::Long-style descriptions, query or set the parsed
values, and then reassemble them in any way desired at exec time.
Declaring an option set automatically declares a set of methods for
dealing with it (see below).

=head1 FUNCTIONAL INTERFACE

Because the extensions to system/exec/qx described here may be useful
for aid in writing portable programs, they're made available for export
without need of accompanying objects, thus:

    use Argv qw(system exec qv);

will override the Perl builtins. There is no way to override the
operator C<qx()> so we provide an alias C<qv()>.

=head1 CONSTRUCTOR

    my $obj = Argv->new(@list)

The C<@list> is what will be parsed/executed/etc by subsequent method
calls. During initial construction, the first element of the list is
separated off as the I<program>; the rest is lumped together as part of
the I<args> until and unless option parsing is done, in which case
matched options are shifted into collectors for their various I<option
sets>. You can also create a "predigested" instance by passing any or
all of the prog, opt, or arg parts as array refs. E.g.

    Argv->new([qw(cleartool ci)], [qw(-nc -ide)], qw(file1 file2 file3));

Predigested options are placed in the default (anonymous) option set.

=head1 METHODS

Unless otherwise indicated, methods are of the traditional get/set
variety, such that:

=over 4

=item 1. SET

If arguments are passed they become the new value of the referenced
attribute. In this case the object is returned.

=item 2. GET

If no arguments are passed the current value of the attribute is
returned.

=item 3. CLEAR

If no arguments are passed I<and> the method is called in a void context,
the attribute is cleared.

=back

=head2 INSTANCE METHODS

=over 4

=item * prog()

Returns or sets the name of the program (the C<"argv[0]">). This can be
a list, e.g. C<qw(rcs co)>.

=item * args()

Returns or sets the list of operands (aka arguments).

=item * optset(<list-of-set-names>);

For each name I<NAME> in the parameter list, an I<option set> of that
name is declared and 3 new methods are registered dynamically:
C<parseI<NAME>(), optsI<NAME>(), and flagI<NAME>()>. These methods are
described below: note that the I<anonymous option set> (see I<OPTION
SETS>) is predefined, so the methods C<parse(), opts(), and flag()> are
always available.  Most users won't need to define any other sets.

=item * parseI<NAME>(...option-descriptions...)

Takes a list of option descriptions and uses Getopt::Long::GetOptions()
to parse them out of the current argv B<and into option set I<NAME>>.
The opt-descs are exactly as supported by parseI<FOO>() are exactly the
same as those described for Getopt::Long, except that no linkage
argument is allowed.

=item * optsI<NAME>()

Returns or sets the list of options in the B<option set I<NAME>>.

=item * flagI<name>()

Returns or sets the value of flag I<name> in the appropriate optset.

=item * system([<optset-list>])

Reassembles the complete argv and invokes system() on it. Return value
and value of $?, $!, etc. are just as described for L<perlfunc/"system">

Arguments to this method determine which of the parsed option-sets will
be used in the executed argv. If passed no arguments, C<$obj->system>
uses the value of the 'dfltsets' attribute as the list of desired
sets. By default only the anonymous option set is used.
A different set of option sets may be requested via the C<$obj->dfltsets
method.

An option set may be requested by passing its name (with an optional
leading '+') or explicitly rejected by using its name with a leading
'-'. Thus, given the existence of option sets I<ONE, TWO, and THREE>,
the following are legal:

   $obj->system;			# use the anonymous set only
   $obj->system('+');			# use all option sets
   $obj->system(qw(ONE THREE);		# use sets ONE and THREE

The following sequence would also use sets ONE and THREE.

   $obj->dfltsets(qw(ONE THREE);
   $obj->system;

while this would use all parsed options:

   $obj->dfltsets('+');
   $obj->system;

and this would set the default to none, class-wide:

    Argv->dfltsets('-');

=item * exec()

Similar to I<system> above, but never returns. On Windows, it blocks
until the new process finishes for a more Unix-like behavior than
L<perlfunc/"exec">.

Option sets are handled as described in I<system> above.

=item * qx()

Same semantics as L<perlfunc/"qx"> (aka backquotes) but has the
capability to process only a set number of arguments at a time to avoid
exceeding the shell's line-length limit. This value is settable with
the I<qxargs> method. No matter how many shell invocations are used,
the total output is returned in a context-sensitive manner a la
L<perlfunc/"qx">.

Option sets are handled as described in I<system> above.

=item * extract

Takes an I<optset> name and a list of option descs; creates the named
optset, extracts any of the named options, places them in the specified
optset, and returns them.

=item * quote(@list)

Protects the argument list against exposure to a shell by quoting each
element. This method is invoked automatically by the I<system>
method on Windows platforms, where L<perlfunc/"system"> always uses
a shell, and by the I<qx> method on all platforms since it always
invokes a shell.

The automatic use of I<quote> can be turned off via the I<autoquote>
method (see).

IMPORTANT: this method quotes its argument list IN PLACE. In other
words, its arguments may be modified.

=item * glob

Expands the argument list using L<perlfunc/"glob">. Primarily useful on
Windows where the invoking shell does not do this for you.

Automatic use of I<glob> (on Windows) can be turned on with the
I<autoglob> method (see).

=back

=head2 CLASS METHODS

The following are auto-generated accessor methods of the classic
get/set variety; if arguments are passed they become the new value of
the eponymous attribute, and the current value is returned whether said
attribute was changed or not.

These also have the property that they may be used as either class or
instance methods. If used as an instance method the attribute is set
only on that instance; if used as a class method it sets or gets the
default for all instances which haven't overridden it.  This is an
implementation of I<translucent attributes> as described in Tom
Christiansen's I<perltootc> tutorial.

=over 4

=item * autoglob

If set, the C<glob()> function is applied to the operands
(C<$self->args>) on Windows only.

=item * autoquote

If set, the operands are automatically quoted against shell expansion
before C<system()> on Windows and C<qx()> on all platforms (since C<qx>
always invokes a shell, and C<system()> always does so on Windows).

=item * dbglevel

Sets the debug level. Level 0 (the default) is no debugging, 1 prints
each command before executing it, and higher levels offer progressively
more output.

=item * dfltsets

Sets and/or returns the default set of I<option sets> to be used in
building up the command line at execution time.  The default-default is
the I<anonymous option set>. I<Note: this method takes an B<array
reference> as its optional argument and returns an array ref as well>.

=item * execwait

If set, C<$self->exec> on Windows blocks until the new process is
finished for a more consistent Unix-like behavior than the traditional
Win32 Perl port.

=item * nativepath

If set, converts pathnames to their native format just before
executing. This is set by default on Windows only, thus converting
/x/y/z to \x\y\z.

=item * noexec

Analogous to the C<-n> flag to I<make>; prints what would be executed
without executing anything.

=item * qxargs

Gets or sets the number of arguments to process per shell invocation
in the C<qx> method. Setting this value to 0 suppresses the behavior.

=item * systemxargs

Analogous to I<qxargs> but turned off (set to 0) by default. The reason
for this is that C<qx()> is typically used to I<read> data whereas
C<system()> is more often used to make stateful changes. Consider
that "ls foo bar" produces the same result if broken up into "ls foo"
and "ls bar" but the same cannot be said for "mv foo bar".

=item * stdout

Default value is true which has no effect. When passed a false value,
e.g:

   $obj->stdout(0);

causes STDOUT to be closed during invocation of any of the I<execution
methods> C<system, exec, and qx>, and restored when they finish. A
fancy (and portable) way of saying 1>/dev/null without needing a
shell.

=item * stderr

As above, for STDERR.

=back

Defaults for all of the above may be provided in the environment, e.g.
ARGV_QXARGS=32 or ARGV_STDERR=0;

=item * stdopts

The attributes above can be set via method calls (e.g.
C<$obj->dbglevel(1)>) or environment variables (ARGV_DBGLEVEL=1). Use
of the <$obj->stdopts> method allows them to be parsed from the command
line as well, e.g. I<myscript -dbglevel 1>. If invoked as a class
method it causes options of the same names as the methods above to be
parsed (and removed) from the current C<@ARGV>.  When applied to an
instance it parses and potentially depletes the current argument vector
of that object.  In either case it calls the same-named methods with
the specified values on an instance or class as appropriate. Example:

    Argv->stdopts;

would cause the script to parse the following command line:

    script -noexec 1 -dbglevel 2 -flag1 -flag2 arg1 arg2 arg3 ...

so as to remove the C<-noexec 1 -dbglevel 2> and set the two class attrs.

=head1 IPC::ChildSafe (CO-PROCESS) SUPPORT

A method I<childsafe> is defined which will cause processes to be run
under control of the IPC::ChildSafe module (see). This keeps one
instance of a given utility running in the background and feeds
commands to it rather than forking/exec-ing each time. The utility
being controlled must conform to the standards defined in the
PODs for IPC::ChildSafe.

This method may be used as either a class or instance method; the scope
of the change in behavior varies accordingly. Parameters passed to this
method are passed directly to the IPC::ChildSafe constructor. Typical
usage (sample from ClearCase) would be:

    $self->childsafe('cleartool', 'pwd -h', 'Usage: pwd');

The first arg is the program name, the 2nd must be a cheap command
providing a well-known one-line output, the 3rd is that output. The
coprocess can be ended explicitly via:

    $self->childsafe(0);

B<Note: this feature is in alpha state and may change.>

=head1 PORTING

This module is known to work on Solaris 2.5-7 and Windows NT 4.0SP3-5.
As these two platforms are quite different, there should be no I<major>
portability issues, but please send reports of tweaks needed for other
platforms to the address below.

=head1 AUTHOR

David Boyce <dsb@world.std.com>

=head1 COPYRIGHT

Copyright (c) 1999 David Boyce. All rights reserved.  This perl program
is free software; you may redistribute and/or modify it under the same
terms as Perl itself.

=head1 SEE ALSO

perl(1), Getopt::Long(3), IPC::ChildSafe(3)

=cut
