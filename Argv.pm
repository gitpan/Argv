package Argv;

use strict;
use vars qw($VERSION @ISA @EXPORT_OK);
use Carp;
require Exporter;
@ISA = qw(Exporter);
$VERSION = '0.42';

@EXPORT_OK = qw(system exec qv); # to support the "FUNCTIONAL INTERFACE"

use constant MSWIN	=> $^O =~ /win32/i;

# Adapted from perltootc (see): an "eponymous meta-object" implementing
# "translucent attributes".
# For each key in the hash below, a method is automatically generated.
# Each of these sets the object attr if called as an instance method or
# the class attr if called as a class method. They return the instance
# attr if it's defined, the class attr otherwise. The method name is
# lower-case; e.g. 'qxargs'. The default values of each attribute come
# from the hash value but may be overridden in the environment as shown.
use vars qw(%Argv);
%Argv = (
    AUTOCHOMP	=> $ENV{ARGV_AUTOCHOMP} || 0,
    AUTOFAIL	=> $ENV{ARGV_AUTOFAIL} || 0,
    AUTOGLOB	=> $ENV{ARGV_AUTOGLOB} || 0,
    AUTOQUOTE	=> defined($ENV{ARGV_AUTOQUOTE}) ? $ENV{ARGV_AUTOQUOTE} : 1,
    DBGLEVEL	=> $ENV{ARGV_DBGLEVEL} || 0,
    DFLTSETS	=> [''],
    EXECWAIT	=> defined($ENV{ARGV_EXECWAIT}) ? $ENV{ARGV_EXECWAIT} : 1,
    PATHNORM	=> $ENV{ARGV_PATHNORM} || scalar(MSWIN),
    NOEXEC	=> $ENV{ARGV_NOEXEC} || 0,
    QXARGS	=> $ENV{ARGV_QXARGS} || (MSWIN ? 16 : 128),
    STDERR	=> defined($ENV{ARGV_STDERR}) ? $ENV{ARGV_STDERR} : 1,
    STDOUT	=> defined($ENV{ARGV_STDOUT}) ? $ENV{ARGV_STDOUT} : 1,
    SYSTEMXARGS	=> 0,
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
	    # In null context with no args, set boolean value 'on'.
	    if (!@_ && !defined(wantarray)) {
		@_ = (1);
	    }
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
sub ipc_childsafe {
    my $self = shift;
    my $ipc_state = $_[0];
    my $ipc_obj;
    if ($ipc_state) {
	eval { require IPC::ChildSafe };
	return undef if $@;
	IPC::ChildSafe->VERSION(3.07);
	$ipc_obj = IPC::ChildSafe->new(@_);
    }
    if (ref $self) {
	if (defined $ipc_state) {
	    $self->{_IPC_CHILDSAFE} = $ipc_obj;
	    return $self;
	} else {
	    return defined($self->{_IPC_CHILDSAFE}) ?
			$self->{_IPC_CHILDSAFE} : __PACKAGE__->{_IPC_CHILDSAFE};
	}
    } else {
	if (defined $ipc_state) {
	    __PACKAGE__->{_IPC_CHILDSAFE} = $ipc_obj;
	    return $self;
	} else {
	    return __PACKAGE__->{_IPC_CHILDSAFE};
	}
    }
}

sub stdopts {
    my $self = shift;
    my $r_argv = ref $_[0] ? shift : undef;
    require Getopt::Long;
    Getopt::Long->VERSION(2.17); # has 'prefix_pattern'
    my @configs = qw(pass_through prefix_pattern=(-/|--));
    my @flags = map {"$_=i"} ((map lc, keys %Argv::Argv), @_);
    my %opt;
    if (ref $self) {
	if ($r_argv) {
	    local @ARGV = @$r_argv;
	    Getopt::Long::Configure(@configs);
	    Getopt::Long::GetOptions(\%opt, @flags);
	    Getopt::Long::Configure('default');
	    @$r_argv = @ARGV;
	} else {
	    local @ARGV = $self->args;
	    if (@ARGV) {
		Getopt::Long::Configure(@configs);
		Getopt::Long::GetOptions(\%opt, @flags);
		Getopt::Long::Configure('default');
		$self->args(@ARGV);
	    }
	}
    } elsif ($r_argv) {
	local @ARGV = @$r_argv;
	Getopt::Long::Configure(@configs);
	Getopt::Long::GetOptions(\%opt, @flags);
	Getopt::Long::Configure('default');
	@$r_argv = @ARGV;
    } elsif (@ARGV) {
	Getopt::Long::Configure(@configs);
	Getopt::Long::GetOptions(\%opt, @flags);
	Getopt::Long::Configure('default');
    }
    for my $method (keys %opt) { $self->$method($opt{$method}) }
    return $self;
}

# A class method which returns a summary of operations performed in
# printable format. Call it with a void context to start data-
# collection, with a scalar context to end it and get the report.
sub summary {
    my $class = shift;
    my($cmds, $operands);
    if (!defined wantarray) {
	# This is a horrible hack ....
	%Argv::Summary = (FOO => 0);
	%Argv::Summary = ();
	return;
    }
    return unless %Argv::Summary;
    my $fmt = "%30s:  %4s\t%s\n";
    my $str = sprintf $fmt, "$class SUMMARY", 'Cmds', 'Operands';
    for (keys %Argv::Summary) {
	my @stats = @{$Argv::Summary{$_}};
	$cmds += $stats[0];
	$operands += $stats[1];
	$str .= sprintf $fmt, $_, $stats[0], $stats[1];
    }
    $str .= sprintf $fmt, 'TOTAL', $cmds, $operands if defined $cmds;
    %Argv::Summary = ();
    return $str;
}

# Constructor.
sub new {
    my $proto = shift;
    my($class, $self);
    if ($class = ref($proto)) {
	my %clone = %{$proto};	# a shallow copy
	$self = \%clone;
    } else {
	$class = $proto;
	$self = {};
	$self->{PROG} = [];
	$self->{ARGS} = [];
    }
    bless $self, $class;
    $self->optset('');
    $self->cmd(@_) if @_;
    return $self;
}

# Instance methods; most class methods are auto-generated above.

sub cmd {
    my $self = shift;
    $self->{PROG} = [];
    $self->{OPTS}{''} = [];
    $self->{ARGS} = [];
    $self->prog(shift) if @_;
    $self->opts(@{shift @_}) if ref $_[0];
    $self->args(@_) if @_;
    return $self;
}

sub prog {
    my $self = shift;
    if (@_) {
	my @prog = ref $_[0] ? @{$_[0]} : @_;
	@{$self->{PROG}} = @prog;
    } elsif (!defined(wantarray)) {
	@{$self->{PROG}} = ();
    }
    if (@_) {
	return $self;
    } else {
	return wantarray ? @{$self->{PROG}} : ${$self->{PROG}}[0];
    }
}

sub args {
    my $self = shift;
    if (@_) {
	my @args = ref $_[0] ? @{$_[0]} : @_;
	@{$self->{ARGS}} = @args;
    } elsif (!defined(wantarray)) {
	@{$self->{ARGS}} = ();
    }
    if (@_) {
	return $self;
    } else {
	return @{$self->{ARGS}};
    }
}

# Generates the parse(), opts(), and flag() method families.
sub optset {
    my $self = shift;
    for (@_) {
	my $set = uc $_;
	$self->{OPTS}{$set} = [];
	$self->{LINKAGE}{$set} = {};
	my($p_meth, $o_meth, $f_meth) = map { $_ . $set } qw(parse opts flag);
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
		@{$self->{DESC}{$set}} = @_;
		$self->factor($set, $self->{DESC}{$set}, $self->{OPTS}{$set},
		$self->{ARGS}, $self->{CFG}{$set});
		if (defined $self->{OPTS}{$set}) {
		    my @parsedout = @{$self->{OPTS}{$set}};
		}
	    }
	    return @{$self->{OPTS}{$set}};
	} unless $Argv::{$p_meth};
	*$o_meth = sub {
	    my $self = shift;
	    $self->{OPTS}{$set} ||= [];
	    if (@_ || !defined(wantarray)) {
		@{$self->{OPTS}{$set}} = @_;
	    }
	    return @_ ? $self : @{$self->{OPTS}{$set}};
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
	Getopt::Long->VERSION(2.17); # faster, also has 'Configure()'
	my @configs = $r_cfg ? @$r_cfg : qw(auto_abbrev pass_through);
	push(@configs, 'debug') if $self->dbglevel == 5;
	local @ARGV = @$r_args;
	Getopt::Long::Configure(@configs);
	Getopt::Long::GetOptions($self->{LINKAGE}{$pset}, @$r_desc) if @$r_desc;
	Getopt::Long::Configure('default');
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
    return @extracts;
}

sub quote {
    my $self = shift;
    for (@_) {
	# If requested, change / for \ in Windows file paths.
	s%/%\\%g if $self->pathnorm;
	# Skip arg if already quoted ...
	next if substr($_, 0, 1) eq '"' && substr($_, -1, 1) eq '"';
	# ... or contains no special chars.
	next unless m%[^-=:_.\w\\/]% || tr%\n%%;
	# Special case - turn any internal newlines back into literal \n.
	s%\n(?=.)%\\n%g if MSWIN;
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
    return __PACKAGE__->new(@_)->exec if !ref($_[0]);
    my $self = shift;
    if ($self->ipc_childsafe) {
	exit($self->system(@_) | $self->ipc_childsafe->finish);
    } elsif (MSWIN && $self->execwait) {
	exit $self->system(@_);
    } else {
	my @cmd = (@{$self->{PROG}}, $self->_sets2opts(@_), @{$self->{ARGS}});
	if ($self->noexec) {
	    print STDERR "- @cmd\n";
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
    # Throw out the prog name since it's already running
    if (@_) {
	$cmd = "@_";
    } else {
	$cmd =~ s/^\w+\s*(.*)/$1/;
    }
    # Hack - there's an "impedance mismatch" between instance
    # methods in this class and the class methods in
    # IPC::ChildSafe, so we toggle the attrs for every cmd.
    $self->ipc_childsafe->dbglevel($self->dbglevel);
    $self->ipc_childsafe->noexec($self->noexec);
    my %results = $self->ipc_childsafe->cmd($cmd);
    return %results;
}

sub system {
    return __PACKAGE__->new(@_)->system if !ref($_[0]);
    my $self = shift;
    $self->glob if MSWIN && $self->autoglob;
    my @prog = @{$self->{PROG}};
    my @opts = $self->_sets2opts(@_);
    my @args = @{$self->{ARGS}};
    my @cmd = (@prog, @opts, @args);
    # Must pass (@prog, @opts, @args) in order for quoting to stick.
    @cmd = $self->quote(@prog, @opts, @args)
	if (((MSWIN && @cmd > 1) || $self->ipc_childsafe) && $self->autoquote);
    my $rc = 0;
    if ($self->noexec) {
	print STDERR "- @cmd\n";
    } else {
	if ($self->ipc_childsafe) {
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
	$self->_addstats("@prog", scalar @args) if defined(%Argv::Summary);
    }
    exit $?>>8 if $? && $self->autofail;
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
	if ((@cmd > 1 || $self->ipc_childsafe) && $self->autoquote);
    my @data;
    if ($self->ipc_childsafe) {
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
		    print STDERR "- @cmd\n";
		} else {
		    $self->dbg("+ @cmd");
		    if (!$self->stderr) { open(_E, ">&STDERR"); close(STDERR) }
		    push(@data, CORE::qx(@cmd));
		    if (defined(fileno(_E))) { open(STDERR, ">&_E"); close(_E) }
		}
	    }
	} else {
	    if ($self->noexec) {
		print STDERR "- @cmd\n";
	    } else {
		$self->dbg("+ @cmd");
		if (!$self->stderr) { open(_E, ">&STDERR"); close(STDERR) }
		@data = CORE::qx(@cmd);
		if (defined(fileno(_E))) { open(STDERR, ">&_E"); close(_E) }
	    }
	}
    }
    $self->_addstats("@prog", scalar @args) if defined(%Argv::Summary);
    exit $?>>8 if $? && $self->autofail;
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
sub qv { return __PACKAGE__->new(@_)->qx }

sub warning {
    my $self = shift;
    carp("Warning: ${$self->{PROG}}[-1]: ", @_);
}

# Not documented; primarily for internal use.
sub dbg {
    my $self = shift;
    return unless $self->dbglevel;
    warn "@_\n" if (caller(1))[3] =~ /system|exec|qx/;
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
    @ARGV = qw(Who -a -y foo -r);	# hack up an @ARGV
    my $who = Argv->new(@ARGV);		# instantiate
    $who->dbglevel(1);			# set verbosity
    $who->optset(qw(UNAME FOO WHO));	# define 3 option sets
    $who->parseUNAME(qw(a m n p));	# parse these to set UNAME
    $who->parseFOO(qw(y=s z));		# parse -y and -z to FOO
    $who->parseWHO('r');		# for the 'who' cmd
    warn "got -y flag in option set FOO\n" if $who->flagFOO('y');
    print Argv->new('uname', $who->optsUNAME)->qx;
    $who->prog(lc $who->prog);		# force $0 to lower case
    $who->exec(qw(WHO));		# exec the who cmd

More advanced examples can be lifted from the test script or the
./examples subdirectory.

=head1 RAISON D'ETRE

This module presents an O-O approach to command lines, allowing you to
instantiate an 'argv object', manipulate it, and eventually run it,
e.g.:

    my $ls = Argv->new('ls', ['-l']));
    my $rc = $ls->system;	# or $ls->exec or $ls->qx

Which raises the immediate question - what value does this mumbo-jumbo
add over Perl's native support, e.g.:

    my $rc = system(qw(ls -l));

The answer comes in a few parts:

=over

=item * STRUCTURE

First, Argv recognizes the underlying property of an arg vector, which
is that it begins with a program name potentially followed by options,
then operands. An Argv object factors a raw argv into these three
groups and provides accessor methods to allow operations on each group
independently, and can then put them back together for execution.


=item * OPTION SETS

Second, Argv encapsulates and extends C<Getopt::Long> to allow parsing
of the argv's options into different I<option sets>. This is useful in
the case of wrapper programs which may, for instance, need to parse out
one set of flags which direct the behavior of the wrapper itself,
extract a different set and pass them to program X, another for program
Y, then exec program Z with the remainder.  Doing this kind of thing on
a basic @ARGV using indexing and splicing is do-able but leads to
spaghetti code and potential off-by-one errors.

=item * EXTRA FEATURES

The I<execution methods> C<system, exec, and qx> extend their Perl
builtin analogues in a few ways, for example:

=over

=item 1. An xargs-like capability.

=item 2. Unix-like C<exec()> behavior on Windows.

=item 3. Automatic quoting of C<system()> on Win32 and C<qx()> everywhere

=item 4. Automatic globbing (primarily for Windows)

=item 5. Automatic chomping.

=item 6. Pathname normalization.

=back

=back

All of these behaviors can be toggled, either as class or instance
attributes. See STANDARD METHODS below.

=head1 DESCRIPTION

An Argv object treats a command line as 3 separate entities: the
I<program>, the I<options>, and the I<args>. The I<options> may be
futher subdivided into user-defined I<option sets> by use of the
C<optset> method. When one of the I<execution methods> is called, the
parts are reassmbled into a single list and passed to the underlying
Perl execution function.

Compare this with the way Perl works natively, keeping the 0th element
of the argv in C<$0> and the rest in C<@ARGV>.

By default there's one option set, known as the I<anonymous option
set>, whose name is the null string. All parsed options go there.  The
advanced user can define more option sets, parse options into them
according to Getopt::Long-style descriptions, query or set the parsed
values, and then reassemble them in any way desired at exec time.
Declaring an option set automatically generates a set of methods for
manipulating it (see below).

All argument-parsing within Argv is done via Getopt::Long.

=head1 FUNCTIONAL INTERFACE

Because the extensions to C<system/exec/qx> described here may be useful
for aid in writing portable programs, they're made available for export
as traditional functions. Thus:

    use Argv qw(system exec qv);

will override the Perl builtins. There is no way to override the
operator C<qx()> so an alias C<qv()> is provided.

=head1 CONSTRUCTOR

    my $obj = Argv->new(@list)

The C<@list> is what will be parsed/executed/etc by subsequent method
calls. During initial construction, the first element of the list is
separated off as the I<program>; the rest is lumped together as part of
the I<args> until and unless option parsing is done, in which case
matched options are shifted into collectors for their various I<option
sets>. You can also create a "predigested" instance by passing any or
all of the prog, opt, or arg parts as array refs. E.g.

    Argv->new([qw(cvs ci)], [qw(-l -n)], qw(file1 file2 file3));

Predigested options are placed in the default (anonymous) option set.

The constructor can be used as a class or instance method. When a new
instance is generated from an existing one, the new one is a copy
of its progenitor.

=head1 METHODS

=head2 INSTANCE METHODS

=over

=item * prog()

Returns or sets the name of the program (the C<"argv[0]">). This can be
a list, e.g. C<qw(rcs co)> or an array reference.

=item * args()

Returns or sets the list of operands (aka arguments). As above, it may
be passed a list or an array reference. If called in a void context and
without args, the effect is to set the list of operands to C<()>.

=item * optset(<list-of-set-names>);

For each name I<NAME> in the parameter list, an I<option set> of that
name is declared and 3 new methods are registered dynamically:
C<parseI<NAME>(), optsI<NAME>(), and flagI<NAME>()>. These methods are
described below: note that the I<anonymous option set> (see I<OPTION
SETS>) is predefined, so the methods C<parse(), opts(), and flag()> are
always available.  Most users won't need to define any other sets.
Note that option-set names are forced to upper-case.

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
and value of $?, $!, etc. are just as described in L<perlfunc/"system">

Arguments to this method determine which of the parsed option-sets will
be used in the executed argv. If passed no arguments, C<$obj->system>
uses the value of the 'dfltsets' attribute as the list of desired sets.
The default value of 'dfltsets' is the anonymous option set.

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

and this would set the default to none class-wide, and then use it:

    Argv->dfltsets('-');
    $obj->system;

By default the C<$obj->system> method autoquotes its arguments I<iff>
the platform is Windows and the arguments are a list, because in this
case a shell is always used. This behavior can be toggled with
C<$obj->autoquote>.  I<Note: if and when Perl 5.6 fixes this, Argv will
be changed to examine the value of $]>.

=item * exec()

Similar to I<system> above, but never returns. On Windows, it blocks
until the new process finishes for a more Unix-like behavior than
the I<exec> implemented by the C runtime library on Windows, if
the B<execwait> attribute is set. This is actually implemented as

	exit $obj->system(LIST);

and thus all C<system> shell-quoting issues apply

Option sets are handled as described in I<system> above.

=item * qx()

Same semantics as described in L<perlfunc/"qx"> but has the capability
to process only a set number of arguments at a time to avoid exceeding
the shell's line-length limit. This value is settable with the
I<qxargs> method.

Also, if I<autoquote> is set the arguments are quoted to protect them
against the platform-standard shell I<on all platforms>. 

Option sets are handled as described in I<system> above.

=item * extract

Takes an I<optset> name and a list of option descs; creates the named
optset, extracts any of the named options, places them in the specified
optset, and returns them.

=item * quote(@list)

Protects the argument list against exposure to a shell by quoting each
element. This method is invoked automatically by the I<system> method
on Windows platforms, where the underlying I<system> primitive always
uses a shell, and by the I<qx> method on all platforms since it invokes
a shell on all platforms.

The automatic use of I<quote> can be turned off via the I<autoquote>
method (see).

IMPORTANT: this method quotes its argument list IN PLACE. In other
words, it may modify its arguments.

=item * glob

Expands the argument list using the Perl I<glob> builtin. Primarily
useful on Windows where the invoking shell does not do this for you.

Automatic use of I<glob> on Windows can be enabled via the I<autoglob>
method (vide infra).

=back

=head2 STANDARD METHODS

The following are auto-generated accessor methods of the classic
get/set variety; if arguments are passed they become the new value of
the eponymous attribute, while the current value is returned whether
said attribute was changed or not.

These also have the property that they may be used as either class or
instance methods. If used as an instance method the attribute is set
only on that object; if used as a class method it sets or gets the
default for all instances which haven't overridden it.  This is an
implementation of I<translucent attributes> as described in Tom
Christiansen's I<perltootc> tutorial.

If called in a void context with no parameters, the boolean attribute
is turned I<on>. Thus these are both I<set> invocations:

	$obj->autochomp(1);
	$obj->autochomp;

while this is a I<get> of the I<class> attribute:

	my $chomping = Argv->autochomp;

=over

=item * autochomp

All data returned by the C<qx> method is chomped first. Unset by default.

=item * autofail

When set, the program will exit immediately if either of the C<system>
or C<qx> methods would return a nonzero status. Unset by default.

=item * autoglob

If set, the C<glob()> function is applied to the operands
(C<$self->args>) on Windows only. Unset by default.

=item * autoquote

If set, the operands are automatically quoted against shell expansion
before C<system()> on Windows and C<qx()> on all platforms (since C<qx>
always invokes a shell, and C<system()> always does so on Windows).
Set by default.

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
Win32 Perl port. Perl just uses the Windows exec() routine, which runs
the new process in the background. Set by default.

=item * pathnorm

If set, normalizes pathnames to their native format just before
executing. This is set by default on Windows only, thus converting
/x/y/z to \x\y\z.

=item * noexec

Analogous to the C<-n> flag to I<make>; prints what would be executed
without executing anything.

=item * qxargs

You can set a maximum number of arguments to be processed at a time,
allowing you to blithely invoke e.g. C<$obj->qx> on a list of any size
without fear of exceeding your shell's limits. A per-platform default
is set; this method allows it to be changed. A value of 0 suppresses
the behavior.

=item * systemxargs

Analogous to I<qxargs> but turned off by default. The reason is that
C<qx()> is typically used to I<read> data whereas C<system()> is more
often used to make stateful changes. Consider that "ls foo bar"
produces the same result if broken up into "ls foo" and "ls bar" but
the same cannot be said for "mv foo bar".

=item * stdout

Default value is true which has no effect. A false value, e.g:

   $obj->stdout(0);

causes STDOUT to be closed during invocation of any of the I<execution
methods> C<system, exec, and qx>, and restored when they finish. A
fancy (and portable) way of saying 1>/dev/null without needing a
shell.

=item * stderr

As above, for STDERR.

=back

Defaults for all of the above may be provided in the environment by
prepending the class name, e.g. ARGV_QXARGS=32 or ARGV_STDERR=0;

=over

=item * stdopts

The attributes above can be set via method calls (e.g.
C<$obj->dbglevel(1)>) or environment variables (ARGV_DBGLEVEL=1). Use
of the <$obj->stdopts> method allows them to be parsed from the command
line as well, e.g. I<myscript -/dbglevel 1>. If invoked as a class
method it causes options of the same names as the methods above to be
parsed (and removed) from the current C<@ARGV> and set as class
attributes.  As an instance method it parses and potentially depletes
the current argument vector of that object, and sets instance attributes
only. E.g.:

    Argv->stdopts;

would cause the script to parse the following command line:

    script -/noexec 1 -/dbglevel 2 -flag1 -flag2 arg1 arg2 arg3 ...

so as to remove the C<-/noexec 1 -/dbglevel 2> and set the two class attrs.
The C<-/> prefix is chosen to prevent conflicts with "real" flags.

=head1 PORTING

This module is known to work on Solaris 2.5-7 and Windows NT 4.0SP3-5,
and with perl 5.004_04 and 5.005_03.  As these two platforms are quite
different, there should be no I<major> portability issues, but please
send bug reports or patches to the address below.

=head1 AUTHOR

David Boyce <dsb@world.std.com>

=head1 COPYRIGHT

Copyright (c) 1999,2000 David Boyce. All rights reserved.  This Perl
program is free software; you may redistribute and/or modify it under
the same terms as Perl itself.

=head1 SEE ALSO

perl(1), Getopt::Long(3), IPC::ChildSafe(3)

=cut
