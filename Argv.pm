package Argv;

$VERSION = '1.09';
@ISA = qw(Exporter);

use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i ? 1 : 0;

# To support the "FUNCTIONAL INTERFACE"
@EXPORT_OK = qw(system exec qv MSWIN);

use strict;
use Carp;
require Exporter;

my $class = __PACKAGE__;

# Adapted from perltootc (see): an "eponymous meta-object" implementing
# "translucent attributes".
# For each key in the hash below, a method is automatically generated.
# Each of these sets the object attr if called as an instance method or
# the class attr if called as a class method. They return the instance
# attr if it's defined, the class attr otherwise. The method name is
# lower-case; e.g. 'qxargs'. The default value of each attribute comes
# from the hash value which may be overridden in the environment.
use vars qw(%Argv);
%Argv = (
    AUTOCHOMP	=> $ENV{ARGV_AUTOCHOMP} || 0,
    AUTOFAIL	=> $ENV{ARGV_AUTOFAIL} || 0,
    AUTOGLOB	=> $ENV{ARGV_AUTOGLOB} || 0,
    AUTOQUOTE	=> defined($ENV{ARGV_AUTOQUOTE}) ? $ENV{ARGV_AUTOQUOTE} : 1,
    DBGLEVEL	=> $ENV{ARGV_DBGLEVEL} || 0,
    DFLTSETS	=> {'' => 1},
    ENVP	=> undef,
    EXECWAIT	=> defined($ENV{ARGV_EXECWAIT}) ?
					    $ENV{ARGV_EXECWAIT} : scalar(MSWIN),
    INPATHNORM	=> $ENV{ARGV_INPATHNORM} || 0,
    MUSTEXEC	=> $ENV{ARGV_MUSTEXEC} || 0,
    NOEXEC	=> $ENV{ARGV_NOEXEC} || 0,
    OUTPATHNORM	=> $ENV{ARGV_OUTPATHNORM} || 0,
    QXARGS	=> $ENV{ARGV_QXARGS} || (MSWIN ? 16 : 128),
    QXFAIL	=> $ENV{ARGV_QXFAIL} || 0,
    QUIET	=> defined($ENV{ARGV_QUIET})  ? $ENV{ARGV_QUIET}  : 0,
    STDIN	=> defined($ENV{ARGV_STDIN})  ? $ENV{ARGV_STDIN}  : 0,
    STDOUT	=> defined($ENV{ARGV_STDOUT}) ? $ENV{ARGV_STDOUT} : 1,
    STDERR	=> defined($ENV{ARGV_STDERR}) ? $ENV{ARGV_STDERR} : 2,
    SYFAIL	=> $ENV{ARGV_SYFAIL} || 0,
    SYXARGS	=> $ENV{ARGV_SYXARGS} || 0,
);

# Generates execution-attribute methods from the table above. Provided
# as a class method itself to potentially allow a derived class to
# generate more. Semantics of these methods are quite context-driven
# and are explained in the PODs.
sub gen_exec_method {
    my $meta = shift;
    no strict 'refs'; # must evaluate $meta as a symbolic ref
    my @data = @_ ? map {uc} @_ : keys %{$meta};
    for my $attr (@data) {
	$$meta{$attr} ||= 0;
	my $method = lc $attr;
	*$method = sub {
	    my $self = shift;
	    # In null context with no args, set boolean value 'on'.
	    @_ = (1) if !@_ && !defined(wantarray);
	    my $ret = 0;
	    if (ref $self) {
		if (@_) {
		    if (defined(wantarray)) {
			if (ref $self->{$attr}) {
			    unshift(@{$self->{$attr}}, shift);
			} elsif (defined $self->{$attr}) {
			    $self->{$attr} = [shift, $self->{$attr}];
			} else {
			    $self->{$attr} = [shift];
			}
			return $self;
		    } else {
			$self->{$attr} = shift;
			return undef;
		    }
		} else {
		    $ret = defined($self->{$attr}) ?
					    $self->{$attr} : $class->{$attr};
		}
	    } else {
		if (@_) {
		    if (defined(wantarray)) {
			if (ref $class->{$attr}) {
			    unshift(@{$class->{$attr}}, shift);
			} else {
			    $class->{$attr} = [shift, $class->{$attr}];
			}
		    } else {
			$class->{$attr} = shift;
		    }
		    # If setting a class attribute, export it to the
		    # env in case we fork a child also using Argv.
		    my $ev = uc join('_', $class, $attr);
		    $ENV{$ev} = $class->{$attr};
		    return $self;
		} else {
		    $ret = $class->{$attr};
		}
	    }
	    if (ref($ret) eq 'ARRAY' && ref($ret->[0]) ne 'CODE') {
		my $stack = $ret;
		$ret = shift @$stack;
		if (ref $self) {
		    if (@$stack) {
			$self->{$attr} = shift @$stack;
		    } else {
			delete $self->{$attr};
		    }
		} else {
		    $self->{$attr} = shift @$stack;
		}
	    }
	    return $ret;
	}
    }
}

# Generate all the attribute methods declared in %Argv above.
$class->gen_exec_method;

# Generate methods for diverting stdin, stdout, and stderr in ->qx.
{
    my %streams = (stdin => 1, stdout => 1, stderr => 2);
    for my $name (keys %streams) {
	my $method = "_qx_$name";
	no strict 'refs';
	*$method = sub {
	    my $self = shift;
	    my $r_cmd = shift;
	    my $nfd = shift;
	    my $fd = $streams{$name};
	    if ($fd == 0) {
		warn "Error: illegal value '$nfd' for $name" if $nfd > 0;
		push(@$r_cmd, "<" . (MSWIN ? 'NUL' : '/dev/null')) if $nfd < 0;
	    } elsif ($nfd == 0) {
		push(@$r_cmd, "$fd>" . (MSWIN ? 'NUL' : '/dev/null'));
	    } elsif ($nfd == (3-$fd)) {
		push(@$r_cmd, sprintf "%d>&%d", $fd, 3-$fd);
	    } elsif ($nfd != $fd) {
		warn "Error: illegal value '$nfd' for $name";
	    }
	};
    }
}

# Getopt::Long::GetOptions() respects '--' but strips it, while
# we want to respect '--' and leave it in. Thus this override.
sub GetOptions {
    @ARGV = map {/^--$/ ? qw(=--= --) : $_} @ARGV;
    my $ret = Getopt::Long::GetOptions(@_);
    @ARGV = map {/^=--=$/ ? qw(--) : $_} @ARGV;
    return $ret;
}

# This method is much like the generated exec methods but has some
# special-case logic: If called with a param which is true, it starts up
# a coprocess. If called with false (aka 0) it shuts down the coprocess
# and destroys the IPC::ChildSafe object. If called with no params at
# all it returns the existing IPC::ChildSafe object.
sub ipc_childsafe {
    my $self = shift;
    my $ipc_state = $_[0];
    my $ipc_obj;
    if ($ipc_state) {
	eval { require IPC::ChildSafe };
	return undef if $@;
	IPC::ChildSafe->VERSION(3.10);
	$ipc_obj = IPC::ChildSafe->new(@_);
    }
    no strict 'refs';
    if (ref $self) {
	if (defined $ipc_state) {
	    $self->{_IPC_CHILDSAFE} = $ipc_obj;
	    return $self;
	} else {
	    return exists($self->{_IPC_CHILDSAFE}) ?
			$self->{_IPC_CHILDSAFE} : $class->{_IPC_CHILDSAFE};
	}
    } else {
	if (defined $ipc_state) {
	    $class->{_IPC_CHILDSAFE} = $ipc_obj;
	    return $self;
	} else {
	    return $class->{_IPC_CHILDSAFE};
	}
    }
}

# Class/instance method. Parses command line for e.g. -/dbg=1. See PODs.
sub attropts {
    my $self = shift;
    my $r_argv = undef;
    my $prefix = '-/';
    if (ref $_[0] eq 'HASH') {
	my $cfg = shift;
	$r_argv = $cfg->{ARGV};
	$prefix = $cfg->{PREFIX};
    }
    require Getopt::Long;
    local $Getopt::Long::passthrough = 1;
    local $Getopt::Long::genprefix = "($prefix)";
    my @flags = map {"$_=i"} ((map lc, keys %Argv::Argv), @_);
    my %opt;
    if (ref $self) {
	if ($r_argv) {
	    local @ARGV = @$r_argv;
	    GetOptions(\%opt, @flags);
	    @$r_argv = @ARGV;
	} else {
	    local @ARGV = $self->args;
	    if (@ARGV) {
		GetOptions(\%opt, @flags);
		$self->args(@ARGV);
	    }
	}
    } elsif ($r_argv) {
	local @ARGV = @$r_argv;
	GetOptions(\%opt, @flags);
	@$r_argv = @ARGV;
    } elsif (@ARGV) {
	GetOptions(\%opt, @flags);
    }
    for my $method (keys %opt) { $self->$method($opt{$method}) }
    return $self;
}
*stdopts = \&attropts;	# backward compatibility

# A class method which returns a summary of operations performed in
# printable format. Called with a void context to start data-
# collection, with a scalar context to end it and get the report.
sub summary {
    my $cls = shift;
    my($cmds, $operands);
    if (!defined wantarray) {
	# This is a horrible hack ....
	%Argv::Summary = (FOO => 0);
	%Argv::Summary = ();
	return;
    }
    return unless %Argv::Summary;
    my $fmt = "%30s:  %4s\t%s\n";
    my $str = sprintf $fmt, "$cls Summary", 'Cmds', 'Operands';
    for (sort keys %Argv::Summary) {
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
    my $attrs = shift if ref($_[0]) eq 'HASH';
    my $self;
    if (ref($proto)) {
	# As an instance method, make a deep clone of the invoking object.
	# Some cloners are fast but not commonly installed, others the
	# reverse. We try them in order of speed and fall back to
	# Data::Dumper which is slow but core Perl as of 5.6.0. I could
	# just inherit from Clone or Storable but want to not force
	# users who don't need cloning to install them.
	eval {
	    require Clone;
	    Clone->VERSION(0.12);       # 0.11 has a bug that breaks Argv
	    $self = Clone::clone($proto);
	};
	if ($@) {
	    eval {
		require Storable;
		$self = Storable::dclone($proto);
	    };
	}
	if ($@) {
	    require Data::Dumper;
	    # Older Perl versions may not have the XS interface installed,
	    # so try it and fall back to the pure-perl version on failure.
	    my $copy = eval {
		Data::Dumper->Deepcopy(1)->new([$proto], ['self'])->Dumpxs;
	    };
	    $copy = Data::Dumper->Deepcopy(1)->new([$proto], ['self'])->Dump
									if $@;
	    eval $copy;
	}
	die $@ if $@ || !$self;
    } else {
	$self = {};
	$self->{AV_PROG} = [];
	$self->{AV_ARGS} = [];
	bless $self, $proto;
	$self->optset('');
    }
    $self->attrs($attrs) if $attrs;
    $self->argv(@_) if @_;
    return $self;
}
*clone = \&new;

# Nothing to do here, just avoiding interaction with AUTOLOAD.
sub DESTROY { }

sub AUTOLOAD {
    my $self = shift;
    (my $cmd = $Argv::AUTOLOAD) =~ s/.*:://;
    return if $cmd eq 'DESTROY';
    no strict 'refs';
    # install a new method '$cmd' to avoid autoload next time ...
    *$cmd = sub {
	my $self = shift;
	if (ref $self) {
	    $self->argv($cmd, @_);
	} else {
	    $self->new($cmd, @_);
	}
    };
    # ... then service this request
    return $self->$cmd(@_);
}

# Instance methods; most class methods are auto-generated above.

# A shorthand way to set a bunch of attributes by passing a hashref
# of their names=>values.
sub attrs {
    my $self = shift;
    my $attrs = shift;
    if ($attrs) {
	for my $key (keys %$attrs) {
	    (my $method = $key) =~ s/^-//;
	    $self->$method($attrs->{$key});
	}
    }
    return $self;
}

# Replace the instance's prog(), opt(), and args() vectors all together.
sub argv {
    my $self = shift;
    $self->attrs(shift) if ref($_[0]) eq 'HASH';
    $self->{AV_PROG} = [];
    $self->{AV_OPTS}{''} = [];
    $self->{AV_ARGS} = [];
    $self->prog(shift) if @_;
    $self->attrs(shift) if ref($_[0]) eq 'HASH';
    $self->opts(@{shift @_}) if ref $_[0];
    $self->args(@_) if @_;
    return $self;
}
*cmd = \&argv;	# backward compatibility

# Set or get the 'prog' part of the command line.
sub prog {
    my $self = shift;
    if (@_) {
	my @prg = ref $_[0] ? @{$_[0]} : @_;
	@{$self->{AV_PROG}} = @prg;
    } elsif (!defined(wantarray)) {
	@{$self->{AV_PROG}} = ();
    }
    if (@_) {
	return $self;
    } else {
	return wantarray ? @{$self->{AV_PROG}} : ${$self->{AV_PROG}}[0];
    }
}

# Set or get the 'args' part of the command line.
sub args {
    my $self = shift;
    if (@_) {
	my @args = ref $_[0] ? @{$_[0]} : @_;
	@{$self->{AV_ARGS}} = @args;
    } elsif (!defined(wantarray)) {
	@{$self->{AV_ARGS}} = ();
    }
    if (@_) {
	return $self;
    } else {
	return @{$self->{AV_ARGS}};
    }
}

# Generates the parse(), opts(), and flag() method families. During
# construction this is used to generate the methods for the anonymous
# option set; it can be used explicitly to generate parseXX(), optsXX(),
# and argsXX() for optset 'XX'.
sub optset {
    my $self = shift;
    for (@_) {
	my $set = uc $_;
	next if defined $self->{AV_OPTS}{$set};
	$self->{AV_OPTS}{$set} = [];
	$self->{AV_LKG}{$set} = {};
	my($p_meth, $o_meth, $f_meth) = map { $_ . $set } qw(parse opts flag);
	$self->{AV_DESC}{$set} = [];
	no strict 'refs'; # needed to muck with symbol table
	*$p_meth = sub {
	    my $self = shift;
	    $self->{AV_DESC}{$set} ||= [];
	    if (@_) {
		if (ref($_[0]) eq 'ARRAY') {
		    $self->{CFG}{$set} = shift;
		} elsif (ref($_[0]) eq 'HASH') {
		    $self->warning("do not provide a linkage specifier");
		    shift;
		}
		@{$self->{AV_DESC}{$set}} = @_;
		$self->factor($set,
				$self->{AV_DESC}{$set}, $self->{AV_OPTS}{$set},
				$self->{AV_ARGS}, $self->{CFG}{$set});
		if (defined $self->{AV_OPTS}{$set}) {
		    my @parsedout = @{$self->{AV_OPTS}{$set}};
		}
	    }
	    return @{$self->{AV_OPTS}{$set}};
	} unless $Argv::{$p_meth};
	*$o_meth = sub {
	    my $self = shift;
	    $self->{AV_OPTS}{$set} ||= [];
	    if (@_ || !defined(wantarray)) {
		@{$self->{AV_OPTS}{$set}} = @_;
	    }
	    return @_ ? $self : @{$self->{AV_OPTS}{$set}};
	} unless $Argv::{$o_meth};
	*$f_meth = sub {
	    my $self = shift;
	    if (@_ > 1) {
		while(my($key, $val) = splice(@_, 0, 2)) {
		    $self->{AV_LKG}{$set}{$key} = $val;
		}
	    } else {
		my $key = shift;
		return $self->{AV_LKG}{$set}{$key};
	    }
	} unless $Argv::{$f_meth};
    }
    return keys %{$self->{AV_DESC}}; # this is the set of known optsets.
}

# Not generally used except internally; not documented. First arg
# is an option set name followed by bunch of array-refs: a pointer
# to a list of Getopt::Long-style option descs, a ref to be filled
# in with a list of found options, another containing the input
# args and to be filled in with the leftovers, and an optional
# one containing Getopt::Long-style config options.
sub factor {
    my $self = shift;
    my($pset, $r_desc, $r_opts, $r_args, $r_cfg) = @_;
    my %vgra;
    {
	local @ARGV = @$r_args;
	if ($r_desc && @$r_desc) {
	    require Getopt::Long;
	    # Need this version so Configure() returns prev state.
	    Getopt::Long->VERSION(2.23);
	    if ($r_cfg && @$r_cfg) {
		my $prev = Getopt::Long::Configure(@$r_cfg);
		GetOptions($self->{AV_LKG}{$pset}, @$r_desc);
		Getopt::Long::Configure($prev);
	    } else {
		local $Getopt::Long::passthrough = 1;
		local $Getopt::Long::autoabbrev = 1;
		local $Getopt::Long::debug = 1 if $self->dbglevel == 5;
		GetOptions($self->{AV_LKG}{$pset}, @$r_desc);
	    }
	}
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

# Extract and return any of the specified options from object.
sub extract {
    my $self = shift;
    my $set = shift;
    $self->optset($set) unless defined $self->{AV_LKG}{$set};
    my $p_meth = 'parse' . $set;
    my $o_meth = 'opts' . $set;
    $self->$p_meth(@_);
    my @extracts = $self->$o_meth();
    return @extracts;
}

# Quotes @_ in place against shell expansion. Usually called via autoquote attr
sub quote {
    my $self = shift;
    my $inpathnorm = $self->inpathnorm;
    for (@_) {
	# If requested, change / for \ in Windows file paths.
	s%/%\\%g if MSWIN && $inpathnorm && !ref($inpathnorm);
	# Hack - allow user to exempt any arg from quoting by prefixing '^'.
	next if s%^\^%%;
	# Special case - turn internal newlines back to literal \n on Win32
	s%\n%\\n%gs if MSWIN;
	# If arg is already quoted with '': on Unix it's safe, leave alone.
	# On Windows, replace the single quotes with escaped double quotes.
	if (m%^'(.*)'$%s) {
	    $_ = qq(\\"$1\\") if MSWIN;
	    next;
	} elsif (m%^"(.*)"$%s) {
	    $_ = qq(\\"$_\\");
	    next;
	}
	# Skip if contains no special chars.
	next unless m%[^-=:_."\w\\/]% || tr%\n%%;
	# Special case - leave things that look like redirections alone.
	next if /^\d?(?:<{1,2})|(?:>{1,2})/;
	# This is a hack to support MKS-built perl 5.004. Don't know
	# if the problem is with MKS builds or 5.004 per se.
	next if MSWIN && $] < 5.005;
	# Now quote embedded quotes ...
	$_ =~ s%(\\*)"%$1$1\\"%g;
	# quote a trailing \ so it won't quote the quote (!) ...
	s%\\{1}$%\\\\%;
	# and last the entire string.
	$_ = qq("$_");
    }
    return $self;
}

# Submits @_ to Perl's glob() function. Usually invoked via autoglob attr.
sub glob {
    my $self = shift;
    my @orig = @_ ? @_ : $self->args;
    if (! @orig) {
	$self->warning("no arguments to glob");
	return 0;
    }
    my @globbed;
    for (@orig) {
	if (/^'(.*)'$/) {		# allow '' to escape globbing
	    push(@globbed, $1);
	} elsif (/[*?]/) {
	    push(@globbed, glob)
	} else {
	    push(@globbed, $_)
	}
    }
    if (defined wantarray) {
	return @globbed;
    } else {
	$self->args(@globbed);
    }
}

# Internal. Takes a list of optset names, returns a list of options.
sub _sets2opts {
    my $self = shift;
    my(@sets, @opts);
    if (! @_) {
	@sets = keys %{$self->dfltsets};
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
	next unless $self->{AV_OPTS}{$set} && @{$self->{AV_OPTS}{$set}};
	push(@opts, @{$self->{AV_OPTS}{$set}});
    }
    return @opts;
}

# Internal, collects data for use by 'summary' method.
sub _addstats {
    my $self = shift;
    my($prg, $argcnt) = @_;
    my $stats = $Argv::Summary{$prg} || [0, 0];
    $$stats[0]++;
    $$stats[1] += $argcnt;
    $Argv::Summary{$prg} = $stats;
}

# Handles ->autofail operations. If given a scalar, exit with the value
# of that scalar on failure unless the scalar == 0, in which case
# don't exit. If given a ref to a scalar, increment the scalar for
# each failure. If given a code ref, call that subroutine. An array ref
# is assumed to contain a code ref followed by parameters for the sub.
sub fail {
    my($self, $specific) = @_;
    my $general = $self->autofail;
    if (my $val = $specific || $general) {
	if (ref($val) eq 'CODE') {
	    &$val($self);
	} elsif (ref($val) eq 'ARRAY') {
	    my @arr = @$val;
	    my $func = shift(@arr);
	    &$func(@arr);
	} elsif (ref($val) eq 'SCALAR') {
	    $$val++;
	} elsif ($val !~ /^\d*$/) {
	    die $val;
	} elsif ($val) {
	    exit $val;
	}
    }
    return $self;
}

# Convert lines to UNIX (/) format iff they represent file pathnames.
sub unixpath {
    my $self = shift;
    for (@_) {
	chomp(my $chomped = $_);
	s%\\%/%g if -e $chomped;
    }
}

# A no-op except it prints the current state of the object to stderr.
sub dump {
    my $self = shift;
    (my $obj = shift || 'argv') =~ s%^\$%%;
    require Data::Dumper;
    print STDERR Data::Dumper->new([$self], [$obj])->Dumpxs;
    return $self;
}

sub readonly {
    my $self = shift;
    no strict 'refs';
    if (@_) {
	$self->{AV_READONLY} = shift;
	return $self;
    } else {
	if (exists($self->{AV_READONLY})) {
	    return $self->{AV_READONLY};		# instance
	} else {
	    my $class = ref $self;
	    if ($class && exists($class->{AV_READONLY})) {
		return $class->{AV_READONLY};	# class
	    } else {
		return 'no';
	    }
	}
    }
}

sub _read_only {
    my $self = shift;
    return $self->readonly =~ /^y/i;
}

# Hidden method for printing debug output.
sub _dbg {
    my $self = shift;
    my($level, $prefix, $fh, @txt) = @_;
    my @tmp = @txt;
    for (@tmp) { $_ = qq("$_") if /\s/ }
    $self->dump if $level >= 3;
    print $fh "$prefix @tmp\n";
}

# Wrapper around Perl's exec().
sub exec {
    $class->new(@_)->exec if !ref($_[0]) || ref($_[0]) eq 'HASH';
    my $self = shift;
    if ((ref($self) ne $class) && $self->ipc_childsafe) {
	exit($self->system(@_) || $self->ipc_childsafe->finish);
    } elsif (MSWIN && $self->execwait) {
	exit($self->system(@_) >> 8);
    } else {
	my $envp = $self->envp;
	my $dbg = $self->dbglevel;
	my @cmd = (@{$self->{AV_PROG}},
			    $self->_sets2opts(@_), @{$self->{AV_ARGS}});
	if ($self->noexec && !$self->_read_only) {
	    $self->_dbg($dbg, '-', \*STDERR, @cmd);
	} else {
	    my($ifd, $ofd, $efd) = ($self->stdin, $self->stdout, $self->stderr);
	    $self->_dbg($dbg, '+', \*STDERR, @cmd) if $dbg;
	    open(_I, '>&STDIN');
	    open(_O, '>&STDOUT');
	    open(_E, '>&STDERR');
	    if ($ifd) {
		warn "Warning: illegal value '$ifd' for stdin" if $ifd > 0;
		close(STDIN) if $ifd < 0;
	    }
	    if ($ofd == 2) {
		open(STDOUT, '>&STDERR') || warn "Can't dup stdout";
	    } elsif ($self->quiet) {
		close(STDOUT);
	    } elsif ($ofd != 1) {
		close(STDOUT) if !$ofd;
		warn "Warning: illegal value '$ofd' for stdout" if $ofd > 2;
	    }
	    if ($efd == 1) {
		open(STDERR, '>&STDOUT') || warn "Can't dup stderr";
	    } elsif ($efd != 2) {
		close(STDERR) if !$efd;
		warn "Warning: illegal value '$efd' for stderr" if $efd > 2;
	    }
	    my $rc;
	    if ($envp) {
		local %ENV = %$envp;
		$rc = CORE::exec(@cmd);
	    } else {
		$rc = CORE::exec(@cmd);
	    }
	    # Shouldn't get here but defensive programming and all that ...
	    if ($rc) {
		my $error = "$!";
		open(STDOUT, '>&_I'); close(_I);
		open(STDOUT, '>&_O'); close(_O);
		open(STDERR, '>&_E'); close(_E);
		die "$0: $cmd[0]: $error\n";
	    }
	}
    }
}

# Internal - service method for system/exec to call into IPC::ChildSafe.
sub _ipccmd {
    my $self = shift;
    my $cmd = shift;
    # Throw out the prog name since it's already running
    if (@_) {
	$cmd = "@_";
    } else {
	$cmd =~ s/^\w+\s*//;
    }
    # Hack - there's an "impedance mismatch" between instance
    # methods in this class and the class methods in
    # IPC::ChildSafe, so we toggle the attrs for every cmd.
    my $csobj = $self->ipc_childsafe;
    $csobj->dbglevel($self->dbglevel);
    $csobj->noexec($self->noexec) if $self->noexec && !$self->_read_only;
    my %results = $csobj->cmd($cmd);
    return %results;
}

# Wrapper around Perl's system().
sub system {
    return $class->new(@_)->system if !ref($_[0]) || ref($_[0]) eq 'HASH';
    my $self = shift;
    my $envp = $self->envp;
    my $rc = 0;
    my($ifd, $ofd, $efd) = ($self->stdin, $self->stdout, $self->stderr);
    $self->args($self->glob) if $self->autoglob;
    my @prog = @{$self->{AV_PROG}};
    my @opts = $self->_sets2opts(@_);
    my @args = @{$self->{AV_ARGS}};
    my $childsafe = ((ref($self) ne $class) &&
			    $self->ipc_childsafe && !$self->mustexec) ? 1 : 0;
    # This potentially modifies (@prog, @opts, @args) in place.
    $self->quote(@prog, @opts, @args)
	if (((MSWIN && (@prog + @opts + @args) > 1) || $childsafe) &&
						       $self->autoquote);
    my @cmd = (@prog, @opts, @args);
    my $dbg = $self->dbglevel;
    if ($childsafe) {
	$self->_addstats("@prog", scalar @args) if defined(%Argv::Summary);
	$self->warning("cannot change \%ENV of child process") if $envp;
	$self->warning("cannot close stdin of child process") if $ifd;
	my %results = $self->_ipccmd(@cmd);
	$? = $rc = ($results{status} << 8);
	if ($self->quiet) {
	    # say nothing
	} elsif ($ofd == 2) {
	    print STDERR @{$results{stdout}} if @{$results{stdout}};
	} else {
	    warn "Warning: illegal value '$ofd' for stdout" if $ofd > 2;
	    print STDOUT @{$results{stdout}} if $ofd && @{$results{stdout}};
	}
	if ($efd == 1) {
	    print STDOUT @{$results{stderr}} if @{$results{stderr}};
	} else {
	    warn "Warning: illegal value '$efd' for stderr" if $efd > 2;
	    print STDERR @{$results{stderr}} if $efd && @{$results{stderr}};
	}
    } else {
	# Reset to defaults in dbg mode (what's this for?)
	($ofd, $efd) = (1, 2) if defined($dbg) && $dbg > 2;
	if ($self->noexec && !$self->_read_only) {
	    $self->_dbg($dbg, '-', \*STDERR, @cmd);
	    return 0;
	}
	open(_I, '>&STDIN');
	open(_O, '>&STDOUT');
	open(_E, '>&STDERR');
	if ($ifd) {
	    warn "Warning: illegal value '$ifd' for stdin" if $ifd > 0;
	    close(STDIN) if $ifd < 0;
	}
	if ($ofd == 2) {
	    open(STDOUT, '>&STDERR') || warn "Can't dup stdout";
	} elsif ($self->quiet) {
	    close(STDOUT);
	} elsif ($ofd != 1) {
	    close(STDOUT) if !$ofd;
	    warn "Warning: illegal value '$ofd' for stdout" if $ofd > 2;
	}
	if ($efd == 1) {
	    open(STDERR, '>&STDOUT') || warn "Can't dup stderr";
	} elsif ($efd != 2) {
	    close(STDERR) if !$efd;
	    warn "Warning: illegal value '$efd' for stderr" if $efd > 2;
	}
	my $limit = $self->syxargs;
	if ($limit && @args) {
	    while (my @chunk = splice(@args, 0, $limit)) {
		$self->_addstats("@prog", scalar @chunk)
						    if defined(%Argv::Summary);
		@cmd = (@prog, @opts, @chunk);
		$self->_dbg($dbg, '+', \*_E, @cmd) if $dbg;
		if ($envp) {
		    local %ENV = %$envp;
		    $rc |= CORE::system @cmd;
		} else {
		    $rc |= CORE::system @cmd;
		}
	    }
	} else {
	    $self->_addstats("@prog", scalar @args) if defined(%Argv::Summary);
	    $self->_dbg($dbg, '+', \*_E, @cmd) if $dbg;
	    if ($envp) {
		local %ENV = %$envp;
		$rc = CORE::system @cmd;
	    } else {
		$rc = CORE::system @cmd;
	    }
	}
	open(STDOUT, '>&_I'); close(_I);
	open(STDOUT, '>&_O'); close(_O);
	open(STDERR, '>&_E'); close(_E);
    }
    print STDERR "+ (\$? == $?)\n" if $dbg > 1;
    $self->fail($self->syfail) if $?;
    return $rc;
}

# Wrapper around Perl's qx(), aka backquotes.
sub qx {
    return $class->new(@_)->qx if !ref($_[0]) || ref($_[0]) eq 'HASH';
    my $self = shift;
    my $envp = $self->envp;
    my @prog = @{$self->{AV_PROG}};
    my @opts = $self->_sets2opts(@_);
    my @args = @{$self->{AV_ARGS}};
    my $childsafe = ((ref($self) ne $class) &&
			    $self->ipc_childsafe && !$self->mustexec) ? 1 : 0;
    @args = $self->glob(@args)
		if MSWIN && $self->autoglob && $childsafe;
    # This potentially modifies (@prog, @opts, @args) in place.
    $self->quote(@prog, @opts, @args)
	if (((@prog + @opts + @args) > 1 || $childsafe) && $self->autoquote);
    my @cmd = (@prog, @opts, @args);
    my @data;
    my $dbg = 0;
    my($ifd, $ofd, $efd) = ($self->stdin, $self->stdout, $self->stderr);
    my $noexec = $self->noexec && !$self->_read_only;
    if ($childsafe) {
	$self->_addstats("@prog", scalar @args) if defined(%Argv::Summary);
	$self->warning("cannot change \%ENV of child process") if $envp;
	$self->warning("cannot close stdin of child process") if $ifd;
	if ($noexec) {
	    $self->_dbg($dbg, '-', \*STDERR, @cmd);
	} else {
	    my %results = $self->_ipccmd(@cmd);
	    $? = $results{status} << 8;
	    if ($ofd == 1) {
		push(@data, @{$results{stdout}});
	    } else {
		print STDERR @{$results{stdout}} if $ofd == 2;
		warn "Warning: illegal value '$ofd' for stdout" if $ofd > 2;
	    }
	    if ($efd == 1) {
		push(@data, @{$results{stderr}});
	    } else {
		print STDERR @{$results{stderr}} if $efd;
		warn "Warning: illegal value '$efd' for stderr" if $efd > 2;
	    }
	}
    } else {
	$dbg = $self->dbglevel;
	# Reset to defaults in dbg mode (what's this for?)
	($ofd, $efd) = (1, 2) if defined($dbg) && $dbg > 2;
	my $limit = $self->qxargs;
	if ($limit && @args) {
	    while (my @chunk = splice(@args, 0, $limit)) {
		$self->_addstats("@prog", scalar @chunk)
						    if defined(%Argv::Summary);
		@cmd = (@prog, @opts, @chunk);
		if ($noexec) {
		    $self->_dbg($dbg, '-', \*STDERR, @cmd);
		} else {
		    $self->_dbg($dbg, '+', \*STDERR, @cmd) if $dbg;
		    $self->_qx_stderr(\@cmd, $efd);
		    $self->_qx_stdout(\@cmd, $ofd);
		    if ($envp) {
			local %ENV = %$envp;
			push(@data, CORE::qx(@cmd));
		    } else {
			push(@data, CORE::qx(@cmd));
		    }
		}
	    }
	} else {
	    $self->_addstats("@prog", scalar @args) if defined(%Argv::Summary);
	    if ($noexec) {
		$self->_dbg($dbg, '-', \*STDERR, @cmd);
	    } else {
		$self->_dbg($dbg, '+', \*STDERR, @cmd) if $dbg;
		$self->_qx_stderr(\@cmd, $efd);
		$self->_qx_stdout(\@cmd, $ofd);
		if ($envp) {
		    local %ENV = %$envp;
		    @data = CORE::qx(@cmd);
		} else {
		    @data = CORE::qx(@cmd);
		}
	    }
	}
    }
    print STDERR "+ (\$? == $?)\n" if $dbg > 1;
    $self->fail($self->qxfail) if $?;
    $self->unixpath(@data) if MSWIN && $self->outpathnorm;
    if (wantarray) {
	print map {"+ <- $_"} @data if @data && $dbg >= 2;
	chomp(@data) if $self->autochomp;
	return @data;
    } else {
	my $data = join('', @data);
	print "+ <- $data" if @data && $dbg >= 2;
	chomp($data) if $self->autochomp;
	return $data;
    }
}
# Can't override qx() in main package so we export an alias instead.
*qv = \&qx;

# Internal - provide a warning with std format and caller's context.
sub warning {
    my $self = shift;
    (my $prog = $0) =~ s%.*[/\\]%%;
    no strict 'refs';
    carp('Warning: ', ${$self->{AV_PROG}}[-1] || $prog, ': ', @_);
}

1;

__END__

=head1 NAME

Argv - Provide an OO interface to an arg vector

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

Argv presents an OO approach to command lines, allowing you to
instantiate an 'argv object', manipulate it, and eventually execute it,
e.g.:

    my $ls = Argv->new('ls', ['-l']));
    my $rc = $ls->system;	# or $ls->exec or $ls->qx

Which raises the immediate question - what value does this mumbo-jumbo
add over Perl's native support such as:

    my $rc = system(qw(ls -l));

The answer comes in a few parts:

=over

=item * STRUCTURE

First, Argv recognizes the underlying property of an arg vector, which
is that it typically begins with a program name potentially followed by
options, then operands. An Argv object factors a raw argv into these
three groups, provides accessor methods to allow operations on each
group independently, and can then put them back together for
execution.


=item * OPTION SETS

Second, Argv encapsulates and extends C<Getopt::Long> to allow parsing
of the argv's options into different I<option sets>. This is useful in
the case of wrapper programs which may, for instance, need to parse out
one set of flags to direct the behavior of the wrapper itself,
extract a different set and pass them to program X, another for program
Y, then exec program Z with the remainder.  Doing this kind of thing on
a basic @ARGV using indexing and splicing is do-able but leads to
spaghetti-ish code.

=item * EXTRA FEATURES

The I<execution methods> C<system, exec, and qx> extend their Perl
builtin analogues in a few ways, for example:

=over

=item 1. An xargs-like capability without shell intervention.

=item 2. UNIX-like C<exec()> behavior on Windows.

=item 3. Automatic quoting of C<system()> on Win32 and C<qx()> everywhere

=item 4. Automatic globbing (primarily for Windows)

=item 5. Automatic chomping.

=item 6. Pathname normalization.

=back

=back

All of these behaviors can be toggled, either as class or instance
attributes. See EXECUTION ATTRIBUTES below.

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

=head1 AUTOLOADING

Argv employs the same technique made famous by the Shell module
to allow any command name to be used as a method. E.g.

	$obj->date->exec;

will run the 'date' command. Internally this is translated into

	$obj->argv('date')->exec;

=head1 FUNCTIONAL INTERFACE

Because the extensions to C<system/exec/qx> described here may be
useful in writing portable programs, they're made available for export
as traditional functions. Thus:

    use Argv qw(system exec qv);

will override the Perl builtins. There's no way to override the
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

The constructor can be used as a class or instance method. In the
latter case the new object is a deep (full) clone of its progenitor.
In fact 'clone' is aliased to 'new', allowing clones to be created
via:

	my $copy = $orig->clone;

The first argument to C<new()> or C<clone()> can be a hash-ref, which
will be used to set I<execution attributes> at construction time. I.e.:

    my $obj = Argv->new({autochomp => 1, stderr => 0}, @ARGV);

you may choose to add the command line later:

    my $obj = Argv->new;
    $obj->prog('cat');
    $obj->args('/etc/motd');

Or

    my $obj = Argv->new({autochomp=>1});
    my $motd = $obj->argv(qw(cat /etc/motd))->qx;

Or (using the autoloading interface)

    my $motd = $obj->cat('/etc/motd')->qx;

=head1 METHODS

=head2 INSTANCE METHODS

=over

=item * prog()

Returns or sets the name of the program (the C<"argv[0]">). This can be
a list, e.g. C<qw(rcs co)> or an array reference.

=item * opts()

Returns or sets the list of operands (aka arguments). As above, it may
be passed a list or an array reference. This is simply the member of
the class of optsI<NAME>() methods (see below) whose <NAME> is null;
it's part of the predefined I<anonymous option set>.

=item * args()

Returns or sets the list of operands (aka arguments).  If called in a
void context and without args, the effect is to set the list of
operands to C<()>.

=item * optset(<list-of-set-names>);

For each name I<NAME> in the parameter list, an I<option set> of that
name is declared and 3 new methods are registered dynamically:
C<parseI<NAME>(), optsI<NAME>(), and flagI<NAME>()>. These methods are
described below: note that the I<anonymous option set> (see I<OPTION
SETS>) is predefined, so the methods C<parse(), opts(), and flag()> are
always available.  Most users won't need to define any other sets.
Note that option-set names are forced to upper case. E.g.:

    $obj->optset('FOO');

=item * parseI<NAME>(...option-descriptions...)

Takes a list of option descriptions and uses Getopt::Long::GetOptions()
to parse them out of the current argv B<and into option set I<NAME>>.
The opt-descs are exactly as supported by parseI<FOO>() are exactly the
same as those described for Getopt::Long, except that no linkage
argument is allowed. E.g.:

    $obj->parseFOO(qw(file=s list=s@ verbose));

=item * optsI<NAME>()

Returns or sets the list of options in the B<option set I<NAME>>.

=item * flagI<NAME>()

Sets or gets the value of a flag in the appropriate optset, e.g.:

    print "blah blah blah\n" if $obj->flagFOO('verbose');
    $obj->flagFOO('verbose' => 1);

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

IMPORTANT: this method quotes its argument list B<in place>. In other
words, it may modify its arguments.

=item * glob

Expands the argument list using the Perl I<glob> builtin. Primarily
useful on Windows where the invoking shell does not do this for you.

Automatic use of I<glob> on Windows can be enabled via the I<autoglob>
method (vide infra).

=item * dump

A no-op except for printing the state of the invoking instance to
stderr. Potentially useful for debugging in situations where access to
I<perl -d> is limited, e.g. across a socket connection or in a
crontab. Invoked automatically at I<dbglevel=3>.

=item * readonly

Sets/gets the "readonly" attribute, which is a string indicating
whether the instance is to be used for read operations only. If the
attribute's value starts with C<y>, execution methods will be allowed
to proceed even if the C<$obj-E<gt>noexec> attribute is set.

The value of this is that it enables your script to have a C<-n> flag,
a la C<make -n>, pretty easily by careful management of
C<-E<gt>noexec>.  Consider a script which runs C<ls> to determine
whether a file exists and then, conditionally, C<rm -f> to remove it.
Causing a C<-n> flag from the user to set C<-E<gt>noexec> alone would
break the program logic since the C<ls> would be skipped too. But, if
you take care to use objects with the I<readonly> attribute set for all
read-only operations, perhaps by defining a special read-only object:

	my $ro = Argv->new;
	$ro->readonly('yes');

then a C<-n> flag will cause only write operations to be skipped.

Note that, if you choose to use this feature at all, determining which
operations are readonly is entirely the programmer's responsibility.
There's no way for Argv to determine whether a child process will
modify state.

=back

=head2 EXECUTION METHODS

The three methods below are direct analogues of the Perl builtins.
They simply reassemble a command line from the I<prog>, I<opts>, and
I<args> parts according to the option-set rules described below and
invoke their builtin equivalent on it.

=over

=item * system([<optset-list>])

Reassembles the argv and invokes system(). Return value and value of
$?, $!, etc. are just as described in I<perlfunc/"system">

Arguments to this method determine which of the parsed option-sets will
be used in the executed argv. If passed no arguments, C<$obj-E<gt>system>
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

    $obj->dfltsets({ONE => 1, THREE => 1});
    $obj->system;

while this would use all parsed options:

    $obj->dfltsets({'+' => 1});
    $obj->system;

and this would set the default to none class-wide, and then use it:

    $obj->dfltsets({'-' => 1});
    $obj->system;

By default the C<$obj-E<gt>system> method autoquotes its arguments I<iff>
the platform is Windows and the arguments are a list, because in this
case a shell is always used. This behavior can be toggled with
C<$obj-E<gt>autoquote>.  I<Note: if and when Perl 5.6 fixes this "bug",
Argv will be changed to examine the value of $]>.

=item * exec()

Similar to I<system> above, but never returns. On Windows, it blocks
until the new process finishes for a more UNIX-like behavior than
the I<exec> implemented by the C runtime library on Windows, if
the B<execwait> attribute is set. This is actually implemented as

    exit $obj->system(LIST);

and thus all C<system> shell-quoting issues apply

Option sets are handled as described in I<system> above.

=item * qx()

Same semantics as described in I<perlfunc/"qx"> but has the capability
to process only a set number of arguments at a time to avoid exceeding
the shell's line-length limit. This value is settable with the
I<qxargs> method.

Also, if I<autoquote> is set the arguments are quoted to protect them
against the platform-standard shell I<on all platforms>. 

Option sets are handled as described in I<system> above.

=back

=head2 EXECUTION ATTRIBUTES

The behavior of the I<execution methods> C<system, exec, and qx> is
governed by a set of I<execution attributes>, which are in turn
manipulated via a set of eponymous methods. These methods are
auto-generated and thus share certain common characteristics:

=over

=item * Translucency

They can all be invoked as class or instance methods.  If used as an
instance method the attribute is set only on that object; if used on
the class it sets or gets the default for all instances which haven't
overridden it.  This is inspired by the section on I<translucent
attributes> in Tom Christiansen's I<perltootc> tutorial.

=item * Class Defaults

Each attribute has a default which may be overridden with an
environment variable by prepending the class name, e.g. ARGV_QXARGS=256
or ARGV_STDERR=0;

=item * Context Sensitivity

The attribute value is always a scalar. If a value is passed it
becomes the new value of the attribute and the object or class is
returned. If no value is passed I<and there is a valid return
context>, the current value is returned. In a void context with no
parameter, the attribute value is set to 1.

=item * Stickiness

A subtlety: if an I<execution attribute> is set in a void context, that
attribute is I<"sticky">, i.e. it retains its state until explicitly
changed. But if I<a new value is provided> and the context is not void,
the new value is B<temporary>. It lasts until the next I<execution
method> (C<system, exec, or qx>) invocation, after which the previous
value is restored. This feature allows locutions like this:

	$obj->cmd('date')->stderr(1)->system;

Assuming that the C<$obj> object already exists and has a set of
attributes; we can override one of them at execution time.  More
examples:

	$obj->stdout(1);          # set attribute, sticky
	$obj->stdout;             # same as above
	$foo = $obj->stdout;      # get attribute value
	$obj2 = $obj->stdout(1);  # set to 1 (temporary), return $obj

WARNING: this attribute-stacking has turned out to be a bad idea. Its
use is now deprecated.

=back

=over

=item * autochomp

All data returned by the C<qx> method is chomped first. Unset by default.

=item * autofail

When set, the program will exit immediately if the C<system> or C<qx>
methods detect a nonzero status. Unset by default.

Autofail may also be given a code-ref, in which case that function will
be called upon error. This provides a basic "exception-handling" system:

    $obj->autofail(sub { print "caught an exception\n"; exit 17 });

Any failed executions by C<$obj> will call C<handler()>. Alternatively,
if the reference provided is an array-ref, the first element of that
array is assumed to be a code-ref as above and the rest of the array is
passed as args to the function on failure.

If the reference is to a scalar, the scalar is incremented for each
error and execution continues, e.g.

    my $rc = 0;
    $obj->autofail(\$rc);
    ...
    print "There were $rc failures counted\n";
    exit($rc);

=item * envp

Allows a different environment to be provided during execution of the
object. This setting is in scope only for the child process and will
not affect the environment of the current process.  Takes a hashref:

    my %newenv = %ENV;
    $newenv{PATH} .= ':/usr/ucb';
    delete @newenv{qw(TERM LANG LD_LIBRARY_PATH)};
    $obj->envp(\%newenv);

Subsequent invocations of I<$obj> will add I</usr/ucb> to PATH and
subtract TERM, LANG, and LD_LIBRARY_PATH;

=item * syfail,qxfail

Similar to C<autofail> but apply only to C<system()> or C<qx()>
respectively. Unset by default.

=item * autoglob

If set, the C<glob()> function is applied to the operands
(C<$obj-E<gt>args>) on Windows only. Unset by default.

=item * autoquote

If set, the operands are automatically quoted against shell expansion
before C<system()> on Windows and C<qx()> on all platforms (since C<qx>
always invokes a shell, and C<system()> always does so on Windows).
Set by default.

An individual word of an argv can "opt out" of autoquoting by
using a leading '^'. For instance:

    ('aa', 'bb', "^I'll do this one myself, thanks!", 'cc')

The '^' is stripped off and the rest of the string left alone.

=item * dbglevel

Sets the debug level. Level 0 (the default) means no debugging, level 1
prints each command before executing it, and higher levels offer
progressively more output. All debug output goes to stderr.

=item * dfltsets

Sets and/or returns the default set of I<option sets> to be used in
building up the command line at execution time.  The default-default is
the I<anonymous option set>. I<Note: this method takes a B<hash
reference> as its optional argument and returns a hash ref as well>.
The selected sets are represented by the hash keys; the values are
meaningless.

=item * execwait

If set, C<$obj-E<gt>exec> on Windows blocks until the new process is
finished for a more consistent UNIX-like behavior than the traditional
Win32 Perl port. Perl just uses the Windows exec() routine, which runs
the new process in the background. Set by default.

=item * inpathnorm

If set, normalizes pathnames to their native format just before
executing. This is NOT set by default; even when set it's a no-op
except on Windows, where it converts /x/y/z to \x\y\z.

=item * outpathnorm

If set, normalizes pathnames returned by the C<qx> method from
\-delimited to /-delimited. This is NOT set by default; even when set
it's a no-op except on Windows.

=item * noexec

Analogous to the C<-n> flag to I<make>; prints what would be executed
without executing anything.

=item * qxargs

You can set a maximum number of arguments to be processed at a time,
allowing you to blithely invoke e.g. C<$obj-E<gt>qx> on a list of any size
without fear of exceeding your shell's limits. A per-platform default
is set; this method allows it to be changed. A value of 0 suppresses
the behavior.

=item * syxargs

Analogous to I<qxargs> but applies to C<system()> and is turned off by
default. The reason is that C<qx()> is typically used to I<read> data
whereas C<system()> is more often used to make stateful changes.
Consider that "ls foo bar" produces the same result if broken up into
"ls foo" and "ls bar" but the same cannot be said for "mv foo bar".

=item * stdout

Setting this attribute to 0, e.g:

    $obj->stdout(0);

causes STDOUT to be closed during invocation of any of the I<execution
methods> C<system, exec, and qx>, and restored when they finish. A
fancy (and portable) way of saying C<1E<gt>/dev/null> without needing a
shell. A value of 2 is the equivalent of C<1E<gt>&2>.

=item * stderr

As above, for STDERR. A value of 1 is the equivalent of C<2E<gt>&1>:

    @alloutput = $obj->stderr(1)->qx;

=item * quiet

This attribute causes STDOUT to be closed during invocation of the
C<system> and C< exec> (but not C<qx>) I<execution methods>. It will
cause the application to run more quietly. This takes precedence over
a redirection of STDOUT using the <$obj-E<gt>stdout> method above.

=back

=over

=item * attropts

The above attributes can be set via method calls (e.g.
C<$obj-E<gt>dbglevel(1)>) or environment variables (ARGV_DBGLEVEL=1). Use
of the <$obj-E<gt>attropts> method allows them to be parsed from the command
line as well, e.g. I<myscript -/dbglevel 1>. If invoked as a class
method it causes options of the same names as the methods above to be
parsed (and removed) from the current C<@ARGV> and set as class
attributes.  As an instance method it parses and potentially depletes
the current argument vector of that object, and sets instance attributes
only. E.g.:

    Argv->attropts;

would cause the script to parse the following command line:

    script -/noexec 1 -/dbglevel=2 -flag1 -flag2 arg1 arg2 arg3 ...

so as to remove the C<-/noexec 1 -/dbglevel 2> and set the two class
attrs.  The C<-/> prefix is chosen to prevent conflicts with "real"
flags. Abbreviations are allowed as long as they're unique within the
set of -/ flags. As an instance method:

    $obj->attropts;

it will parse the current value of C<$obj-E<gt>args> and run

    $obj->foo(1);

for every instance of C<-/foo=1> found there.

=back

=head1 PORTING

This module is known to work on Solaris 2.5-8 and Windows 2000 SP2, and
with perl 5.004_04 and 5.6.  As these platforms are quite different,
there should be no I<major> portability issues, but please send bug
reports or patches to the address below. Recent testing is with
newer (5.6+) versions of Perl so some backporting may be necessary
for older Perls.

=head1 AUTHOR

David Boyce <dsb@boyski.com>

=head1 COPYRIGHT

Copyright (c) 1999-2002 David Boyce. All rights reserved.  This Perl
program is free software; you may redistribute and/or modify it under
the same terms as Perl itself.

=head1 SEE ALSO

perl(1), Getopt::Long(3), IPC::ChildSafe(3)

=cut
