use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Argv',
    'VERSION_FROM'	=> 'Argv.pm',
    # The code doesn't actually require Getopt::Long 2.17 but we
    # prefer and thus request it here.
    'PREREQ_PM'		=> { Getopt::Long => 2.17 },
    'dist'		=> {
	COMPRESS => 'gzip --force --best',
	PREOP	 => "pod2html Argv.pm --title='Argv Module' >Argv.html",
	POSTOP	 => 'rm -f pod2html-*cache',
	TAR	 => 'gtar',
	TARFLAGS => '--mode=u+w -cvf',
    },
);
