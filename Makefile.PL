use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Argv',
    'VERSION_FROM'	=> 'Argv.pm',
    'dist'		=> {
	COMPRESS => 'gzip --force --best',
	PREOP	 => "pod2html Argv.pm --title='Argv Module' >Argv.html",
	POSTOP	 => 'rm -f pod2html-*cache',
	TAR	 => 'gtar',
	TARFLAGS => '--mode=u+w -cvf',
    },
);
