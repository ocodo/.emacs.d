#!/usr/bin/env perl
use strict;
use warnings;

use File::Find;
use File::Spec;
use Config;

my $helm_perl5lib = shift;
my @dirs;
if ($helm_perl5lib) {
    @dirs = map { File::Spec->catfile($_, $Config{archname}), $_ } split ':', $helm_perl5lib;
}
push @dirs, @INC;

my %modules = ();
my %inc = map { $_ => $_ } map { File::Spec->canonpath($_) } grep { -d $_ && $_ ne '.' } @dirs;

for my $path (keys %inc) {
    find(+{
        wanted => sub {
            my $name = $File::Find::fullname;

            if (-d $name) {
                if (exists $inc{$name} && $inc{$name} ne $path) {
                    $File::Find::prune = 1;
                }
                return;
            }
            return unless $name =~ s/\.p(?:m|od)\z//;

	    $name = File::Spec->abs2rel($name, $path);
            my $module = join('::', File::Spec->splitdir($name));
            $modules{$module}++;
        },
        follow => 1,
        follow_skip => 2,
    }, $path);
}
print "$_\n" for sort keys %modules;
