#!/usr/bin/env perl
use strict;
use warnings;
use Term::ANSIColor;

my $mode = "codeworld";
my $url = "https://blackeyepeas.phys.lsu.edu/cw/";
my $file = $ARGV[0];

print "Uploading $file ...\n";

my $json = `curl -S -s -F "mode=$mode" -F "source=<$file" ${url}compile`;
my $dhash = `echo '$json' | jq -r .dhash`;
my $hash = `echo '$json' | jq -r .hash`;
chomp($dhash);
chomp($hash);
my $errormsg = `curl -S -s -F "mode=$mode" -F "hash=$hash" ${url}runMsg`;

if ($errormsg eq "" or index($errormsg, "error") == -1) {
    my $curl = "$url#$hash";
    my $durl = "${url}run.html?mode=$mode&dhash=$dhash";
    print color("green");
    print("Successfully uploaded files!\n");
    print color("reset");
    print("Source at: $curl\n");
    print("Runnable at: $durl\n");

    if ($errormsg ne "") {
        print color('yellow');
        print("Compiled with warnings:\n");
        print("$errormsg\n");
        print color('reset');
    }

    exec("firefox \'$durl\'");
} else {
    print color("red");
    print("\nCompilation failed:\n");
    print("${errormsg}\n");
    print color("reset");
}
