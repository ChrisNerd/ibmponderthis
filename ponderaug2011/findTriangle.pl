open(MYINPUTFILE, "<tris.txt"); # open for input
my(@lines) = <MYINPUTFILE>; # read file into list
my($line);
foreach $line (@lines) # loop thru list
 {
my($perimeter) = @{[$line =~ m/\w+/g]}[4];
my(@perimeterSet) = grep "perimeter $perimeter" tris.txt;
my($numberOfTrianglesThatCanBeFormedMoreThanOneWay)
foreach $triangle (@perimeterSet)
my($area) = @{[$triangle =~ m/\w+/g]}[6];
if (grep "area $area\$" tris | wc != 1)
    $numberOfTrianglesThatCanBeFormedMoreThanOneWay =
    $numberOfTrianglesThatCanBeFormedMoreThanOneWay + 1;
if $numberOfTrianglesThatCanBeFormedMoreThanOneWay) > 1
    break;

 }
if $numberOfTrianglesThatCanBeFormedMoreThanOneWay) == 1
print "Solution" @triangle

}

close(MYINPUTFILE);
