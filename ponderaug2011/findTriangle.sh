#!/bin/bash
# The correct triangle needs to produce an area that can be produced with other triangles called set A, or else Ariella would know right away.
#     Of the set of triangles that Charlie can form, all but one must have only one way of producing its area.     
# Of the triangles in Set A, each perimeter forms a set Pi, such that all but one member of set A, has all the members of Pi giving unique areas.



Set that Charlie can form
1 4 4 perimeter 9 area 3.9375
2 3 4 perimeter 9 area 8.4375
3 3 3 perimeter 9 area 15.1875


Area=3.9375
1 4 4 perimeter 9 area 3.9375
2 2 3 perimeter 7 area 3.9375

unique for area
2 3 4 perimeter 9 area 8.4375

Unique for area
3 3 3 perimeter 9 area 15.1875



So now Ariella knows its either 1 4 4, or 2 2 3, perimeter 9 or 7.

If it 7 then Ariella thinks Charlie can form 1 3 3 area 2.1875 or 2 2 3 area 3.9375


unique for Area
1 3 3 perimeter 7 area 2.1875

1 4 4 doesn't work!!!  Given the area 3.9375 Arellia can't determine the perimeter.



Totally!!!!
3 4 4 perimeter 11 area 30.9375


1 5 5 perimeter 11 area 6.1875
2 4 5 perimeter 11 area 14.4375
3 3 5 perimeter 11 area 17.1875
3 4 4 perimeter 11 area 30.9375

grep "area 6\.1875$" tris.txt 
1 5 5 perimeter 11 area 6.1875
grep "area 14\.4375$" tris.txt 
2 4 5 perimeter 11 area 14.4375
grep "area 17\.1875$" tris.txt 
3 3 5 perimeter 11 area 17.1875
grep "area 30\.9375$" tris.txt 
2 6 7 perimeter 15 area 30.9375
3 4 4 perimeter 11 area 30.9375


grep "perimeter 15 " tris.txt 
1 7 7 perimeter 15 area 12.1875
2 6 7 perimeter 15 area 30.9375
3 5 7 perimeter 15 area 42.1875
3 6 6 perimeter 15 area 75.9375
4 4 7 perimeter 15 area 45.9375
4 5 6 perimeter 15 area 98.4375
5 5 5 perimeter 15 area 117.1875


Ariella thinks that if Charlie had a perimeter=15 there are still 2 possibilities for areas that can be form more than 1 way, i.e., area 42.1875 and 98.4375, and therefore wouldn't know the triangle.

grep "area 12\.1875$" tris.txt 
1 7 7 perimeter 15 area 12.1875
grep "area 42\.1875$" tris.txt 
1 13 13 perimeter 27 area 42.1875
3 5 7 perimeter 15 area 42.1875
grep "area 75\.9375$" tris.txt 
3 6 6 perimeter 15 area 75.9375
grep "area 45\.9375$" tris.txt 
4 4 7 perimeter 15 area 45.9375
grep "area 98\.4375$" tris.txt 
2 11 12 perimeter 25 area 98.4375
3 8 10 perimeter 21 area 98.4375
4 5 6 perimeter 15 area 98.4375
grep "area 117\.1875$" tris.txt 
5 5 5 perimeter 15 area 117.1875
