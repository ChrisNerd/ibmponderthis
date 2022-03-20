// LEXICODE
// Pascal code, 2005, J.A.O. Ekberg
//
// Syntax: LEXICODE n d w
// LEXICODE 30 10 6
//
// Description: Constructs the lexicographic code with
// length n, minimum distance d, and constant weight w.
// Outputs the entire code in hexadecimal as well as the
// lower bound on A(n,d,w) it produces.
//
// Compatibility: Tested with Free Pascal Compiler 2.0.0.
program Lexicode;
uses
 SysUtils;
type
// Data type to store the code; each codeword consists of
// 32 bits packed in a longword
TCode = array of longword;
var
testword: longword; // Variable that goes through all
// vectors of the correct length
newCode: TCode;
n, d, w: byte;
// Converts a nonnegative integer to hexadecimal
function IntToHex(i: longword): string;
var
temp: longword;
s: string;
begin
   s := '';
temp := i;
while temp > 0 do
begin
case temp mod 16 of
0..9:
 s := IntToStr(temp mod 16) + s;
10..15: s := Chr(temp mod 16 + 55) + s;
end;
temp := temp shr 4;

end;
exit(s);
end;
// Computes the weight of a word
function wt(v: longword): byte;
var
count: byte;
temp: longword;
begin
count := 0;
temp := v;
while temp > 0 do
begin
if Odd(temp) then
Inc(count);
temp := temp shr 1;
end;
exit(count);
end;
// Computes the distance between two words
function dist(u, v: longword): byte;
begin
exit(wt(u xor v));
end;
// Computes the minimum distance between a word and a code
function distToCode(v: longword; code: TCode): byte;
var
i: byte;
temp: longword;
begin
//temp := Max longword;
temp := $FFFFFFFF;
for i := 0 to Length(code) - 1 do
if dist(v, code[i]) < temp then
temp := dist(v, code[i]);
exit(temp);
end;

// Terminates when too low distance encountered
function distToCodeAtLeast(v: longword; code: TCode;
min: byte): boolean;
var
i: byte;
begin
for i := 0 to Length(code) - 1 do
if dist(v, code[i]) < min then
exit(FALSE);
exit(TRUE);
end;
begin
n := StrToInt(ParamStr(1));
d := StrToInt(ParamStr(2));
w := StrToInt(ParamStr(3));
SetLength(newCode, 0);
for testword := 0 to 1 shl n - 1 do
// If weight and distance are good enough, add the word
// to the code and output it
if (wt(testword) = w) and ((Length(newCode) = 0) or
distToCodeAtLeast(testword, newCode, d)) then
begin
SetLength(newCode, Length(newCode) + 1);
newCode[Length(newCode) - 1] := testword;
Write(IntToHex(testword), '; ');
end;
Writeln;
Writeln;
Writeln('Lexicode lower bound: A(', n, ',', d, ',', w,
') >= ', Length(newCode));
end.



//pas 30 10 6

// Output is: 
//3F; 7C1; 7842; 38884; C9108; 152210; 1A4420; E00910; 32010A0; 5402404; 6804208; 9860001; A490002; C308040; 30210408; 

//Lexicode lower bound: A(30,10,6) >= 15
//pas 31 10 6                                                                                                                         
//3F; 7C1; 7842; 38884; C9108; 152210; 1A4420; E00910; 32010A0; 5402404; 6804208; 9860001; A490002; C308040; 30210408; 50408220; 60880044;                                                  
                                                                                                                                                                                          
//Lexicode lower bound: A(31,10,6) >= 17              
