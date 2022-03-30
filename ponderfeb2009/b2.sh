#!/bin/bash
rm h???.pgm
for col in {1..510}; do
#((510 / $col * $col - 510)) && continue
col3d=`printf "%03d" $col`
echo "P5" >> h"$col3d".pgm
echo "#feep.pgm" >> h"$col3d".pgm
((255 / $col * $col - 255)) && echo "$col $((255 / $col + 1))" >> h"$col3d".pgm || echo "$col $((255 / $col))" >> h"$col3d".pgm
echo "256" >> h"$col3d".pgm
echo "900F 80F0 8F00 80CA BE12 AA90 9400 0048 3E5B 8AC0" >> h"$col3d".pgm
echo "3400 00CB BC81 8A08 3C00 0050 BE43 00C0 3E00 A019" >> h"$col3d".pgm
echo "8059 BE13 2000 0092 BE9B 2A0B 2A00 8052 8841 04C0" >> h"$col3d".pgm
echo "3E00 840B 084B 0098 E000 8819 845A 8012 0300 0050" >> h"$col3d".pgm
echo "826F 0500 0600 846E 8264 0900 0A00 8065 0C00 0072" >> h"$col3d".pgm
echo "A054 8368 8569 4800 4400 8573 4200 4100 8349 8542" >> h"$col3d".pgm
echo "2800 2400 854D 2200 2100 9F00 E000 8888 8444 8000" >> h"$col3d".pgm
echo "0030 0DED 8222 0050 0060 8444 8222 0090 00A0 8000" >> h"$col3d".pgm
echo "00C0 0DED A000 8333 8555 4080 4040 8555 4020 4010" >> h"$col3d".pgm
echo "8333 8555 2080 2040 8555 2020 2010 8300 8500 8030" >> h"$col3d".pgm
echo "8050 0880 0840 8050 0820 0810 8030 8050 0480 0440" >> h"$col3d".pgm
echo "8050 0420 0410 8500 8030 8050 0280 0240 8050 0220" >> h"$col3d".pgm
echo "0210 8030 8050 0180 0140 8050 0120 0110 90F0 9F00" >> h"$col3d".pgm
echo "E000 8888 8444 8000 0003 0DED 8222 0005 0006 8444" >> h"$col3d".pgm
echo "8222 0009 000A 8000 000C 0DED A000 8333 8555 4008" >> h"$col3d".pgm
echo "4004 8555 4002 4001 8333 8555 2008 2004 8555 2002" >> h"$col3d".pgm
echo "2001 8300 8500 8003 8005 0808 0804 8005 0802 0801" >> h"$col3d".pgm
echo "8003 8005 0408 0404 8005 0402 0401 8500 8003 8005" >> h"$col3d".pgm
echo "0208 0204 8005 0202 0201 8003 8005 0108 0104 8005" >> h"$col3d".pgm
echo "0102 0101 9F00 8030 8050 8003 8005 0088 0084 8005" >> h"$col3d".pgm
echo "0082 0081 8003 8005 0048 0044 8005 0042 0041 8050" >> h"$col3d".pgm
echo "8003 8005 0028 0024 8005 0022 0021 8003 8005 0018" >> h"$col3d".pgm
echo "0014 8005 0012 0011 80FF 8F0F A333 8000 5000 0DED" >> h"$col3d".pgm
echo "8000 3000 0DED A333 C555 1800 1400 C555 1200 1100" >> h"$col3d".pgm
echo "8F0F A333 A555 1080 1040 A555 1020 1010 A333 A555" >> h"$col3d".pgm
echo "1008 1004 A555 1002 1001" >> h"$col3d".pgm
done