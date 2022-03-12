import scala.io.Source
object ponder {
 val n = 7                                        //> n  : Int = 7
 val c = 7                                        //> c  : Int = 7
 def binaryListOfSize(l: Int): List[List[Int]] =
 {
  if (l == 0)
   List(List())
  else
   for {
    first <- (0 to 1).toList
    rest <- binaryListOfSize(l-1)
   } yield first :: rest
 }                                                //> binaryListOfSize: (l: Int)List[List[Int]]

 val allGenderCombs = binaryListOfSize(n)         //> allGenderCombs  : List[List[Int]] = List(List(0, 0, 0, 0, 0, 0, 0), List(0, 
                                                  //| 0, 0, 0, 0, 0, 1), List(0, 0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 1, 1), Lis
                                                  //| t(0, 0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 0, 1), List(0, 0, 0, 0, 1, 1, 0)
                                                  //| , List(0, 0, 0, 0, 1, 1, 1), List(0, 0, 0, 1, 0, 0, 0), List(0, 0, 0, 1, 0, 
                                                  //| 0, 1), List(0, 0, 0, 1, 0, 1, 0), List(0, 0, 0, 1, 0, 1, 1), List(0, 0, 0, 1
                                                  //| , 1, 0, 0), List(0, 0, 0, 1, 1, 0, 1), List(0, 0, 0, 1, 1, 1, 0), List(0, 0,
                                                  //|  0, 1, 1, 1, 1), List(0, 0, 1, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0, 1), List(
                                                  //| 0, 0, 1, 0, 0, 1, 0), List(0, 0, 1, 0, 0, 1, 1), List(0, 0, 1, 0, 1, 0, 0), 
                                                  //| List(0, 0, 1, 0, 1, 0, 1), List(0, 0, 1, 0, 1, 1, 0), List(0, 0, 1, 0, 1, 1,
                                                  //|  1), List(0, 0, 1, 1, 0, 0, 0), List(0, 0, 1, 1, 0, 0, 1), List(0, 0, 1, 1, 
                                                  //| 0, 1, 0), List(0, 0, 1, 1, 0, 1, 1), List(0, 0, 1, 1, 1, 0, 0), List(0, 0, 1
                                                  //| , 1, 1, 0, 1), List(0, 0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 1, 1, 1), List(0,
                                                  //|  1, 0, 0, 0, 0, 0), List(0, 1, 0, 0, 0, 0, 1), List(0, 1, 0, 0, 0, 1, 0), Li
                                                  //| st(0, 1, 0, 0, 0, 1, 1), List(0, 1, 0, 0, 1, 0, 0), List(0, 1, 0, 0, 1, 0, 1
                                                  //| ), List(0, 1, 0, 0, 1, 1, 0), List(0, 1, 0, 0, 1, 1, 1), List(0, 1, 0, 1, 0,
                                                  //|  0, 0), List(0, 1, 0, 1, 0, 0, 1), List(0, 1, 0, 1, 0, 1, 0), List(0, 1, 0, 
                                                  //| 1, 0, 1, 1), List(0, 1, 0, 1, 1, 0, 0), List(0, 1, 0, 1, 1, 0, 1), List(0, 1
                                                  //| , 0, 1, 1, 1, 0), List(0, 1, 0, 1, 1, 1, 1), List(0, 1, 1, 0, 0, 0, 0), List
                                                  //| (0, 1, 1, 0, 0, 0, 1), List(0, 1, 1, 0, 0, 1, 0), List(0, 1, 1, 0, 0, 1, 1),
                                                  //|  List(0, 1, 1, 0, 1, 0, 0), List(0, 1, 1, 0, 1, 0, 1), List(0, 1, 1, 0, 1, 1
                                                  //| , 0), List(0, 1, 1, 0, 1, 1, 1), List(0, 1, 1, 1, 0, 0, 0), List(0, 1, 1, 1,
                                                  //|  0, 0, 1), List(0, 1, 1, 1, 0, 1, 0), List(0, 1, 1, 1, 0, 1, 1), List(0, 1, 
                                                  //| 1, 1, 1, 0, 0), List(0, 1, 1, 1, 1, 0, 1), List(0, 1, 1, 1, 1, 1, 0), List(0
                                                  //| , 1, 1, 1, 1, 1, 1), List(1, 0, 0, 0, 0, 0, 0), List(1, 0, 0, 0, 0, 0, 1), L
                                                  //| ist(1, 0, 0, 0, 0, 1, 0), List(1, 0, 0, 0, 0, 1, 1), List(1, 0, 0, 0, 1, 0, 
                                                  //| 0), List(1, 0, 0, 0, 1, 0, 1), List(1, 0, 0, 0, 1, 1, 0), List(1, 0, 0, 0, 1
                                                  //| , 1, 1), List(1, 0, 0, 1, 0, 0, 0), List(1, 0, 0, 1, 0, 0, 1), List(1, 0, 0,
                                                  //|  1, 0, 1, 0), List(1, 0, 0, 1, 0, 1, 1), List(1, 0, 0, 1, 1, 0, 0), List(1, 
                                                  //| 0, 0, 1, 1, 0, 1), List(1, 0, 0, 1, 1, 1, 0), List(1, 0, 0, 1, 1, 1, 1), Lis
                                                  //| t(1, 0, 1, 0, 0, 0, 0), List(1, 0, 1, 0, 0, 0, 1), List(1, 0, 1, 0, 0, 1, 0)
                                                  //| , List(1, 0, 1, 0, 0, 1, 1), List(1, 0, 1, 0, 1, 0, 0), List(1, 0, 1, 0, 1, 
                                                  //| 0, 1), List(1, 0, 1, 0, 1, 1, 0), List(1, 0, 1, 0, 1, 1, 1), List(1, 0, 1, 1
                                                  //| , 0, 0, 0), List(1, 0, 1, 1, 0, 0, 1), List(1, 0, 1, 1, 0, 1, 0), List(1, 0,
                                                  //|  1, 1, 0, 1, 1), List(1, 0, 1, 1, 1, 0, 0), List(1, 0, 1, 1, 1, 0, 1), List(
                                                  //| 1, 0, 1, 1, 1, 1, 0), List(1, 0, 1, 1, 1, 1, 1), List(1, 1, 0, 0, 0, 0, 0), 
                                                  //| List(1, 1, 0, 0, 0, 0, 1), List(1, 1, 0, 0, 0, 1, 0), List(1, 1, 0, 0, 0, 1,
                                                  //|  1), List(1, 1, 0, 0, 1, 0, 0), List(1, 1, 0, 0, 1, 0, 1), List(1, 1, 0, 0, 
                                                  //| 1, 1, 0), List(1, 1, 0, 0, 1, 1, 1), List(1, 1, 0, 1, 0, 0, 0), List(1, 1, 0
                                                  //| , 1, 0, 0, 1), List(1, 1, 0, 1, 0, 1, 0), List(1, 1, 0, 1, 0, 1, 1), List(1,
                                                  //|  1, 0, 1, 1, 0, 0), List(1, 1, 0, 1, 1, 0, 1), List(1, 1, 0, 1, 1, 1, 0), Li
                                                  //| st(1, 1, 0, 1, 1, 1, 1), List(1, 1, 1, 0, 0, 0, 0), List(1, 1, 1, 0, 0, 0, 1
                                                  //| ), List(1, 1, 1, 0, 0, 1, 0), List(1, 1, 1, 0, 0, 1, 1), List(1, 1, 1, 0, 1,
                                                  //|  0, 0), List(1, 1, 1, 0, 1, 0, 1), List(1, 1, 1, 0, 1, 1, 0), List(1, 1, 1, 
                                                  //| 0, 1, 1, 1), List(1, 1, 1, 1, 0, 0, 0), List(1, 1, 1, 1, 0, 0, 1), List(1, 1
                                                  //| , 1, 1, 0, 1, 0), List(1, 1, 1, 1, 0, 1, 1), List(1, 1, 1, 1, 1, 0, 0), List
                                                  //| (1, 1, 1, 1, 1, 0, 1), List(1, 1, 1, 1, 1, 1, 0), List(1, 1, 1, 1, 1, 1, 1))
                                                  //| 

  val allGroupsOf3 = (0 until n).toSet.subsets(3) //> allGroupsOf3  : Iterator[scala.collection.immutable.Set[Int]] = non-empty it
                                                  //| erator

  // Oh Geez! the size of this is ((5 choose 3) choose 7). Gah! ((n choose p) choose q) is O(n!!)
  val pickingCGroups = allGroupsOf3.toSet.subsets(c)
                                                  //> pickingCGroups  : Iterator[scala.collection.immutable.Set[scala.collection.i
                                                  //| mmutable.Set[Int]]] = non-empty iterator

 val solutions = pickingCGroups.filter(           // all the ways to pick 7 groups, filtering for ...
  possibleSolution => allGenderCombs.forall(      // passing the test for all the 2^n ways that the genders can be arranged ...
   genderCombo => possibleSolution.exists(        // is there at least 1 set (group of 3) from our possible solution ...
    _.map(                                        // that finds a mono-gendered group?
     genderCombo(_)                               // (indexing genderCombo by the entries in possibleSolution, since the entry in possibleSolution is a Set,
    ).size == 1                                   // the map also returns a set - eliminating duplicates
   )
  )                                               // Do we pass all 2^n cases?
 )                                                //> solutions  : Iterator[scala.collection.immutable.Set[scala.collection.immut
                                                  //| able.Set[Int]]] = non-empty iterator
 //solutions.foreach(println)

 
 val all3LetterWords =  Source.fromFile("3letters2.txt").getLines.toList//.mkString
                                                  //> all3LetterWords  : List[String] = List("	# Includes all 3-letter words ac
                                                  //| ceptable in U.S. Club and Tournament Play as of 2009.", https://github.com/
                                                  //| hzlzh/Domain-Name-List/blob/master/3-letter-words.txt, "", ABS, ACE, ACT, A
                                                  //| DD, AFT, AGE, AGO, AID, AIL, AIM, AIR, ALE, ALL, ALP, AMP, AND, ANT, ANY, A
                                                  //| PE, APT, ARC, ARE, ARK, ARM, ARS, ART, ASH, ASK, ASP, ATE, AWE, AWL, AXE, B
                                                  //| AD, BAG, BAN, BAR, BAT, BAY, BED, BEE, BEG, BET, BID, BIG, BIN, BIO, BIT, B
                                                  //| OB, BOG, BOO, BOP, BOT, BOW, BOX, BOY, BRA, BRR, BUD, BUG, BUM, BUN, BUS, B
                                                  //| UT, BUY, BYE, CAB, CAN, CAP, CAR, CAT, CIS, COB, COG, CON, COP, COW, COY, C
                                                  //| RY, CUB, CUD, CUE, CUP, CUT, CWM, DAB, DAD, DAM, DAY, DEF, DEN, DEW, DIB, D
                                                  //| ID, DIE, DIG, DIM, DIN, DIP, DIT, DOE, DOG, DON, DOT, DOW, DRY, DUB, DUD, D
                                                  //| UE, DUG, DUH, DUO, DYE, EAR, EAT, EGG, EGO, EKE, ELF, ELK, ELM, EMU, END, E
                                                  //| ON, ERA, ERE, ERG, ERR, EVE, EWE, EYE, FAD, FAN, FAR, FAT, FAX, FEE, FES, F
                                                  //| EW, FEZ, FIB, FIG, FIN, FIR, FIT, FIX, FLU, FLY, FOE, FOG, FOR, FOU, FOX, F
                                                  //| RO, FRY, FUN, FUR, GAB, GAG, GAL, GAP, GAS, GAY, GEE, GEL, GEM, GET, GIG, G
                                                  //| IN, GIT, GNU, GOB, GOD, GOO, GOT, GUM, GUN, GUT, GUY, GYM, GYP, HAD, HAG, H
                                                  //| AM, HAS, HAT, HAY, HEM, HEN, HER, HEX, HEY, HID, HIM, HIP, HIS, HIT, HOE, H
                                                  //| OG, HOP, HOT, HOW, HUB, HUE, HUG, HUH, HUM, HUT, ICE, ICY, IFS, ILK, ILL, I
                                                  //| MP, INK, INN, INS, ION, IRE, IRK, ISM, ITS, IVY, JAB, JAM, JAR, JAW, JET, J
                                                  //| EW, JIB, JIG, JOB, JOE, JOG, JOT, JOY, JUG, KEG, KEY, KID, KIN, KIT, LAB, L
                                                  //| AD, LAG, LAM, LAP, LAT, LAW, LAY, LEA, LED, LEE, LEG, LEI, LET, LEX, LEY, L
                                                  //| IB, LID, LIE, LIP, LIT, LOB, LOG, LOO, LOP, LOT, LUG, LYE, MAD, MAN, MAP, M
                                                  //| AR, MAT, MAX, MAY, MEN, MET, MHO, MIC, MIX, MOB, MOC, MOD, MOM, MOP, MOW, M
                                                  //| UD, MUG, MUM, MUT, NAB, NAG, NAN, NAP, NET, NEW, NIB, NIL, NIM, NIP, NIT, N
                                                  //| IX, NOD, NOG, NOR, NOT, NOW, NTH, NUN, NUT, OAF, OAK, OAR, OAT, OBI, ODD, O
                                                  //| DE, OFF, OFT, OHM, OIL, OLD, ONE, OPT, ORB, ORC, OUR, OUT, OVA, OWL, OWN, P
                                                  //| AD, PAL, PAN, PAP, PAT, PAW, PAY, PEA, PEC, PEG, PEN, PEP, PER, PET, PHI, P
                                                  //| IC, PIE, PIG, PIN, PIP, PIT, PLY, POD, POP, POT, POW, POX, PRO, PRY, PSI, P
                                                  //| UB, PUG, PUN, PUP, PUS, PUT, RAD, RAG, RAM, RAN, RAP, RAT, RAW, RAY, RED, R
                                                  //| EF, REM, REP, RHO, RIB, RID, RIG, RIM, RIP, ROB, ROD, ROE, ROM, ROT, ROW, R
                                                  //| UB, RUE, RUG, RUM, RUN, RUT, RYE, SAD, SAG, SAP, SAT, SAW, SAY, SEA, SEC, S
                                                  //| EE, SET, SEW, SEX, SHE, SHH, SHY, SIC, SIN, SIP, SIR, SIS, SIT, SIX, SKI, S
                                                  //| KY, SLY, SOB, SOD, SOL, SON, SOT, SOW, SOX, SOY, SPA, SPY, SRI, STY, SUB, S
                                                  //| UE, SUM, SUN, TAB, TAD, TAG, TAN, TAO, TAP, TAR, TAU, TAX, TEA, TEE, TEN, T
                                                  //| HE, THY, TIC, TIE, TIL, TIN, TIP, TIT, TOD, TOE, TOG, TON, TOO, TOP, TOT, T
                                                  //| OY, TRY, TUB, TUG, TUN, TWO, TYE, UKE, UPS, URN, USE, VAN, VAT, VIA, VIM, V
                                                  //| OW, VOX, WAD, WAG, WAR, WAS, WAY, WEB, WED, WEE, WHO, WHY, WIG, WIN, WIT, W
                                                  //| OE, WOK, WON, WOO, WOW, WYE, YAK, YAM, YAP, YAW, YEN, YES, YET, YIN, YOU, Y
                                                  //| UK, YUM, ZAG, ZAP, ZIG, ZIP, ZIT, ZOO)
val allWords = all3LetterWords.map(_.toLowerCase()).filter(_.distinct.size==3)
                                                  //> allWords  : List[String] = List(abs, ace, act, aft, age, ago, aid, ail, aim
                                                  //| , air, ale, alp, amp, and, ant, any, ape, apt, arc, are, ark, arm, ars, art
                                                  //| , ash, ask, asp, ate, awe, awl, axe, bad, bag, ban, bar, bat, bay, bed, beg
                                                  //| , bet, bid, big, bin, bio, bit, bog, bop, bot, bow, box, boy, bra, bud, bug
                                                  //| , bum, bun, bus, but, buy, bye, cab, can, cap, car, cat, cis, cob, cog, con
                                                  //| , cop, cow, coy, cry, cub, cud, cue, cup, cut, cwm, dab, dam, day, def, den
                                                  //| , dew, dib, die, dig, dim, din, dip, dit, doe, dog, don, dot, dow, dry, dub
                                                  //| , due, dug, duh, duo, dye, ear, eat, ego, elf, elk, elm, emu, end, eon, era
                                                  //| , erg, fad, fan, far, fat, fax, fes, few, fez, fib, fig, fin, fir, fit, fix
                                                  //| , flu, fly, foe, fog, for, fou, fox, fro, fry, fun, fur, gab, gal, gap, gas
                                                  //| , gay, gel, gem, get, gin, git, gnu, gob, god, got, gum, gun, gut, guy, gym
                                                  //| , gyp, had, hag, ham, has, hat, hay, hem, hen, her, hex, hey, hid, him, hip
                                                  //| , his, hit, hoe, hog, hop, hot, how, hub, hue, hug, hum, hut, ice, icy, ifs
                                                  //| , ilk, imp, ink, ins, ion, ire, irk, ism, its, ivy, jab, jam, jar, jaw, jet
                                                  //| , jew, jib, jig, job, joe, jog, jot, joy, jug, keg, key, kid, kin, kit, lab
                                                  //| , lad, lag, lam, lap, lat, law, lay, lea, led, leg, lei, let, lex, ley, lib
                                                  //| , lid, lie, lip, lit, lob, log, lop, lot, lug, lye, mad, man, map, mar, mat
                                                  //| , max, may, men, met, mho, mic, mix, mob, moc, mod, mop, mow, mud, mug, mut
                                                  //| , nab, nag, nap, net, new, nib, nil, nim, nip, nit, nix, nod, nog, nor, not
                                                  //| , now, nth, nut, oaf, oak, oar, oat, obi, ode, oft, ohm, oil, old, one, opt
                                                  //| , orb, orc, our, out, ova, owl, own, pad, pal, pan, pat, paw, pay, pea, pec
                                                  //| , peg, pen, per, pet, phi, pic, pie, pig, pin, pit, ply, pod, pot, pow, pox
                                                  //| , pro, pry, psi, pub, pug, pun, pus, put, rad, rag, ram, ran, rap, rat, raw
                                                  //| , ray, red, ref, rem, rep, rho, rib, rid, rig, rim, rip, rob, rod, roe, rom
                                                  //| , rot, row, rub, rue, rug, rum, run, rut, rye, sad, sag, sap, sat, saw, say
                                                  //| , sea, sec, set, sew, sex, she, shy, sic, sin, sip, sir, sit, six, ski, sky
                                                  //| , sly, sob, sod, sol, son, sot, sow, sox, soy, spa, spy, sri, sty, sub, sue
                                                  //| , sum, sun, tab, tad, tag, tan, tao, tap, tar, tau, tax, tea, ten, the, thy
                                                  //| , tic, tie, til, tin, tip, tod, toe, tog, ton, top, toy, try, tub, tug, tun
                                                  //| , two, tye, uke, ups, urn, use, van, vat, via, vim, vow, vox, wad, wag, war
                                                  //| , was, way, web, wed, who, why, wig, win, wit, woe, wok, won, wye, yak, yam
                                                  //| , yap, yaw, yen, yes, yet, yin, you, yuk, yum, zag, zap, zig, zip, zit)
allWords contains "nai"                           //> res0: Boolean = false
                                                  
val counts = ('a' to 'z') map ( x=> (x, allWords.count(word => word contains x)))
                                                  //> counts  : scala.collection.immutable.IndexedSeq[(Char, Int)] = Vector((a,14
                                                  //| 0), (b,60), (c,32), (d,59), (e,103), (f,32), (g,63), (h,39), (i,98), (j,14)
                                                  //| , (k,18), (l,47), (m,51), (n,66), (o,111), (p,68), (q,0), (r,71), (s,59), (
                                                  //| t,93), (u,66), (v,8), (w,43), (x,16), (y,56), (z,6))
counts.sortBy(- _._2)                             //> res1: scala.collection.immutable.IndexedSeq[(Char, Int)] = Vector((a,140), 
                                                  //| (o,111), (e,103), (i,98), (t,93), (r,71), (p,68), (n,66), (u,66), (g,63), (
                                                  //| b,60), (d,59), (s,59), (y,56), (m,51), (l,47), (w,43), (h,39), (c,32), (f,3
                                                  //| 2), (k,18), (x,16), (j,14), (v,8), (z,6), (q,0))
val c1 = counts.sortBy(- _._2).take(14).map(_._1) //> c1  : scala.collection.immutable.IndexedSeq[Char] = Vector(a, o, e, i, t, r
                                                  //| , p, n, u, g, b, d, s, y)
val d = counts.sortBy(- _._2).take(14).map(_._1).toSet
                                                  //> d  : scala.collection.immutable.Set[Char] = Set(e, s, n, y, t, u, a, i, b, 
                                                  //| g, p, r, o, d)
                                                  
 // This gives no solutions unless we take(at least 14) (up to at least 'y')
 // We'll go through all the 7 letter subsets of the top 16? letters.
 val sevenLettersSubsets = counts.sortBy(- _._2).take(20).map(_._1).toSet.subsets(7)
                                                  //> sevenLettersSubsets  : Iterator[scala.collection.immutable.Set[Char]] = non
                                                  //| -empty iterator
 // I think all solutions are the same by symmetry, so we'll just go ahead and take the first.
 val firstSolution = solutions.take(1).toList.head.toList
                                                  //> firstSolution  : List[scala.collection.immutable.Set[Int]] = List(Set(0, 2,
                                                  //|  3), Set(5, 1, 3), Set(0, 5, 4), Set(5, 6, 2), Set(0, 1, 6), Set(6, 3, 4), 
                                                  //| Set(1, 2, 4))
  Set('e', 's', 't', 'u', 'a', 'i', 'o').mkString //> res2: String = estuaio
  Set('e', 's', 't', 'u', 'a', 'i', 'o').toList.mkString
                                                  //> res3: String = estuaio
  Set('e', 's', 't', 'u', 'a', 'i', 'o').toList.mkString.permutations.toList.size
                                                  //> res4: Int = 5040

 val stringSolutitons = sevenLettersSubsets.filter { sevenLetters =>
  val finalDict = allWords.filter(_.toCharArray.toSet subsetOf sevenLetters )
  val finalDictWithPermutations = finalDict.flatMap(_.permutations)
  sevenLetters.mkString.permutations
  .exists( sevenLetterWord =>
   firstSolution.forall( finalDictWithPermutations contains _.map( sevenLetterWord.toCharArray()(_)).mkString) )
 }                                                //> stringSolutitons  : Iterator[scala.collection.immutable.Set[Char]] = non-em
                                                  //| pty iterator
 
 stringSolutitons.foreach{ sevenLetters =>
  val finalDict = allWords.filter(_.toCharArray.toSet subsetOf sevenLetters )
  val finalDictWithPermutations = finalDict.flatMap(_.permutations)

  val sevenLetterWordsSolutions = sevenLetters.mkString.permutations.filter( sevenLetterWord =>
   firstSolution.forall{ finalDictWithPermutations contains _.map{ sevenLetterWord.toCharArray()(_)}.mkString}
  )
  
  // No point in taking more than 1 permutation, they are the same 3-letter words, just in a different order
  sevenLetterWordsSolutions.take(1).foreach{ sevenLetterWord =>
   println("sevenLetterWord Solutions = ", sevenLetterWord)
   firstSolution.foreach{ setOfInts =>
    val scrambledWord = setOfInts.map( sevenLetterWord.toCharArray()(_)).mkString
    val unscrambledWord = finalDict.find(_.toCharArray.toSet == scrambledWord.toSet)
    println (scrambledWord, unscrambledWord.get)
   }
  }
 }                                                //> (sevenLetterWord Solutions = ,esnyotu)
                                                  //| (eny,yen)
                                                  //| (syt,sty)
                                                  //| (eto,toe)
                                                  //| (ntu,nut)
                                                  //| (esu,sue)
                                                  //| (yuo,you)
                                                  //| (sno,son)
                                                  //| (sevenLetterWord Solutions = ,esnyoht)
                                                  //| (eny,yen)
                                                  //| (syh,shy)
                                                  //| (eho,hoe)
                                                  //| (nth,nth)
                                                  //| (est,set)
                                                  //| (yto,toy)
                                                  //| (sno,son)
                                                  //| (sevenLetterWord Solutions = ,esybtou)
                                                  //| (eyb,bye)
                                                  //| (sbo,sob)
                                                  //| (eto,toe)
                                                  //| (yuo,you)
                                                  //| (esu,sue)
                                                  //| (tub,but)
                                                  //| (syt,sty)
                                                  //| (sevenLetterWord Solutions = ,esythou)
                                                  //| (eyt,tye)
                                                  //| (sto,sot)
                                                  //| (eho,hoe)
                                                  //| (yuo,you)
                                                  //| (esu,sue)
                                                  //| (tuh,hut)
                                                  //| (syh,shy)
                                                  //| (sevenLetterWord Solutions = ,esybhou)
                                                  //| (eyb,bye)
                                                  //| (sbo,sob)
                                                  //| (eho,hoe)
                                                  //| (yuo,you)
                                                  //| (esu,sue)
                                                  //| (ubh,hub)
                                                  //| (syh,shy)
                                                  //| (sevenLetterWord Solutions = ,esydhou)
                                                  //| (eyd,dye)
                                                  //| (sod,sod)
                                                  //| (eho,hoe)
                                                  //| (yuo,you)
                                                  //| (esu,sue)
                                                  //| (uhd,duh)
                                                  //| (syh,shy)
                                                  //| (sevenLetterWord Solutions = ,entoury)
                                                  //| (eto,toe)
                                                  //| (nro,nor)
                                                  //| (eur,rue)
                                                  //| (ytr,try)
                                                  //| (eny,yen)
                                                  //| (yuo,you)
                                                  //| (ntu,nut)
                                                  //| (sevenLetterWord Solutions = ,entahry)
                                                  //| (eta,ate)
                                                  //| (nar,ran)
                                                  //| (ehr,her)
                                                  //| (ytr,try)
                                                  //| (eny,yen)
                                                  //| (yah,hay)
                                                  //| (nth,nth)
                                                  //| (sevenLetterWord Solutions = ,enurfoy)
                                                  //| (eur,rue)
                                                  //| (nro,nor)
                                                  //| (efo,foe)
                                                  //| (yuo,you)
                                                  //| (eny,yen)
                                                  //| (yfr,fry)
                                                  //| (nuf,fun)
                                                  //| (sevenLetterWord Solutions = ,enucroy)
                                                  //| (euc,cue)
                                                  //| (nco,con)
                                                  //| (ero,roe)
                                                  //| (yuo,you)
                                                  //| (eny,yen)
                                                  //| (ycr,cry)
                                                  //| (nur,run)
                                                  //| (sevenLetterWord Solutions = ,enudroy)
                                                  //| (eud,due)
                                                  //| (nod,don)
                                                  //| (ero,roe)
                                                  //| (yuo,you)
                                                  //| (eny,yen)
                                                  //| (yrd,dry)
                                                  //| (nur,run)
                                                  //| (sevenLetterWord Solutions = ,entghuo)
                                                  //| (etg,get)
                                                  //| (nug,gnu)
                                                  //| (euh,hue)
                                                  //| (tuo,out)
                                                  //| (eno,eon)
                                                  //| (gho,hog)
                                                  //| (nth,nth)
                                                  //| (sevenLetterWord Solutions = ,entphuo)
                                                  //| (etp,pet)
                                                  //| (nup,pun)
                                                  //| (euh,hue)
                                                  //| (tuo,out)
                                                  //| (eno,eon)
                                                  //| (pho,hop)
                                                  //| (nth,nth)
                                                  //| (sevenLetterWord Solutions = ,entihop)
                                                  //| (eti,tie)
                                                  //| (nio,ion)
                                                  //| (eho,hoe)
                                                  //| (tpo,opt)
                                                  //| (enp,pen)
                                                  //| (iph,hip)
                                                  //| (nth,nth)
                                                  //| (sevenLetterWord Solutions = ,entihod)
                                                  //| (eti,tie)
                                                  //| (nio,ion)
                                                  //| (eho,hoe)
                                                  //| (tod,dot)
                                                  //| (end,den)
                                                  //| (ihd,hid)
                                                  //| (nth,nth)
                                                  //| (sevenLetterWord Solutions = ,enawcim)
                                                  //| (eaw,awe)
                                                  //| (niw,win)
                                                  //| (eic,ice)
                                                  //| (ami,aim)
                                                  //| (enm,men)
                                                  //| (mcw,cwm)
                                                  //| (nac,can)
                                                  //| (sevenLetterWord Solutions = ,eytorub)
                                                  //| (eto,toe)
                                                  //| (yuo,you)
                                                  //| (eur,rue)
                                                  //| (tub,but)
                                                  //| (eyb,bye)
                                                  //| (bro,orb)
                                                  //| (ytr,try)
                                                  //| (sevenLetterWord Solutions = ,eytohur)
                                                  //| (eto,toe)
                                                  //| (yuo,you)
                                                  //| (euh,hue)
                                                  //| (tur,rut)
                                                  //| (eyr,rye)
                                                  //| (hro,rho)
                                                  //| (yth,thy)
                                                  //| (sevenLetterWord Solutions = ,eyudort)
                                                  //| (eud,due)
                                                  //| (yrd,dry)
                                                  //| (ero,roe)
                                                  //| (tur,rut)
                                                  //| (eyt,tye)
                                                  //| (tod,dot)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyumogb)
                                                  //| (eum,emu)
                                                  //| (ymg,gym)
                                                  //| (ego,ego)
                                                  //| (ubg,bug)
                                                  //| (eyb,bye)
                                                  //| (mbo,mob)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyumogh)
                                                  //| (eum,emu)
                                                  //| (ymg,gym)
                                                  //| (ego,ego)
                                                  //| (ugh,hug)
                                                  //| (eyh,hey)
                                                  //| (mho,mho)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyumogr)
                                                  //| (eum,emu)
                                                  //| (ymg,gym)
                                                  //| (ego,ego)
                                                  //| (ugr,rug)
                                                  //| (eyr,rye)
                                                  //| (mro,rom)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyumogd)
                                                  //| (eum,emu)
                                                  //| (ymg,gym)
                                                  //| (ego,ego)
                                                  //| (ugd,dug)
                                                  //| (eyd,dye)
                                                  //| (mod,mod)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyucorb)
                                                  //| (euc,cue)
                                                  //| (ycr,cry)
                                                  //| (ero,roe)
                                                  //| (ubr,rub)
                                                  //| (eyb,bye)
                                                  //| (bco,cob)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyurodb)
                                                  //| (eur,rue)
                                                  //| (yrd,dry)
                                                  //| (eod,doe)
                                                  //| (ubd,bud)
                                                  //| (eyb,bye)
                                                  //| (bro,orb)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyurodh)
                                                  //| (eur,rue)
                                                  //| (yrd,dry)
                                                  //| (eod,doe)
                                                  //| (uhd,duh)
                                                  //| (eyh,hey)
                                                  //| (hro,rho)
                                                  //| (yuo,you)
                                                  //| (sevenLetterWord Solutions = ,eyacmrw)
                                                  //| (eac,ace)
                                                  //| (ycr,cry)
                                                  //| (emr,rem)
                                                  //| (arw,raw)
                                                  //| (eyw,wye)
                                                  //| (mcw,cwm)
                                                  //| (yam,may)
                                                  //| (sevenLetterWord Solutions = ,etawcim)
                                                  //| (eaw,awe)
                                                  //| (tiw,wit)
                                                  //| (eic,ice)
                                                  //| (ami,aim)
                                                  //| (etm,met)
                                                  //| (mcw,cwm)
                                                  //| (tac,act)
                                                  //| (sevenLetterWord Solutions = ,nytorua)
                                                  //| (nto,not)
                                                  //| (yuo,you)
                                                  //| (nur,run)
                                                  //| (tua,tau)
                                                  //| (nya,any)
                                                  //| (aro,oar)
                                                  //| (ytr,try)
                                                  //| (sevenLetterWord Solutions = ,ytuoabr)
                                                  //| (yuo,you)
                                                  //| (tbo,bot)
                                                  //| (yab,bay)
                                                  //| (ubr,rub)
                                                  //| (ytr,try)
                                                  //| (aro,oar)
                                                  //| (tua,tau)
                                                  //| (sevenLetterWord Solutions = ,ytuoagr)
                                                  //| (yuo,you)
                                                  //| (tgo,got)
                                                  //| (yag,gay)
                                                  //| (ugr,rug)
                                                  //| (ytr,try)
                                                  //| (aro,oar)
                                                  //| (tua,tau)
                                                  //| (sevenLetterWord Solutions = ,ytuomgh)
                                                  //| (yuo,you)
                                                  //| (tgo,got)
                                                  //| (ymg,gym)
                                                  //| (ugh,hug)
                                                  //| (yth,thy)
                                                  //| (mho,mho)
                                                  //| (tum,mut)
                                                  //| (sevenLetterWord Solutions = ,ytuomgr)
                                                  //| (yuo,you)
                                                  //| (tgo,got)
                                                  //| (ymg,gym)
                                                  //| (ugr,rug)
                                                  //| (ytr,try)
                                                  //| (mro,rom)
                                                  //| (tum,mut)
                                                  //| (sevenLetterWord Solutions = ,ytuopgh)
                                                  //| (yuo,you)
                                                  //| (tgo,got)
                                                  //| (ygp,gyp)
                                                  //| (ugh,hug)
                                                  //| (yth,thy)
                                                  //| (pho,hop)
                                                  //| (tup,put)
                                                  //| (sevenLetterWord Solutions = ,ytuopgr)
                                                  //| (yuo,you)
                                                  //| (tgo,got)
                                                  //| (ygp,gyp)
                                                  //| (ugr,rug)
                                                  //| (ytr,try)
                                                  //| (pro,pro)
                                                  //| (tup,put)
                                                  //| (sevenLetterWord Solutions = ,ytuordh)
                                                  //| (yuo,you)
                                                  //| (tod,dot)
                                                  //| (yrd,dry)
                                                  //| (uhd,duh)
                                                  //| (yth,thy)
                                                  //| (hro,rho)
                                                  //| (tur,rut)
                                                  //| (sevenLetterWord Solutions = ,yumgdro)
                                                  //| (ymg,gym)
                                                  //| (ugr,rug)
                                                  //| (yrd,dry)
                                                  //| (mro,rom)
                                                  //| (yuo,you)
                                                  //| (god,dog)
                                                  //| (umd,mud)
                                                  //| (sevenLetterWord Solutions = ,yugprco)
                                                  //| (ygp,gyp)
                                                  //| (upc,cup)
                                                  //| (ycr,cry)
                                                  //| (gco,cog)
                                                  //| (yuo,you)
                                                  //| (pro,pro)
                                                  //| (ugr,rug)-


 /*val etaPerms = "etaoirs".permutations.toList
 val solPerms = etaPerms.filter( etaPerm =>
  firstSolution.forall( numericalIndexes => finalDictWithPermutations contains numericalIndexes.map( numericalIndex => etaPerm.toCharArray()(numericalIndex)).mkString
 )
)
 
 
 solPerms.foreach(println)
 
 Set(0, 2, 3).map( numericalIndex => "etaoins".toCharArray()(numericalIndex))
 Set(0, 1, 2).map( numericalIndex => "etaoins".toCharArray()(numericalIndex)).mkString
 firstSolution.map( numericalIndexes => numericalIndexes.map ( numericalIndex => "etaoins".toCharArray()(numericalIndex)).mkS tring)
 firstSolution.forall( numericalIndexes => finalDictWithPermutations contains numericalIndexes.map ( numericalIndex => "etaoins".toCharArray()(numericalIndex)).mkString)
 firstSolution.map( numericalIndexes => finalDictWithPermutations contains numericalIndexes.map ( numericalIndex => "etaoins".toCharArray()(numericalIndex)).mkString)

 //finalDictWithPermutations contains Set(0, 1, 2).map( numericalIndex => "etaoins".toCharArray()(numericalIndex)).mkString
 */
}