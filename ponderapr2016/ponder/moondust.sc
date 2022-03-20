
object moondust {
 val moondust = "moondust"                        //> moondust  : String = moondust
 val com3 = (moondust combinations 3 ).toList     //> com3  : List[String] = List(moo, mon, mod, mou, mos, mot, mnd, mnu, mns, mnt,
                                                  //|  mdu, mds, mdt, mus, mut, mst, oon, ood, oou, oos, oot, ond, onu, ons, ont, o
                                                  //| du, ods, odt, ous, out, ost, ndu, nds, ndt, nus, nut, nst, dus, dut, dst, ust
                                                  //| )
 com3.length                                      //> res0: Int = 41
val per3 = com3 flatMap (_ permutations)          //> per3  : List[String] = List(moo, omo, oom, mon, mno, omn, onm, nmo, nom, mod
                                                  //| , mdo, omd, odm, dmo, dom, mou, muo, omu, oum, umo, uom, mos, mso, oms, osm,
                                                  //|  smo, som, mot, mto, omt, otm, tmo, tom, mnd, mdn, nmd, ndm, dmn, dnm, mnu, 
                                                  //| mun, nmu, num, umn, unm, mns, msn, nms, nsm, smn, snm, mnt, mtn, nmt, ntm, t
                                                  //| mn, tnm, mdu, mud, dmu, dum, umd, udm, mds, msd, dms, dsm, smd, sdm, mdt, mt
                                                  //| d, dmt, dtm, tmd, tdm, mus, msu, ums, usm, smu, sum, mut, mtu, umt, utm, tmu
                                                  //| , tum, mst, mts, smt, stm, tms, tsm, oon, ono, noo, ood, odo, doo, oou, ouo,
                                                  //|  uoo, oos, oso, soo, oot, oto, too, ond, odn, nod, ndo, don, dno, onu, oun, 
                                                  //| nou, nuo, uon, uno, ons, osn, nos, nso, son, sno, ont, otn, not, nto, ton, t
                                                  //| no, odu, oud, dou, duo, uod, udo, ods, osd, dos, dso, sod, sdo, odt, otd, do
                                                  //| t, dto, tod, tdo, ous, osu, uos, uso, sou, suo, out, otu, uot, uto, tou, tuo
                                                  //| , ost, ots, sot, sto, tos, tso, ndu, nud, dnu, dun, und, udn, nds, nsd, dns,
                                                  //|  dsn, snd, sdn, ndt, ntd, dnt, dtn, tnd, tdn, nus, nsu, uns, usn, snu, sun, 
                                                  //| nut, ntu, unt, utn, tnu, tun, nst, nts, snt, stn, tns, tsn, dus, dsu, uds, u
                                                  //| sd, sdu, sud, dut, dtu, udt, utd, tdu, tud, dst, dts, sdt, std, tds, tsd, us
                                                  //| t, uts, sut, stu, tus, tsu)
 per3.length                                      //> res1: Int = 228
per3 foreach println                              //> moo
                                                  //| omo
                                                  //| oom
                                                  //| mon
                                                  //| mno
                                                  //| omn
                                                  //| onm
                                                  //| nmo
                                                  //| nom
                                                  //| mod
                                                  //| mdo
                                                  //| omd
                                                  //| odm
                                                  //| dmo
                                                  //| dom
                                                  //| mou
                                                  //| muo
                                                  //| omu
                                                  //| oum
                                                  //| umo
                                                  //| uom
                                                  //| mos
                                                  //| mso
                                                  //| oms
                                                  //| osm
                                                  //| smo
                                                  //| som
                                                  //| mot
                                                  //| mto
                                                  //| omt
                                                  //| otm
                                                  //| tmo
                                                  //| tom
                                                  //| mnd
                                                  //| mdn
                                                  //| nmd
                                                  //| ndm
                                                  //| dmn
                                                  //| dnm
                                                  //| mnu
                                                  //| mun
                                                  //| nmu
                                                  //| num
                                                  //| umn
                                                  //| unm
                                                  //| mns
                                                  //| msn
                                                  //| nms
                                                  //| nsm
                                                  //| smn
                                                  //| snm
                                                  //| mnt
                                                  //| mtn
                                                  //| nmt
                                                  //| ntm
                                                  //| tmn
                                                  //| tnm
                                                  //| mdu
                                                  //| mud
                                                  //| dmu
                                                  //| dum
                                                  //| umd
                                                  //| udm
                                                  //| mds
                                                  //| msd
                                                  //| dms
                                                  //| dsm
                                                  //| smd
                                                  //| sdm
                                                  //| mdt
                                                  //| mtd
                                                  //| dmt
                                                  //| dtm
                                                  //| tmd
                                                  //| tdm
                                                  //| mus
                                                  //| msu
                                                  //| ums
                                                  //| usm
                                                  //| smu
                                                  //| sum
                                                  //| mut
                                                  //| mtu
                                                  //| umt
                                                  //| utm
                                                  //| tmu
                                                  //| tum
                                                  //| mst
                                                  //| mts
                                                  //| smt
                                                  //| stm
                                                  //| tms
                                                  //| tsm
                                                  //| oon
                                                  //| ono
                                                  //| noo
                                                  //| ood
                                                  //| odo
                                                  //| doo
                                                  //| oou
                                                  //| ouo
                                                  //| uoo
                                                  //| oos
                                                  //| oso
                                                  //| soo
                                                  //| oot
                                                  //| oto
                                                  //| too
                                                  //| ond
                                                  //| odn
                                                  //| nod
                                                  //| ndo
                                                  //| don
                                                  //| dno
                                                  //| onu
                                                  //| oun
                                                  //| nou
                                                  //| nuo
                                                  //| uon
                                                  //| uno
                                                  //| ons
                                                  //| osn
                                                  //| nos
                                                  //| nso
                                                  //| son
                                                  //| sno
                                                  //| ont
                                                  //| otn
                                                  //| not
                                                  //| nto
                                                  //| ton
                                                  //| tno
                                                  //| odu
                                                  //| oud
                                                  //| dou
                                                  //| duo
                                                  //| uod
                                                  //| udo
                                                  //| ods
                                                  //| osd
                                                  //| dos
                                                  //| dso
                                                  //| sod
                                                  //| sdo
                                                  //| odt
                                                  //| otd
                                                  //| dot
                                                  //| dto
                                                  //| tod
                                                  //| tdo
                                                  //| ous
                                                  //| osu
                                                  //| uos
                                                  //| uso
                                                  //| sou
                                                  //| suo
                                                  //| out
                                                  //| otu
                                                  //| uot
                                                  //| uto
                                                  //| tou
                                                  //| tuo
                                                  //| ost
                                                  //| ots
                                                  //| sot
                                                  //| sto
                                                  //| tos
                                                  //| tso
                                                  //| ndu
                                                  //| nud
                                                  //| dnu
                                                  //| dun
                                                  //| und
                                                  //| udn
                                                  //| nds
                                                  //| nsd
                                                  //| dns
                                                  //| dsn
                                                  //| snd
                                                  //| sdn
                                                  //| ndt
                                                  //| ntd
                                                  //| dnt
                                                  //| dtn
                                                  //| tnd
                                                  //| tdn
                                                  //| nus
                                                  //| nsu
                                                  //| uns
                                                  //| usn
                                                  //| snu
                                                  //| sun
                                                  //| nut
                                                  //| ntu
                                                  //| unt
                                                  //| utn
                                                  //| tnu
                                                  //| tun
                                                  //| nst
                                                  //| nts
                                                  //| snt
                                                  //| stn
                                                  //| tns
                                                  //| tsn
                                                  //| dus
                                                  //| dsu
                                                  //| uds
                                                  //| usd
                                                  //| sdu
                                                  //| sud
                                                  //| dut
                                                  //| dtu
                                                  //| udt
                                                  //| utd
                                                  //| tdu
                                                  //| tud
                                                  //| dst
                                                  //| dts
                                                  //| sdt
                                                  //| std
                                                  //| tds
                                                  //| tsd
                                                  //| ust
                                                  //| uts
                                                  //| sut
                                                  //| stu
                                                  //| tus
                                                  //| tsu

}