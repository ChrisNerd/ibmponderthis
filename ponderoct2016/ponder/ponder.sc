import scala.io.Source
object ponder {

  val piFile = Source.fromFile("/home/shannon/outside/ponder/ponderoct2016/Pi - 10 Million Digits @ http___Pi.Karmona.com.html").getLines.mkString.filter(_.isDigit)
                                                  //> piFile  : String = 314159265358979323846264338327950288419716939937510582097
                                                  //| 4944592307816406286208998628034825342117067982148086513282306647093844609550
                                                  //| 5822317253594081284811174502841027019385211055596446229489549303819644288109
                                                  //| 7566593344612847564823378678316527120190914564856692346034861045432664821339
                                                  //| 3607260249141273724587006606315588174881520920962829254091715364367892590360
                                                  //| 0113305305488204665213841469519415116094330572703657595919530921861173819326
                                                  //| 1179310511854807446237996274956735188575272489122793818301194912983367336244
                                                  //| 0656643086021394946395224737190702179860943702770539217176293176752384674818
                                                  //| 4676694051320005681271452635608277857713427577896091736371787214684409012249
                                                  //| 5343014654958537105079227968925892354201995611212902196086403441815981362977
                                                  //| 4771309960518707211349999998372978049951059731732816096318595024459455346908
                                                  //| 3026425223082533446850352619311881710100031378387528865875332083814206171776
                                                  //| 691473035982534904287554
                                                  //| Output exceeds cutoff limit.
  val eFile = Source.fromFile("/home/shannon/outside/ponder/ponderoct2016/e.txt").getLines.mkString.filter(_.isDigit)
                                                  //> eFile  : String = 2718281828459045235360287471352662497757247093699959574966
                                                  //| 9676277240766303535475945713821785251664274274663919320030599218174135966290
                                                  //| 4357290033429526059563073813232862794349076323382988075319525101901157383418
                                                  //| 7930702154089149934884167509244761460668082264800168477411853742345442437107
                                                  //| 5390777449920695517027618386062613313845830007520449338265602976067371132007
                                                  //| 0932870912744374704723069697720931014169283681902551510865746377211125238978
                                                  //| 4425056953696770785449969967946864454905987931636889230098793127736178215424
                                                  //| 9992295763514822082698951936680331825288693984964651058209392398294887933203
                                                  //| 6250944311730123819706841614039701983767932068328237646480429531180232878250
                                                  //| 9819455815301756717361332069811250996181881593041690351598888519345807273866
                                                  //| 7385894228792284998920868058257492796104841984443634632449684875602336248270
                                                  //| 4197862320900216099023530436994184914631409343173814364054625315209618369088
                                                  //| 870701676839642437814059
                                                  //| Output exceeds cutoff limit.
  val root2File = Source.fromFile("/home/shannon/outside/ponder/ponderoct2016/sqrt2.10mil.txt").getLines.mkString.filter(_.isDigit)
                                                  //> root2File  : String = 141421356237309504880168872420969807856967187537694807
                                                  //| 3176679737990732478462107038850387534327641572735013846230912297024924836055
                                                  //| 8507372126441214970999358314132226659275055927557999505011527820605714701095
                                                  //| 5997160597027453459686201472851741864088919860955232923048430871432145083976
                                                  //| 2603627995251407989687253396546331808829640620615258352395054745750287759961
                                                  //| 7298355752203375318570113543746034084988471603868999706990048150305440277903
                                                  //| 1645424782306849293691862158057846311159666871301301561856898723723528850926
                                                  //| 4861249497715421833420428568606014682472077143585487415565706967765372022648
                                                  //| 5447015858801620758474922657226002085584466521458398893944370926591800311388
                                                  //| 2464681570826301005948587040031864803421948972782906410450726368813137398552
                                                  //| 5611732204024509122770022694112757362728049573810896750401836986836845072579
                                                  //| 9364729060762996941380475654823728997180326802474420629269124859052181004459
                                                  //| 842150591120249441341728
                                                  //| Output exceeds cutoff limit.

  // create a map m of all 5 digit numbers to their first occurance
  val pis = collection.mutable.Map[String, Int]() //> pis  : scala.collection.mutable.Map[String,Int] = Map()
  val es = collection.mutable.Map[String, Int]()  //> es  : scala.collection.mutable.Map[String,Int] = Map()
  val root2s = collection.mutable.Map[String, Int]()
                                                  //> root2s  : scala.collection.mutable.Map[String,Int] = Map()

  val lengthToFind = 5                            //> lengthToFind  : Int = 5

  piFile.sliding(lengthToFind).zipWithIndex.foreach { x => pis getOrElseUpdate (x._1, x._2 + lengthToFind) }
  eFile.sliding(lengthToFind).zipWithIndex.foreach { x => es getOrElseUpdate (x._1, x._2 + lengthToFind) }
  root2File.sliding(lengthToFind).zipWithIndex.foreach { x => root2s getOrElseUpdate (x._1, x._2 + lengthToFind) }

  val combined = collection.mutable.Map[String, Int]()
                                                  //> combined  : scala.collection.mutable.Map[String,Int] = Map()
  for (nInt <- 0 until math.pow(10, lengthToFind).toInt) {
    val n = nInt.toString.reverse.padTo(lengthToFind, '0').reverse.toString
    if ((pis contains n) &&
      (es contains n) &&
      (root2s contains n))
      combined(n) = pis(n) + es(n) + root2s(n)
    else {
      println(n)
      println(pis contains n)
      println(es contains n)
      println(root2s contains n)
    }
  }

  combined.size                                   //> res0: Int = 100000

  //pis("23")
  //es("23")
  //root2s("23")
  //combined("23")
  combined.maxBy(_._2)                            //> res1: (String, Int) = (40346,1755463)
  val minKey = combined.minBy(_._2)               //> minKey  : (String, Int) = (46652,2166)
  
  combined.filter(_._2==2166)                     //> res2: scala.collection.mutable.Map[String,Int] = Map(46652 -> 2166)
  combined.toSeq.sortBy(_._2)
  pis(minKey._1)
  es(minKey._1)
  root2s(minKey._1)
  //   pis("35")
  // es("35")
  //root2s("35")
  //combined("35")
}