//> using file Helper.scala
import util.chaining.scalaUtilChainingOps
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

type MappingName =
  "seed-to-soil map:" | "soil-to-fertilizer map:" | "fertilizer-to-water map:" | "water-to-light map:" | "light-to-temperature map:" |
    "temperature-to-humidity map:" | "humidity-to-location map:"

val mappingNameRegex =
  """(seed|soil|fertilizer|water|light|temperature|humidity)-to-(seed|soil|fertilizer|water|light|temperature|humidity|location) map:""".r

case class Space(domain: BigInt, coDomain: BigInt, length: BigInt):
  def domainRangeContains(x: BigInt): Boolean = (domain <= x) && (x <= domain + length)
  def mapValue(domainValue: BigInt): Option[BigInt] =
    if (domainRangeContains(domainValue)) Some(coDomain + (domainValue - domain)) else None

object Space:
  private def unapply(lsI: List[BigInt]): Some[Space] = Some(Space(lsI(1), lsI(0), lsI(2)))
  def apply(string: String): Space = {
    val Space(ms) = string.split(" ").map(BigInt(_)).toList
    ms
  }

case class Mapping(name: MappingName, mappings: Seq[Space]):
  def get(input: BigInt): BigInt =
    mappings
      .find(_.domainRangeContains(input))
      .flatMap(_.mapValue(input))
      .getOrElse(input)

def parseMapping(input: String)(targetMapping: MappingName) =
  scala.io.Source
    .fromString(input)
    .getLines()
    .dropWhile(!_.contains(targetMapping))
    .drop(1)
    .filterNot(_.isBlank())
    .takeWhile(x => mappingNameRegex.findFirstIn(x).isEmpty)
    .map(Space.apply)
    .toList
    .pipe(Mapping(targetMapping, _))

def parseSeedNums1(input: String): Seq[BigInt] =
  scala.io.Source
    .fromString(input)
    .getLines()
    .take(1)
    .collect(x =>
      """seeds: (.+)""".r
        .findAllIn(x)
        .group(1)
        .split(" ")
        .filterNot(_.isBlank)
        .map(BigInt(_))
    )
    .flatten
    .toList

def run1(input: String) = {
  val seeds = parseSeedNums1(input)

  parseMapping(input)
    .pipe { parser =>
      (parser("seed-to-soil map:").get andThen
        parser("soil-to-fertilizer map:").get andThen
        parser("fertilizer-to-water map:").get andThen
        parser("water-to-light map:").get andThen
        parser("light-to-temperature map:").get andThen
        parser("temperature-to-humidity map:").get andThen
        parser("humidity-to-location map:").get)
    }
    .pipe(seeds.map)
    .min
}

case class SeedRange(start: BigInt, length: BigInt)
def parseSeedNums2(input: String): Seq[SeedRange] =
  scala.io.Source
    .fromString(input)
    .getLines()
    .take(1)
    .flatMap(x =>
      """seeds: (.+)""".r
        .findAllIn(x)
        .group(1)
        .split(" ")
        .filterNot(_.isBlank)
        .sliding(2, 2)
        .map(x => SeedRange(BigInt(x(0)), BigInt(x(1))))
    )
    .toList
  // .tap(x => println(s"[DEBUG ps2] $x"))

def run2(input: String) = {
  val seeds = parseSeedNums2(input)

  val composedMappings = parseMapping(input)
    .pipe { parser =>
      (parser("seed-to-soil map:").get andThen
        parser("soil-to-fertilizer map:").get andThen
        parser("fertilizer-to-water map:").get andThen
        parser("water-to-light map:").get andThen
        parser("light-to-temperature map:").get andThen
        parser("temperature-to-humidity map:").get andThen
        parser("humidity-to-location map:").get)
    }

  val seedLocationF: Seq[Future[BigInt]] = seeds
    .map { seed =>
      Future {
        var res = BigInt("9999999999")
        var nxt = seed.start

        while (nxt < (seed.start + seed.length - 1)) {
          val intermed = composedMappings(nxt)
          if (intermed < res) {
            res = intermed
          }

          nxt = nxt + 1
        }

        res
      }
    }

  Await
    .result(
      Future.sequence(seedLocationF),
      (5.minute)
    )
    .min
}

//>>>>>>>>>>>
// TESTING
//>>>>>>>>>>>

// run(parseMappingSeedToSoil(_)("seed-to-soil map:"))(sample)

// test(parseMapping(sample)("seed-to-soil map:"), Mapping("seed-to-soil map:", List(Space(98, 50, 2), Space(50, 52, 48))))

// test(
//   parseMapping(sample)("soil-to-fertilizer map:"),
//   Mapping("soil-to-fertilizer map:", List(Space(15, 0, 37), Space(52, 37, 2), Space(0, 39, 15)))
// )

// test.ignore(
//   run1(sample),
//   List(82, 43, 86, 35)
// )
// run(run1)(puzzle)

test(run1(puzzle), BigInt("313045984"))

test(
  run2(sample),
  BigInt("46")
)

val result = run2(puzzle)
test.where(result, _ < BigInt("20283861"))
println(s"[RUN] $result")

def sample: String =
  """seeds: 79 14 55 13
    |
    |seed-to-soil map:
    |50 98 2
    |52 50 48
    |
    |soil-to-fertilizer map:
    |0 15 37
    |37 52 2
    |39 0 15
    |
    |fertilizer-to-water map:
    |49 53 8
    |0 11 42
    |42 0 7
    |57 7 4
    |
    |water-to-light map:
    |88 18 7
    |18 25 70
    |
    |light-to-temperature map:
    |45 77 23
    |81 45 19
    |68 64 13
    |
    |temperature-to-humidity map:
    |0 69 1
    |1 0 69
    |
    |humidity-to-location map:
    |60 56 37
    |56 93 4""".stripMargin

/*
"""seed  soil
  |0     0
  |1     1
  |...   ...
  |48    48
  |49    49
  |50    52
  |51    53
  |...   ...
  |96    98
  |97    99
  |98    50
  |99    51""".stripMargin
 */
def puzzle: String =
  """seeds: 2019933646 2719986 2982244904 337763798 445440 255553492 1676917594 196488200 3863266382 36104375 1385433279 178385087 2169075746 171590090 572674563 5944769 835041333 194256900 664827176 42427020
  |
  |seed-to-soil map:
  |3566547172 3725495029 569472267
  |2346761246 1249510998 267846697
  |1812605508 937956667 271194541
  |1421378697 1209151208 40359790
  |2083800049 2788751092 262961197
  |2938601691 473979048 463977619
  |473979048 1517357695 947399649
  |4136019439 3566547172 158947857
  |1461738487 3051712289 350867021
  |2614607943 2464757344 323993748
  |
  |soil-to-fertilizer map:
  |3107230831 2583931429 576709409
  |970181981 608291332 1441137369
  |743954495 3859046283 158951815
  |3683940240 3227916509 91282070
  |608291332 2448268266 135663163
  |3775222310 2049428701 398839565
  |2411319350 3319198579 539847704
  |2951167054 4017998098 156063777
  |902906310 3160640838 67275671
  |
  |fertilizer-to-water map:
  |1257642402 395703749 69589612
  |1800674 2215701547 90550534
  |2757853693 358464863 37238886
  |3285451399 181079109 43937782
  |2346544130 3513448371 192150886
  |3866348216 4231433060 63534236
  |1327232014 1560332334 90281838
  |2538695016 616206288 114467702
  |255018176 225016891 46372244
  |1171065990 3705599257 27021880
  |1070753744 730673990 442780
  |221369008 3479799203 33649168
  |2987721226 271389135 80072982
  |1198087870 732917444 24556356
  |199036270 2306252081 22332738
  |0 731116770 1800674
  |3929882452 3989920675 212758268
  |631506549 757473800 322942578
  |301390420 0 157952443
  |2795092579 157952443 1997721
  |1222644226 2619085211 34998176
  |954449127 499901671 116304617
  |1429766246 159950164 21128945
  |2205492221 2074649638 141051909
  |2749302577 3732621137 8551116
  |459342863 1219863414 57737723
  |3329389181 3741172253 59047790
  |2797090300 2328584819 190630926
  |3278448653 351462117 7002746
  |126959518 1277601137 72076752
  |92351208 465293361 34608310
  |4142640720 3866348216 123572459
  |2143980560 1080416378 43307177
  |1450895191 2672287871 693085369
  |517080586 3365373240 114425963
  |3388436971 1662866566 411783072
  |2187287737 2654083387 18204484
  |1417513852 1650614172 12252394
  |3067794208 1349677889 210654445
  |4266213179 4202678943 28754117
  |2653162718 1123723555 96139859
  |1071196524 2519215745 99869466
  |
  |water-to-light map:
  |512627839 90187036 1196629
  |3379634653 2059506154 33434334
  |3286651054 4276482087 18485209
  |4233695090 28914830 61272206
  |3413068987 3322576776 23288997
  |3736304424 3345865773 43267308
  |1246285471 2994853001 251748584
  |3779571732 1946298040 113208114
  |390808412 3287769466 34807310
  |1881283842 2879009693 106527924
  |3964031050 2506138169 12994476
  |3436357984 793897944 162691614
  |2255160753 2092940488 151061610
  |853985057 3506201042 119010035
  |301385394 1856875022 89423018
  |972995092 658665705 34308693
  |4159948022 1315925500 65322692
  |640912738 250463411 213072319
  |1761800914 91383665 102591221
  |450345319 3246601585 5793995
  |3186220306 4173678310 91115364
  |28914830 3633635453 176360375
  |456139314 193974886 56488525
  |2523290324 3809995828 187303152
  |2406222363 3389133081 117067961
  |205275205 2782899504 96110189
  |2135785589 1100569535 119375164
  |1121466033 533846267 124819438
  |1007303785 2244002098 114162248
  |3599049598 3997298980 137254826
  |4077949072 463535730 70310537
  |4225270714 3625211077 8424376
  |1498034055 2519132645 263766859
  |2710593476 1381248192 475626830
  |3977025526 692974398 100923546
  |4148259609 4264793674 11688413
  |1987811766 2358164346 147973823
  |3892779846 1244674296 71251204
  |3340510149 4134553806 39124504
  |1864392135 956589558 16891707
  |425615722 1219944699 24729597
  |513824468 973481265 127088270
  |3277335670 2985537617 9315384
  |3305136263 3252395580 35373886
  |
  |light-to-temperature map:
  |1094191559 698410082 28110394
  |383870732 1189042355 107231661
  |3711052230 2164474756 34756304
  |745558539 170241759 7170863
  |491102393 503970250 194439832
  |4034618875 3142749029 146609939
  |3781998432 1718948669 129329785
  |2440091414 3071819711 70929318
  |1301358031 55123603 115118156
  |0 2789116652 87933685
  |770729148 177412622 48955790
  |3772681560 3886204605 9316872
  |752729402 37123857 17999746
  |3745808534 2137385460 7147939
  |2028807236 3677936618 208267987
  |2237075223 3289358968 92979022
  |88764920 1960439220 176946240
  |3568470355 2258695303 142581875
  |3276170082 1848278454 112160766
  |2637902204 1129503077 39814191
  |3000547589 892603630 188042422
  |2511020732 226368412 126881472
  |1122301953 1296274016 52818372
  |1440958847 1353023078 243104929
  |2963423732 0 37123857
  |3388330848 2199231060 48304954
  |1175120325 377732544 126237706
  |819684938 1349092388 3930690
  |3752956473 1169317268 19725087
  |3911328217 2144533399 19941357
  |1416476187 353249884 24482660
  |2677716395 3895521477 285707337
  |265711160 2413138935 118159572
  |685542225 1080646052 48857025
  |3556608598 2401277178 11861757
  |734399250 2247536014 11159289
  |87933685 3677105383 831235
  |3188590011 1596128007 87580071
  |836373414 2531298507 257818145
  |3471876393 2877050337 84732205
  |1684063776 726520476 166083154
  |823615628 3560998296 12757786
  |3436635802 1683708078 35240591
  |3931269574 3573756082 103349301
  |1850146930 3382337990 178660306
  |2330054245 2961782542 110037169
  |
  |temperature-to-humidity map:
  |1773059646 4122818507 172148789
  |2417158855 2859734866 110076859
  |977168274 1576624124 28149321
  |4275291678 3797606290 19675618
  |1141296808 749646180 267286171
  |3592756112 2969811725 273274339
  |0 19621130 7167651
  |2059084943 2697725300 48133058
  |2107218001 3920609496 145140777
  |1453481278 1152911564 151292167
  |1408582979 1465584228 44898299
  |7167651 0 19621130
  |2907567891 1829621431 240604380
  |2252358778 3652347291 145258999
  |1005317595 1016932351 135979213
  |1945208435 2745858358 113876508
  |2397617777 4065750273 16506015
  |3251499859 1776094709 53526722
  |2867005672 4082256288 40562219
  |26788781 1304203731 161380497
  |3305026581 2409995769 287729531
  |3866030451 3243086064 409261227
  |2414123792 1773059646 3035063
  |911026677 1510482527 66141597
  |3148172271 3817281908 103327588
  |2527235714 2070225811 339769958
  |188169278 26788781 722857399
  |
  |humidity-to-location map:
  |3907319746 3137303541 31421983
  |3085093695 1018495475 286155292
  |2898003508 2491485887 87665522
  |2546787368 2901838353 7997221
  |3835317650 2829836257 72002096
  |2554784589 3509894030 133012322
  |3487595595 3719561871 104747874
  |3714670750 2667334372 120646900
  |975094571 2909835574 227467967
  |2985669030 3864000834 99424665
  |3672962118 2449777255 41708632
  |3631107133 2787981272 41854985
  |3938741729 3963425499 15057061
  |3447904506 3824309745 39691089
  |1824175159 1304650767 641793976
  |242892183 0 6504921
  |3371248987 3642906352 76655519
  |1698833898 2258930589 81940357
  |0 6504921 242892183
  |2465969135 3978482560 80818233
  |3592343469 4256203632 38763664
  |3953798790 3168725524 341168506
  |2775979874 4134179998 122023634
  |1780774255 975094571 43400904
  |1311468847 1946444743 312485846
  |2687796911 2579151409 88182963
  |1202562538 2340870946 108906309
  |1623954693 4059300793 74879205""".stripMargin
