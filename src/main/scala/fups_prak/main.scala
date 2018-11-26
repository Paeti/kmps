import scala.io.Source

import fups_prak.prak_2._
import fups_prak.prak_3._
import fups_prak.classes._

object show_res {

def main(args: Array[String]): Unit = {
    val file_content =
      Source.fromFile("alben.xml").mkString.toCharArray.toList
    val albenListe = createTokenList(file_content)
    val objectListe = parseFile(albenListe)

    //Aufgabe 1

    val res1 = map[Album](x => x.copy(title = x.title.toUpperCase()), objectListe)

    val res2 = map[Album](x => {val a = x.copy(title = x.title.toUpperCase());
      a.copy(tracks = map[Track](x => x.copy(x.title.toUpperCase()), a.tracks))} , objectListe)

    val res3 = poly_map[Album, List[String]](x => poly_map[Track, String](y => y.length,
      x.tracks), objectListe)

    //Aufgabe 2

    val res4 =
      poly_map[Album, List[Track]](x => filter[Track](y => y.rating >= 4, x.tracks),
                                   objectListe)
    val res5 = poly_map[Album, List[String]](x => poly_map[Track,
                String](y => y.title,
                filter[Track](z => filter[String](a => a == "Rod Temperton", z.writers)
                != List[String](), x.tracks)), objectListe)

    //Aufgabe 3

    val res6 =
      poly_map[Album, List[List[String]]](x => partition[String](y => y == "Thriller",
            poly_map[Track, String](z => z.title, x.tracks)), objectListe)

    val res7 = filter[String](x => x.isEmpty == false,
                              poly_map[List[Char], String](y => y.mkString,
                              partition[Char](z => z == '<' | z == '>',
                              filter[Char](a => a != '\n' & a != '\r' & a != '\t',
                                                           file_content))))

    //Aufgabe 4

    val res8 = templateMethod(x => x*x, (a, b) => a + b, 0, 1, 3)

    val res9 = foldr[Int]((x, y) => x*y)(1)(map[Int](x => x*x, range(1,3)))

    //println(res1)
    //println(res2)
    //println(res3)
    //println(res4)
    //println(res5)
    //println(res6)
    //println(res7)
    //println(res8)
    println(res9)
  }
}
