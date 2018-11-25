import scala.io.Source

case class Track(title: String, length: String, rating: Int,
                 features: List[String], writers: List[String])
case class Album(title: String, date: String, artist: String,
                 tracks: List[Track])

object SelfFunktional_3 {

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
      poly_map[Album, List[Track]](x => filter[Track](y => y.rating > 4, x.tracks),
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

    val test1 = templateMethod(x => x*x, (a, b) => a + b, 1, 3)

    println(test1)

    //println(res1)
    //println(res2)
    //println(res3)
    //println(res4)
    //println(res5)
    //println(res6)
    //println(res7)
  }


  def createTokenList(fcontent_left: List[Char]): List[String] = fcontent_left match{
    case Nil =>  List[String]()
    case _ => val x = createTokenList("", List[String](), fcontent_left);
      filterList(x, List[String]())
  }


  def filterList(unfil_list: List[String], filt_list: List[String]): List[String]
    = unfil_list match{
      case Nil => filt_list
      case x::xs => x match{
          case  "" => filterList(xs, filt_list)
          case _ => filterList(xs, filt_list :+ x)
      }
  }


  def createTokenList(Token: String, fc_string: List[String], fcontent_left: List[Char]):
      List[String] = fcontent_left match{
    case Nil => fc_string
      case y::ys => y match{
        case '\n' | '\r' | '\t' => createTokenList(Token, fc_string, ys)
        case '<' => createTokenList("", fc_string :+ Token, ys)
        case '>' => ys match{
          case Nil => fc_string :+ Token
          case '<'::zs => createTokenList("", fc_string :+ Token, zs)
          case _::zs => createTokenList("", fc_string :+ Token, ys)
      }
      case _ => createTokenList(Token + y, fc_string, ys)
    }
  }


  def parseFile(content: List[String]): List[Album] = {
    parseFile(content, List[Album]())
  }


  def parseFile(content: List[String], alben: List[Album]): List[Album] = content match{
    case Nil => alben
    case "album"::ys => val (content_left, album) =
      parseAlbum(content,Album("", "", "", List[Track]()));
      parseFile(content_left, alben :+ album)
    case _ => alben
  }


  def parseAlbum(content: List[String], album: Album) : (List[String], Album)
    = content match {
      case Nil => (Nil, album)
        case "/album"::xs => (xs, album)
      case "track"::xs => val (content_left, track) =
        parseTrack(xs, Track("", "", 0, List[String](), List[String]()));
        parseAlbum(content_left, album.copy(tracks = album.tracks :+ track))
      case "artist"::artist::_::xs => parseAlbum(xs, album.copy(artist = artist))
      case "title"::title::_::xs => parseAlbum(xs, album.copy(title = title))
      case "date"::date::_::xs => parseAlbum(xs, album.copy(date = date))
      case _::xs => parseAlbum(xs, album)
    }


  def parseTrack(content: List[String], track: Track): (List[String], Track)
    = content match {
      case Nil => (Nil, track)
        case "/track"::xs => (xs, track)
        case "title"::title::xs => parseTrack(xs.tail, track.copy(title = title))
        case "length"::length::xs => parseTrack(xs.tail, track.copy(length = length))
        case "rating"::rating::xs => parseTrack(xs.tail, track.copy(rating = rating.toInt))
      case "feature"::feature::xs =>
        parseTrack(xs.tail, track.copy(features = track.features :+ feature))
      case "writing"::writer::xs =>
        parseTrack(xs.tail, track.copy(writers = track.writers :+ writer))
      case _::xs => parseTrack(xs, track)
    }

  def map[A](func: A => A, input_list: List[A]): List[A] = input_list match{
    case Nil => List[A]()
    case x::xs => func(x) :: map[A](func, xs)
  }

  def albUp(input: Album): Album = input.copy(title = input.title.toUpperCase())

  def albtitUp(input: Album): Album = {
    val a = input.copy(title = input.title.toUpperCase())
    a.copy(tracks = map[Track](x =>x.copy(x.title.toUpperCase()), a.tracks))
  }

  def poly_map[A, B](func: A => B, input_list: List[A]): List[B] = input_list match{
    case Nil => List[B]()
    case x::xs => func(x) :: poly_map[A, B](func, xs)
  }

  def filter[A](condition: A => Boolean, input_list: List[A]): List[A] = input_list match{
    case Nil => List[A]()
    case x::xs => condition(x) match{
      case true => x :: filter[A](condition, xs)
      case false => filter[A](condition, xs)
    }
  }

  def partition[A](condition: A => Boolean, input_list: List[A]): List[List[A]]
    = input_list match{
      case Nil => List[A]() :: List[List[A]]()
      case x::xs => condition(x) match{
        case true => List[A]() :: partition[A](condition, xs)
        case false =>  val y = partition[A](condition, xs); (List[A](x) ::: y.head)::y.tail
      }
    }

  def templateMethod(func: Int => Int, conc: (Int, Int) => Int, a: Int, b: Int): Int =
    if (a > b) 0 else conc(func(a), templateMethod(func, conc, a + 1, b))

  }
