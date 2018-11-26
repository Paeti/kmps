package fups_prak {

package object classes {

case class Track(title: String, length: String, rating: Int,
                 features: List[String], writers: List[String])
case class Album(title: String, date: String, artist: String,
                 tracks: List[Track])

}
}
