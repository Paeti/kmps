import scala.io.Source

package fups_prak {

object prak_3 {

  def map[A](func: A => A, input_list: List[A]): List[A] = input_list match{
    case Nil => List[A]()
    case x::xs => func(x) :: map[A](func, xs)
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

  def templateMethod(func: Int => Int, conc: (Int, Int) => Int,
                     neutr_elem: Int, a: Int, b: Int): Int =
    if (a > b) neutr_elem else conc(func(a), templateMethod(func, conc, neutr_elem, a + 1, b))

  def foldr[A](op: (A, A) => A)(neutr_elem: A)(input_list: List[A]): A = input_list match{
    case Nil => neutr_elem
    case x::xs => op(x, foldr(op)(neutr_elem)(xs))
  }

  def range(a: Int, b: Int): List[Int] =
    if(a == b) List(b) else a :: range(a + 1, b)

}
}
