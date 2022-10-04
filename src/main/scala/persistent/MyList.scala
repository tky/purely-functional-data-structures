package persistent

object MyList {

  /** Ex 2.1
    */
  def suffixes[A](xs: List[A]): List[List[A]] = xs match {
    case Nil     => List(List())
    case _ :: hs => xs :: suffixes(hs)
  }
}
