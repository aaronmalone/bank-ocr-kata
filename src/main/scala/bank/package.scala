package object bank {

  /* this pattern kept coming up -- following DRY principle and implementing once here */
  def requireForAll[T](traversable: TraversableOnce[T], requirement: T => Boolean, message: T => Any) {
    for (element <- traversable) {
      require(requirement(element), message(element))
    }
  }
}