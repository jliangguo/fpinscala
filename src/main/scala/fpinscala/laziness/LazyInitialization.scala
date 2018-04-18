object LazyInitialization {
  lazy val resource: Int = init()

  def init(): Int = {
    //
    0
  }

  def main(args: Array[String]): Unit = {
    println(resource)
  }
}
