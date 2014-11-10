package util

class Cache[-I, +O](f: I => O) extends (I => O) {
  private[this] val cache = scala.collection.concurrent.TrieMap.empty[I, O]

  def apply(x: I): O = {
    if (cache.contains(x)) {
      cache(x)
    } else {
      val y = f(x)
      cache += (x -> y)
      y
    }
  }
}

object Cache {
  def apply[I, O](f: I => O) = new Cache(f)
}