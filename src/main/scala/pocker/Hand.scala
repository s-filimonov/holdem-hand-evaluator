package pocker

import pocker.HandRank.HandRank

case class Hand(rank: HandRank, subRank: Seq[Int], cards: Set[Card]) extends Comparable[Hand] {
  final val HandSize = 5
  assert(cards.size == HandSize, "Hand must contain 5 cards.")

  override def compareTo(o: Hand): Int = {
    if (rank == o.rank) {
      for ((t, j) <- subRank zip o.subRank) {
        if (t > j) return 1
        else if (t < j) return -1
      }
    }
    rank.compare(o.rank)
  }
}

object HandRank extends Enumeration {
  type HandRank = HandRank.Value
  val RoyalFlush, StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPairs, Pair, HighCard = Value
}
