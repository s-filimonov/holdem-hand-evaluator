package pocker

import pocker.CardRank.CardRank
import pocker.Suit.Suit

case class Card(rank: CardRank, suit: Suit) extends Comparable[Card] {
  override def compareTo(o: Card): Int = rank.compare(o.rank)
}

object CardRank extends Enumeration {
  type CardRank = Value
  val _2, _3, _4, _5, _6, _7, _8, _9, _10, J, Q, K, A = Value
}

object Suit extends Enumeration {
  type Suit = Suit.Value
  val C = Value(name = "Club")
  val D = Value(name = "Diamond")
  val H = Value(name = "Heart")
  val S = Value(name = "Spade")
}