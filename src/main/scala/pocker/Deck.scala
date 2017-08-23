package pocker

import pocker.CardRank.CardRank
import pocker.Suit.Suit

trait Deck {
  def pullCard(rank: CardRank, suit: Suit): Option[Card]
  def pullRandomCardWithRank(rank: CardRank): Option[Card]
  def pullRandomCardWithSuit(suit: Suit): Option[Card]
  def pullRandomCard(): Option[Card]
}