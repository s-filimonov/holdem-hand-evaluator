package pocker

import pocker.CardRank.{CardRank, _}
import pocker.HandRank._
import pocker.HoldemHandEvaluator._

class HoldemHandEvaluator extends HandEvaluator {

  override def evaluate(cards: Set[Card]): Hand = {
    assert(cards.size == 7)

    royalFlush(cards)
      .orElse(straightFlush(cards))
      .orElse(fourOfAKind(cards))
      .orElse(fullHouse(cards))
      .orElse(flush(cards))
      .orElse(straight(cards))
      .orElse(threeOfAKind(cards))
      .orElse(twoPairs(cards))
      .orElse(pair(cards))
      .getOrElse(highCard(cards))
  }
}

object HoldemHandEvaluator {

  private implicit val cardOrdering = Ordering[Card].reverse

  private[pocker] def highCard(cards: Set[Card]): Hand = {
    val highCards: Seq[Card] = cards.toSeq.sorted.take(5)
    Hand(
      rank = HighCard,
      subRank = highCards.map(_.rank.id),
      cards = highCards.toSet
    )
  }

  private[pocker] def pair(cards: Set[Card]): Option[Hand] = sortedPairs(cards)
    .headOption
    .map {
      rankAndTwoCards: (CardRank, Set[Card]) =>
        val highCards: Seq[Card] = (cards -- rankAndTwoCards._2).toSeq.sorted.take(3)
        Hand(
          rank = Pair,
          subRank = Seq(rankAndTwoCards._1.id) ++ highCards.map(_.rank.id),
          cards = rankAndTwoCards._2 ++ highCards
        )
    }

  private[pocker] def twoPairs(cards: Set[Card]): Option[Hand] = {
    val highestPairs = sortedPairs(cards).take(2)
    if (highestPairs.size == 2) {
      val pairCards: Seq[Card] = highestPairs.flatMap(_._2)
      val highCard = (cards -- pairCards).toSeq.sorted.headOption
      Some(Hand(
        rank = TwoPairs,
        subRank = highestPairs.map(_._1.id) ++ Seq(highCard.get.rank.id),
        cards = (pairCards ++ highCard).toSet
      ))
    } else {
      None
    }
  }

  private def sortedPairs(cards: Set[Card]): Seq[(CardRank, Set[Card])] = {
    cards
      .groupBy(_.rank)
      .filter(_._2.size == 2)
      .toSeq.sortBy(_._1)(Ordering[CardRank].reverse)
  }

  private[pocker] def threeOfAKind(cards: Set[Card]): Option[Hand] = cards
    .groupBy(_.rank)
    .filter(_._2.size == 3)
    .toSeq.sortBy(_._1)
    .headOption
    .map {
      rankAndThreeCards: (CardRank, Set[Card]) =>
        val highCards: Seq[Card] = (cards -- rankAndThreeCards._2).toSeq.sorted.take(2)
        Hand(
          rank = ThreeOfAKind,
          subRank = Seq(rankAndThreeCards._1.id) ++ highCards.map(_.rank.id),
          cards = rankAndThreeCards._2 ++ highCards
        )
    }

  private[pocker] def straight(cards: Set[Card]): Option[Hand] = {
    normalStraight(cards) orElse wheelStraight(cards)
  }

  private final val WheelStraightRanks = Set(A, _2, _3, _4, _5)

  private def wheelStraight(cards: Set[Card]): Option[Hand] = {
    val wheelStraightCards: Set[Card] = cards
      .filter(card => WheelStraightRanks.contains(card.rank))
      .groupBy(_.rank)
      .map(_._2.head)
      .toSet
    if (wheelStraightCards.size == 5) {
      Some(Hand(rank = Straight, subRank = Seq(_5.id), cards = wheelStraightCards))
    } else {
      None
    }
  }

  private def normalStraight(cards: Set[Card]): Option[Hand] = {
    cards.groupBy(_.rank).map(_._2.head).toSeq.sorted.take(5) match {
      case fewCards if fewCards.size < 5 => None
      case straightCards if straightCards.sliding(2).forall(p => p.head.rank.id - p.last.rank.id == 1) =>
        Some(Hand(rank = Straight, subRank = Seq(straightCards.head.rank.id), cards = straightCards.toSet))
      case nonStraightCards => normalStraight(cards - nonStraightCards.head)
    }
  }

  private[pocker] def flush(cards: Set[Card]): Option[Hand] = cards
    .groupBy(_.suit)
    .find(_._2.size >= 5)
    .map(_._2.toSeq.sorted.take(5))
    .map(flushCards => Hand(rank = Flush, subRank = flushCards.map(_.rank.id), cards = flushCards.toSet))


  private[pocker] def fullHouse(cards: Set[Card]): Option[Hand] = {
    cards.groupBy(_.rank).filter(_._2.size == 3) match {
      case threes if threes.isEmpty => None
      case threes: Map[CardRank, Set[Card]] =>
        val threeCards: Set[Card] = threes.maxBy(_._1)._2
        (cards -- threeCards).groupBy(_.rank).filter(_._2.size >= 2) match {
          case twos if twos.isEmpty => None
          case twos: Map[CardRank, Set[Card]] =>
            val twoCards: Set[Card] = twos.maxBy(_._1)._2
            Some(Hand(
              rank = HandRank.FullHouse,
              subRank = Seq(threeCards, twoCards).map(_.head.rank.id),
              cards = threeCards ++ twoCards
            ))
        }
    }
  }

  private[pocker] def fourOfAKind(cards: Set[Card]): Option[Hand] = cards
    .groupBy(_.rank)
    .find(_._2.size == 4)
    .map {
      rankAndFourCards: (CardRank, Set[Card]) =>
        val highCard = (cards -- rankAndFourCards._2).toSeq.sorted.headOption
        Hand(
          rank = FourOfAKind,
          subRank = Seq(rankAndFourCards._1.id),
          cards = rankAndFourCards._2 ++ highCard
        )
    }

  private[pocker] def straightFlush(cards: Set[Card]): Option[Hand] =
    cards.groupBy(_.suit).find(_._2.size >= 5).map(_._2).flatMap(straight).map(_.copy(StraightFlush))

  private[pocker] def royalFlush(cards: Set[Card]): Option[Hand] = {
    straightFlush(cards).filter(hand => Set(A, K).subsetOf(hand.cards.map(_.rank))).map(_.copy(rank = RoyalFlush))
  }
}
