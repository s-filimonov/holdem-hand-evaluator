package pocker

import org.scalatest.{Matchers, WordSpec}
import pocker.CardRank._
import pocker.HandRank._
import pocker.HoldemHandEvaluator._
import pocker.Suit._

/**
 * @author s.filimonov
 */
class HoldemHandEvaluatorSpec extends WordSpec with Matchers {

  private implicit class CardRankSet(ranks: Set[CardRank]) {
    def toCards(suit: Suit): Set[Card] = ranks.map(Card(_, suit))
  }

  "highCard" should {
    "return combination of highest ranked cards" in {
      val cards: Set[Card] = CardRank.values.range(_2, _9).toCards(suit = C)

      highCard(cards) shouldBe Hand(
        rank = HighCard,
        subRank = CardRank.values.range(_4, _9).map(_.id).toSeq.sortBy(-_),
        cards = CardRank.values.range(_4, _9).toCards(suit = C)
      )
    }

  }

  "highCard for two combinations" when {
    "highest card rank is the same in both combinations " +
      "but a second card rank is greater in the first combination" should {

      "return higher ranked and lower ranked combinations" in {
        val higherHandCards: Set[Card] = Set(A, K, J, _10, _9, _8, _2).toCards(suit = C)
        val lowerHandCards: Set[Card] = Set(A, Q, J, _10, _9, _8, _2).toCards(suit = D)

        highCard(higherHandCards) compareTo highCard(lowerHandCards) should be > 0
      }
    }

    "almost all highest cards rank is the same in both combinations " +
      "and only the last one is greater in the first hand" should {

      "return higher ranked and lower ranked hands" in {
        val higherHandCards: Set[Card] = Set(A, K, Q, J, _10, _9, _2).toCards(suit = C)
        val lowerHandCards: Set[Card] = Set(A, K, Q, J, _9, _8, _2).toCards(suit = D)

        highCard(higherHandCards) compareTo highCard(lowerHandCards) should be > 0
      }
    }

    "top 5 cards are equally ranked in both combinations" should {
      "return equally ranked hands" in {
        val firstHandCards: Set[Card] = Set(A, K, Q, J, _10, _9, _2).toCards(suit = C)
        val secondHand: Set[Card] = Set(A, K, Q, J, _10, _9, _2).toCards(suit = D)

        highCard(firstHandCards) compareTo highCard(secondHand) shouldBe 0
      }
    }
  }

  "pair" when {
    "cards contain pair" should {
      s"return $TwoPairs hand" in {
        val fiveCards: Set[Card] = CardRank.values.range(_4, _9).toCards(suit = C)
        val pairCards = Set(Card(_2, C), Card(_2, D))

        val cards: Set[Card] = pairCards ++ fiveCards

        pair(cards) shouldBe Some(
          Hand(
            rank = Pair,
            subRank = Seq(_2.id, _8.id, _7.id, _6.id),
            cards = Set(Card(_2, C), Card(_2, D)) ++ CardRank.values.range(_6, _9).toCards(suit = C)
          )
        )
      }
    }

    "cards doesn't contain pair" should {
      "return no combination" in {
        pair(CardRank.values.range(_2, A).toCards(suit = C)) shouldBe None
      }
    }
  }

  "pair for two combinations" when {
    "first hand contains same rank higher pair and higher ranked second pair " should {
      "return first (higher) hand and second (lower) hand" in {
        val firstHandCards = (A, A, Q, Q, _10, _9, _8)
      }
    }
  }

  "twoPairs" when {
    "cards contain more than 2 pairs" should {
      "return highest ranked combination" in {
        val cards = Set(
          Card(A, C), Card(A, D),
          Card(K, C),
          Card(_2, C), Card(_2, D),
          Card(_3, C), Card(_3, D),
          Card(_9, C), Card(_9, D)
        )

        twoPairs(cards) shouldBe Some(
          Hand(
            rank = TwoPairs,
            subRank = Seq(A.id, _9.id, K.id),
            cards = Set(Card(A, C), Card(A, D), Card(_9, C), Card(_9, D), Card(K, C))
          )
        )
      }
    }

    "cards contain only one pair" should {
      "return no combination" in {
        val cards: Set[Card] = Set(Card(_2, C), Card(_2, D)) ++ CardRank.values.range(_3, _9).toCards(suit = C)

        twoPairs(cards) shouldBe None
      }
    }
  }

  "threeOfAKind" when {
    "cards contain three of a kind" should {
      "return combination of three cards plus two highest ranked" in {
        val cards = Set(
          Card(A, C), Card(A, D), Card(A, H),
          Card(K, C),
          Card(Q, C),
          Card(J, C),
          Card(_10, C)
        )

        threeOfAKind(cards) shouldBe Some(
          Hand(
            rank = ThreeOfAKind,
            subRank = Seq(A.id, K.id, Q.id),
            cards = Set(Card(A, C), Card(A, D), Card(A, H), Card(K, C), Card(Q, C))
          )
        )
      }
    }

    "cards doesn't contain three of a kind" should {
      "return no combination" in {
        val cards = CardRank.values.range(_2, _9).toCards(suit = C)

        threeOfAKind(cards) shouldBe None
      }
    }
  }

  "straight" when {
    "cards contain no straight" should {
      "return no combination" in {
        val cards = Set(A, K, Q, J, _9, _8, _7).toCards(suit = C)

        straight(cards) shouldBe None
      }
    }

    "all cards are of sequential rank" should {
      "return combination starting from the highest ranked card" in {
        val cards = CardRank.values.from(_8).toCards(suit = C)

        straight(cards) shouldBe Some(
          Hand(rank = Straight, subRank = Seq(A.id), cards = CardRank.values.from(_10).toCards(suit = C))
        )
      }
    }

    "cards contain 5 with sequential rank and two pairs" should {
      "return combination of cards with sequential rank" in {
        val straightRanks = Set(A, K, J, Q, _10)
        val straightCards = Set(A, K, J, Q, _10).toCards(suit = C)
        val cards = straightCards + Card(A, D) + Card(K, D)

        val result: Option[Hand] = straight(cards)
        result shouldBe defined

        val straightHand: Hand = result.get
        val straightHandRanks: Seq[CardRank] = straightHand.cards.toSeq.map(_.rank)

        straightHand.rank shouldBe Straight
        straightHand.subRank shouldBe Seq(A.id)

        straightHandRanks should contain allElementsOf straightRanks
      }
    }

    "cards contain only five cards of sequential rank" should {
      "return combination starting from the highest ranked card from the sequence" in {
        val cards = Set(A, Q, J, _10, _9, _8, _2).toCards(suit = C)

        straight(cards) shouldBe Some(
          Hand(rank = Straight, subRank = Seq(Q.id), cards = Set(Q, J, _10, _9, _8).toCards(suit = C))
        )
      }
    }

    "cards contain wheel straight" should {
      s"return combination of ${Seq(A, _2, _3, _4, _5)} with subrank ${_5.id}" in {
        val cards = Set(A, _2, _3, _4, _5, _9, _10).toCards(suit = C)

        straight(cards) shouldBe Some(
          Hand(rank = Straight, subRank = Seq(_5.id), cards = Set(A, _2, _3, _4, _5).toCards(suit = C))
        )
      }
    }

    "cards contain wheel straight and other aces" should {
      "return combination with single ace" in {
        val cards = Set(A, _2, _3, _4, _5).toCards(suit = C) + Card(A, D) + Card(A, H)

        val result: Option[Hand] = straight(cards)
        result shouldBe defined

        val straightHand: Hand = result.get

        straightHand.rank shouldBe Straight
        straightHand.subRank shouldBe Seq(_5.id)
        straightHand.cards should contain allElementsOf Set(_2, _3, _4, _5).toCards(suit = C)
        straightHand.cards should contain oneElementOf Seq(C, D, H).map(Card(A, _))
      }
    }

    "cards contain wheel straight and higher ranked straight" should {
      "return higher ranked straight combination" in {
        val cards = Set(A, _2, _3, _4, _5, _6, _10).toCards(suit = C)

        straight(cards) shouldBe Some(
          Hand(rank = Straight, subRank = Seq(_6.id), cards = Set(_2, _3, _4, _5, _6).toCards(suit = C))
        )
      }
    }
  }

  "fourOfAKind" when {
    "cards contain four cards with the same suit" should {
      "return combination of four cards with the same suit plus highest card" in {
        val fourCards: Set[Card] = Set(C, D, H, S).map(Card(A, _))
        val otherCards: Set[Card] = Set(Card(K, C), Card(Q, C), Card(J, C))

        val cards: Set[Card] = fourCards ++ otherCards

        fourOfAKind(cards) shouldBe Some(
          Hand(rank = FourOfAKind, subRank = Seq(A.id), fourCards + Card(K, C))
        )
      }
    }
  }

  "straightFlush" when {
    "cards contain just simple flush" should {
      "return no combination" in {
        val cards = Set(Card(A, C), Card(K, D), Card(Q, H), Card(J, S), Card(_10, C), Card(_9, D), Card(_8, H))

        straightFlush(cards) shouldBe None
      }
    }

    "all cards are of sequential rank and same suit" should {
      "return combination of cards with sequential rank" in {
        val cards: Set[Card] = CardRank.values.from(_8).toCards(suit = C)

        straightFlush(cards) shouldBe Some(
          Hand(rank = StraightFlush, subRank = Seq(A.id), CardRank.values.from(_10).toCards(suit = C))
        )
      }
    }
  }


}
