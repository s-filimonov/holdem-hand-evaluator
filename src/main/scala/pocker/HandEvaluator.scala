package pocker

trait HandEvaluator {
  def evaluate(cards: Set[Card]): Hand
}
