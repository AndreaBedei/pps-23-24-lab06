package ex2

enum Question:
  case Relevance
  case Significance
  case Confidence
  case Final

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def accepted(article: Int): Boolean
  def acceptedArticles: Set[Int]
  def sortedAcceptedArticles: List[(Int, Double)]
  def averageWeightedFinalScore(article: Int): Double
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  private class ConferenceReviewingImpl extends ConferenceReviewing:

    private var reviews: List[(Int, Map[Question, Int])] = List()

    def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      if scores.size < Question.values.length then throw IllegalArgumentException() else reviews = List((article, scores)) ::: reviews

    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      reviews = List((article, Map(
        Question.Relevance -> relevance,
        Question.Significance -> significance,
        Question.Confidence -> confidence,
        Question.Final -> fin)
      )) ::: reviews

    def orderedScores(article: Int, question: Question): List[Int] =
      reviews.filter((x, y) => x == article).map((p, t) => t(question)).sortWith((f, s) => f < s)

    def averageFinalScore(article: Int): Double =
      val v = reviews.filter((x, y) => x == article).map((p, t) => t(Question.Final))
      v.sum / v.size

    def accepted(article: Int): Boolean =
      averageFinalScore(article) > 5.0 && reviews.filter((x, _) => x == article).map(s => s._2).flatMap(q => q.toList).count((w, e) => w == Question.Relevance && e >= 8) > 0

    def acceptedArticles: Set[Int] = reviews.map(x => x._1).distinct.filter(accepted).toSet

    def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.map(e => (e, averageFinalScore(e))).toList.sortWith((e1, e2) => e1._2 < e2._2)

    def averageWeightedFinalScore(article: Int): Double =
      val v = reviews.filter((x, _) => x == article).map((_, y) => y(Question.Final))
      v.sum / v.size

    def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.map((x, _) => x).distinct.map(t => (t, averageWeightedFinalScore(t))).toMap