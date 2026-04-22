package it.unibo.pps.ex2

import it.unibo.pps.ex2.ConferenceReviewingExercise.Question.{CONFIDENCE, FINAL, RELEVANCE}

object ConferenceReviewingExercise:

  case class Pair[A, B](x: A, y: B)

  enum Question:
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL

  trait ConferenceReviewing:
    def loadReview(article: Int, scores: Map[Question, Int]): Unit
    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
    def orderedScores(article: Int, question: Question): List[Int]
    def averageFinalScore(article: Int): Double
    def acceptedArticles(): Set[Int]
    def sortedAcceptedArticles(): List[Pair[Int, Double]]
    def averageWeightedFinalScoreMap(): Map[Int, Double]

  class ConferenceReviewingImpl extends ConferenceReviewing:

    var articlesScores: Map[Int, Map[Question, List[Int]]] = Map.empty

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      val currentArticleScores = articlesScores.getOrElse(article, Map.empty)
      //si fa la foldLeft sulla mappa per iterare le entry, che sono coppie chiave valore
      val updatedArticleScores = scores.foldLeft(currentArticleScores){ case (acc, (q, v)) =>
        /*creo nuova lista che è quella attuale per la domanda con aggiunto in coda il nuovo valore*/
          val updatedList = acc.getOrElse(q, List.empty) :+ v
          acc + (q -> updatedList) //si aggiorna la entry presente nella mappa per la domanda
      }
      //aggiornamento del campo aggiornando la mappa associata all'articolo
      articlesScores = articlesScores + (article -> updatedArticleScores)

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      /*var currentArticleScores = articlesScores.getOrElse(article, Map.empty)
      val listValues = List(relevance, significance, confidence, fin)
      var i = 0
      for q <- Question.values do
        val updateArticleScores = currentArticleScores.getOrElse(q, List.empty) :+ listValues(i)
        currentArticleScores = currentArticleScores + (q -> updateArticleScores)
        i += 1
      articlesScores = articlesScores + (article -> currentArticleScores)*/
      //!! Per evitare di usare i si crea una nuova mappa temporanea
      val newValues = Map(
        Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> fin
      )
      /*var currentArticleScores = articlesScores.getOrElse(article, Map.empty)
      for (q, v) <- newValues do
        val updatedList = currentArticleScores.getOrElse(q, List.empty) :+ v
        currentArticleScores = currentArticleScores + (q -> updatedList)
      articlesScores = articlesScores + (article -> currentArticleScores)*/
      val currentArticleScores = articlesScores.getOrElse(article, Map.empty)
      val updatedArticleScores = newValues.foldLeft(currentArticleScores){ case (acc, (q, v)) =>
        val updatedScoresOfQuestion = acc.getOrElse(q, List.empty) :+ v
        acc + (q -> updatedScoresOfQuestion)
      }
      articlesScores = articlesScores + (article -> updatedArticleScores)

    override def orderedScores(article: Int, question: Question): List[Int] =
      articlesScores.getOrElse(article, Map.empty).getOrElse(question, List.empty).sorted

    override def averageFinalScore(article: Int): Double =
      val listValuesFinal = articlesScores.getOrElse(article, Map.empty).getOrElse(Question.FINAL, List.empty)
      listValuesFinal.sum.toDouble / listValuesFinal.size

    override def acceptedArticles(): Set[Int] =
      articlesScores.foldLeft(Set.empty){ case (acc, (article, mapQuestions)) =>
        //Un articolo viene  accettato se il valore medio della valutazione
        //alla domanda "FINAL" è >5 e se ha almeno una valutazione "RELEVANCE" >= 8.
        if averageFinalScore(article) > 5 && mapQuestions.getOrElse(RELEVANCE, List.empty).exists(_ >= 8) then
          acc + article
        else
          acc
      }

    override def sortedAcceptedArticles(): List[Pair[Int, Double]] =
      val listAcceptedArticles = acceptedArticles()
      val disorderedList = listAcceptedArticles.foldLeft(List.empty[Pair[Int, Double]]) { case (acc, article) =>
        acc :+ Pair(article, averageFinalScore(article))
      }
      disorderedList.sortBy(e => e.y)//ordino rispetto al secondo valore

    /*@return a map from articles to their average "weighted final score" the average value of CONFIDENCE * FINAL/10 */
    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      articlesScores.foldLeft(Map.empty[Int, Double]){ case (acc, (article, mapQuestions)) =>
        val listConf = mapQuestions.getOrElse(CONFIDENCE, List.empty)
        val listFinal = mapQuestions.getOrElse(FINAL, List.empty)
        var average = 0.0
        for i <- listFinal.indices do
          average += listFinal(i) * listConf(i)
        average = average / listFinal.size
        acc + (article -> average / 10)
      }


      /*
      def _computeAllAverage(article: Int): Double = {
        val articleMapQuestions = articlesScores.getOrElse(article, Map.empty)
        Question.values.foldLeft(0.0) { case (acc, q) =>
          val listScores = articleMapQuestions.getOrElse(q, List.empty)
          acc + listScores.sum.toDouble / listScores.size //aggiungo lo score medio per
        }
      }*/

