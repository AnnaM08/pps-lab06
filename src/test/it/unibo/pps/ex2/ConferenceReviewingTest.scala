package it.unibo.pps.ex2

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import it.unibo.pps.ex2.ConferenceReviewingExercise.*

class ConferenceReviewingTest:

  private var cr: ConferenceReviewing = _

  @BeforeEach
  def init(): Unit =
    cr = new ConferenceReviewingImpl()

    // Articolo 1
    cr.loadReview(1, 8, 8, 6, 8) // 4.8
    cr.loadReview(1, 9, 9, 6, 9) // 5.4

    // Articolo 2
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0

    // Articolo 3
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6

    // Articolo 4
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6

    // Test caricamento tramite Map
    val map = Map(
      Question.RELEVANCE -> 8,
      Question.SIGNIFICANCE -> 8,
      Question.CONFIDENCE -> 7,
      Question.FINAL -> 8
    )
    cr.loadReview(4, map) // 5.6

    // Articolo 5
    cr.loadReview(5, 6, 6, 6, 10) // 6.0
    cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @Test
  def testOrderedScores(): Unit =
    // L'articolo 2 ha preso su RELEVANCE i due voti 4, 9
    assertEquals(List(4, 9), cr.orderedScores(2, Question.RELEVANCE))
    assertEquals(List(6, 7, 8), cr.orderedScores(4, Question.CONFIDENCE))
    assertEquals(List(10, 10), cr.orderedScores(5, Question.FINAL))

  @Test
  def testAverageFinalScore(): Unit =
    // Delta 0.01 per il confronto tra double
    assertEquals(8.5, cr.averageFinalScore(1), 0.01)
    assertEquals(7.5, cr.averageFinalScore(2), 0.01)
    assertEquals(3.5, cr.averageFinalScore(3), 0.01)
    assertEquals(7.0, cr.averageFinalScore(4), 0.01)
    assertEquals(10.0, cr.averageFinalScore(5), 0.01)


  @Test
  def testAcceptedArticles(): Unit =
    // Verifica articoli accettati (1, 2, 4)
    assertEquals(Set(1, 2, 4), cr.acceptedArticles())

  @Test
  def testSortedAcceptedArticles(): Unit =
    // Lista ordinata di Pair(Articolo, Media)
    val expected = List(
      Pair(4, 7.0),
      Pair(2, 7.5),
      Pair(1, 8.5)
    )
    assertEquals(expected, cr.sortedAcceptedArticles())

  @Test
  def optionalTestAverageWeightedFinalScore(): Unit =
    val weightedMap = cr.averageWeightedFinalScoreMap()
    assertEquals((4.8 + 5.4) / 2, weightedMap(1), 0.01)
    assertEquals((9.0 + 6.0) / 2, weightedMap(2), 0.01)
    assertEquals((0.9 + 1.6) / 2, weightedMap(3), 0.01)
    assertEquals((3.6 + 5.6 + 5.6) / 3, weightedMap(4), 0.01)
    assertEquals((6.0 + 7.0) / 2, weightedMap(5), 0.01)
    assertEquals(5, weightedMap.size)
