package canopy.ml

import canopy.sim.{SimConfig, Simulator}

class TrainerSuite extends munit.FunSuite:
  private lazy val sim = Simulator.run(SimConfig(
    seed = 3,
    cardholders = 600,
    days = 21,
    cardTestingAttacks = 25,
    takeovers = 25,
    fraudRings = 4
  ))
  private lazy val rows = Trainer.buildRows(sim.transactions, sim.labels)

  test("rows line up with transactions and carry the eventual label") {
    assertEquals(rows.size, sim.transactions.size)
    val fraud = sim.fraudIds
    rows.foreach(r => assertEquals(r.label == 1, fraud.contains(r.transactionId)))
  }

  test("graph labels are only visible after their report time") {
    val first = sim.labels.head
    val before = rows.filter(r => r.timestamp < first.reportedAt.toEpochMilli)
    assert(before.forall(_.features.num("graph.fraud_cards").contains(0.0)))
  }

  test("time split keeps every training row before every validation row") {
    val (train, valid) = Trainer.timeSplit(rows, 0.25)
    assert(train.map(_.timestamp).max <= valid.map(_.timestamp).min)
    assertEquals(train.size + valid.size, rows.size)
  }

  test("a model trained on simulated history separates fraud well on later data") {
    val result = Trainer.train(rows)
    assert(result.fit.converged, result.fit.toString)
    assert(result.validation.auc > 0.9, result.validation.render)
    assert(result.validation.averagePrecision > 0.5, result.validation.render)
    assert(result.model.thresholds.decline >= result.model.thresholds.review)
  }
