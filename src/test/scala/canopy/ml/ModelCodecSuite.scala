package canopy.ml

import canopy.cli.Io
import canopy.codec.JsonCodecs.given
import canopy.features.{FeatureSchema, FeatureVector}
import io.circe.parser.decode
import io.circe.syntax.*
import munit.CatsEffectSuite

class ModelCodecSuite extends CatsEffectSuite:
  test("the bundled model loads, covers the model inputs and round-trips through JSON") {
    Io.loadModel(None).map { model =>
      assertEquals(model.preprocessor.specs.map(_.name), FeatureSchema.modelInputs)
      val again = decode[LogisticModel](model.asJson.noSpaces).toOption.get
      assertEquals(again.coefficients, model.coefficients)
      assertEquals(again.preprocessor, model.preprocessor)
      assertEquals(again.thresholds, model.thresholds)
    }
  }

  test("decoding rejects mismatched coefficient counts and invalid thresholds") {
    Io.loadModel(None).map { model =>
      val shortened = model.asJson.mapObject(_.add("coefficients", Vector(1.0).asJson))
      assert(decode[LogisticModel](shortened.noSpaces).isLeft)
      val badThresholds = model.asJson.mapObject(_.add("thresholds", io.circe.Json.obj("review" -> 0.9.asJson, "decline" -> 0.1.asJson)))
      assert(decode[LogisticModel](badThresholds.noSpaces).isLeft)
    }
  }

  test("scoring an empty vector yields a valid probability and one contribution per feature") {
    Io.loadModel(None).map { model =>
      val s = model.score(FeatureVector.empty)
      assert(s.probability > 0 && s.probability < 1)
      assertEquals(s.contributions.size, model.preprocessor.specs.size)
      assertEqualsDouble(s.contributions.map(_.contribution).sum + model.intercept, s.logit, 1e-9)
    }
  }
