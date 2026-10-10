package canopy.codec

import canopy.domain.*
import canopy.ml.*
import io.circe.*
import io.circe.syntax.*

/** JSON wire formats: snake_case for the public API, camelCase for the model artifact. */
object JsonCodecs:
  given Encoder[Verdict] = Encoder.encodeString.contramap(_.wire)

  given Encoder[Contribution] = Encoder.instance { c =>
    Json.obj(
      "feature" -> c.feature.asJson,
      "value" -> c.value.filter(v => !v.isNaN && !v.isInfinite).asJson,
      "contribution" -> round(c.contribution).asJson
    )
  }

  given Encoder[RuleHit] = Encoder.instance { h =>
    Json.obj("rule" -> h.rule.asJson, "action" -> h.action.asJson, "reason" -> h.reason.asJson)
  }

  given Encoder[Explanation] = Encoder.instance { e =>
    Json.obj(
      "summary" -> e.summary.asJson,
      "top_risk_factors" -> e.topRiskFactors.asJson,
      "top_mitigating_factors" -> e.topMitigatingFactors.asJson,
      "rule_hits" -> e.ruleHits.asJson
    )
  }

  given Encoder[Decision] = Encoder.instance { d =>
    Json.obj(
      "transaction_id" -> d.transactionId.asJson,
      "verdict" -> d.verdict.asJson,
      "model_score" -> round(d.modelScore).asJson,
      "final_score" -> round(d.finalScore).asJson,
      "decided_by" -> d.decidedBy.asJson,
      "explanation" -> d.explanation.asJson,
      "tags" -> d.tags.asJson
    )
  }

  given Encoder[Transaction] = Encoder.instance { t =>
    Json.obj(
      "id" -> t.id.asJson,
      "timestamp" -> t.timestamp.toString.asJson,
      "account_id" -> t.accountId.asJson,
      "card_id" -> t.cardId.asJson,
      "merchant_id" -> t.merchantId.asJson,
      "mcc" -> t.mcc.asJson,
      "amount" -> t.amount.toMajor.setScale(t.amount.currency.exponent).toString.asJson,
      "currency" -> t.amount.currency.code.asJson,
      "channel" -> t.channel.wire.asJson,
      "device_id" -> t.deviceId.asJson,
      "ip" -> t.ip.map(_.canonical).asJson,
      "ip_country" -> t.ipCountry.map(_.value).asJson,
      "billing_country" -> t.billingCountry.value.asJson
    ).dropNullValues
  }

  given Encoder[Label] = Encoder.instance { l =>
    Json.obj(
      "transaction_id" -> l.transactionId.asJson,
      "card_id" -> l.cardId.asJson,
      "outcome" -> l.outcome.wire.asJson,
      "reported_at" -> l.reportedAt.toString.asJson
    )
  }

  /** Six significant decimals is plenty for probabilities and keeps responses readable. */
  private def round(d: Double): Double =
    if d.isNaN || d.isInfinite then 0.0
    else BigDecimal(d).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

  // Model artifact.

  given Codec[Transform] = Codec.from(
    Decoder.decodeString.emap(s =>
      Transform.values.find(_.toString == s).toRight(s"unknown transform $s")
    ),
    Encoder.encodeString.contramap(_.toString)
  )
  given Codec.AsObject[FeatureSpec] = Codec.AsObject.derived
  given Codec.AsObject[Preprocessor] = Codec.AsObject.derived
  given Codec.AsObject[Metrics.Report] = Codec.AsObject.derived
  given Decoder[Thresholds] = Decoder.forProduct2[Thresholds, Double, Double]("review", "decline") {
    (r, d) =>
      Thresholds(r, d)
  }.validate(
    c =>
      (for
        r <- c.downField("review").as[Double]
        d <- c.downField("decline").as[Double]
      yield 0 <= r && r <= d && d <= 1).getOrElse(false),
    "thresholds must satisfy 0 <= review <= decline <= 1"
  )
  given Encoder[Thresholds] = Encoder.forProduct2("review", "decline")(t => (t.review, t.decline))
  given Encoder[LogisticModel] = Encoder.forProduct6(
    "version",
    "preprocessor",
    "intercept",
    "coefficients",
    "thresholds",
    "validation"
  )(m => (m.version, m.preprocessor, m.intercept, m.coefficients, m.thresholds, m.validation))
  given Decoder[LogisticModel] = Decoder.instance { c =>
    for
      version <- c.downField("version").as[String]
      pre <- c.downField("preprocessor").as[Preprocessor]
      intercept <- c.downField("intercept").as[Double]
      coefficients <- c.downField("coefficients").as[Vector[Double]]
      thresholds <- c.downField("thresholds").as[Thresholds]
      validation <- c.downField("validation").as[Option[Metrics.Report]]
      model <-
        if coefficients.size != pre.width then
          Left(DecodingFailure(
            s"model has ${coefficients.size} coefficients for ${pre.width} columns",
            c.history
          ))
        else Right(LogisticModel(version, pre, intercept, coefficients, thresholds, validation))
    yield model
  }
