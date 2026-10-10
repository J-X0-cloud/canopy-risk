package canopy.features

import canopy.domain.{FxRates, Transaction}

import java.time.ZoneOffset
import scala.util.chaining.*

/**
 * Turns a transaction plus the aggregate state into a point-in-time feature vector.
 *
 * Ordering matters for correctness: "history" features (z-score, time since last, new device)
 * are read from the state as it was before this event, while velocity counts include the event
 * itself. Training replays history through the same function, so the model never sees a feature
 * computed with information from the future.
 */
final class FeatureExtractor(config: FeatureConfig = FeatureConfig(), fx: FxRates = FxRates.reference):
  import FeatureConfig.*

  def observe(state: FeatureState, tx: Transaction): (FeatureState, FeatureVector) =
    val ts = tx.timestamp.toEpochMilli
    val usd = tx.amount.inUsd(fx)
    val logUsd = math.log1p(usd)
    val countryKey = tx.ipCountry.map(_.value).getOrElse(tx.billingCountry.value)

    val priorCard = state.cards.get(tx.cardId)
    val card = priorCard match
      case Some(c) =>
        c.copy(
          activity = c.activity.add(ts, usd, tx.merchantId),
          countries = c.countries.add(ts, 0.0, countryKey),
          amounts = c.amounts.update(ts, logUsd),
          lastSeen = math.max(c.lastSeen, ts)
        )
      case None =>
        CardState(
          EventWindow.empty(config.windowHorizonMillis).add(ts, usd, tx.merchantId),
          EventWindow.empty(config.windowHorizonMillis).add(ts, 0.0, countryKey),
          DecayingStats.empty(config.amountHalfLifeMillis).update(ts, logUsd),
          ts,
          ts
        )

    val priorAccount = state.accounts.get(tx.accountId)
    val newDevice = tx.deviceId.exists(d => !priorAccount.exists(_.devices.contains(d)))
    val accountDevices = priorAccount
      .map(_.devices.expire(ts - config.deviceMemoryMillis))
      .getOrElse(LruSet.empty(config.deviceMemory))
    val account = AccountState(
      tx.deviceId.fold(accountDevices)(d => accountDevices.touch(d, ts)),
      math.max(priorAccount.fold(ts)(_.lastSeen), ts)
    )

    def shared(map: Map[String, SharedState], key: String): SharedState =
      map.get(key) match
        case Some(s) => SharedState(s.cards.add(ts, usd, tx.cardId), math.max(s.lastSeen, ts))
        case None    => SharedState(EventWindow.empty(config.windowHorizonMillis).add(ts, usd, tx.cardId), ts)

    val device = tx.deviceId.map(d => d -> shared(state.devices, d))
    val network = tx.ip.map(ip => ip.network -> shared(state.networks, ip.network))

    val nextState = FeatureState(
      state.cards.updated(tx.cardId, card),
      state.accounts.updated(tx.accountId, account),
      device.fold(state.devices)(state.devices + _),
      network.fold(state.networks)(state.networks + _)
    )

    val hour = tx.timestamp.atZone(ZoneOffset.UTC).getHour
    val secondsSinceLast = priorCard.flatMap { c =>
      val prev = c.activity.lastBefore(ts).getOrElse(c.lastSeen)
      Option.when(prev <= ts)((ts - prev) / 1000.0)
    }

    FeatureVector.empty
      .withNum("amount", tx.amount.toMajor.toDouble)
      .withNum("amount_usd", usd)
      .withNum("log_amount_usd", logUsd)
      .withStr("currency", tx.amount.currency.code)
      .withStr("channel", tx.channel.wire)
      .withBool("card_not_present", tx.channel.cardNotPresent)
      .withNum("hour_of_day", hour.toDouble)
      .withBool("is_night", hour <= 5)
      .withStr("billing_country", tx.billingCountry.value)
      .withNum("merchant.mcc", tx.mcc.toDouble)
      .withBool("merchant.high_risk", config.highRiskMccs.contains(tx.mcc))
      .withNum("card.count_10m", card.activity.count(ts, 10 * Minute).toDouble)
      .withNum("card.count_1h", card.activity.count(ts, Hour).toDouble)
      .withNum("card.count_24h", card.activity.count(ts, Day).toDouble)
      .withNum("card.sum_usd_1h", card.activity.sum(ts, Hour))
      .withNum("card.sum_usd_24h", card.activity.sum(ts, Day))
      .withNum(
        "card.small_count_1h",
        card.activity.countWhere(ts, Hour)(_ < config.smallAmountUsd).toDouble
      )
      .withNum("card.distinct_merchants_24h", card.activity.distinctKeys(ts, Day).toDouble)
      .withNum("card.distinct_countries_24h", card.countries.distinctKeys(ts, Day).toDouble)
      .withBool("card.is_new", priorCard.isEmpty)
      .withNum(
        "card.age_days",
        priorCard.fold(0.0)(c => math.max(0L, ts - c.firstSeen) / Day.toDouble)
      )
      .withBool("account.new_device", newDevice)
      .withNum("account.devices_30d", account.devices.size.toDouble)
      .withSome(tx.ipCountry) { (v, c) =>
        v.withStr("ip_country", c.value).withBool("geo_mismatch", c != tx.billingCountry)
      }
      .withSome(priorCard.flatMap(_.amounts.zScore(logUsd)))(_.withNum("card.amount_zscore", _))
      .withSome(secondsSinceLast)(_.withNum("card.seconds_since_last", _))
      .withSome(device.map(_._2)) { (v, d) =>
        v.withNum("device.distinct_cards_24h", d.cards.distinctKeys(ts, Day).toDouble)
          .withNum("device.count_1h", d.cards.count(ts, Hour).toDouble)
      }
      .withSome(network.map(_._2)) { (v, n) =>
        v.withNum("ip.distinct_cards_24h", n.cards.distinctKeys(ts, Day).toDouble)
          .withNum("ip.count_1h", n.cards.count(ts, Hour).toDouble)
      }
      .pipe(nextState -> _)
