# canopy-risk

A streaming payment-fraud scoring engine written in Scala 3 on cats-effect, fs2 and http4s.

Each card authorization goes through the same pipeline: sliding-window velocity features, entity
graph features over cards, devices, IP networks and accounts, a logistic-regression model trained
offline in this repository, and a small rules language that analysts can hot-reload. The engine
returns approve, review or decline together with an explanation of why.

Everything is implemented here: the windowed aggregates, the graph search, the rules lexer, parser,
type checker and evaluator, the Newton solver for the model, the metrics and the threshold tuner.
The only runtime dependencies are the Typelevel stack, circe and decline.

## Features

- **Event ingestion**: JSON and NDJSON transactions validated with `cats.data.Validated`, so a
  malformed payload reports every bad field at once. Money is parsed into exact minor units per
  ISO 4217 exponent; over-precise amounts are rejected rather than rounded. IPv4 and IPv6 are
  parsed and normalised to /24 and /48 networks.
- **Streaming utilities** (fs2): event-time reordering with bounded lateness (late events are
  surfaced, not dropped) and time-windowed de-duplication with bounded memory.
- **Velocity features**: per-card, per-device and per-network sliding windows backed by a sorted
  vector with running cumulative sums (window sums are one subtraction, window bounds are binary
  searches). Out-of-order events inside the horizon are spliced in. Amount anomaly uses a
  time-decayed mean and variance (West's weighted algorithm) with a configurable half-life.
- **Graph features**: an immutable multi-entity graph with time-stamped edges and TTL. A bounded
  breadth-first search counts cards linked through shared devices or networks and how many of them
  are confirmed fraud. Hub nodes (a carrier NAT, a shared kiosk) are reached but never expanded,
  and the search reports when it hits its node cap. Union-find gives offline ring components.
- **Rules DSL**: hand-written lexer, Pratt parser, type checker with "did you mean" suggestions,
  minimal-parenthesis pretty printer (print then parse is a fixed point), and a compiler to
  closures. Missing data follows SQL-style three-valued logic, and a rule fires only when its
  condition is definitely true.
- **Model**: L2-regularised logistic regression fitted by Newton-Raphson (IRLS) with Armijo line
  search and a Cholesky solver with jitter fallback. Class weighting with prior correction keeps
  probabilities calibrated. Preprocessing learns log transforms for skewed features, missing-value
  indicators and clipping.
- **Evaluation**: ROC AUC (Mann-Whitney with tie handling), average precision, log loss, Brier,
  expected calibration error, and a threshold tuner that picks the decline cut-off from a
  precision target and the review cut-off from an analyst-capacity budget.
- **Point-in-time training**: history is replayed through the exact serving pipeline, and
  chargeback labels reach the graph only once the replay clock passes their report time.
- **Decision engine**: score adjustments compose in log-odds; review and decline rules escalate
  but never downgrade the model; approve rules (allow-lists) are capped by a probability ceiling.
  Every decision carries the top risk and mitigating feature contributions and all rule hits.
- **HTTP API** (http4s Ember): single and streaming NDJSON scoring, dry runs, label ingestion,
  rules hot reload with located diagnostics, model and schema introspection, Prometheus metrics,
  and idempotent handling of retried transaction ids.
- **Tooling**: a seeded simulator with card testing, account takeover and fraud rings, a rule
  backtester that reports precision, recall and incremental catches per rule, and a benchmark.

## Architecture

```
            NDJSON / JSON
                 |
        EventParser (Validated)        StreamOps.dedupe / StreamOps.reorder (fs2)
                 |
                 v
   +------------ FeaturePipeline (pure) ----------------+
   |  FeatureExtractor          EntityGraph             |
   |   EventWindow per card,     card-device-network-   |
   |   device, network;          account edges, BFS,    |
   |   DecayingStats; LruSet     fraud labels           |
   +-------------------------+--------------------------+
                             | FeatureVector
              +--------------+---------------+
              v                              v
       LogisticModel.score            RuleSet.evaluate
       (probability, logit,           (compiled closures,
        contributions)                 three-valued logic)
              +--------------+---------------+
                             v
                      DecisionEngine  ->  Decision + Explanation
                             |
          ScoringService (AtomicCell state, idempotency cache, metrics)
                             |
                 http4s Routes / CLI replay
```

Package layout under `src/main/scala/canopy`:

| package    | contents                                                                 |
|------------|--------------------------------------------------------------------------|
| `domain`   | money, currencies, transactions, IP addresses, labels, decisions          |
| `features` | event windows, decayed statistics, LRU memory, extractor, schema           |
| `graph`    | entity graph, bounded neighbourhood search, union-find                     |
| `rules`    | AST, lexer, parser, printer, typer, builtins, evaluator, rule sets, backtest |
| `ml`       | linear algebra, preprocessing, logistic regression, metrics, thresholds, trainer |
| `decision` | policy and decision engine                                                 |
| `engine`   | feature pipeline, scoring service, metrics registry, idempotency cache     |
| `ingest`   | JSON validation and fs2 stream operators                                   |
| `codec`    | circe encoders and decoders                                                |
| `sim`      | SplitMix64, a small random monad, and the transaction simulator            |
| `api`      | http4s routes and server                                                   |
| `cli`      | command implementations, file IO and the benchmark                         |

The feature pipeline is one pure function, `(EngineState, Transaction) => (EngineState,
FeatureVector)`, used both by the trainer and by the service, so there is no training/serving
skew to manage. The service serialises state updates through an `AtomicCell`, which is what keeps
per-card velocity exact under concurrent requests (there is a test that scores 50 transactions on
one card in parallel and checks the hourly count).

## The rules language

```
let tiny_amount = amount_usd < 5
let cards_on_device = coalesce(device.distinct_cards_24h, 0)

rule card_testing priority 100
  when tiny_amount and card.small_count_1h >= 3 and card.distinct_merchants_1h >= 3
  then decline
  reason "Burst of small authorizations across several merchants, typical of card testing"
  tag "card-testing"

rule unusual_amount_online
  when coalesce(card.amount_zscore, 0) > 3 and card_not_present
  then score +0.8
```

- Actions: `approve`, `review`, `decline`, or `score +x` / `score -x` (log-odds adjustment).
- Operators: `or`, `and`, `not`, comparisons (not chainable), `in` / `not in` over lists or
  substrings, `between ... and ...`, arithmetic with `+ - * / %`.
- Functions: `abs`, `min`, `max`, `log1p`, `sqrt`, `missing`, `present`, `coalesce`, `lower`,
  `starts_with`, `contains`.
- Identifiers are feature names from the schema (`GET /v1/schema`) or earlier `let` bindings.
- Errors carry line and column and render with the offending source line:

```
error at 2:8: unknown feature or let 'card.cnt_1h'; did you mean 'card.count_1h'?
  |
2 |   when card.cnt_1h > 3
  |        ^
```

The bundled policy lives in `src/main/resources/default.rules`. Its rules were chosen with the
`backtest` command; an earlier draft had a card-testing rule at 17.5% precision because legitimate
transit and top-up bursts also produce several small charges, which is why the shipped rule also
requires several distinct merchants within the hour.

## Quick start

Requirements: JDK 21 and sbt 1.11.

```bash
sbt test                                   # 187 tests
sbt "run check-rules src/main/resources/default.rules"
sbt "run serve --port 8080"                # or set $PORT
```

Score a transaction:

```bash
curl -s localhost:8080/v1/score -H 'content-type: application/json' -d '{
  "id": "tx-1", "timestamp": "2026-03-02T12:00:00Z",
  "account_id": "acct-1", "card_id": "card-1", "merchant_id": "m-1", "mcc": 5732,
  "amount": "249.99", "currency": "USD", "channel": "ecommerce",
  "device_id": "dev-9", "ip": "203.0.113.7", "ip_country": "RO", "billing_country": "US"
}'
```

The response contains `verdict`, `model_score`, `final_score`, `decided_by`, the explanation and
tags. Add `?dry_run=true` to score without recording the transaction.

| method | path                  | purpose                                                     |
|--------|-----------------------|-------------------------------------------------------------|
| GET    | `/health`             | liveness plus state counters                                |
| POST   | `/v1/score`           | score one transaction                                       |
| POST   | `/v1/score/stream`    | NDJSON in, NDJSON decisions out (errors reported per line)  |
| POST   | `/v1/features`        | the feature vector a transaction would get                  |
| POST   | `/v1/labels`          | report `fraud` or `legit` for a card                        |
| GET    | `/v1/rules`           | active rules, their features and warnings                   |
| PUT    | `/v1/rules`           | replace the rules (text body); 422 with diagnostics         |
| POST   | `/v1/rules/validate`  | compile rules without installing them                       |
| GET    | `/v1/model`           | model version, thresholds, validation metrics, weights      |
| GET    | `/v1/schema`          | feature catalogue                                           |
| GET    | `/metrics`            | Prometheus text format                                      |

### Command line

```bash
sbt "run simulate --seed 99 --events data/events.ndjson --labels data/labels.ndjson"
sbt "run train --events data/events.ndjson --labels data/labels.ndjson --out models/model.json"
sbt "run replay --events data/events.ndjson --labels data/labels.ndjson --model models/model.json"
sbt "run backtest --events data/events.ndjson --labels data/labels.ndjson --rules my.rules"
sbt "run bench --rounds 5"
```

`train` without `--events` simulates its own history. `sbt assembly` builds
`target/scala-3.3.7/canopy-risk.jar`, and the Dockerfile builds and runs that jar, listening on
`$PORT` (default 8080).

## Running tests and checks

```bash
sbt scalafmtCheckAll scalafmtSbtCheck   # formatting
sbt test                                # compiles with -Werror and -Wunused:all, then tests
```

The suites cover the lexer and parser (precedence, chained comparisons, error positions, print
then parse round trips), the type checker, three-valued evaluation, window sums against a
brute-force reference on random streams, decayed statistics, the extractor's point-in-time
behaviour, graph hub and depth limits, coefficient recovery for the logistic regression, AUC
against brute-force pair counting, threshold budgets with ties, the HTTP routes, and the CLI end
to end over temporary files.

## Measured results

All numbers below were produced on this repository's own synthetic data, on one machine (AMD
Ryzen 7 7800X3D, JDK 21, WSL2) that was also running other workloads. They describe how the
pieces behave on the simulator's fraud patterns. They say nothing about performance on real
payment traffic, which is far harder.

**Model training** (`train`, seed 7, 2,000 cardholders over 30 days, time-based 75/25 split):

| split      | rows   | fraud | ROC AUC | avg precision | log loss | ECE    |
|------------|--------|-------|---------|---------------|----------|--------|
| train      | 52,168 | 419   | 0.9993  | 0.889         | 0.0077   | 0.0013 |
| validation | 17,390 | 188   | 0.9988  | 0.850         | 0.0120   | 0.0024 |

Newton converged in 9 iterations. At an 80% decline-precision target the tuned decline threshold
caught 96.3% of validation fraud, with 2% of traffic routed to review.

**Held-out replay** (`replay` on a dataset simulated with seed 2026, which neither the model nor
the rules were tuned on; 69,076 transactions, 545 labelled fraud):

| configuration     | fraud declined | fraud sent to review | decline precision |
|-------------------|----------------|----------------------|-------------------|
| model only        | 508 (93.2%)    | 36                   | 75.8%             |
| model plus rules  | 513 (94.1%)    | 31                   | 74.7%             |

The rules add a handful of catches at a small precision cost on this data. That trade is what the
`backtest` output makes visible per rule.

**Throughput** (`bench --rounds 5`, single thread, best round after warm-up, two separate runs):

| stage                                   | per item      | per second      |
|-----------------------------------------|---------------|-----------------|
| feature pipeline (windows plus graph)   | 15 to 22 us   | 45k to 66k      |
| rule evaluation (7 rules)               | 1.0 to 1.6 us | 640k to 960k    |
| model scoring with contributions        | 2.5 to 3.7 us | 270k to 390k    |
| `ScoringService.score` end to end       | 40 to 60 us   | 17k to 25k      |

A cold `replay` of the 69k-transaction file, including JSON parsing, de-duplication and
reordering, took 11 to 13 seconds (about 5k to 6k transactions per second) with JIT warm-up
included.

## Roadmap

These are known gaps, not features in progress:

- State lives in memory only. A restart loses windows and graph edges; there is no snapshotting
  or event-log recovery yet.
- The service is a single instance. Partitioning state by card and routing requests to the owner
  is the obvious next step for horizontal scale.
- `ScoringService.score` costs about three times the pure pipeline. The likely sources are the
  mutex inside `AtomicCell`, the idempotency cache and per-request metric updates; this has not
  been profiled in detail yet.
- Feature state is persistent immutable maps. A mutable, single-writer store would be faster at
  the cost of the current simplicity.
- Logistic regression is the only model. Gradient-boosted trees would capture the interactions
  the rules currently encode by hand.
- The FX table is static and only covers eight currencies.
- The simulator is deliberately simple; its fraud is far easier to separate than real fraud, as
  the AUC above shows.
- No authentication on the HTTP API; it is meant to sit behind a gateway.

## License

MIT, see [LICENSE](LICENSE).
