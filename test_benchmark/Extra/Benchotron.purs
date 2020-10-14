module TestBenchmark.Extra.Benchotron
  ( Benchmark'
  , mkBenchmark'
  , Benchmark_
  , mkBenchmark_
  , prettyPrintBenchmarkResult
  , benchmarkToStdoutReadable
  ) where

import Prelude
import Ansi.Codes (Color(..))
import Ansi.Output (dim, foreground, withGraphics)
import Benchotron.Core (Benchmark, BenchmarkFunction, BenchmarkResult, DataPoint, ResultSeries, benchFn, mkBenchmark, unpackBenchmark)
import Benchotron.StdIO (stdoutWrite)
import Benchotron.UI.Console (runBenchM', runBenchmarkConsole)
import Data.Array (length, sortWith)
import Data.Int (toNumber)
import Data.Lens (view)
import Data.Lens.Record (prop)
import Data.Number.Format (fixed, precision, toStringWith)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Test.QuickCheck.Gen (Gen)

type Benchmark' a
  = { slug :: String
    , title :: String
    , gen :: Gen a
    , functions :: Array (BenchmarkFunction a)
    }

mkBenchmark' :: forall a. Benchmark' a -> Benchmark
mkBenchmark' { slug, title, gen, functions } =
  mkBenchmark
    { slug
    , title
    , functions
    , sizes: [ 1 ]
    , sizeInterpretation: "Not used"
    , inputsPerSize: 1
    , gen: \_ -> gen
    }

type Benchmark_ r
  = { slug :: String
    , title :: String
    , fn :: Unit -> r
    }

mkBenchmark_ :: forall r. Benchmark_ r -> Benchmark
mkBenchmark_ { slug, title, fn } = mkBenchmark' { slug, title, gen: pure unit, functions: [ benchFn slug fn ] }

prettyPrintBenchmarkResult :: Int -> BenchmarkResult -> String
prettyPrintBenchmarkResult inputsPerSize results =
  withGraphics (foreground BrightCyan)
    ( results.slug
        <> ": "
        <> results.title
    )
    <> "\n"
    <> (joinWith "\n" $ map prettyPrintSeries results.series)
  where
  _mean = prop (SProxy :: SProxy "stats") <<< prop (SProxy :: SProxy "mean")

  prettyPrintSeries :: ResultSeries -> String
  prettyPrintSeries series = joinWith "\n" $ map prettyPrintDataPoint (sortWith (view _mean) series.results)
    where
    prettyPrintDataPoint :: DataPoint -> String
    prettyPrintDataPoint d =
      withGraphics (foreground BrightGreen) series.name
        <> maybeSize
        <> "\t"
        <> withGraphics (dim) "x"
        <> "\t"
        <> withGraphics (foreground BrightMagenta) opsPerSec
        <> "\tops/sec ±"
        <> withGraphics (foreground BrightGreen) rmePercent
        <> "%\t"
        <> withGraphics (dim)
            ( "("
                <> show (length d.stats.sample)
                <> " runs sampled)"
            )
      where
      maybeSize
        | d.size == 1 = ""
        | otherwise = " (" <> show d.size <> ")"

      opsPerSec = toStringWith (fixed 0) $ toNumber inputsPerSize * 1.0 / d.stats.mean

      rmePercent
        | d.stats.rme < 1.0 = toStringWith (precision 2) $ d.stats.rme
        | otherwise = toStringWith (precision 3) $ d.stats.rme

-- | Run a benchmark and print the results to standard output. This will only
-- | work on node.js.
benchmarkToStdoutReadable :: Benchmark -> Effect Unit
benchmarkToStdoutReadable bench = do
  results <- runBenchM' $ runBenchmarkConsole bench
  stdoutWrite "\n"
  stdoutWrite $ prettyPrintBenchmarkResult (unpackBenchmark _.inputsPerSize bench) results
  stdoutWrite "\n"
  stdoutWrite "\n"
