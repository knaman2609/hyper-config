module TestBenchmark.Main where

import Prelude
import Benchotron.Core (Benchmark, benchFn)
import Data.Foldable (for_)
import Data.Nullable (Nullable, notNull, null)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous (mkSubRecordRecursive)
import Effect (Effect)
import Extra.Heterogeneous.ForAll (ForAllMapping(..))
import Extra.Heterogeneous.Recursive (RecursiveMapping(..))
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Mapping (class HMap, hmap)
import HyperWidgetConfig.Config as Config
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Config.Screens.SomeScreen as SomeScreen
import HyperWidgetConfig.Reference (Reference(..))
import HyperWidgetConfig.Resolve (resolveConfig)
import TestBenchmark.Extra.Benchotron (benchmarkToStdoutReadable, mkBenchmark')

exampleParent :: Config.ResolvedProps
exampleParent =
  { themeConfig:
      { padding: 1
      , margin: 12
      }
  , components:
      { primaryButton:
          { margin: 123
          , states:
              { enabled:
                  { margin: 222
                  }
              }
          }
      }
  , containers:
      { gridItem:
          { padding: 3210
          , primaryButton:
              { component: "components.primaryButton"
              , margin: 1234
              , states:
                  { enabled:
                      { margin: 222
                      }
                  }
              }
          , states:
              { hidden:
                  { padding: 3210
                  , primaryButton:
                      { margin: 1234
                      , states:
                          { enabled:
                              { margin: 222
                              }
                          }
                      }
                  }
              }
          }
      }
  , screens:
      { someScreen:
          { someScreenProp: 123.0
          , primaryButton:
              { component: "components.primaryButton"
              , margin: 123
              , states:
                  { enabled:
                      { margin: 222
                      }
                  }
              }
          , gridItem:
              { component: "containers.gridItem"
              , padding: 3210
              , primaryButton:
                  { component: "components.primaryButton"
                  , margin: 1234
                  , states:
                      { enabled:
                          { margin: 222
                          }
                      }
                  }
              , states:
                  { hidden:
                      { padding: 3210
                      , primaryButton:
                          { margin: 1234
                          , states:
                              { enabled:
                                  { margin: 222
                                  }
                              }
                          }
                      }
                  }
              }
          , states:
              { someScreenState:
                  { someScreenProp: 123.0
                  , primaryButton:
                      { margin: 123
                      , states:
                          { enabled:
                              { margin: 222
                              }
                          }
                      }
                  , gridItem:
                      { padding: 3210
                      , primaryButton:
                          { margin: 1234
                          , states:
                              { enabled:
                                  { margin: 222
                                  }
                              }
                          }
                      , states:
                          { hidden:
                              { padding: 3210
                              , primaryButton:
                                  { margin: 1234
                                  , states:
                                      { enabled:
                                          { margin: 222
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }

exampleConfig :: Config.Props
exampleConfig =
  mkSubRecordRecursive
    { themeConfig:
        { padding: 123
        }
    , components:
        { primaryButton:
            { margin: Literal 444
            }
        }
    , containers:
        { gridItem:
            { padding: Literal 321
            , primaryButton:
                { component: "components.primaryButton"
                , margin: Reference "themeConfig.margin"
                }
            }
        }
    }

benchResolveConfig :: Benchmark
benchResolveConfig =
  mkBenchmark'
    { slug: "resolveConfig"
    , title: "Resolving example config against default config"
    , gen: pure exampleConfig
    , functions:
        let
          exampleRequiredConfig :: Config.RequiredProps
          exampleRequiredConfig =
            { themeConfig:
                { padding: 1
                , margin: 12
                }
            , components:
                { primaryButton:
                    { margin: Literal 123
                    , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
                    }
                }
            , containers:
                { gridItem:
                    { padding: Literal 123
                    , primaryButton:
                        { component: "components.primaryButton"
                        , margin: null
                        , states: null
                        }
                    , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
                    }
                }
            , screens:
                { someScreen:
                    { someScreenProp: Literal 123.0
                    , primaryButton:
                        { component: "components.primaryButton"
                        , margin: null
                        , states: null
                        }
                    , gridItem:
                        { component: "containers.gridItem"
                        , padding: null
                        , primaryButton: null
                        , states: null
                        }
                    , states: mkSubRecord {} :: SomeScreen.StatesF SomeScreen.State SubRecord
                    }
                }
            }
        in
          [ benchFn "resolveConfig                " \v -> resolveConfig exampleRequiredConfig v
          , benchFn "resolveConfigPartiallyApplied" (resolveConfig exampleRequiredConfig)
          ]
    }

benchSimpleTraversalConfig :: Benchmark
benchSimpleTraversalConfig =
  mkBenchmark'
    { slug: "simpleTraversalConfig"
    , title: "Example of very simple traversal of the config"
    , gen: pure exampleConfig
    , functions:
        let
          f ::
            forall s.
            HMap (RecursiveMapping (ForAllMapping String)) Config.Props (SubRecord s) =>
            Config.Props -> SubRecord s
          f = hmap (RecursiveMapping $ ForAllMapping unsafeStringify)
        in
          [ benchFn "simpleTraversalConfig                " \v -> f v
          , benchFn "simpleTraversalConfigPartiallyApplied" f
          ]
    }

benchSimpleTraversalResolvedConfig :: Benchmark
benchSimpleTraversalResolvedConfig =
  mkBenchmark'
    { slug: "simpleTraversalResolvedConfig"
    , title: "Example of very simple traversal of the resolved config"
    , gen: pure exampleParent
    , functions:
        let
          f ::
            forall r.
            HMap (RecursiveMapping (ForAllMapping String)) Config.ResolvedProps (Record r) =>
            Config.ResolvedProps -> Record r
          f = hmap (RecursiveMapping $ ForAllMapping unsafeStringify)
        in
          [ benchFn "simpleTraversalResolvedConfig                " \v -> f v
          , benchFn "simpleTraversalResolvedConfigPartiallyApplied" f
          ]
    }

benchTrivialTraversalConfig :: Benchmark
benchTrivialTraversalConfig =
  mkBenchmark'
    { slug: "trivialTraversalConfig"
    , title: "Example of very trivial traversal of the config"
    , gen: pure exampleConfig
    , functions:
        let
          f ::
            forall s.
            HMap (ForAllMapping (Nullable String)) Config.Props (SubRecord s) =>
            Config.Props -> SubRecord s
          f = hmap (ForAllMapping (notNull <<< unsafeStringify))
        in
          [ benchFn "trivialTraversalConfig                " \v -> f v
          , benchFn "trivialTraversalConfigPartiallyApplied" f
          ]
    }

main :: Effect Unit
main =
  for_
    [ benchResolveConfig
    , benchSimpleTraversalConfig
    , benchSimpleTraversalResolvedConfig
    , benchTrivialTraversalConfig
    ]
    benchmarkToStdoutReadable
