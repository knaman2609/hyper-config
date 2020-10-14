module Test.HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents (spec) where

import Test.Prelude
import Control.Comonad.Env (env)
import Control.Monad.Except (runExcept)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Extra.Data.Lens.Indexed (fromPath)
import Foreign.Object as Object
import HyperWidgetConfig.Resolve.ConfigTree.Probe (probeComponentDefinition)
import HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents (resolveComponentDefinition, resolveComponents, toConfigRoot)
import HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents as ResolveComponents
import HyperWidgetConfig.Resolve.ConfigTree.Types (ComponentDefinition(..), ComponentReference(..), ConfigRoot(..), ConfigTree(..), PropsTree(..))
import HyperWidgetConfig.Resolve.ConfigTree.Value (mkValue)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)

exampleConfigRoot :: ConfigRoot
exampleConfigRoot =
  ConfigRoot
    $ Object.fromHomogeneous
        { themeConfig:
            Section
              $ Object.fromHomogeneous
                  { styling:
                      Section
                        $ Object.fromHomogeneous
                            { margin: Value $ mkValue 123
                            }
                  }
        , components:
            Section
              $ Object.fromHomogeneous
                  { twoLineItem:
                      Component
                        $ ComponentDefinition
                            { props:
                                Object.fromHomogeneous
                                  { margin: Prop $ mkValue 12
                                  }
                            , states: Object.fromHomogeneous {}
                            , subComponents: Object.fromHomogeneous {}
                            , identifier: "components.listItem"
                            }
                  , oneLineItem:
                      Component
                        $ ComponentDefinition
                            { props:
                                Object.fromHomogeneous
                                  { margin: Prop $ mkValue 10
                                  }
                            , states: Object.fromHomogeneous {}
                            , subComponents: Object.fromHomogeneous {}
                            , identifier: "components.listItem"
                            }
                  }
        , containers:
            Section
              $ Object.fromHomogeneous
                  { list:
                      Component
                        $ ComponentDefinition
                            { props:
                                Object.fromHomogeneous
                                  { background: Prop $ mkValue "green"
                                  }
                            , states: Object.fromHomogeneous {}
                            , subComponents:
                                Object.fromHomogeneous
                                  { listItem:
                                      ComponentReference
                                        { component:
                                            { reference: Just "components.oneLineItem"
                                            , identifier: "components.listItem"
                                            }
                                        , props:
                                            Object.fromHomogeneous
                                              { margin: Prop $ mkValue 5
                                              }
                                        , states: Object.fromHomogeneous {}
                                        , subComponents: Object.fromHomogeneous {}
                                        }
                                  }
                            , identifier: "containers.list"
                            }
                  }
        , screens:
            Section
              $ Object.fromHomogeneous
                  { netBanking:
                      Component
                        $ ComponentDefinition
                            { props: Object.fromHomogeneous {}
                            , states: Object.fromHomogeneous {}
                            , identifier: "screens.netBanking"
                            , subComponents:
                                Object.fromHomogeneous
                                  { list:
                                      ComponentReference
                                        { component:
                                            { reference: Just "containers.list"
                                            , identifier: "containers.list"
                                            }
                                        , props:
                                            Object.fromHomogeneous
                                              { background: Prop $ mkValue "red"
                                              }
                                        , states: Object.fromHomogeneous {}
                                        , subComponents:
                                            Object.fromHomogeneous
                                              { listItem:
                                                  ComponentReference
                                                    { component:
                                                        { reference: Just "components.twoLineItem"
                                                        , identifier: "components.listItem"
                                                        }
                                                    , props:
                                                        Object.fromHomogeneous {}
                                                    -- {
                                                    --   margin: Prop $ mkValue 3
                                                    -- }
                                                    , states: Object.fromHomogeneous {}
                                                    , subComponents: Object.fromHomogeneous {}
                                                    }
                                              }
                                        }
                                  }
                            }
                  }
        }

spec :: Spec Unit
spec = do
  describe "resolveComponentDefinition" do
    it "should pass example" do
      let
        cd = unsafePartial fromRight $ runExcept $ probeComponentDefinition (fromPath "screens.netBanking") exampleConfigRoot

        e = env { config: exampleConfigRoot, path: fromPath "screens.netBanking" } cd

        result = ResolveComponents.runF (resolveComponentDefinition e)

        expected =
          pure
            $ ComponentDefinition
                { props: Object.fromHomogeneous {}
                , states: Object.fromHomogeneous {}
                , identifier: "screens.netBanking"
                , subComponents:
                    Object.fromHomogeneous
                      { list:
                          ComponentReference
                            { component:
                                { reference: Just "containers.list"
                                , identifier: "containers.list"
                                }
                            , props:
                                Object.fromHomogeneous
                                  { background: Prop $ mkValue "red"
                                  }
                            , states: Object.fromHomogeneous {}
                            , subComponents:
                                Object.fromHomogeneous
                                  { listItem:
                                      ComponentReference
                                        { component:
                                            { reference: Just "components.twoLineItem"
                                            , identifier: "components.listItem"
                                            }
                                        , props:
                                            Object.fromHomogeneous
                                              { margin: Prop $ mkValue 5
                                              }
                                        , states: Object.fromHomogeneous {}
                                        , subComponents: Object.fromHomogeneous {}
                                        }
                                  }
                            }
                      }
                }
      result `shouldEqual` expected
  describe "resolveComponents" do
    it "should pass example" do
      let
        result = toConfigRoot <$> resolveComponents exampleConfigRoot

        expected =
          pure $ ConfigRoot
            $ Object.fromHomogeneous
                { themeConfig:
                    Section
                      $ Object.fromHomogeneous
                          { styling:
                              Section
                                $ Object.fromHomogeneous
                                    { margin: Value $ mkValue 123
                                    }
                          }
                , components:
                    Section
                      $ Object.fromHomogeneous
                          { twoLineItem:
                              Component
                                $ ComponentDefinition
                                    { props:
                                        Object.fromHomogeneous
                                          { margin: Prop $ mkValue 12
                                          }
                                    , states: Object.fromHomogeneous {}
                                    , subComponents: Object.fromHomogeneous {}
                                    , identifier: "components.listItem"
                                    }
                          , oneLineItem:
                              Component
                                $ ComponentDefinition
                                    { props:
                                        Object.fromHomogeneous
                                          { margin: Prop $ mkValue 10
                                          }
                                    , states: Object.fromHomogeneous {}
                                    , subComponents: Object.fromHomogeneous {}
                                    , identifier: "components.listItem"
                                    }
                          }
                , containers:
                    Section
                      $ Object.fromHomogeneous
                          { list:
                              Component
                                $ ComponentDefinition
                                    { props:
                                        Object.fromHomogeneous
                                          { background: Prop $ mkValue "green"
                                          }
                                    , states: Object.fromHomogeneous {}
                                    , subComponents:
                                        Object.fromHomogeneous
                                          { listItem:
                                              ComponentReference
                                                { component:
                                                    { reference: Just "components.oneLineItem"
                                                    , identifier: "components.listItem"
                                                    }
                                                , props:
                                                    Object.fromHomogeneous
                                                      { margin: Prop $ mkValue 5
                                                      }
                                                , states: Object.fromHomogeneous {}
                                                , subComponents: Object.fromHomogeneous {}
                                                }
                                          }
                                    , identifier: "containers.list"
                                    }
                          }
                , screens:
                    Section
                      $ Object.fromHomogeneous
                          { netBanking:
                              Component
                                $ ComponentDefinition
                                    { props: Object.fromHomogeneous {}
                                    , states: Object.fromHomogeneous {}
                                    , identifier: "screens.netBanking"
                                    , subComponents:
                                        Object.fromHomogeneous
                                          { list:
                                              ComponentReference
                                                { component:
                                                    { reference: Just "containers.list"
                                                    , identifier: "containers.list"
                                                    }
                                                , props:
                                                    Object.fromHomogeneous
                                                      { background: Prop $ mkValue "red"
                                                      }
                                                , states: Object.fromHomogeneous {}
                                                , subComponents:
                                                    Object.fromHomogeneous
                                                      { listItem:
                                                          ComponentReference
                                                            { component:
                                                                { reference: Just "components.twoLineItem"
                                                                , identifier: "components.listItem"
                                                                }
                                                            , props:
                                                                Object.fromHomogeneous
                                                                  { margin: Prop $ mkValue 5
                                                                  }
                                                            , states: Object.fromHomogeneous {}
                                                            , subComponents: Object.fromHomogeneous {}
                                                            }
                                                      }
                                                }
                                          }
                                    }
                          }
                }
      result `shouldEqual` expected
