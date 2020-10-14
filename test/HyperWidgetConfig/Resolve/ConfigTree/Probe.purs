module Test.HyperWidgetConfig.Resolve.ConfigTree.Probe (spec) where

import Test.Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (isRight)
import Data.Maybe (Maybe(..))
import Extra.Data.Lens.Indexed (fromPath)
import Foreign.Object as Object
import HyperWidgetConfig.Error (ResolveError(..))
import HyperWidgetConfig.Resolve.ConfigTree.Probe (probeComponentDefinition, probe_)
import HyperWidgetConfig.Resolve.ConfigTree.Types (ComponentDefinition(..), ComponentReference(..), ConfigRoot(..), ConfigTree(..), PropsTree(..))
import HyperWidgetConfig.Resolve.ConfigTree.Value (mkValue)
import Test.Spec.Assertions (shouldSatisfy)

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
                                                        Object.fromHomogeneous
                                                          { margin: Prop $ mkValue 3
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

spec :: Spec Unit
spec = do
  describe "probe_" do
    it "should pass" do
      let
        result = probe_ (fromPath "components") exampleConfigRoot
      runExcept result `shouldSatisfy` isRight
  describe "probeComponentDefinition" do
    it "should fail to find non existing path" do
      let
        result = probeComponentDefinition (fromPath "hello.world.not.found.path") exampleConfigRoot

        expected = throwError NotFound
      result `shouldEqualExcept` expected
    it "should find existing path" do
      let
        result = probeComponentDefinition (fromPath "containers.list") exampleConfigRoot

        expected =
          pure
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
      result `shouldEqualExcept` expected
