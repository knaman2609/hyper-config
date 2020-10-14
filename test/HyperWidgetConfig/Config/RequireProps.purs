module Test.HyperWidgetConfig.Config.RequireProps (spec) where

import Test.Prelude
import Data.Nullable (null)
import Data.SubRecord (SubRecord, mkSubRecord)
import HyperWidgetConfig.Config (isRequiredProps)
import HyperWidgetConfig.Config.RequireProps (isRequiredProps')
import HyperWidgetConfig.Reference (Reference(..))

spec :: Spec Unit
spec = do
  describe "RequireProps" do
    itCompileTimeCheck "should pass" do
      let
        _ = isRequiredProps' (any :: SubRecord ( foo :: Reference Int )) { foo: Reference "qwer" }
      pure unit
    itCompileTimeCheck "should pass 2" do
      let
        _ =
          isRequiredProps'
            ( any ::
                SubRecord
                  ( foo :: Int
                  , components ::
                      SubRecord
                        ( primaryButton ::
                            SubRecord ( padding :: Int )
                        )
                  , containers ::
                      SubRecord
                        ( gridItem ::
                            SubRecord
                              ( primaryButton ::
                                  SubRecord
                                    ( padding :: Int
                                    , component :: String
                                    )
                              )
                        )
                  )
            )
            { foo: 123
            , components:
                { primaryButton:
                    { padding: 123 }
                }
            , containers:
                { gridItem:
                    { primaryButton:
                        { component: "qwer"
                        , padding: null
                        }
                    }
                }
            }
      pure unit
    itCompileTimeCheck "should pass" do
      let
        _ =
          isRequiredProps
            { themeConfig:
                { padding: 1
                , margin: 12
                }
            , components:
                { primaryButton:
                    { margin: Literal 123
                    , states: mkSubRecord {}
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
                    , states: mkSubRecord {}
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
                    , states: mkSubRecord {}
                    }
                }
            }
      pure unit
