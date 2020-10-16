module Test.Main where

import Test.Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Data.SubRecord as Data.SubRecord
import Test.Data.SubRecord.Heterogeneous as Data.SubRecord.Heterogeneous
import Test.Data.SubRecord.Heterogeneous.Nullable as Data.SubRecord.Heterogeneous.Nullable
import Test.Extra.Data.Row as Extra.Data.Row
import Test.Extra.Data.String as Extra.Data.String
import Test.Extra.Heterogeneous as Extra.Heterogeneous
import Test.Extra.Heterogeneous.ForAll as Extra.Heterogeneous.ForAll
import Test.Extra.Heterogeneous.Loosely as Extra.Heterogeneous.Loosely
import Test.Extra.Heterogeneous.Recursive as Extra.Heterogeneous.Recursive
import Test.Extra.Heterogeneous.Sequence as Extra.Heterogeneous.Sequence
import Test.Extra.Heterogeneous.Zip as Extra.Heterogeneous.Zip
import Test.HyperWidgetConfig.Config.Components.PrimaryButton as HyperWidgetConfig.Config.Components.PrimaryButton
import Test.HyperWidgetConfig.Config.Containers.GridItem as HyperWidgetConfig.Config.Containers.GridItem
import Test.HyperWidgetConfig.Config.RequireProps as HyperWidgetConfig.Config.RequireProps
import Test.HyperWidgetConfig.Reify as HyperWidgetConfig.Reify
import Test.HyperWidgetConfig.Resolve as HyperWidgetConfig.Resolve
import Test.HyperWidgetConfig.Resolve.ToConfigTree as HyperWidgetConfig.Resolve.ToConfigTree
import Test.HyperWidgetConfig.Resolve.ConfigTree.Probe as HyperWidgetConfig.Resolve.ConfigTree.Probe
import Test.HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents as HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents
import Test.HyperWidgetConfig.Resolve.ConfigTree.Value as HyperWidgetConfig.Resolve.ConfigTree.Value
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ do
  runSpec [ consoleReporter ] do
    describe "Data" do
      describe "SubRecord" do
        describe "Heterogeneous" do
          describe "Nullable" Data.SubRecord.Heterogeneous.Nullable.spec
          Data.SubRecord.Heterogeneous.spec
        Data.SubRecord.spec
    describe "Extra" do
      describe "Data" do
        describe "Row" Extra.Data.Row.spec
        describe "String" Extra.Data.String.spec
      describe "Heterogeneous" do
        describe "ForAll" Extra.Heterogeneous.ForAll.spec
        describe "Loosely" Extra.Heterogeneous.Loosely.spec
        describe "Recursive" Extra.Heterogeneous.Recursive.spec
        describe "Sequence" Extra.Heterogeneous.Sequence.spec
        describe "Zip" Extra.Heterogeneous.Zip.spec
        Extra.Heterogeneous.spec
    describe "HyperWidgetConfig" do
      describe "Config" do
        describe "Components" do
          describe "PrimaryButton" HyperWidgetConfig.Config.Components.PrimaryButton.spec
        describe "Containers" do
          describe "GridItem" HyperWidgetConfig.Config.Containers.GridItem.spec
        describe "RequireProps" HyperWidgetConfig.Config.RequireProps.spec
      describe "Reify" HyperWidgetConfig.Reify.spec
      describe "Resolve" do
        describe "ToConfigTree" HyperWidgetConfig.Resolve.ToConfigTree.spec
        describe "ConfigTree" do
          describe "Probe" HyperWidgetConfig.Resolve.ConfigTree.Probe.spec
          describe "ResolveComponents" HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents.spec
          describe "Value" HyperWidgetConfig.Resolve.ConfigTree.Value.spec
        HyperWidgetConfig.Resolve.spec
