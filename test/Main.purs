module Test.Main where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Level as Level
import Utils (Direction(..))

level1 :: Level.Level
level1 = Level.build
  { grid:
      [ "#@ "
      , "#<#"
      ]
  , name: "Spec"
  , hint: Nothing
  , chips: 1
  }

level2 :: Level.Level
level2 = Level.build
  { grid:
      [ "#?#"
      , "+@-"
      , "# #"
      ]
  , name: "Spec"
  , hint: Nothing
  , chips: 1
  }

level3 :: Level.Level
level3 = Level.build
  { grid:
      [ "#R#"
      , "r@g"
      , "#G#"
      ]
  , name: "Spec"
  , hint: Nothing
  , chips: 1
  }

main :: Effect Unit
main = run [consoleReporter] do
  describe "move" do
    describe "into a wall" do
      let Tuple level actions = Level.movePlayer Left level1
      pending "stuck sound"
      it "doesn't move" do
        level.player.pos `shouldEqual` { x: 1, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Left

    describe "out of map" do
      let Tuple level actions = Level.movePlayer Up level1
      pending "stuck sound"
      it "doesn't move" do
        level.player.pos `shouldEqual` { x: 1, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Up

    describe "on empty floor" do
      let Tuple level actions = Level.movePlayer Right level1
      it "without actions" do
        actions `shouldEqual` []
      it "changes position" do
        level.player.pos `shouldEqual` { x: 2, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Right

    describe "on exit" do
      let Tuple level actions = Level.movePlayer Down level1
      it "produces complete action" do
        actions `shouldEqual` [Level.CompleteLevel]
      it "changes position" do
        level.player.pos `shouldEqual` { x: 1, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Down

    describe "on chip" do
      let Tuple level actions = Level.movePlayer Left level2
      pending "pick sound"
      it "changes position" do
        level.player.pos `shouldEqual` { x: 0, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Left
      it "withdraws chip" do
        level.chipsLeft `shouldEqual` 0
      it "removes chip from map" do
        (Map.lookup { x: 0, y: 1 } level.tiles) `shouldEqual` Nothing

    describe "on hint" do
      let Tuple level actions = Level.movePlayer Up level2
      it "without actions" do
        actions `shouldEqual` []
      it "changes position" do
        level.player.pos `shouldEqual` { x: 1, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Up
      it "leaves hint" do
        (Map.lookup { x: 1, y: 0} level.tiles) `shouldEqual` Just Level.Hint

    -- TODO: Right 2 down 2

    describe "on common key" do
      let Tuple level actions = Level.movePlayer Left level3
      it "without actions" do
        actions `shouldEqual` []
      it "changes position" do
        level.player.pos `shouldEqual` { x: 0, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Left
      it "removes key from map" do
        (Map.lookup { x: 0, y: 1 } level.tiles) `shouldEqual` Nothing
      it "adds key to inventory" do
        Level.hasKey Level.Red level `shouldEqual` true

    describe "on common door" do
      describe "without a key" do
        let Tuple level actions = Level.movePlayer Up level3
        pending "stuck sound"
        it "doesn't move" do
          level.player.pos `shouldEqual` { x: 1, y: 1 }
        it "turns" do
          level.player.direction `shouldEqual` Up
      describe "with a key" do
        let Tuple level actions = Level.movePlayer Up (level3 { inventory { red = 1 } })
        it "without actions" do
          actions `shouldEqual` []
        it "changes position" do
          level.player.pos `shouldEqual` { x: 1, y: 0 }
        it "turns" do
          level.player.direction `shouldEqual` Up
        it "removes door from map" do
          (Map.lookup { x: 1, y: 0 } level.tiles) `shouldEqual` Nothing
        it "takes key from inventory" do
          Level.hasKey Level.Red level `shouldEqual` false

    describe "on green key" do
      let Tuple level actions = Level.movePlayer Right level3
      it "without actions" do
        actions `shouldEqual` []
      it "changes position" do
        level.player.pos `shouldEqual` { x: 2, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Right
      it "removes key from map" do
        (Map.lookup { x: 2, y: 1 } level.tiles) `shouldEqual` Nothing
      it "adds key to inventory" do
        Level.hasKey Level.Green level `shouldEqual` true

    describe "on green door" do
      describe "without a key" do
        let Tuple level actions = Level.movePlayer Down level3
        pending "stuck sound"
        it "doesn't move" do
          level.player.pos `shouldEqual` { x: 1, y: 1 }
        it "turns" do
          level.player.direction `shouldEqual` Down
      describe "with a key" do
        let Tuple level actions = Level.movePlayer Down (level3 { inventory { green = true } })
        it "without actions" do
          actions `shouldEqual` []
        it "changes position" do
          level.player.pos `shouldEqual` { x: 1, y: 2 }
        it "turns" do
          level.player.direction `shouldEqual` Down
        it "removes door from map" do
          (Map.lookup { x: 1, y: 2 } level.tiles) `shouldEqual` Nothing
        it "leaves key in inventory" do
          Level.hasKey Level.Green level `shouldEqual` true
