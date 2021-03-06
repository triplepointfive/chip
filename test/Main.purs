module Test.Main where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Chip.Action (Action(..), Sound(..))
import Chip.Action.AI (actAI)
import Chip.Action.MovePlayer (movePlayer)
import Chip.Level.Build (build)
import Chip.Model (has, Level, Color(..), Direction(..), Enemy(..), Item(..), Tile(..) )

level1 :: Level
level1 = build
  { grid:
      [ "#@ "
      , "#<#"
      ]
  , name: "Spec"
  , hint: Nothing
  , chips: 1
  , timeLimit: Nothing
  , blocks: []
  , trapConnections: Nothing
  }

level2 :: Level
level2 = build
  { grid:
      [ "#?#"
      , "+@-"
      , "# #"
      ]
  , name: "Spec"
  , hint: Nothing
  , chips: 1
  , timeLimit: Nothing
  , blocks: []
  , trapConnections: Nothing
  }

level3 :: Level
level3 = build
  { grid:
      [ "#R#"
      , "r@g"
      , "#G#"
      ]
  , name: "Spec"
  , hint: Nothing
  , chips: 1
  , timeLimit: Nothing
  , blocks: []
  , trapConnections: Nothing
  }

level4 :: Level
level4 = build
  { grid:
      [ "@##"
      , "#b#"
      , "#b#"
      , "#b#"
      , "###"
      ]
  , name: "Spec"
  , hint: Nothing
  , chips: 0
  , timeLimit: Nothing
  , blocks: []
  , trapConnections: Nothing
  }

main :: Effect Unit
main = run [consoleReporter] do
  describe "movePlayer" do
    describe "into a wall" do
      let { result: level, actions } = movePlayer true Left level1
      it "stuck sound" do
        actions `shouldEqual` [PlaySound Oof]
      it "doesn't move" do
        level.player.pos `shouldEqual` { x: 1, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Left

    describe "out of map" do
      let { result: level, actions } = movePlayer true Up level1
      it "stuck sound" do
        actions `shouldEqual` [PlaySound Oof]
      it "doesn't move" do
        level.player.pos `shouldEqual` { x: 1, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Up

    describe "on empty floor" do
      let { result: level, actions } = movePlayer true Right level1
      it "without actions" do
        actions `shouldEqual` []
      it "changes position" do
        level.player.pos `shouldEqual` { x: 2, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Right

    describe "on exit" do
      let { result: level, actions } = movePlayer true Down level1
      it "produces complete action" do
        actions `shouldEqual` [Complete]
      it "changes position" do
        level.player.pos `shouldEqual` { x: 1, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Down

    describe "on chip" do
      let { result: level, actions } = movePlayer true Left level2
      it "pick sound" do
        actions `shouldEqual` [PlaySound PickUpChip]
      it "changes position" do
        level.player.pos `shouldEqual` { x: 0, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Left
      it "withdraws chip" do
        level.chipsLeft `shouldEqual` 0
      it "removes chip from map" do
        (Map.lookup { x: 0, y: 1 } level.tiles) `shouldEqual` Nothing

    describe "on hint" do
      let { result: level, actions } = movePlayer true Up level2
      it "without actions" do
        actions `shouldEqual` []
      it "changes position" do
        level.player.pos `shouldEqual` { x: 1, y: 0 }
      it "turns" do
        level.player.direction `shouldEqual` Up
      it "leaves hint" do
        (Map.lookup { x: 1, y: 0} level.tiles) `shouldEqual` Just Hint

    -- TODO: Right 2 down 2

    describe "on common key" do
      let { result: level, actions } = movePlayer true Left level3
      it "pickup sound" do
        actions `shouldEqual` [PlaySound PickUpItem]
      it "changes position" do
        level.player.pos `shouldEqual` { x: 0, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Left
      it "removes key from map" do
        (Map.lookup { x: 0, y: 1 } level.tiles) `shouldEqual` Nothing
      it "adds key to inventory" do
        has (Key Red) level.inventory `shouldEqual` true

    describe "on common door" do
      describe "without a key" do
        let { result: level, actions } = movePlayer true Up level3
        it "stuck sound" do
          actions `shouldEqual` [PlaySound Oof]
        it "doesn't move" do
          level.player.pos `shouldEqual` { x: 1, y: 1 }
        it "turns" do
          level.player.direction `shouldEqual` Up
      describe "with a key" do
        let { result: level, actions } = movePlayer true Up (level3 { inventory { red = 1 } })
        it "open sound" do
          actions `shouldEqual` [PlaySound DoorOpen]
        it "changes position" do
          level.player.pos `shouldEqual` { x: 1, y: 0 }
        it "turns" do
          level.player.direction `shouldEqual` Up
        it "removes door from map" do
          (Map.lookup { x: 1, y: 0 } level.tiles) `shouldEqual` Nothing
        it "takes key from inventory" do
          has (Key Red) level.inventory `shouldEqual` false

    describe "on green key" do
      let { result: level, actions } = movePlayer true Right level3
      it "pickup sound" do
        actions `shouldEqual` [PlaySound PickUpItem]
      it "changes position" do
        level.player.pos `shouldEqual` { x: 2, y: 1 }
      it "turns" do
        level.player.direction `shouldEqual` Right
      it "removes key from map" do
        (Map.lookup { x: 2, y: 1 } level.tiles) `shouldEqual` Nothing
      it "adds key to inventory" do
        has (Key Green) level.inventory `shouldEqual` true

    describe "on green door" do
      describe "without a key" do
        let { result: level, actions } = movePlayer true Down level3
        it "stuck sound" do
          actions `shouldEqual` [PlaySound Oof]
        it "doesn't move" do
          level.player.pos `shouldEqual` { x: 1, y: 1 }
        it "turns" do
          level.player.direction `shouldEqual` Down
      describe "with a key" do
        let { result: level, actions } = movePlayer true Down (level3 { inventory { green = true } })
        it "open sound" do
          actions `shouldEqual` [PlaySound DoorOpen]
        it "changes position" do
          level.player.pos `shouldEqual` { x: 1, y: 2 }
        it "turns" do
          level.player.direction `shouldEqual` Down
        it "removes door from map" do
          (Map.lookup { x: 1, y: 2 } level.tiles) `shouldEqual` Nothing
        it "leaves key in inventory" do
          has (Key Green) level.inventory `shouldEqual` true

  describe "actAI" do
    describe "can not move" do
      let { result: level, actions } = actAI (actAI level4).result
      it "doesn't move" do
        level.enemies `shouldEqual` Map.fromFoldable
          [ Tuple { x: 1, y: 1 } (Bee Up)
          , Tuple { x: 1, y: 2 } (Bee Up)
          , Tuple { x: 1, y: 3 } (Bee Up)
          ]
