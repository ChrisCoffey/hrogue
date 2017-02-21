module Engine.Types where

import Data.Matrix
import Numeric.Natural

type ID = String
data NPCStyle 
    = Enemy 
    | Neutral
    | Friendly

data NPC = NPC {
    npcName :: String,
    npcStats :: Stats,
    npcEquipment :: Equipment,
    npcInventory :: Inventory,
    npcLocation :: Cell
    }

type Level =  Matrix LevelPart
data LevelPart 
    = HWall
    | Door
    | VWall
    | Floor
    deriving (Eq)

instance Show LevelPart where
    show HWall = "_"
    show Door = "/"
    show VWall = "|"
    show Floor = " "


type Range = (Natural, Natural)

data Game = Game {
        settings :: String,
        map :: [Level],
        state:: GameState
    }
    
data GameState = GameState {
    player :: PlayerState,
    npcs :: [NPC]
    }

type Cell = (Int, Int)

data PlayerState = PlayerState {
    playerName:: String,
    playerInventory :: Inventory,
    playerEquipment :: Equipment,
    playerStats :: Stats,
    playerLocation :: Cell
    }

data Stats = Stats {
        level :: Natural,
        health :: Natural,
        strength :: Natural,
        endurance :: Natural,
        intelligence :: Natural,
        agility :: Natural,
        dexterity :: Natural
    }

type EffectChance = Float

data WeaponStats = WeaponStats {
    wRequirements :: Stats -> Bool,
    wRange :: Natural,
    wAccuracy :: Float, -- A number between 0 & 1
    wDamage :: Range
    }

data ArmorStats = ArmorStats {
    aRequirements :: Stats -> Bool,
    aAbsorbtion :: Natural
    }

data Equipment = Equipment {
        armor :: Maybe Item,
        weapon :: Maybe Item,
        utilty :: Maybe Item
    }

type Inventory = [Item]

data Item 
    = Consumable {cName :: String, cDesc :: String, cEffect :: Stats -> Stats}
    | Weapon {wName :: String, wDesc :: String, wStats :: WeaponStats}
    | Armor {aName :: String, aDesc:: String, aStats :: ArmorStats}
