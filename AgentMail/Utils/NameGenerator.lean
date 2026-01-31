/-
  AgentMail.Utils.NameGenerator - Generate memorable agent names
-/

namespace AgentMail.Utils.NameGenerator

/-- Adjectives for name generation -/
def adjectives : Array String := #[
  "Azure", "Bright", "Calm", "Dawn", "Ember",
  "Fleet", "Golden", "Hidden", "Iron", "Jade",
  "Keen", "Lunar", "Misty", "Noble", "Olive",
  "Pale", "Quick", "Rustic", "Silver", "Tidal",
  "Urban", "Vivid", "Warm", "Young", "Zealous",
  "Amber", "Bold", "Coral", "Deep", "Echo",
  "Frost", "Green", "Hazy", "Ivory", "Jolly",
  "Kind", "Light", "Maple", "Nimble", "Oak",
  "Prime", "Quiet", "Royal", "Stone", "True",
  "Ultra", "Vast", "Wild", "Xeric", "Zephyr"
]

/-- Nouns for name generation -/
def nouns : Array String := #[
  "Anchor", "Bridge", "Castle", "Delta", "Eagle",
  "Falcon", "Garden", "Harbor", "Island", "Jasper",
  "Kite", "Lake", "Mountain", "Nebula", "Orchard",
  "Peak", "Quartz", "River", "Summit", "Tower",
  "Umbra", "Valley", "Willow", "Xylon", "Yard",
  "Apex", "Brook", "Crest", "Drift", "Edge",
  "Forge", "Grove", "Haven", "Inlet", "Junction",
  "Knoll", "Ledge", "Mesa", "Nest", "Oasis",
  "Pier", "Quest", "Ridge", "Shore", "Trail",
  "Unity", "Vault", "Wave", "Zenith", "Zone"
]

/-- Simple LCG random number generator -/
def lcgNext (seed : UInt64) : UInt64 :=
  seed * 6364136223846793005 + 1442695040888963407

/-- Generate a random index from a seed -/
def randomIndex (seed : UInt64) (size : Nat) : Nat :=
  (seed.toNat % size)

/-- Generate a name from a seed -/
def generateNameFromSeed (seed : UInt64) : String :=
  let adjIdx := randomIndex seed adjectives.size
  let seed' := lcgNext seed
  let nounIdx := randomIndex seed' nouns.size
  let adj := adjectives.getD adjIdx "Unknown"
  let noun := nouns.getD nounIdx "Agent"
  s!"{adj}{noun}"

/-- Generate a random name using IO for randomness -/
def generateName : IO String := do
  let nanos ← IO.monoNanosNow
  pure (generateNameFromSeed nanos.toUInt64)

/-- Generate a name from a provided seed (for deterministic testing) -/
def generateNameDeterministic (seed : Nat) : String :=
  generateNameFromSeed seed.toUInt64

end AgentMail.Utils.NameGenerator
