module Phys where

data Pstate = Pstate {stx :: Float, stv :: Float} deriving (Show)
data Derivative = Derivative {dx :: Float, dv :: Float} deriving (Show)
data Config = Config {kvalue :: Float, bvalue :: Float} deriving (Show)

acceleration :: Config -> Pstate -> Float -> Float
acceleration config state t = (-1.0) * (kvalue config) * (stx state) - (bvalue config * (stv state))

evaluate :: Config -> Pstate -> Float -> Float -> Derivative -> (Pstate, Derivative)
evaluate config initial_state t dt initial_derivative = (new_state, output) where
    new_state_x = (stx initial_state) + (dx initial_derivative) * dt
    new_state_v = (stv initial_state) + (dv initial_derivative) * dt
    new_state = Pstate new_state_x new_state_v
    output = Derivative (stv new_state) (acceleration config new_state (t + dt))

evaluate' :: Pstate -> Float -> Float -> Derivative -> (Pstate, Derivative)
evaluate' = evaluate base_config where
    base_config = Config 10 1
