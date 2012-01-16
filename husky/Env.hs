module Env
( World(World), Body(Body)
) where

import qualified AI.Core as Ai


data World = World	{
} deriving ()

instance Ai.World World

data Body = Body	{
} deriving ()

instance Ai.Body Body
