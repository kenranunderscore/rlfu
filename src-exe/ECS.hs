{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module ECS where

-- Let's see if we can somehow capture the "essence" of an ECS (modulo
-- performance; though that seems to be one of the main points or
-- /the/ main point, so it just might not work at all...) by
-- implementing it in the most naive way possible.  As far as I can
-- tell this means at least separating entities from components, and
-- storing components contiguously, so the 'Entity' type doesn't have
-- a direct reference to its signature/collection of components.

-- I'm following this tutorial and sometimes decide to just do it
-- similar to what's shown there:

-- https://austinmorlan.com/posts/entity_component_system/#the-entity

-- If there's a more Haskell way of doing things, I might refactor
-- that later on, but I want to get to a point where I can profile and
-- examine the program first.

import Control.Exception (assert)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq(..), (|>))

newtype Entity = Entity { entityId :: Int }
  deriving newtype (Eq, Show, Enum)

instance Bounded Entity where
  minBound = Entity 0
  maxBound = Entity 10_000

allEntities :: Seq Entity
allEntities = Sequence.fromList [minBound .. maxBound]

createEntity :: Seq entity -> (entity, Seq entity)
createEntity = \case
  Sequence.Empty -> error "should not happen"
  entity :<| rest -> (entity, rest)

destroyEntity :: Seq entity -> entity -> Seq entity
destroyEntity entities entity =
  entities |> entity

type EntityToIndex = IntMap Int
type IndexToEntity = IntMap Entity

data ComponentArray a = ComponentArray
  { components :: Vector a
  , entityToIndex :: EntityToIndex
  , indexToEntity :: IndexToEntity
  , size :: Int -- dunno if that's needed, but I think I can keep
                -- garbage at the end and only iterate up to this
  }
  deriving stock (Show)

emptyComponentArray :: ComponentArray a
emptyComponentArray =
  ComponentArray Vector.empty IntMap.empty IntMap.empty 0

addComponent ::
  Entity ->
  a ->
  ComponentArray a ->
  ComponentArray a
addComponent entity component arr =
  assert (not (IntMap.member (entityId entity) (entityToIndex arr))) $
  let comps = components arr
      newIndex = size arr
      entityToIndex' = IntMap.insert (entityId entity) newIndex $ entityToIndex arr
      indexToEntity' = IntMap.insert newIndex entity $ indexToEntity arr
      newComponents =
        if newIndex >= Vector.length comps
        then Vector.snoc comps component
        else comps Vector.// [(newIndex, component)]
  in ComponentArray
       newComponents
       entityToIndex'
       indexToEntity'
       (newIndex + 1)

removeComponent ::
  Entity ->
  ComponentArray a ->
  ComponentArray a
removeComponent entity arr =
  let indexOfRemovedEntity = entityToIndex arr IntMap.! entityId entity
      comps = components arr
      lastIndex = Vector.length comps - 1
      lastEntity = indexToEntity arr IntMap.! lastIndex
  in ComponentArray
       (comps Vector.// [(indexOfRemovedEntity, comps Vector.! lastIndex)])
       (IntMap.delete (entityId lastEntity)
         $ IntMap.insert (entityId lastEntity) indexOfRemovedEntity
         $ entityToIndex arr)
       (IntMap.delete lastIndex
         $ IntMap.insert indexOfRemovedEntity lastEntity
         $ indexToEntity arr)
       (size arr - 1)

getComponent :: Entity -> ComponentArray a -> a
getComponent entity arr =
  let index = entityToIndex arr IntMap.! entityId entity
  in components arr Vector.! index
