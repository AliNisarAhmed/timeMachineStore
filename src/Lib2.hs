{-# LANGUAGE LambdaCase #-}

module Lib2 where

import           Lens.Micro.Platform

data Client i
  = GovOrg i String
  | Company i String Person String
  | Individual i Person
  deriving (Show)

data Person
  = Person String String
  deriving (Show)


firstName :: Lens' Person String
firstName = lens (\(Person f _) -> f)
                 (\(Person _ l) newF -> Person newF l)

lastName :: Lens' Person String
lastName = lens (\(Person _ l) -> l) (\(Person f _) newL -> Person f newL)

identifier :: Lens (Client i) (Client j) i j
identifier = lens (\case (GovOrg i _) -> i
                         (Company i _ _ _) -> i
                         (Individual i _) -> i)
                  (\client newId -> case client of
                    GovOrg _ n      -> GovOrg newId n
                    Company _ x y z -> Company newId x y z
                    Individual _ p  -> Individual newId p)
