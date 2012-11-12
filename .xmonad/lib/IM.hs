{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

module IM (
        Property(..), withIM, gridIM
) where

import XMonad
import qualified XMonad.StackSet as S
import Data.List
import XMonad.Layout (splitHorizontallyBy)
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties
import Control.Monad

-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRoster a = AddRoster Rational [Property] deriving (Read, Show)
 
instance LayoutModifier AddRoster Window where
  modifyLayout (AddRoster ratio prop) = applyIM ratio prop
  modifierDescription _                = "IM"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIM :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRoster l a
withIM ratio prop = ModifiedLayout $ AddRoster ratio prop
 
-- | IM layout modifier applied to the Grid layout
gridIM :: Rational -> [Property] -> ModifiedLayout AddRoster Grid a
gridIM ratio prop = withIM ratio prop Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w
 
-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIM :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIM ratio prop wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty prop) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy ratio rect
    let rosterRects = splitVertically n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
