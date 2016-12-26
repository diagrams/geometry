{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TrailLike
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The 'TrailLike' class abstracts over anything which can be
-- constructed from a concretely located 'Trail', including
-- lines, loops, trails, paths, vertex lists, and diagrams.
--
-----------------------------------------------------------------------------

module Geometry.TrailLike
  (
  ) where

