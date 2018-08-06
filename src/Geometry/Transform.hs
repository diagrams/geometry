{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Transform
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Affine transformations, parameterized by any vector space.  For
-- transformations on particular vector spaces, see /e.g./
-- "Geometry.TwoD.Transform" and "Geometry.ThreeD.Transform".
--
-----------------------------------------------------------------------------

module Geometry.Transform
    ( -- * Transformations

    -- * Affine transformations
    Transformation(..)

    -- ** Extracting parts of a transformation
  , inv, transp, transl
  , dropTransl

    -- ** Applying transformations
  , apply
  , papply

    -- ** Contructing transformations
  , fromLinear
  , fromInvoluted
  , fromOrthogonal
  , eye

    -- ** Transformation utilities
  , basis
  , dimension
  , onBasis
  , listRep
  , matrixRep
  , matrixHomRep
  , determinant
  , isReflection
  , avgScale

    -- * The Transformable class

  , Transformable(..)

    -- * Translational invariance

  , TransInv(TransInv)

    -- * Vector space independent transformations
    -- | Most transformations are specific to a particular vector
    --   space, but a few can be defined generically over any
    --   vector space.

  , translation, translate
  , scaling, scale
  , scalingV, scaleV

    -- * Some specific transformations
  , moveTo, place

    -- * Miscellaneous transformation-related utilities
  , conjugate, underT, transformed, translated, movedTo, movedFrom

    -- * The HasOrigin class

  , HasOrigin(..), moveOriginBy

  ) where

import           Control.DeepSeq       (NFData (..))
import           Control.Lens          hiding (transform)
import           Control.Lens          (Rewrapped, Traversable, Wrapped (..),
                                        iso, (&), (.~))
import           Data.Binary           as Binary
import           Data.Bytes.Serial
import           Data.Distributive
import           Data.Foldable         (Foldable, toList)
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Hashable
import           Data.Hashable.Lifted
import           Data.HashMap.Lazy     (HashMap)
import           Data.IntMap           (IntMap)
import           Data.Map              (Map)
import           Data.Proxy
import           Data.Semigroup
import           Data.Sequence         (Seq)
import           Data.Serialize        as Cereal
import qualified Data.Set              as S
import           Data.Tree             (Tree)
import           Data.Typeable
import           Geometry.HasOrigin
import           Geometry.Space

import           Data.Monoid.Action
import           Data.Monoid.Deletable

import           Linear.Affine
import           Linear.Vector

import           Linear                hiding (conjugate, translation)

------------------------------------------------------------------------
--  Affine transformations
------------------------------------------------------------------------

-- | Affine transformations are represented by a transform matrix, its
--   invert and a transpose vector.
--
--   The reason we keep track of the inverse is for better numeric
--   stability.
--
--   For more general, non-invertible transformations, see
--   @Diagrams.LinearMap@ or @Diagrams.Deform@ (in @diagrams-lib@).

data Transformation v n = T !(v (v n)) !(v (v n)) !(v n)
  deriving Typeable

eqMatrix :: (Eq1 v, Eq n) => v (v n) -> v (v n) -> Bool
eqMatrix = liftEq eq1

instance (Eq1 v, Eq n) => Eq (Transformation v n) where
  T m1 mInv1 v1  == T m2 mInv2 v2 = eqMatrix m1 m2 && eqMatrix mInv1 mInv2 && eq1 v1 v2
  {-# INLINE (==) #-}

instance (Show1 v, Show n) => Show (Transformation v n) where
  showsPrec p (T m mInv v) = showParen (p > 10) $
    showString "T " . showMatrix 11 m
     . showChar ' ' . showMatrix 11 mInv
     . showChar ' ' . showsPrec1 11 v

showMatrix :: (Show1 v, Show n) => Int -> v (v n) -> ShowS
showMatrix = liftShowsPrec showsPrec1 (liftShowList showsPrec showList)

type instance V (Transformation v n) = v
type instance N (Transformation v n) = n

-- | The identity matrix.
eye :: (HasBasis v, Num n) => v (v n)
eye = tabulate $ \(E e) -> zero & e .~ 1
{-# INLINE eye #-}

-- | Invert a transformation.
inv :: (Additive v, Foldable v, Num n) => Transformation v n -> Transformation v n
inv (T m mInv v) = T mInv m (negated v *! mInv)
{-# INLINE inv #-}

-- | Get the translational component of a transformation.
transl :: Transformation v n -> v n
transl (T _ _ v) = v
{-# INLINE transl #-}

-- | Get the matrix transpose of (the linear part of) a transformation.
transp :: Distributive v => Transformation v n -> v (v n)
transp (T m _ _) = distribute m
{-# INLINE transp #-}

-- | Drop the translational component of a transformation, leaving only
--   the linear part.
dropTransl :: (Additive v, Num n) => Transformation v n -> Transformation v n
dropTransl (T a a' _) = T a a' zero
{-# INLINE dropTransl #-}

-- | Append (/i.e./ compose) two transformations. Satisfies the law
--
-- @
-- 'transform' (t1 `tappend` t2) a === 'transform' t1 ('transform' t2 a)
-- @
tappend :: (Additive v, Foldable v, Num n)
        => Transformation v n -> Transformation v n -> Transformation v n
tappend (T m1 m1Inv v1) (T m2 m2Inv v2)
    = T (m1 !*! m2) (m2Inv !*! m1Inv) (v1 ^+^ m1 !* v2)
{-# SPECIALIZE INLINE tappend :: Transformation V2 Double -> Transformation V2 Double -> Transformation V2 Double #-}
{-# SPECIALIZE INLINE tappend :: Transformation V3 Double -> Transformation V3 Double -> Transformation V3 Double #-}

-- | The empty, /i.e./ identity transformation.
tempty :: (HasBasis v, Num n) => Transformation v n
tempty = T eye eye zero
{-# INLINE tempty #-}

-- | Transformations are closed under composition; @t1 <> t2@ is the
--   transformation which performs first @t2@, then @t1@.
instance (Additive v, Foldable v, Num n) => Semigroup (Transformation v n) where
  (<>) = tappend
  {-# INLINE (<>) #-}

instance (HasBasis v, Foldable v, Num n) => Monoid (Transformation v n) where
  mempty  = tempty
  {-# INLINE mempty #-}
  mappend = tappend
  {-# INLINE mappend #-}

-- | Transformations can act on transformable things.
instance (Transformable a, V a ~ v, N a ~ n) => Action (Transformation v n) a where
  act = transform
  {-# INLINE act #-}

-- This isn't strictly correct because you could have a @v@ that isn't
-- entirly defined by its element. But this wouldn't make much sense to
-- use with a Transformation. We really need an NFData1 class
instance (Foldable v, NFData n) => NFData (Transformation v n) where
  rnf (T m n v) = rnfMat m `seq` rnfMat n `seq` rnfVec v
    where
      rnfVec = foldMap rnf
      rnfMat = foldMap rnfVec

instance Hashable1 v => Hashable1 (Transformation v) where
  liftHashWithSalt f s (T m n v) =
    liftHashWithSalt f (hashMatWithSalt (hashMatWithSalt s m) n) v
    where
      hashMatWithSalt = liftHashWithSalt (liftHashWithSalt f)
  {-# INLINE liftHashWithSalt #-}

instance (Hashable1 v, Hashable n) => Hashable (Transformation v n) where
  hashWithSalt = hashWithSalt1
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (Transformation v) where
  serializeWith f (T m n v) = do
    let serializeMat = serializeWith (serializeWith f)
    serializeMat m
    serializeMat n
    serializeWith f v
  deserializeWith f = do
    let deserializeMat = deserializeWith (deserializeWith f)
    m <- deserializeMat
    n <- deserializeMat
    v <- deserializeWith f
    return (T m n v)

instance (Serial1 v, Serial n) => Serial (Transformation v n) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Serial1 v, Binary n) => Binary (Transformation v n) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance (Serial1 v, Serialize n) => Serialize (Transformation v n) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get

-- | Apply a transformation to a vector.  Note that any translational
--   component of the transformation will not affect the vector, since
--   vectors are invariant under translation.
apply :: (Additive v, Foldable v, Num n) => Transformation v n -> v n -> v n
apply (T m _ _) = (m !*)
{-# INLINE apply #-}

-- | Apply a transformation to a point.
papply :: (Additive v, Foldable v, Num n) => Transformation v n -> Point v n -> Point v n
papply (T t _ v) = \(P p) -> P (t !* p ^+^ v)
{-# INLINE papply #-}

-- | Create a general affine transformation from an linear
--   transformation and its inverse. The translational component is
--   assumed to be zero.
fromLinear :: (Additive v, Num n) => v (v n) -> v (v n) -> Transformation v n
fromLinear l1 l2 = T l1 l2 zero
{-# INLINE fromLinear #-}

-- | Create a transformation from an involuted linear map, /i.e./ one
--   whose inverse is itself.
fromInvoluted :: (Additive v, Num n) => v (v n) -> Transformation v n
fromInvoluted m = fromLinear m m
{-# INLINE fromInvoluted #-}

-- | Create a transformation from an orthogonal linear map, /i.e./ one
--   one whose inverse is also its transpose.
fromOrthogonal :: (Additive v, Distributive v, Num n) => v (v n) -> Transformation v n
fromOrthogonal t = fromLinear t (transpose t)
{-# INLINE fromOrthogonal #-}

-- | Get the dimension of an object whose vector space is an instance of
--   @HasLinearMap@, e.g. transformations, paths, diagrams, etc.
dimension :: forall a. (Additive (V a), Traversable (V a)) => a -> Int
dimension = const (dimen (Proxy :: Proxy (V a)))
{-# INLINE dimension #-}

dimen :: forall (v :: * -> *) . (Additive v, Traversable v) => Proxy v -> Int
dimen = \_ -> length (basis :: [v Int])
{-# NOINLINE dimen #-}

{-# RULES
  "dimenV1" dimen = const 1 :: Proxy V1 -> Int;
  "dimenV2" dimen = const 2 :: Proxy V2 -> Int;
  "dimenV3" dimen = const 3 :: Proxy V3 -> Int;
  "dimenV4" dimen = const 4 :: Proxy V4 -> Int;
 #-}

-- | Get the matrix equivalent of the linear transform, (as a list of
--   columns) and the translation vector. This is mostly useful for
--   implementing backends.
onBasis :: Foldable v => Transformation v n -> ([v n], v n)
onBasis (T m _ v) = (toList m, v)

-- Remove the nth element from a list
remove :: Int -> [a] -> [a]
remove n xs = ys ++ tail zs
  where
    (ys, zs) = splitAt n xs

-- Minor matrix of cofactore C(i,j)
minor :: Int -> Int -> [[a]] -> [[a]]
minor i j xs = remove j $ map (remove i) xs

-- The determinant of a square matrix represented as a list of lists
-- representing column vectors, that is [column].
det :: Num a => [[a]] -> a
det (a:[]) = head a
det m = sum [(-1)^i * (c1 !! i) * det (minor i 0 m) | i <- [0 .. (n-1)]]
  where
    c1 = head m
    n = length m

-- | Convert a vector v to a list of scalars.
listRep :: Foldable v => v n -> [n]
listRep = toList

-- | Convert the linear part of a 'Transformation' to a matrix
--   representation as a list of column vectors which are also lists.
matrixRep :: (Additive v, Traversable v, Num n) => Transformation v n -> [[n]]
matrixRep (T m _ _) = map (toList . (m !*)) basis

-- | Convert a @'Transformation' v@ to a homogeneous matrix
--   representation.  The final list is the translation.  The
--   representation leaves off the last row of the matrix as it is
--   always [0,0, ... 1] and this representation is the de facto
--   standard for backends.
matrixHomRep :: (Additive v, Traversable v, Num n) => Transformation v n -> [[n]]
matrixHomRep t = mr ++ [toList tl]
  where
    mr = matrixRep t
    tl = transl t

-- | The determinant of (the linear part of) a 'Transformation'.
determinant :: (Additive v, Traversable v, Num n) => Transformation v n -> n
determinant = det . matrixRep
{-# NOINLINE determinant #-}

{-# RULES
  "det22" determinant = \(T m _ _) -> det22 m;
  "det33" determinant = \(T m _ _) -> det33 m;
  "det44" determinant = \(T m _ _) -> det44 m;
 #-}

-- | Determine whether a 'Transformation' includes a reflection
--   component, that is, whether it reverses orientation.
isReflection :: (Additive v, Traversable v, Num n, Ord n) => Transformation v n -> Bool
isReflection = (<0) . determinant
{-# INLINE isReflection #-}

-- | Compute the \"average\" amount of scaling performed by a
--   transformation.  Satisfies the properties
--
--   @
--   avgScale (scaling k) == k
--   avgScale (t1 <> t2)  == avgScale t1 * avgScale t2
--   @
--
avgScale :: forall v n. (Additive v, Traversable v, Floating n) => Transformation v n -> n
avgScale = \t -> (abs . determinant) t ** (recip . fromIntegral $ dimen (Proxy :: Proxy v))
{-# INLINE avgScale #-}

{-

avgScale is computed as the nth root of the positive determinant.
This works because the determinant is the factor by which a transformation
scales area/volume. See http://en.wikipedia.org/wiki/Determinant.

Proofs for the specified properties:

1. |det (scaling k)|^(1/n) = (k^n)^(1/n) = k
2. |det t1|^(1/n) * |det t2|^(1/n)
   = (|det t1| * |det t2|)^(1/n)
   = |det t1 * det t2|^(1/n)
   = |det (t1 <> t2)|^(1/n)

-}

------------------------------------------------------------------------
-- The Transformable class
------------------------------------------------------------------------

-- | Type class for things @t@ which can be transformed.
class Transformable t where

  -- | Apply a transformation to an object.
  transform :: Transformation (V t) (N t) -> t -> t

  default transform :: (V (f a) ~ V a,  N (f a) ~ N a, t ~ f a, Transformable a, Functor f) => Transformation (V t) (N t) -> t -> t
  transform = fmap . transform
  {-# INLINE transform #-}

instance (Additive v, Foldable v, Num n) => Transformable (Transformation v n) where
  transform = (<>)
  {-# INLINE transform #-}

instance (Additive v, Num n) => HasOrigin (Transformation v n) where
  moveOriginTo (P p) = \(T m mI v) -> T m mI (v ^-^ p)
  {-# INLINE moveOriginTo #-}

instance (SameSpace s t, Transformable t, Transformable s)
    => Transformable (t, s) where
  transform t (x,y) = ( transform t x
                      , transform t y
                      )

instance (SameSpace s t, SameSpace t u, Transformable t, Transformable s, Transformable u)
    => Transformable (t,s,u) where
  transform t (x,y,z) = ( transform t x
                        , transform t y
                        , transform t z
                        )

-- | Functions can be transformed by conjugation. That is,
--   reverse-transform the argument and forward-transform the
--   result. Intuition: If someone shrinks you, you see your
--   environment enlarged. If you rotate right, you see your
--   environment rotating left. Etc. This technique was used
--   extensively in Pan for modular construction of image
--   filters. Works well for curried functions, since all arguments
--   get inversely transformed.
instance ( InSpace v n s, SameSpace s t, Foldable v, Num n
         , Transformable t, Transformable s)
         => Transformable (s -> t) where
  transform tr f = transform tr . f . transform (inv tr)

instance (Transformable t, Ord t) => Transformable (S.Set t) where
  transform = S.map . transform

instance Transformable a => Transformable [a]
instance Transformable a => Transformable (Seq a)
instance Transformable a => Transformable (Tree a)
instance Transformable a => Transformable (Option a)
instance Transformable a => Transformable (Map k a)
instance Transformable a => Transformable (IntMap a)
instance Transformable a => Transformable (HashMap k a)
instance Transformable m => Transformable (Deletable m)

instance (Additive v, Foldable v, Num n) => Transformable (Point v n) where
  transform = papply
  {-# INLINE transform #-}

------------------------------------------------------------------------
-- Translational invariance
------------------------------------------------------------------------

-- | @TransInv@ is a wrapper which makes a transformable type
--   translationally invariant; the translational component of
--   transformations will no longer affect things wrapped in
--   @TransInv@.
newtype TransInv t = TransInv t
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Wrapped (TransInv t) where
  type Unwrapped (TransInv t) = t
  _Wrapped' = iso (\(TransInv t) -> t) TransInv

instance Rewrapped (TransInv t) (TransInv t')

type instance V (TransInv t) = V t
type instance N (TransInv t) = N t

instance HasOrigin (TransInv t) where
  moveOriginTo = const id

instance (Num (N t), Additive (V t), Transformable t) => Transformable (TransInv t) where
  transform (T a a' _) (TransInv t)
    = TransInv (transform (T a a' zero) t)

------------------------------------------------------------------------
-- Generic transformations
------------------------------------------------------------------------

-- | Create a translation.
translation :: (HasBasis v, Num n) => v n -> Transformation v n
translation = T eye eye
{-# INLINE translation #-}

-- | Translate by a vector.
translate :: (HasBasis (V t), Num (N t), Transformable t) => Vn t -> t -> t
translate = transform . translation
{-# INLINE translate #-}

-- | Create a uniform scaling transformation.
scaling :: (HasBasis v, Fractional n) => n -> Transformation v n
scaling s = fromLinear (fmap (s *^) eye) (fmap (^/ s) eye)
{-# INLINE scaling #-}

-- | Create a transformation from a vector specifying the scaling
--   factor along each dimension.
scalingV :: (HasLinearMap v, Fractional n) => v n -> Transformation v n
scalingV s = fromLinear (scaled s) (scaled $ fmap recip s)
{-# INLINE scalingV #-}

-- | Scale uniformly in every dimension by the given scalar.
scale :: (InSpace v n a, HasBasis v, Eq n, Fractional n, Transformable a)
      => n -> a -> a
scale s = transform $ scaling s
{-# INLINE scale #-}

-- | Scale by a given factor along each dimension, as specified by the
--   given vector.
scaleV :: (InSpace v n a, HasLinearMap v, Fractional n, Transformable a)
       => v n -> a -> a
scaleV s = transform $ scalingV s
{-# INLINE scaleV #-}

-- | Conjugate one transformation by another. @conjugate t1 t2@ is the
--   transformation which performs first @t1@, then @t2@, then the
--   inverse of @t1@.
conjugate :: (Additive v, Foldable v, Num n)
          => Transformation v n -> Transformation v n -> Transformation v n
conjugate t1 t2 = inv t1 <> t2 <> t1
{-# INLINE conjugate #-}

-- | Carry out some transformation \"under\" another one: @f ``underT``
--   t@ first applies @t@, then @f@, then the inverse of @t@.  For
--   example, @'scaleX' 2 ``underT`` 'rotation' (-1/8 \@\@ Turn)@
--   is the transformation which scales by a factor of 2 along the
--   diagonal line y = x.
--
--   Note that
--
-- @
-- (transform t2) `underT` t1 == transform (conjugate t1 t2)
-- @
--
--   for all transformations @t1@ and @t2@.
--
--   See also the isomorphisms like 'transformed', 'movedTo',
--   'movedFrom', and 'translated'.
underT :: (InSpace v n a, Foldable v, SameSpace a b, Transformable a, Transformable b)
      => (a -> b) -> Transformation v n -> a -> b
f `underT` t = transform (inv t) . f . transform t
{-# INLINE underT #-}

-- | Use a 'Transformation' to make an 'Iso' between an object
--   transformed and untransformed. This is useful for carrying out
--   functions 'under' another transform:
--
-- @
-- under (transformed t) f               == transform (inv t) . f . transform t
-- under (transformed t1) (transform t2) == transform (conjugate t1 t2)
-- transformed t ## a                    == transform t a
-- a ^. transformed t                    == transform (inv t) a
-- @
transformed :: (InSpace v n a, Foldable v, SameSpace a b, Transformable a, Transformable b)
            => Transformation v n -> Iso a b a b
transformed t = iso (transform $ inv t) (transform t)
{-# INLINE transformed #-}

-- | Use a 'Point' to make an 'Iso' between an object
--   moved to and from that point:
--
-- @
-- under (movedTo p) f == moveTo (-p) . f . moveTo p
-- over (movedTo p) f  == moveTo p . f . moveTo (-p)
-- movedTo p           == from (movedFrom p)
-- movedTo p ## a      == moveTo p a
-- a ^. movedTo p      == moveOriginTo p a
-- @
movedTo :: (InSpace v n a, SameSpace a b, HasOrigin a, HasOrigin b)
        => Point v n -> Iso a b a b
movedTo p = iso (moveTo (negated p)) (moveTo p)
{-# INLINE movedTo #-}

-- | Use a 'Transformation' to make an 'Iso' between an object
--   transformed and untransformed. We have
--
-- @
-- under (movedFrom p) f == moveTo p . f . moveTo (-p)
-- movedFrom p           == from (movedTo p)
-- movedFrom p ## a      == moveOriginTo p a
-- a ^. movedFrom p      == moveTo p a
-- over (movedFrom p) f  == moveTo (-p) . f . moveTo p
-- @
movedFrom :: (InSpace v n a, SameSpace a b, HasOrigin a, HasOrigin b)
          => Point v n -> Iso a b a b
movedFrom p = iso (moveOriginTo (negated p)) (moveOriginTo p)
{-# INLINE movedFrom #-}

-- | Use a vector to make an 'Iso' between an object translated and
--   untranslated.
--
-- @
-- under (translated v) f == translate (-v) . f . translate v
-- translated v ## a      == translate v a
-- a ^. translated v      == translate (-v) a
-- over (translated v) f  == translate v . f . translate (-v)
-- @
translated :: (InSpace v n a, HasBasis v, Foldable v, SameSpace a b, Transformable a, Transformable b)
           => v n -> Iso a b a b
translated = transformed . translation
{-# INLINE translated #-}

