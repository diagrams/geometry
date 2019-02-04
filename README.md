## geometry

A Haskell geometry library build on top of [`linear`](http://hackage.haskell.org/package/linear).

### Overview of changes from diagrams

* Transformations are now in strict matrix form. The linear matrix is
  stored along with its inverse and a translation:

  ```
  data Transformation v n = T !(v (v n)) !(v (v n)) !(v n)
  ```

  This should give us O(1) transform, and I hope it's more efficient
  overall but I haven't benchmarked this yet.

* Envelopes now return an `Interval` instead of a single value. The
  calculations to compute the lower interval are exactly the same, we
  just take the `min` instead of the `max`. This means that many
  operations like `boundingBox` and `hcat` are essentially twice as
  fast.

  The internal envelope function now always expects a unit vector.
  This means we no longer need to calculate the norm of the vector at
  every step.

  Since envelopes are an integral part of diagrams, a lot of time has
  been spent optimising them. Envelopes for trails are around 20x faster
  (and almost to 100x faster if we use unboxed trails (needs more
  benchmarking)).

* Segments are now separated into `Segment` and `ClosingSegment`. A
  large part of this change is for performance reasons. There's one
  fewer constructors to unravel and ghc seemed to have a hard time
  optimising the previous GADT layout (especially for unboxed trails).

  I also believe this is a simpler representation to use, especially for
  beginners. The old system of `Open` segments are used to close loops
  was confusing (at least to me).

* Lines now only cache the total offset and use `Data.Sequence` instead
  of `Data.FingerTree`s:

  ```
  Line v n = Line (Seq (Segment v n)) !(v n)
  ```

  The old system of caching envelopes didn't really work since you still
  have to go through each segment to calculate an envelope anyway. The
  total offset is still cached because it's used for operations like
  `closeLine` and is relatively cheap to cache. I don't cache the
  arc-length because this is less used value, much more expensive to
  calculate and can't be done exactly (in general). I plan to add other
  means for working with the arc length.

  There is still some benefit to using `Data.FingerTree` even if we're
  only caching the offset. Split operations with `Data.Sequence` are
  O(n) since we need to recompute the offset up to the split, but it's
  O(log n) for fingertrees. However the `splitAtParam` instance for
  lines is dubious at best (I can't see much practical use for it).
  Operations that do need to split lines, like path intersections, need
  to go through each segment anyway.

  `Data.Sequence` is a fingertree internally but I found it around 50%
  faster than `Data.FingerTree` so I've stuck with it.

* The old `Trail'` is gone. The hierarchy is now:

  ```
  Line v n = Line (Seq (Segment v n)) !(v n)
  Loop v n = Loop (Line v n) (ClosingSegment v n)
  Trail v n = Line v n | Loop v n
  ```

  Again, I believe this is easier for beginners to understand as well as
  easier for ghc to optimise.

* The `TrailLike` class has changed its name to `FromTrail`. I felt like
  `TrailLike` wasn't clear and `FromTrail` is a more consistent with
  other things like `ToPath`, `HasX` etc.

* `Diagrams.Core.V` has moved to `Geometry.Space`. Also many of the
  alias type classes (`OrderedField`, etc.) have been moved here.

* `Diagrams.Tangent` has merged into `Geometry.Parametric` since this is
  all it really gets used for. (I'm trying to reduce the total number
  of modules since there's so many)

* The `Alignable` class has been removed. Alignment functions now use
  `Enveloped`. (The previous `Alignable` instances either used enveloped
  anyway, or had bogus definitions (like for functions)). This also
  lets us make more performant definitions for things like `hcat`.

* Generally type constructors are strict and exposed. This is aimed at
  wider audience than just diagrams users and sometimes exposing the
  constructors allows more flexibility, especially for performance. It
  also may help people understand how something works.

  I'm undecided whether `Geometry` should hide the constructors (for
  example you'd have to import `Geometry.BoundingBox` explicitly to get
  the bounding box constructor).

### Things to still think about

* I've added a `Rotational` class to help with 3D rotations (which turn
  out to be pretty tricky). I've also added Euler angles which are also
  pretty confusing and have no canonical form, I've just picked the one
  I needed for my camera. I'm still unsure about these.

* I've rewritten the `Geometry.ThreeD.Camera` so I could use it with the
  GL backend. I'm still not happy with it. Part of the problem is
  there's no canonical representation for a camera (look vectors or
  euler angles or something else). Also I use GL camera conventions and
  it would be difficult for someone to use it with a 3D system with
  another camera convention.

* I have written a working implementation for unboxed trails. These
  aren't as nice for constructing paths since they have O(n)
  concatenation, but once a path has been constructed they are around
  4-8x faster for things like envelope calculations. Making them ideal
  for storing in diagrams.

  The implementation is more difficult, we have things like an ugly
  Unbox (v n) constraint and I'm not sure the best internal
  representation for them. I've left them out for now. There's already
  so much to do, I don't know if they'll make it to the initial release.

* I haven't added the `Deformable` class yet. I'd like to a have a
  proper way to project 3D onto 2D but for now I'll probably just port
  `Deformable` as it is.

### Future work (doubt it'll make the initial release)

* The cubic spline implementation is pretty inefficient. It keeps the
  whole list in memory and traverses it multiple times. It also has an
  ugly `Num (v n)` constraint.

* I feel like we need to add quadratic segments sooner or later. They
  have lots of nice properties like being closed under projective
  transforms and an exact formula for arc length. They're also used a
  lot in fonts and rendering.

* I'd like to have to have boolean path operations. I know kirstof's
  `cubicbezier` package has them and we use them in `diagrams-contrib`
  but I don't feel like it's a proper geometry library without native
  boolean operations.


