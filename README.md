# opaleye-classy

Classy MTL extension of the lovely Opaleye library, which simply just wraps things up in a MonadReader , MonadError context where the config/error are constrained by Classy Lenses/Prisms rather than by concrete non-extensible types.

More info on this pattern can be found in George Wilson's BFPG talk:

http://talks.bfpg.org/talks/2015-06-09.next_level_mtl.html

Note: This API may not be complete. If you need other functions exported then let me know or drop in a PR.
