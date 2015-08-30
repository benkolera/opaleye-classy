# opaleye-classy

Classy MTL extension of the lovely Opaleye library, which simply just wraps things up in a MonadReader , MonadError context where the config/error are constrained by Classy Lenses/Prisms rather than by concrete non-extensible types.

More info on this pattern can be found in George Wilson's BFPG talk:

http://talks.bfpg.org/talks/2015-06-09.next_level_mtl.html

## Warnings

  - This will only work if you only talk to one DB in your transformer stack. 
  - This API is missing a few IO operations from [Opaleye][1]. If you need other functions exported then let me know or drop in a PR.

[1]: http://hackage.haskell.org/package/opaleye-0.4.0.0/docs/Opaleye-Manipulation.html
