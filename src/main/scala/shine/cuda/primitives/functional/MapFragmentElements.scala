package shine.cuda.primitives.functional

import shine.DPIA.->:
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.expPrimitive

/**
  * Returns a fragment with the values of an applied function to elements of a fragment. <br>
  * This primitive needs to be executed by a full warp!
  * @param fragType type of the fragment
  * @param fragment fragment of type `fragType` on whose elements the function should be applied
  * @param fun      function which takes an element of type `fragType.dataType` and
  *                 returns an element of type `fragType.dataType`
  */
@expPrimitive
case class MapFragmentElements(fragType: FragmentType,
                               fragment: Phrase[ExpType],
                               fun: Phrase[ExpType ->: ExpType],
                              ) extends ExpPrimitive {

  fragment :: ExpType(fragType, read)
  fun :: ExpType(fragType.dataType, read) ->: ExpType(fragType.dataType, write)

  override val t: ExpType = ExpType(fragType, write)
}
