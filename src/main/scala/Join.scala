package join

import shapeless._

object Join {
  case class Select[T](fields: List[String])
  case class WhereField[T](field: String)
  case class WhereExpression[S, T](a: WhereField[S], b: WhereField[T])

  type SelectHList[T] = Select[T] :: HList
  type Field          = Witness.Lt[Symbol]

  def select[T](field: Field)(implicit mkl: MkFieldLens[T, field.T]): SelectHList[T] =
    Select[T](List(fieldName[T](field))) :: HNil

  def select[T](field1: Field, field2: Field)(implicit mkl1: MkFieldLens[T, field1.T],
                                              mkl2: MkFieldLens[T, field2.T]): SelectHList[T] =
    Select[T](List(fieldName[T](field1), fieldName[T](field2))) :: HNil

  def select[T](field1: Field, field2: Field, field3: Field)(implicit mkl1: MkFieldLens[T, field1.T],
                                                             mkl2: MkFieldLens[T, field2.T],
                                                             mkl3: MkFieldLens[T, field3.T]): SelectHList[T] =
    Select[T](List(fieldName[T](field1), fieldName[T](field2), fieldName[T](field3))) :: HNil

  implicit class SelectOps[L <: SelectHList[_]](l: L) {
    def and[S](field: Field)(implicit mkl: MkFieldLens[S, field.T]) =
      Select[S](List(fieldName[S](field))) :: l

    def and[S](field1: Field, field2: Field)(implicit mkl1: MkFieldLens[S, field1.T], mkl2: MkFieldLens[S, field2.T]) =
      Select[S](List(fieldName[S](field1), fieldName[S](field2))) :: l

    def where[S](field: Field)(implicit mkl: MkFieldLens[S, field.T]): WhereField[S] :: HList =
      WhereField[S](fieldName[S](field)) :: l
  }

  implicit class WhereOps[W, X](l: WhereField[W] :: HList) {
    def isEqualTo[S](field: Field)(implicit mkl1: MkFieldLens.Aux[S, field.T, X],
                                   mkl2: MkFieldLens.Aux[W, field.T, X]) =
      WhereExpression(l.head: WhereField[W], WhereField[S](fieldName[S](field))) :: l.tail
  }

  implicit class WhereExpressionOps(l: WhereExpression[_, _] :: HList) {
    def and[S](field: Field)(implicit mkl: MkFieldLens[S, field.T]): WhereField[S] :: HList =
      WhereField[S](fieldName[S](field)) :: l
  }

  def fieldName[A](field: Field)(implicit proof: MkFieldLens[A, field.T]) =
    field.value.name
}

object ExampleMain extends App {

  import Join._

  case class Series(id: String, name: String)
  case class Season(id: String, number: Int, parent: String)
  case class Title(id: String, name: String, episodeNumber: Int, parent: String)

  //OK
  println(
    select[Series]('id, 'name)
      .and[Season]('number)
      .where[Season]('parent)
      .isEqualTo[Series]('id)
      .and[Title]('parent)
      .isEqualTo[Season]('id)
  )
  //>WhereExpression(WhereField(parent),WhereField(id)) :: WhereExpression(WhereField(parent),WhereField(id)) :: Select(List(number)) :: Select(List(id, name)) :: HNil

  /*
  doesn't compile as field not on Series:

  select[Series]('spellingmistake, 'name)
    .and[Season]('number)
    .where[Season]('parent)
    .isEqualTo[Series]('id)
    .and[Title]('parent)
    .isEqualTo[Season]('id)

  doesn't compile as isEqualTo Title::parent different type to Season::number:

  select[Series]('id, 'name)
    .and[Season]('number)
    .where[Season]('parent)
    .isEqualTo[Series]('id)
    .and[Title]('parent)
    .isEqualTo[Season]('number)
 */
}
