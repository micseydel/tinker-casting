package me.micseydel.util

import cats.data.ValidatedNel
import cats.implicits.{catsSyntaxOption, catsSyntaxValidatedId}
import me.micseydel.model.Entity

object RasaUtil {
  def entitiesMapToValidated[T](
         entitiesMap: Map[String, String],
         entityName: String,
         adapter: String => Option[T]
       ): ValidatedNel[String, T] = {
    entitiesMap.get(entityName).toValidNel(s"no $entityName entity")
      .andThen(entityValue =>
        adapter(entityValue)
          .toValidNel(s"$entityValue is not a valid $entityName choice")
      )
  }

  def entitiesMapToValidatedOptional[T](entitiesMap: Map[String, String], entityName: String, adapter: String => Option[T]): ValidatedNel[String, Option[T]] = {
    if (entitiesMap.contains(entityName)) {
      RasaUtil.entitiesMapToValidated(entitiesMap, entityName, adapter)
        .andThen(Some(_).validNel)
    } else {
      None.validNel
    }
  }

  def entitiesToMap(entities: List[Entity]): Map[String, String] = {
    entities
      .map(el => el.entity -> el.value)
      .toMap
  }
}
