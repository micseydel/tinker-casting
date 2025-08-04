package me.micseydel.util

import cats.data.NonEmptyList
import net.jcazevedo.moultingyaml
import net.jcazevedo.moultingyaml.{PimpedAny, YamlArray, YamlFormat, YamlValue}

object YamUtil {
  def nonEmptyListYamlFormat[T]()(implicit listYamlFormat: YamlFormat[List[T]], yamlFormat: YamlFormat[T]): YamlFormat[NonEmptyList[T]] = new YamlFormat[NonEmptyList[T]] {
    def write(m: NonEmptyList[T]): YamlValue = {
      YamlArray(m.toList.toVector.map(_.toYaml))
    }

    def read(value: YamlValue): NonEmptyList[T] = {
      value.convertTo[List[T]] match {
        case head :: tail => NonEmptyList(head, tail)
        case Nil => throw moultingyaml.DeserializationException("list was empty")
      }
    }
  }
}
