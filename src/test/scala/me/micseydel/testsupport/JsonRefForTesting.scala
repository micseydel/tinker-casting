package me.micseydel.testsupport

import me.micseydel.NoOp
import me.micseydel.vault.persistence.JsonRef

import java.io.FileNotFoundException
import scala.util.{Failure, Success, Try}

object JsonRefForTesting {
  def apply(jsonName: String): JsonRef = new JsonRef {
    override val filename: String = jsonName

    var contents: Option[String] = None

    override def set(contents: String): Try[NoOp.type] = {
      this.contents = Some(contents)
//      TestHelpers.log(s"""[json/$jsonName].set("${TestHelpers.compressedNewlines(contents)}") returning Success""")
      Success(NoOp)
    }

    override def read(): Try[String] = {
      this.contents match {
        case Some(value) =>
//          TestHelpers.log(s"""[json/$jsonName].read() returning Success("$value")""")
          Success(value)
        case None =>
          Failure(new FileNotFoundException(s"$jsonName has not yet had a set() call"))
      }
    }

    override def append(contents: String): Try[NoOp.type] = {
      this.contents = Some(this.contents match {
        case Some(existing) => existing + contents
        case None => contents
      })

      Success(NoOp)
    }
  }
}
