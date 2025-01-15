package me.micseydel.vault

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import me.micseydel.util.FileSystemUtil

import java.nio.file.{Files, Path}

final class VaultPath private (val path: Path) extends AnyVal {
  def resolve(other: String): Path = path.resolve(other)
  def resolve(other: Path): Path = path.resolve(other)

  override def toString: String = path.toString
}

object VaultPath {
  def apply(path: Path): Validated[String, VaultPath] = {
    if (Files.exists(path)) {
      Valid(new VaultPath(path))
    } else {
      Invalid(s"File $path does not exist")
    }
  }

  def apply(path: String): Validated[String, VaultPath] =
    FileSystemUtil.validateDirectory(path).map(new VaultPath(_))
}