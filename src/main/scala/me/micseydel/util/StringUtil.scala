package me.micseydel.util

object StringUtil {
  def truncateText(rawText: String, maxLength: Int = 200): String = {
    val strippedText = rawText.strip()
    if (strippedText.length > maxLength) {
      strippedText.take(maxLength) + "..."
    } else {
      strippedText
    }
  }
}

object StringImplicits {
  implicit class RichString(val s: String) extends AnyVal {
    def splitLikePy: Array[String] = s.split("\\s+")
    def wordCount: Int = s.splitLikePy.count(_.nonEmpty)
  }
}
