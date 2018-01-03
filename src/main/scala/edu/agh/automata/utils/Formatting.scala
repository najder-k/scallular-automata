package edu.agh.automata.utils


object Formatting {
  def formattedNanos(duration: Long): String =
    duration.formatted("%8d") + "ns"
  def formattedMicros(duration: Long): String =
    (duration / 1000.0).formatted("%8.4f") + "μs"
  def formattedMillis(duration: Long): String =
    (duration / 1000000.0).formatted("%8.4f") + "ms"
  def formattedSeconds(duration: Long): String =
    (duration / 1000000000.0).formatted("%8.4f") + " s"
  def formatDuration(duration: Long): String = {
    val length = math.log10(duration)
    if      (length <= 3) formattedNanos(duration)
    else if (length <= 6) formattedMicros(duration)
    else if (length <= 9) formattedMillis(duration)
    else                  formattedSeconds(duration)
  }
}

