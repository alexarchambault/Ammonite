package fansi

object FansiHelper {
  def attr(resetMask: Long, applyMask: Long): Attrs =
    ResetAttr(resetMask, applyMask)
  def attr(attrs: Attrs): Attrs =
    attr(attrs.resetMask, attrs.applyMask)
}
