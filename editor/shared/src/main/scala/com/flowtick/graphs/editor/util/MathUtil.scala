package com.flowtick.graphs.editor.util

/** line segment intersection adapted from
  * https://www.codeproject.com/tips/862988/find-the-intersection-point-of-two-line-segments
  */
object MathUtil {
  final case class Vector2(x: Double, y: Double) {
    def -(other: Vector2): Vector2 = copy(x = x - other.x, y = y - other.y)
    def +(other: Vector2): Vector2 = copy(x = x + other.x, y = y + other.y)
    def *(other: Vector2): Double = x * other.x + y * other.y
    def times(factor: Double): Vector2 = copy(x = x * factor, y = y * factor)
    def cross(other: Vector2): Double = x * other.y - y * other.x
    def same(other: Vector2): Boolean =
      MathUtil.isZero(x - other.x) && MathUtil.isZero(y - other.y)
  }

  final case class LineSegment(start: Vector2, end: Vector2)

  final case class Rectangle(topLeft: Vector2, bottomRight: Vector2) {
    def top: LineSegment = LineSegment(topLeft, topLeft.copy(x = bottomRight.x))
    def left: LineSegment =
      LineSegment(topLeft, topLeft.copy(y = bottomRight.y))
    def bottom: LineSegment =
      LineSegment(bottomRight, bottomRight.copy(x = topLeft.x))
    def right: LineSegment =
      LineSegment(bottomRight, bottomRight.copy(y = topLeft.y))
  }

  private val Epsilon: Double = 1e-10

  def isZero(d: Double): Boolean = Math.abs(d) < Epsilon

  def segmentIntersect(
      a: LineSegment,
      b: LineSegment,
      considerCollinearOverlapAsIntersect: Boolean = false
  ): Option[Vector2] = {
    val p = a.start
    val p2 = a.end
    val q = b.start
    val q2 = b.end

    val r = p2 - p
    val s = q2 - q
    val rxs = r cross s
    val qpxr = (q - p) cross r

    // If r x s = 0 and (q - p) x r = 0, then the two lines are collinear.
    if (isZero(rxs) && isZero(qpxr)) {
      // 1. If either  0 <= (q - p) * r <= r * r or 0 <= (p - q) * s <= * s
      // then the two lines are overlapping,
      if (
        considerCollinearOverlapAsIntersect && ((0 <= (q - p) * r && (q - p) * r <= r * r) || (0 <= (p - q) * s && (p - q) * s <= s * s))
      ) {
        // note: on line overlap there will be two intersection points, the choice here is random
        Some(b.start + ((a.end - a.start) - (b.end - b.start)))
      } else None
      // 2. If neither 0 <= (q - p) * r = r * r nor 0 <= (p - q) * s <= s * s
      // then the two lines are collinear but disjoint.
    }
    // 3. If r x s = 0 and (q - p) x r != 0, then the two lines are parallel and non-intersecting.
    else if (isZero(rxs) && !isZero(qpxr)) {
      None
    } else {
      // t = (q - p) x s / (r x s)
      val t = ((q - p) cross s) / rxs

      // u = (q - p) x r / (r x s)
      val u = ((q - p) cross r) / rxs

      // 4. If r x s != 0 and 0 <= t <= 1 and 0 <= u <= 1
      // the two line segments meet at the point p + t r = q + u s.
      if (!isZero(rxs) && (0 <= t && t <= 1) && (0 <= u && u <= 1)) {
        // We can calculate the intersection point using either t or u.
        Some(p + (r times t))
      } else None
      // 5. Otherwise, the two line segments are not parallel but do not intersect.
    }
  }

  def pointInRect(point: Vector2, rect: Rectangle): Boolean =
    rect.topLeft.x < point.x &&
      point.x < rect.bottomRight.x &&
      rect.topLeft.y < point.y &&
      point.y < rect.bottomRight.y

  def rectIntersect(segment: LineSegment, rect: Rectangle): Option[Vector2] = {
    // both outside?
    val bothOut =
      !pointInRect(segment.start, rect) && !pointInRect(segment.end, rect)
    val bothIn =
      pointInRect(segment.start, rect) && pointInRect(segment.end, rect)

    if (bothIn || bothOut) {
      None
    } else
      segmentIntersect(segment, rect.top)
        .orElse(segmentIntersect(segment, rect.bottom))
        .orElse(segmentIntersect(segment, rect.left))
        .orElse(segmentIntersect(segment, rect.right))
  }

}
