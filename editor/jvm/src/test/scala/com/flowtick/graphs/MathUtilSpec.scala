package com.flowtick.graphs

import com.flowtick.graphs.util.MathUtil
import com.flowtick.graphs.util.MathUtil.{LineSegment, Vector2}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MathUtilSpec extends AnyFlatSpec with Matchers {
  "MathUtil" should "find intersection of line segments" in {
    val a = LineSegment(Vector2(0, 0), Vector2(5, 5))
    val b = LineSegment(Vector2(0, 5), Vector2(5, 0))

    MathUtil.segmentIntersect(a, b) should be(
      Some(
        Vector2(2.5, 2.5)
      )
    )

    // no intersection
    val c = LineSegment(Vector2(3, 0), Vector2(3, 4))
    val d = LineSegment(Vector2(0, 5), Vector2(5, 5))

    MathUtil.segmentIntersect(c, d) should be(None)

    // collinear / overlapping
    val e = LineSegment(Vector2(0, 0), Vector2(2, 0))
    val f = LineSegment(Vector2(1, 0), Vector2(3, 0))

    MathUtil.segmentIntersect(e, f) should be(None)
    MathUtil.segmentIntersect(
      e,
      f,
      considerCollinearOverlapAsIntersect = true
    ) should be(Some(Vector2(1, 0)))
  }
}
