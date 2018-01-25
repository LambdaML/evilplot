/*
 * Copyright 2017 CiBO Technologies
 */

package com.cibo.evilplot.plot

import com.cibo.evilplot.{StrokeStyle, Utils}
import com.cibo.evilplot.colors.{Colors, HEX, HTMLNamedColors}
import com.cibo.evilplot.colors.Colors.{ScaledColorBar, SingletonColorBar}
import com.cibo.evilplot.geometry.{AffineTransform, Disc, Drawable, Extent, Path}
import com.cibo.evilplot.numeric.{Bounds, MarchingSquares, Point, Segment}
import com.cibo.evilplot.plotdefs.XYPosteriorPlotDef

class PosteriorPlot(val chartSize: Extent, data: XYPosteriorPlotDef)
    extends Chart
    with ContinuousAxes {
  val options = data.options
  private val numContours = data.numContours
  private val grid = data.gridData
  val defaultXAxisBounds: Bounds = data.xBounds.get
  val defaultYAxisBounds: Bounds = data.yBounds.get

  def plottedData(extent: Extent): Drawable = {
    val xBounds = xAxisDescriptor.axisBounds
    val yBounds = yAxisDescriptor.axisBounds
    val affine = AffineTransform(shiftX = -xBounds.min).scale(x = extent.width / xBounds.range)
        .compose(AffineTransform(scaleY = -1).translate(dx = 0, dy = yBounds.max)
          .scale(y = extent.height / yBounds.range))

    val colorBar: Colors.ColorBar = data.colorBar
    val binWidth = data.zBounds.range / numContours
    val levels = Seq.tabulate[Double](numContours - 1)(bin =>
      grid.zBounds.min + (bin + 1) * binWidth)
    val contours = {
      (for {
        z <- levels
        contourSegments = MarchingSquares.getContoursAt(z, grid)
        if contourSegments.nonEmpty
      } yield
        contourSegments.map { seg: Segment =>
          StrokeStyle {
            colorBar match {
              case SingletonColorBar(color) => color
              case colors: ScaledColorBar   => colors.getColor(z)
            }
          }(Path(Seq(affine(seg.a), affine(seg.b)), 2))
        }).flatten.group
    }
    val priors = makePaths(data.priors, affine)
    val best = Utils.maybeDrawable(data.best)(b => Disc(3, affine(b)) filled HTMLNamedColors.red)
    priors.group behind contours behind best
  }

  private def makePaths(pathPoints: Seq[Seq[Point]],
                        affine: AffineTransform): Seq[Drawable] = {
    pathPoints.map { points =>
      if (points.take(1).nonEmpty && points.tail.isEmpty)
        Disc(3, affine(points.head)) filled HEX("#008000")
      else Path(points.map(affine.apply) :+ affine(points.head), 2) colored HEX("#008000")
    }
  }
}