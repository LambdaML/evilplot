package com.cibo.evilplot.plot

import com.cibo.evilplot.colors.Color
import com.cibo.evilplot.geometry.{Clipping, Drawable, EmptyDrawable, Extent, LineDash, LineStyle, Path, StrokeStyle}
import com.cibo.evilplot.numeric.{Datum2d, _}
import com.cibo.evilplot.plot.BoxPlot.makePlot
import com.cibo.evilplot.plot.LinePlot.LinePlotRenderer
import com.cibo.evilplot.plot.ScatterPlot.ScatterPlotRenderer
import com.cibo.evilplot.plot.aesthetics.Theme
import com.cibo.evilplot.plot.renderers.BoxRenderer.BoxRendererContext
import com.cibo.evilplot.plot.renderers.{BoxRenderer, PathRenderer, PlotRenderer, PointRenderer}
import com.cibo.evilplot.plot.renderers.PathRenderer.calcLegendStrokeLength

trait TransformWorldToScreen {
  type Transformer = Double => Double

  def xCartesianTransformer(xBounds: Bounds, plotExtent: Extent): Double => Double = {
    val scale = plotExtent.width / xBounds.range
    (x: Double) => (x - xBounds.min) * scale
  }

  def yCartesianTransformer(yBounds: Bounds, plotExtent: Extent): Double => Double = {
    val scale = plotExtent.height / yBounds.range
    (y: Double) => { plotExtent.height - (y - yBounds.min) * scale}
  }

  def createTransformers(yBounds: Bounds, xBounds: Bounds, plotExtent: Extent): (Double => Double, Double => Double) = {
    val xtransformer = xCartesianTransformer(xBounds, plotExtent)
    val ytransformer = yCartesianTransformer(yBounds, plotExtent)

    (xtransformer, ytransformer)
  }

  def transformDatumToWorld[X <: Datum2d[X]](point: X,
                                             xtransformer: Transformer,
                                             ytransformer: Transformer): X = {
    val x = xtransformer(point.x)
    val y = ytransformer(point.y)
    point.setXY(x = x,y = y)
  }

  def transformDatumToPlotSpace[X <: Datum2d[X]](data: Seq[X],
                                                 xtransformer: Transformer,
                                                 ytransformer: Transformer): Seq[X] = {

    data.map( p => transformDatumToWorld(p, xtransformer, ytransformer))
  }

}

object TransformWorldToScreen extends TransformWorldToScreen

case class PlotContext(plot: Plot,
                       plotExtent: Extent){

  lazy val xBounds: Bounds = plot.xbounds
  lazy val yBounds: Bounds = plot.ybounds

  def xCartesianTransform: Double => Double = TransformWorldToScreen.xCartesianTransformer(xBounds, plotExtent)
  def yCartesianTransform: Double => Double = TransformWorldToScreen.yCartesianTransformer(yBounds, plotExtent)

  def transformDatumToWorld[X <: Datum2d[X]](point: X): X = TransformWorldToScreen.transformDatumToWorld(point, xCartesianTransform, yCartesianTransform)
  def transformDatumsToWorld[X <: Datum2d[X]](points: Seq[X]): Seq[X] = points.map(transformDatumToWorld)

}

object PlotContext {
  def fromPlotExtent(plot: Plot, extent: Extent): PlotContext = apply(plot, extent)
}

object PlotUtils {


  def bounds[X <: Datum2d[X]](data: Seq[X],
                              defaultBoundBuffer: Double,
                              xboundBuffer: Option[Double] = None,
                              yboundBuffer: Option[Double] = None): (Bounds, Bounds) = {
    require(xboundBuffer.getOrElse(0.0) >= 0.0)
    require(yboundBuffer.getOrElse(0.0) >= 0.0)
    val xs = data.map(_.x)
    val xbuffer = xboundBuffer.getOrElse(defaultBoundBuffer)
    val ybuffer = yboundBuffer.getOrElse(defaultBoundBuffer)
    val xbounds = Plot.expandBounds(
      Bounds(
        xs.reduceOption[Double](math.min).getOrElse(0.0),
        xs.reduceOption[Double](math.max).getOrElse(0.0)),
      if (data.length == 1 && xbuffer == 0) 0.1 else xbuffer
    )

    val ys = data.map(_.y)
    val ybounds = Plot.expandBounds(
      Bounds(
        ys.reduceOption[Double](math.min).getOrElse(0.0),
        ys.reduceOption[Double](math.max).getOrElse(0.0)),
      if (data.length == 1 && ybuffer == 0) 0.1 else xbuffer
    )
    (xbounds, ybounds)
  }

}
